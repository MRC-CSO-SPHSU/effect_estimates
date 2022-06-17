# import libraries, it is assumed they've been installed already
library(data.table) # parallel data manipulation
library(gsubfn) # string templates
library(readstata13) # to read from .dta files
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)

# set the number of threads to decrease the time of file reading/writing
data.table::getDTthreads()
data.table::setDTthreads(8)

# a list of variables in the main table
main_out_colnames =  c('dhm', 'les_c4', 'laboursupplyweekly', 'out_ghqcase',
                       'equivaliseddisposableincomeyearl', 'atriskofpoverty')

ids = c("scenario", "run", "time")

main_loop <- function(eid_, dft) {
  per_run <- get_summary_statistics(ids, dft, eid_)
  per_run <- rename_final_columns(per_run, eid_)

  per_all <- get_summary_statistics(c("time", "scenario"), dft, eid_)
  per_all <- rename_final_columns(per_all, eid_)
  bind_rows(per_run, per_all)
}

rename_final_columns <- function(data_table_name, postfix) {
  # TODO this can be wrapped into a function as the number of columns increases
  names(data_table_name)[names(data_table_name) == 'dhm'] <- paste("out_ghq", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'out_ghqcase'] <- paste("out_ghqcase", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'les_c4'] <- paste("out_emp", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'laboursupplyweekly'] <- paste("out_emphrs", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'equivaliseddisposableincomeyearl'] <- paste("out_income", postfix, sep="_")
  names(data_table_name)[names(data_table_name) == 'atriskofpoverty'] <- paste("out_poverty", postfix, sep="_")
  data_table_name
}

get_summary_statistics <- function(main_column_names, # common columns for all minor tables, run id is missing when summarize over all runs
                                   data_table_name, experiment_stage) {
  action_columns <- list(averaging = main_out_colnames)
  action_list <- list(averaging = mean) # list of functions to evaluate
  todo <- tibble(action_columns, action_list) # combinations of functions applied to corresponding columns

  pmap_dfc(todo, ~data_table_name[experiment==experiment_stage] %>% group_by(across(all_of(main_column_names))) %>% summarise(across(all_of(.x), .y), .groups = 'keep'))
}

get_effects <-function(data_table) {
  column_names <- c('ghq', 'ghqcase', 'emp', 'emphrs', 'income', 'poverty')
  for (n in column_names) {
    data_table[paste("eff", n, sep="_")] <- data_table[paste("out", n, "baseline", sep="_")] - data_table[paste("out", n, "reform", sep="_")]
  }
  data_table
}

df <- data.table()
#scenario_id <- c("S1", "S2", "S3")
scenario_id <- c("S1")
experiment_id <- c("baseline", "reform")
# no parallel data reading at the moment
for (sid in scenario_id) {
  for (eid in experiment_id) {
    # reads the file, only one at the moment. Decompression takes some time,
    # but compressed files use 5 times less space. We also read
    # a selected set of columns to save memory/time
    fn <- paste(paste("data", sid, eid, sep="/"), "csv.gz", sep=".")
    df_scratch <- fread(file=fn,
                        select = c('laboursupplyweekly', 'atriskofpoverty',
                                   'equivaliseddisposableincomeyearl',
                                   'n_children_0', 'n_children_1',
                                   'n_children_10', 'n_children_11', 'run',
                                   'n_children_12','n_children_13', 'dag',
                                   'n_children_14', 'n_children_15', 'dgn',
                                   'n_children_16', 'n_children_17', 'deh_c3',
                                   'n_children_2', 'n_children_3', 'time',
                                   'n_children_4', 'n_children_5', 'dhm',
                                   'n_children_6', 'n_children_7', 'les_c4',
                                   'n_children_8', 'n_children_9'))

    # add scenario/arm id now at the cost of memory
    df_scratch[, scenario:=sid]
    df_scratch[, experiment:=eid]
    # do the type conversion right within the loop
    # no check for NA for now
    df_scratch$grp_hchild <- select(df_scratch, contains("n_children")) %>% rowSums %>% as.logical
    df_scratch$grp_nchild <- !df_scratch$grp_hchild

    # drop number of children columns
    df_scratch <- df_scratch %>% select(-starts_with("n_children"))

    # glue together
    df <- rbind(df, df_scratch)
  }
}
# free memory
rm(df_scratch)
gc()

# convert certain columns to categorical
df$scenario <- as.factor(df$scenario)
df$experiment <- as.factor(df$experiment)
df$dgn <- as.factor(df$dgn)
df$deh_c3 <- as.factor(df$deh_c3)

gc()

# convert initial raw data into something to work with
unique_years <- unique(df$time) # all available year values
unique_runs <- unique(df$run) # all values of run ids

unique_weekly_hours <- unique(df$laboursupplyweekly) # string values of working hours/week
unique_weekly_hours_numeric <- c(0, 20, 10, 40, 30) # integer values of unique_weekly_hours, used to convert chars to ints properly

unique_labour_status <- unique(df$les_c4) # employment status
unique_labour_status_numeric <- c(0, 1, 0, 0) # conversion to employed/unemployed as integer/boolean

unique_atriskofpoverty_status <- unique(df$atriskofpoverty) # "0" "1" "null" as no, yes, missing value (interpreted as yes)
unique_atriskofpoverty_status_numeric <- c(0, 1, 1) # integer/boolean values of poverty status

df$out_ghqcase <- ifelse(df$dhm<=24, 1, 0) # cut quasi-continuous interval into two sub-intervals

# atriskofpoverty is chars, check for empty strings, char nulls
# les_c4 is the same

# convert poverty status to ints
df$atriskofpoverty <- as.integer(format(factor(df$atriskofpoverty,
                                           levels = unique_atriskofpoverty_status,
                                           labels = unique_atriskofpoverty_status_numeric))) # kind of slow

# convert labour category to ints
df$laboursupplyweekly <- as.integer(format(factor(df$laboursupplyweekly,
                                                  levels = unique_weekly_hours,
                                                  labels = unique_weekly_hours_numeric))) # kind of slow

# convert employment category to ints
df$les_c4 <- as.integer(format(factor(df$les_c4,
                                       levels = unique_labour_status,
                                       labels = unique_labour_status_numeric))) # kind of slow

gc()

result <- lapply(experiment_id, main_loop, dft=df) %>% reduce(left_join, by = ids)
result$grp_all <- TRUE
result <- result %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[dgn=="Male"]) %>% reduce(left_join, by = ids)
subpop_result$grp_male <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[dgn=="Female"]) %>% reduce(left_join, by = ids)
subpop_result$grp_female <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[dag >= 25 & dag < 45]) %>% reduce(left_join, by = ids)
subpop_result$grp_age25 <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[dag >= 45 & dag < 65]) %>% reduce(left_join, by = ids)
subpop_result$grp_age45 <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[grp_hchild == TRUE]) %>% reduce(left_join, by = ids)
subpop_result$grp_hchild <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[grp_nchild == TRUE]) %>% reduce(left_join, by = ids)
subpop_result$grp_nchild <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[les_c4 == 1]) %>% reduce(left_join, by = ids)
subpop_result$grp_emp <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[les_c4 == 0]) %>% reduce(left_join, by = ids)
subpop_result$grp_unemp <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[les_c4 == 1 & atriskofpoverty == 1]) %>% reduce(left_join, by = ids)
subpop_result$grp_povin <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[les_c4 == 0 & atriskofpoverty == 1]) %>% reduce(left_join, by = ids)
subpop_result$grp_povout <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[deh_c3 == "Low"]) %>% reduce(left_join, by = ids)
subpop_result$grp_edlow <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[deh_c3 == "Medium"]) %>% reduce(left_join, by = ids)
subpop_result$grp_edmed <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

subpop_result <- lapply(experiment_id, main_loop, dft=df[deh_c3 == "High"]) %>% reduce(left_join, by = ids)
subpop_result$grp_edhi <- TRUE
result <- do.call(rbind, list(result, subpop_result)) %>% get_effects

# NOTE check original subgroup restrictions, make sure everything is correct
# especially the employment status that's reduced to two possible states with loss of information
