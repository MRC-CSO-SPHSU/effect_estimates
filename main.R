# import libraries, it is assumed they've been installed already
library(data.table) # parallel data manipulation
library(gsubfn) # string templates
library(readstata13) # to read from dta
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)

# a list of metric names in the main table
main_out_colnames =  c('dhm', 'out_ghqcase', 'out_emp', 'laboursupplyweekly',
                       'equivaliseddisposableincomeyearl', 'out_poverty')

col_rename <- function(table_name, postfix = '') {
  #' Renames columns for them to have more meaningful names.
  #'
  #' @param table_name a table to be modified.
  #' @param postfix a column name postfix that's added to differentiate metrics
  #' for various population groups, defaults to ''.
  name_pairs <- list('dhm' = 'mean_GHQ12', 'out_ghqcase' = 'prevalence_GHQ12',
                     'out_emp' = 'prevalence_employment',
                     'laboursupplyweekly' = 'mean_hours',
                     'equivaliseddisposableincomeyearl' = 'mean_income',
                     'out_poverty' = 'prevalence_poverty')

  for (name_ in main_out_colnames) {
    new_name = paste(name_pairs[[name_]], postfix, sep = "", collapse = NULL)
    names(table_name)[names(table_name) == name_] <- new_name
  }
  table_name
}

# scenario_id <- c("S1", "S2", "S3")
# experiment_id <- c("baseline", "reform")
# data.table::getDTthreads()
# data.table::setDTthreads(8)
# # no parallel data reading at the moment
# for (sid in scenario_id) {
#   for (eid in experiment_id) {
#     fn <- gsubfn(, list(id1 = sid, id2 = eid), "data/$id1/$id2")
#
#     filename <- paste(fn,".dta", sep="")
#     print(filename)
#     tictoc::tic("read full file")
#     datum <- read.dta13(filename) # read Stata file
#     tictoc::toc()
#
#     filename <- paste(fn,".csv.gz", sep="")
#     print(filename)
#     tictoc::tic("write full file")
#     fwrite(datum, file = filename) #parallel write using data.table
#     tictoc::toc()
#   }
# }

# set the number of threads to decrease the time of file reading/writing
data.table::getDTthreads()
data.table::setDTthreads(8)

# reads the file, only one at the moment. Decompression takes some time,
# but compressed files use 5 times less space. We also read
# a selected set of columns to save memory/time
df <- fread(file="data/S1/baseline.csv.gz",
            select = c('dhm', 'les_c4', 'laboursupplyweekly', 'dag',
                       'equivaliseddisposableincomeyearl', 'atriskofpoverty',
                       'dgn', 'deh_c3', 'run', 'time',
                       'n_children_0', 'n_children_1', 'n_children_10',
                       'n_children_11', 'n_children_12','n_children_13',
                       'n_children_14', 'n_children_15', 'n_children_16',
                       'n_children_17', 'n_children_2', 'n_children_3',
                       'n_children_4', 'n_children_5', 'n_children_6',
                       'n_children_7', 'n_children_8', 'n_children_9'))


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
df$out_poverty <- as.integer(format(factor(df$atriskofpoverty,
                                           levels = unique_atriskofpoverty_status,
                                           labels = unique_atriskofpoverty_status_numeric))) # kind of slow

# convert labour category to ints
df$laboursupplyweekly <- as.integer(format(factor(df$laboursupplyweekly,
                                                  levels = unique_weekly_hours,
                                                  labels = unique_weekly_hours_numeric))) # kind of slow

# convert employment category to ints
df$out_emp <- as.integer(format(factor(df$les_c4,
                                       levels = unique_labour_status,
                                       labels = unique_labour_status_numeric))) # kind of slow

# FIXME this works for every run, but not for all runs simultaneously
ids = c("time","run") # common columns for all minor tables
action_columns <- list(averaging = main_out_colnames)
action_list <- list(averaging = mean) # list of functions to evaluate
todo <- tibble(action_columns, action_list) # combinations of functions applied to corresponding columns


result <- pmap_dfc(todo, ~df %>% group_by_at(ids) %>% summarise_at(.x, .y))
result <- col_rename(result)

result_t <- pmap_dfc(todo, ~df[dgn == "Male"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_male")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[dgn == "Female"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_female")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[dag >= 25 & dag < 45] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_young")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[dag >= 45 & dag < 65] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_old")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[les_c4 == "EmployedOrSelfEmployed"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_employed")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[les_c4 == "NotEmployed"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_unemployed")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[deh_c3 == "Low"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_low_ed")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[deh_c3 == "Medium"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_medium_ed")
result <- merge(result, result_t, by=ids)

result_t <- pmap_dfc(todo, ~df[deh_c3 == "High"] %>% group_by_at(ids) %>% summarise_at(.x, .y))
result_t <- col_rename(result_t, "_high_ed")
result <- merge(result, result_t, by=ids)
