# try installing haven
# try installing other packages
# load data in parallel - large memory consumption, but a single .dta file takes ages to load

#install.packages("haven") # to read from dta
#install.packages("data.table") # parallel data manipulation
#install.packages("gsubfn") # string templates

library("haven")
library("data.table")
library("gsubfn")
library("readstata13")
library(magrittr)
library(tibble)
library(dplyr)
library(tidyverse)
library(R.utils)

col_rename <- function(table_name, postfix = '') {
  old_names = c('dhm', 'dummy_dhm', 'dummy_les_c4', 'laboursupplyweekly',
                'equivaliseddisposableincomeyearl', 'dummy_atriskofpoverty')
  name_pairs <- list('dhm' = 'mean_GHQ12', 'dummy_dhm' = 'prevalence_GHQ12',
                     'dummy_les_c4' = 'prevalence_employment',
                     'laboursupplyweekly' = 'mean_hours',
                     'equivaliseddisposableincomeyearl' = 'mean_income',
                     'dummy_atriskofpoverty' = 'prevalence_poverty')

  for (name_ in old_names) {
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

data.table::getDTthreads()
data.table::setDTthreads(8)

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


unique_years <- unique(df$time)
unique_runs <- unique(df$run)
unique_weekly_hours <- unique(df$laboursupplyweekly)
unique_weekly_hours_numeric <- c(0, 20, 10, 40, 30)
unique_labour_status <- unique(df$les_c4)
unique_labour_status_numeric <- c(0, 1, 0, 0)
unique_atriskofpoverty_status <- unique(df$atriskofpoverty) # "0" "1" "null"
unique_atriskofpoverty_status_numeric <- c(0, 1, 1)

df$dummy_dhm <- ifelse(df$dhm<=24, 1, 0)

# atriskofpoverty is chars, check for empty strings, char nulls
# les_c4 is the same

df$dummy_atriskofpoverty <- as.integer(format(factor(df$atriskofpoverty,
                                                     levels = unique_atriskofpoverty_status,
                                                     labels = unique_atriskofpoverty_status_numeric))) # kind of slow

df$laboursupplyweekly <- as.integer(format(factor(df$laboursupplyweekly,
                                                  levels = unique_weekly_hours,
                                                  labels = unique_weekly_hours_numeric))) # kind of slow

df$dummy_les_c4 <- as.integer(format(factor(df$les_c4,
                                                  levels = unique_labour_status,
                                                  labels = unique_labour_status_numeric))) # kind of slow


ids = c("time","run") # FIXME this works for every run, but not for all runs simultaneously

# colnames for averaging
do_over <- list(averaging = c("dhm", "dummy_dhm", "laboursupplyweekly",
                              "equivaliseddisposableincomeyearl",
                              "dummy_atriskofpoverty", "dummy_les_c4"))
do_what <- list(averaging = mean)

todo <- tibble(do_over, do_what)


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
