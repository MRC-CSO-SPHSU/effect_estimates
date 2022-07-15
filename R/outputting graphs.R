library(readr)
library(tidyverse)

out_data <- read_csv("C:/Programming/covid19_effect_estimates/data/new_data.csv")


out_data |>
  count(scenario, run, time)


out_data |>
  pivot_longer(-c(scenario, run, time), names_to = c("metric", "outcome", "policy"),
               values_to = "val",
               names_pattern = "(.*)_(.*)_(baseline|reform)") |>
  pivot_wider(c(scenario, run, time, outcome, policy), names_from = metric, values_from = val)


