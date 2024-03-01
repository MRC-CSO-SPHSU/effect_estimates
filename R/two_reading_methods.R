library(arrow)
library(tidyverse)
library(SPHSUgraphs)
library(furrr)

plan(multisession, workers = 5)

baseline_main <- open_dataset(file.path("out_data", "baseline_main", "combined_data"))
runs <- baseline_main |> 
  select(run) |> 
  unique() |> 
  collect()


summarise_arrow <- function(data_name) {
  
  dataset <- open_dataset(file.path("out_data", data_name, "combined_data"))
  
  dataset |> 
  filter(dag >= 18, dag <= 65) |>
  mutate(ghq_case = dhm_ghq,
         employed = les_c4 == "EmployedOrSelfEmployed"
         ) |> 
  group_by(run, time) |> 
  summarise(
    across(
      c(
        dhm,
        employed,
        ghq_case,
        equivalisedDisposableIncomeYearly,
        atRiskOfPoverty
      ),
      list(mean = mean)
    ),
    across(
      c(ghq_case, atRiskOfPoverty, employed),
      list(n = sum)
    ),
    N = n(),
    .groups = "drop"
  ) |> 
  mutate(scenario = data_name) |> 
  collect()
  
}

summarise_batch <- function(data_name) {
  
  labour_supply_levels <-  c("ZERO" = 0,
                             "TEN" = 10,
                             "TWENTY" = 20,
                             "THIRTY" = 30,
                             "FORTY" = 40)
  
  median_inc_batched <- runs |>
    mutate(data_out = future_map(run, \(run_n) {
      open_dataset(file.path(
        "out_data",
        data_name,
        "combined_data",
        paste0("run=", run_n)
      )) |>
        select(time, equivalisedDisposableIncomeYearly, dhm, labourSupplyWeekly) |>
        collect() |>
        mutate(labour_supply_numeric = labour_supply_levels[labourSupplyWeekly]) |> 
        group_by(time) |>
        summarise(
          across(
            c(dhm, equivalisedDisposableIncomeYearly, labour_supply_numeric),
            list(
              median = median,
              q_10 = ~ quantile(.x, 0.10),
              q_90 = ~ quantile(.x, 0.90),
              q_25 = ~ quantile(.x, 0.25),
              q_75 = ~ quantile(.x, 0.75)
            )
          ),
          across(
            c(labour_supply_numeric),
            list(mean = mean)
          ),
          .groups = "drop"
        ) |> 
        mutate(scenario = data_name)
    })) |> 
    unnest(data_out)
  
  
}

count_labour_cats <- function(data_name) {
  open_dataset(file.path("out_data", data_name, "combined_data")) |>
    count(run, time, labourSupplyWeekly) |>
    collect() |>
    pivot_wider(names_from = labourSupplyWeekly,
                values_from = n,
                names_glue = "n_labour_{labourSupplyWeekly}") |> 
    mutate(scenario = data_name)
  
  
}


# Use functions to do this double-reading for each scenario:

baseline_dat1 <- summarise_arrow("baseline_main")
baseline_dat2 <- summarise_batch("baseline_main")
baseline_dat3 <- count_labour_cats("baseline_main")

baseline_main_stats <-
  full_join(baseline_dat1, baseline_dat2, by = c("run", "time")) |>
  full_join(baseline_dat3, by = c("run", "time"))

reform_dat1 <- summarise_arrow("reform_main")
reform_dat2 <- summarise_batch("reform_main")
reform_dat3 <- count_labour_cats("reform_main")

reform_main_stats <-
  full_join(reform_dat1, reform_dat2, by = c("run", "time")) |>
  full_join(reform_dat3, by = c("run", "time"))