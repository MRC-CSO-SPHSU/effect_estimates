library(arrow)
library(tidyverse)
library(SPHSUgraphs)

baseline_main <- open_dataset(file.path("out_data", "baseline_main", "combined_data"))

runs <- baseline_main |> 
  select(run) |> 
  unique() |> 
  collect()

batched_meds <- baseline_main |>
  filter(run == 100) |>
  select(run, time, equivalisedDisposableIncomeYearly) |>
  collect() |>
  group_by(run, time) |>
  summarise(
    medinc = median(equivalisedDisposableIncomeYearly),
    q_10 = quantile(equivalisedDisposableIncomeYearly, 0.10)
  )

one_off_meds <- baseline_main |> 
  filter(run == 100) |> 
  group_by(run, time) |> 
  summarise(
    medinc = median(equivalisedDisposableIncomeYearly),
    q_10 = quantile(equivalisedDisposableIncomeYearly, 0.10)
    ) |> 
  collect()

batched_meds |> 
  left_join(one_off_meds, by = c("run", "time"), suffix = c("_batched", "_arrowed")) |> 
  pivot_longer(-c(run, time), names_to = c("measure", "method"), values_to = "value",
               names_pattern = "(.*)_(.*)") |> 
  pivot_wider(names_from = measure, values_from = value) |> 
  ggplot(aes(time, medinc, colour = method)) + geom_point()

batched_meds |> 
  left_join(one_off_meds, by = c("run", "time"), suffix = c("_batched", "_arrowed")) |> 
  pivot_longer(-c(run, time), names_to = c("measure", "method"), values_to = "value",
               names_pattern = "(.*)_(.*)") |> 
  pivot_wider(names_from = measure, values_from = value) |> 
  ggplot(aes(time, q_10, colour = method)) + geom_point()

library(furrr)

plan(multisession, workers = 10)

labour_supply_levels <-  c("ZERO" = 0,
                           "TEN" = 10,
                           "TWENTY" = 20,
                           "THIRTY" = 30,
                           "FORTY" = 40)

t1 <- now()
median_inc_batched <- runs |>
  mutate(data_out = future_map(run, \(run_n) {
    open_dataset(file.path(
      "out_data",
      "baseline_main",
      "combined_data",
      paste0("run=", run_n)
    )) |>
      select(time, equivalisedDisposableIncomeYearly, dhm, labourSupplyWeekly) |>
      collect() |>
      mutate(labourSupplyWeekly = labour_supply_levels[labourSupplyWeekly]) |> 
      group_by(time) |>
      summarise(
        across(
          c(dhm, equivalisedDisposableIncomeYearly),
          list(
            median = median,
            q_10 = ~ quantile(.x, 0.10),
            q_90 = ~ quantile(.x, 0.90),
            q_25 = ~ quantile(.x, 0.25),
            q_75 = ~ quantile(.x, 0.75)
          )
        ),
        across(
          c(labourSupplyWeekly),
          list(mean = mean)
        )
      )
  }))
now() - t1

plan(sequential)

median_inc_batched |> 
  unnest(data_out)
