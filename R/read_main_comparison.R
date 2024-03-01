library(arrow)
library(tidyverse)
library(SPHSUgraphs)

baseline_main <- open_dataset(file.path("out_data", "baseline_main", "combined_data"))
baseline_sens <- open_dataset(file.path("out_data", "baseline_main", "combined_data"))
reform_main <- open_dataset(file.path("out_data", "reform_main", "combined_data"))
reform_sens <- open_dataset(file.path("out_data", "reform_main", "combined_data"))



baseline_means <- baseline_main |> 
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
        # labourSupplyWeekly  # Currenty cannot convert this to numeric
      ),
      mean
    ),
    N = n(),
    medianinc = median(equivalisedDisposableIncomeYearly),
    .groups = "drop"
  ) |> 
  mutate(scenario = "baseline") |> 
  collect()

reform_means <- reform_main |> 
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
        # labourSupplyWeekly  # Currenty cannot convert this to numeric
      ),
      mean
    ),
    N = n(),
    medianinc = median(equivalisedDisposableIncomeYearly),
    inc_10p = quantile(equivalisedDisposableIncomeYearly, 0.1),
    .groups = "drop"
  ) |> 
  mutate(scenario = "reform") |> 
  collect()



bind_rows(baseline_means,
          reform_means) |>
  ggplot(aes(time, employed, colour = scenario, fill = scenario)) +
  geom_vline(aes(xintercept = 2019, linetype = "Covid reform\nimplementation"),
             colour = "red") +
  stat_summary(
    fun.data = median_hilow,
    geom = "ribbon",
    alpha = 0.5,
    colour = NA
  ) +
  stat_summary(fun = median, geom = "line") +
  stat_summary(fun = median, geom = "point") +
  scale_fill_manual(
    "Policy:",
    aesthetics = c("fill", "colour"),
    breaks = c("baseline", "reform"),
    labels = c("Baseline", "Covid policy"),
    values = sphsu_cols("University Blue", "Rust", names = FALSE)
  ) +
  scale_linetype("") +
  xlab("Year") +
  scale_y_continuous(
    "Percentage of working age population in employment",
    labels = scales::percent
    ) +
  labs(
    caption = paste(
      "Notes:",
      "1,000 simulation runs in each condition.",
      "Red line denotes reform implementation point",
      sep = "\n"
    )
  ) +
  theme_sphsu_light() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))


bind_rows(baseline_means,
          reform_means) |>
  ggplot(aes(time, dhm_ghq, colour = scenario, fill = scenario)) +
  geom_vline(aes(xintercept = 2019, linetype = "Covid reform\nimplementation"),
             colour = "red") +
  stat_summary(
    fun.data = median_hilow,
    geom = "ribbon",
    alpha = 0.5,
    colour = NA
  ) +
  stat_summary(fun = median, geom = "line") +
  stat_summary(fun = median, geom = "point") +
  scale_fill_manual(
    "Policy:",
    aesthetics = c("fill", "colour"),
    breaks = c("baseline", "reform"),
    labels = c("Baseline", "Covid policy"),
    values = sphsu_cols("University Blue", "Rust", names = FALSE)
  ) +
  scale_linetype("") +
  xlab("Year") +
  scale_y_continuous(
    "Mean mental health score"
    ) +
  labs(
    caption = paste(
      "Notes:",
      "1,000 simulation runs in each condition.",
      "Red line denotes reform implementation point",
      sep = "\n"
    )
  ) +
  theme_sphsu_light() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))


bind_rows(baseline_means,
          reform_means) |>
  ggplot(aes(time, atRiskOfPoverty, colour = scenario, fill = scenario)) +
  geom_vline(aes(xintercept = 2019, linetype = "Covid reform\nimplementation"),
             colour = "red") +
  stat_summary(
    fun.data = median_hilow,
    geom = "ribbon",
    alpha = 0.5,
    colour = NA
  ) +
  stat_summary(fun = median, geom = "line") +
  stat_summary(fun = median, geom = "point") +
  scale_fill_manual(
    "Policy:",
    aesthetics = c("fill", "colour"),
    breaks = c("baseline", "reform"),
    labels = c("Baseline", "Covid policy"),
    values = sphsu_cols("University Blue", "Rust", names = FALSE)
  ) +
  scale_linetype("") +
  xlab("Year") +
  scale_y_continuous(
    "Percentage of working age population at risk of poverty",
    labels = scales::percent
  ) +
  labs(
    caption = paste(
      "Notes:",
      "1,000 simulation runs in each condition.",
      "Red line denotes reform implementation point",
      sep = "\n"
    )
  ) +
  theme_sphsu_light() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))
