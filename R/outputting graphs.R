library(readr)
library(tidyverse)
library(SPHSUgraphs)

out_data <-
  read_csv("C:/Programming/covid19_effect_estimates/data/new_data.csv",
           show_col_types = FALSE)


# tidying dataset ---------------------------------------------------------


compare_results <- out_data |>
  filter(grp_all == TRUE, !is.na(run)) |>
  select(-contains("eff"), -starts_with("grp")) |>
  pivot_longer(
    -c(scenario, run, time),
    names_to = c("metric", "outcome", "policy"),
    values_to = "val",
    names_pattern = "(.*)_(.*)_(baseline|reform)"
  ) |>
  pivot_wider(
    c(scenario, run, time, outcome, policy),
    names_from = metric,
    values_from = val
  )


# faceted graph -----------------------------------------------------------

compare_results |>
  ggplot(aes(time, out, colour = policy, fill = policy)) +
  geom_vline(xintercept = 2019, colour = "red") +
  stat_summary(
    fun.data = mean_se,
    geom = "ribbon",
    alpha = 0.5,
    colour = NA
  ) +
  stat_summary(fun.data = mean_se, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "point") +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_fill_manual(
    "Policy",
    aesthetics = c("fill", "colour"),
    labels = c("Baseline", "Covid policy"),
    values = sphsu_cols("University Blue", "Thistle", names = FALSE)
  ) +
  theme(legend.position = "bottom")
