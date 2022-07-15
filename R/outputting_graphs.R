library(readr)
library(tidyverse)
library(SPHSUgraphs)
library(hmisc)

out_data <-
  read_csv("C:/Programming/covid19_effect_estimates/data/new_data.csv",
           show_col_types = FALSE)


# tidying dataset ---------------------------------------------------------


compare_results <-
  out_data |>
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


# Examining quantiles
compare_results |>
  group_by(scenario, time, outcome, policy) |>
  summarise(median = median(out),
            lower = quantile(out, 0.05),
            upper = quantile(out, 0.95))

# faceted graph -----------------------------------------------------------


compare_results |>
  ggplot(aes(time, out, colour = policy, fill = policy)) +
  geom_vline(xintercept = 2019, colour = "red") +
  stat_summary(
    fun.data = median_hilow,
    geom = "ribbon",
    alpha = 0.5,
    colour = NA
  ) +
  stat_summary(fun.data = median_hilow, geom = "line") +
  stat_summary(fun.data = median_hilow, geom = "point") +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_fill_manual(
    "Policy:",
    aesthetics = c("fill", "colour"),
    labels = c("Baseline", "Covid policy"),
    values = sphsu_cols("University Blue", "Rust", names = FALSE)
  ) +
  labs(caption = paste("Notes:",
       "50 simulation runs in each condition.",
       "Red line denotes reform implementation point",
       sep = "\n")) +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))
