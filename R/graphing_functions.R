#' Outputting line graphs comparing policy scenarios over time
#'
#' @param baseline_var Column to graph baseline
#' @param comparison_var Column to graph comparison
#' @param group_var Column with `TRUE` values corresponding to group
#' @param y_lab Title for y axis

graph_policy_comparisons <-
  function(.data, baseline_var, comparison_var, group_var = grp_all, y_lab = "") {

    require(ggplot2)
    require(dplyr)
    require(tidyr)

    baseline_var <- enquo(baseline_var)
    comparison_var <- enquo(comparison_var)
    group_var <- enquo(group_var)

    .data |>
      filter(!!group_var, !is.na(run)) |>
      select(run, time, !!baseline_var, !!comparison_var) |>
      pivot_longer(
        -c(run, time),
        names_to = "case",
        values_to = "val",
        names_prefix = ".*_.*_"
      ) |>
      ggplot(aes(time, val, colour = case, fill = case)) +
      geom_vline(aes(xintercept = 2019, linetype = "Covid reform\nimplementation"),
                 colour = "red") +
      stat_summary(
        fun.data = median_hilow,
        geom = "ribbon",
        alpha = 0.5,
        colour = NA
      ) +
      stat_summary(fun.data = median_hilow, geom = "line") +
      stat_summary(fun.data = median_hilow, geom = "point") +
      scale_fill_manual(
        "Policy:",
        aesthetics = c("fill", "colour"),
        labels = c("Baseline", "Covid policy"),
        values = sphsu_cols("University Blue", "Rust", names = FALSE)
      ) +
      scale_linetype("") +
      xlab("Year") +
      ylab(y_lab) +
      labs(
        caption = paste(
          "Notes:",
          "50 simulation runs in each condition.",
          "Red line denotes reform implementation point",
          sep = "\n"
        )
      ) +
      theme_sphsu_light() +
      theme(legend.position = "bottom",
            plot.caption = element_text(hjust = 0))

  }

# out_data |>
#   graph_policy_comparisons(out_ghq_baseline, out_ghq_reform,  y_lab = "GQH score")
#
# out_data |>
#   graph_policy_comparisons(out_ghqcase_baseline, out_ghqcase_reform, grp_male)
