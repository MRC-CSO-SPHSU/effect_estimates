#' Outputting line graphs comparing policy scenarios over time
#'
#' @param baseline_var Column to graph baseline
#' @param comparison_var Column to graph comparison
#' @param group_var Column with `TRUE` values corresponding to group
#' @param y_lab Title for y axis
#' @param agg_method Aggregating method for combining lines/points (defaults to median; does not affect confidence intervals)
#' @param ci_geom `geom` to display confidence intervals (defaults to "ribbon")
#' @return A ggplot object
#' @examples
#' out_data |>
#'   graph_policy_comparisons(out_ghq_baseline, out_ghq_reform,  y_lab = "GQH score")


graph_policy_comparisons <-
  function(.data,
           baseline_var,
           comparison_var,
           group_var = grp_all,
           y_lab = "",
           agg_method = median,
           ci_geom = c("ribbon", "errorbar", "linerange", "crossbar")) {

    ci_geom <- match.arg(ci_geom)

    require(ggplot2)
    require(SPHSUgraphs)
    require(dplyr)
    require(tidyr)

    # agg_method <- match.arg(agg_method)

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
      {if (ci_geom == "ribbon") {
        stat_summary(
        fun.data = median_hilow,
        geom = "ribbon",
        alpha = 0.5,
        colour = NA)
      } else {
        stat_summary(
        fun.data = median_hilow,
        geom = ci_geom,
        width = 0.2,
        size = 1)
      }} +
      stat_summary(fun = agg_method, geom = "line") +
      stat_summary(fun = agg_method, geom = "point") +
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
#   graph_policy_comparisons(out_ghq_baseline, out_ghq_reform,  y_lab = "GQH score", agg_method = median,
#                            ci_geom = "ribbon")
#
# out_data |>
#   graph_policy_comparisons(
#     out_ghq_baseline,
#     out_ghq_reform,
#     y_lab = "GQH score",
#     agg_method = mean,
#     ci_geom = "errorbar"
#   )
#
# out_data |>
#   graph_policy_comparisons(out_ghqcase_baseline, out_ghqcase_reform, grp_male)
