# Wasp reward-navigation performance for the six naturalistic stimulus
# experiments (excludes the Canopy G1 pilot, natcan3_080825, shown
# separately in fig-supp-canopyG1). Extracted from 5_results_report.qmd's
# fig-q3-tc chunk. Note: the original chunk also computed an unused
# `tbNatTcPts` object (assigned then immediately rm()'d, never referenced
# in the plot or elsewhere) -- dropped here as dead code.
make_fig_q3_tc <- function(emmAll, emmPts, choices_rolling, nLabels,
                            natcanLabels, natcanImgLabels) {
  emmAllNat <- emmAll %>%
    filter(nat_or_art == "naturalistic", experiment != "natcan3_080825") %>%
    droplevels() %>%
    mutate(experiment = recode(experiment, !!!natcanLabels))
  emmPtsNat <- emmPts %>%
    filter(nat_or_art == "naturalistic", experiment != "natcan3_080825",
           trial_pos %in% c("last_test", "ctrl_start")) %>%
    droplevels() %>%
    mutate(experiment = recode(experiment, !!!natcanLabels))

  rollingNat <- choices_rolling %>%
    filter(nat_or_art == "naturalistic", experiment != "natcan3_080825") %>%
    droplevels() %>%
    mutate(experiment = recode(experiment, !!!natcanLabels))

  p <- ggplot(emmAllNat, aes(x = rank_trial, y = prob)) +
    geom_line() +
    geom_ribbon(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      alpha = 0.15,
      colour = NA
    ) +
    scale_y_continuous(labels = function(x) x * 100, limits = c(0, 1)) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    facet_grid(experiment ~ manipulation, scales = "free_x",
               labeller = labeller(experiment = as_labeller(natcanImgLabels))) +
    labs(x = "Trial", y = "Probability of correct choice (%)") +
    theme_bw() +
    theme(legend.position = "none", panel.spacing.y = unit(0.75, "lines"),
          panel.spacing.x = unit(0, "lines"),
          strip.text.y.right = ggtext::element_markdown(angle = 0),
          strip.background = element_blank())

  p <- p + geom_point(data = rollingNat, aes(x = rank_trial, y = pcorr),
                      colour = "black", alpha = 0.1)

  p <- p +
    geom_errorbar(data = emmPtsNat,
                  aes(x = rank_trial, ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 2) +
    geom_point(data = emmPtsNat, aes(x = rank_trial, y = prob))

  p <- p +
    geom_text(
      data = nLabels %>%
        filter(experiment %in% names(natcanLabels), experiment != "natcan3_080825") %>%
        mutate(
          experiment   = recode(experiment, !!!natcanLabels),
          manipulation = factor(manipulation, levels = c("test", "control"))
        ),
      aes(label = label),
      x = Inf, y = -Inf, hjust = 1.05, vjust = -0.3,
      size = 2.5, inherit.aes = FALSE
    )

  # One letter per experiment row, left of the y-axis title (patchwork object)
  add_row_letters(p)
}
