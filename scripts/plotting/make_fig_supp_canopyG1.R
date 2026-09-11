# Wasp performance for the Canopy G1 naturalistic stimulus experiment
# (the "pilot" version later repeated with more trials as Canopy G2).
# Extracted from 5_results_report.qmd's fig-supp-canopyG1 chunk.
make_fig_supp_canopyG1 <- function(emmAll, emmPts, choices_rolling, nLabels,
                                    natcanLabels, natcanImgLabels) {
  g1ID <- "natcan3_080825"

  emmG1All <- emmAll %>%
    filter(experiment == g1ID) %>%
    mutate(experiment = recode(experiment, !!!natcanLabels))
  emmG1Pts <- emmPts %>%
    filter(experiment == g1ID, trial_pos %in% c("last_test", "ctrl_start")) %>%
    mutate(experiment = recode(experiment, !!!natcanLabels))
  rollG1 <- choices_rolling %>%
    filter(experiment == g1ID) %>%
    mutate(experiment = recode(experiment, !!!natcanLabels))

  ggplot(emmG1All, aes(x = rank_trial, y = prob)) +
    geom_line() +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.15, colour = NA) +
    scale_y_continuous(labels = function(x) x * 100, limits = c(0, 1)) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_point(data = rollG1, aes(x = rank_trial, y = pcorr), colour = "black", alpha = 0.1) +
    geom_errorbar(data = emmG1Pts, aes(x = rank_trial, ymin = asymp.LCL, ymax = asymp.UCL), width = 2) +
    geom_point(data = emmG1Pts, aes(x = rank_trial, y = prob)) +
    geom_text(
      data = nLabels %>%
        filter(experiment == g1ID) %>%
        mutate(experiment = recode(experiment, !!!natcanLabels),
               manipulation = factor(manipulation, levels = c("test", "control"))),
      aes(label = label),
      x = Inf, y = -Inf, hjust = 1.05, vjust = -0.3, size = 2.5, inherit.aes = FALSE
    ) +
    facet_grid(experiment ~ manipulation, scales = "free_x",
               labeller = labeller(experiment = as_labeller(natcanImgLabels))) +
    labs(x = "Trial", y = "Probability of\ncorrect choice (%)") +
    theme_bw() +
    theme(panel.spacing.x = unit(0, "lines"),
          strip.text.y.right = ggtext::element_markdown(angle = 0),
          strip.background = element_blank())
}
