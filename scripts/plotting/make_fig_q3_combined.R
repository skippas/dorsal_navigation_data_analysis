# Probability of correct choices for naturalistic and artificial experiments
# at the maximum common test-phase trial. Panel A: overlaid learning curves
# for every experiment. Panel B: performance at the shared trial, artificial
# vs. naturalistic. Extracted from 5_results_report.qmd's fig-q3-combined
# chunk (previously combined with fig-q3-natart, which supplied `canopyCols`
# via the shared qmd chunk environment -- now a local constant here since
# nothing else uses it).
make_fig_q3_combined <- function(emmAll, artLabels, natcanLabels) {
  canopyCols <- c(naturalistic = "forestgreen", artificial = "black")

  # Per-experiment colour mapping: black for artificial, green for naturalistic
  allExpLabels <- c(artLabels, natcanLabels)
  expCols <- c(
    setNames(rep("black",       length(artLabels)),    unname(artLabels)),
    setNames(rep("forestgreen", length(natcanLabels)), unname(natcanLabels))
  )

  # Test-phase prediction curves for all experiments
  allExpTestAll <- emmAll %>%
    filter(manipulation == "test") %>%
    mutate(experiment = dplyr::recode(experiment, !!!allExpLabels))

  # Maximum shared trial: highest trial number reached by every experiment
  maxSharedTrial <- emmAll %>%
    filter(manipulation == "test") %>%
    group_by(experiment) %>%
    summarise(max_trial = max(rank_trial), .groups = "drop") %>%
    pull(max_trial) %>%
    min()

  # Predicted point at the shared trial for each experiment
  emmSharedPt <- emmAll %>%
    filter(manipulation == "test", rank_trial == maxSharedTrial) %>%
    mutate(experiment = dplyr::recode(experiment, !!!allExpLabels))

  # Panel A: overlaid learning curves
  pA <- ggplot(allExpTestAll,
               aes(x = rank_trial, y = prob,
                   group = experiment, colour = experiment)) +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = experiment),
                colour = NA, alpha = 0.1) +
    geom_line(alpha = 0.5) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_point(data = emmSharedPt, size = 2, alpha = 0.5) +
    scale_colour_manual(values = expCols, guide = "none") +
    scale_fill_manual(values = expCols, guide = "none") +
    scale_y_continuous(labels = function(x) x * 100, limits = c(0.25, 1)) +
    labs(x = "Trial", y = "Probability of correct choice (%)") +
    theme_bw()

  # Panel B: jittered points at the shared trial reference point
  pB <- ggplot(emmSharedPt, aes(x = nat_or_art, y = prob)) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_point(aes(color = nat_or_art), size = 2, alpha = 0.7,
               position = position_jitter(width = 0.08, height = 0, seed = 3)) +
    scale_color_manual(values = canopyCols,
                       labels = c(artificial = "Artificial", naturalistic = "Naturalistic")) +
    scale_y_continuous(labels = function(x) x * 100, limits = c(0.25, 1)) +
    labs(x = "Stimulus type", y = "Probability of correct choice (%) \nat trial 26", color = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  pA + pB + plot_layout(widths = c(2, 1)) + plot_annotation(tag_levels = "A")
}
