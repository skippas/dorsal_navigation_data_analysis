# Wasp reward-navigation performance for the four artificial stimulus
# experiments (perpendicular/parallel, thin oblique, thick oblique,
# transparent/opaque). Extracted from 5_results_report.qmd's fig-q2 chunk.
make_fig_q2 <- function(emmAll, emmPts, choices_rolling, nLabels,
                         artLabels, artImgLabels) {
  q2ExpOrder <- c("perpPara_170725", "thinOb_150823", "thickOb_140823", "brightDiff_250725")

  emmAllArt <- emmAll %>%
    filter(experiment %in% q2ExpOrder) %>%
    mutate(
      experiment = recode(experiment, !!!artLabels),
      experiment = factor(experiment, levels = unname(artLabels[q2ExpOrder])),
      manipulation = factor(manipulation, levels = c("test", "control"))
    )

  p <- ggplot(emmAllArt, aes(x = rank_trial, y = prob)) +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
    geom_line() +
    labs(x = "Trial", y = "Probability of correct choice (%)") +
    scale_y_continuous(labels = function(x) x * 100) +
    geom_hline(yintercept = 0.5, lty = 2) +
    theme_bw() +
    facet_grid(experiment ~ manipulation, scales = "free_x",
               labeller = labeller(experiment = as_labeller(artImgLabels))) +
    theme(panel.spacing.y = unit(1, "lines"),
          panel.spacing.x = unit(0, "lines"),
          strip.background = element_blank(),
          strip.text.y.right = ggtext::element_markdown(angle = 0)
  )

  rollingArt <- choices_rolling %>%
    filter(experiment %in% q2ExpOrder) %>%
    mutate(
      experiment = recode(experiment, !!!artLabels),
      experiment = factor(experiment, levels = unname(artLabels[q2ExpOrder])),
      manipulation = factor(manipulation, levels = c("test", "control"))
    )

  p <- p + geom_point(data = rollingArt, aes(x = rank_trial, y = pcorr),
                      colour = "black", alpha = 0.1)

  emmPtsArt <- emmPts %>%
    filter(experiment %in% q2ExpOrder,
           trial_pos %in% c("last_test", "ctrl_start")) %>%
    mutate(
      experiment = recode(experiment, !!!artLabels),
      experiment = factor(experiment, levels = unname(artLabels[q2ExpOrder]))
    )

  # The perpendicular/parallel end-of-test point (trial 47) is drawn as an
  # open circle and repeated on the thin-oblique test panel: trial 47 is the
  # last test trial common to the two experiments and the trial at which they
  # are compared in the text (Eq. 3). Overlaying it shows the reader that the
  # comparison point lies off the oblique curve, without conflating it with
  # the oblique's own (later) end-of-test point.
  ppEnd <- emmPtsArt %>%
    filter(experiment == artLabels[["perpPara_170725"]], trial_pos == "last_test")
  ppEndOnOb <- ppEnd %>%
    mutate(experiment = factor(artLabels[["thinOb_150823"]],
                               levels = levels(emmPtsArt$experiment)))
  emmPtsFilled <- emmPtsArt %>%
    filter(!(experiment == artLabels[["perpPara_170725"]] & trial_pos == "last_test"))

  p <- p +
    geom_errorbar(data = emmPtsArt,
                  aes(x = rank_trial, ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 2) +
    geom_point(data = emmPtsFilled, aes(x = rank_trial, y = prob)) +
    geom_errorbar(data = ppEndOnOb,
                  aes(x = rank_trial, ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 2) +
    geom_point(data = bind_rows(ppEnd, ppEndOnOb), aes(x = rank_trial, y = prob),
               shape = 21, fill = "white", size = 2)

  p <- p +
    geom_text(
      data = nLabels %>%
        filter(experiment %in% q2ExpOrder) %>%
        mutate(
          experiment   = recode(experiment, !!!artLabels),
          experiment   = factor(experiment, levels = unname(artLabels[q2ExpOrder])),
          manipulation = factor(manipulation, levels = c("test", "control"))
        ),
      aes(label = label),
      x = Inf, y = -Inf, hjust = 1.05, vjust = -0.3,
      size = 2.5, inherit.aes = FALSE
    )

  # One letter per experiment row, left of the y-axis title (patchwork object)
  add_row_letters(p)
}
