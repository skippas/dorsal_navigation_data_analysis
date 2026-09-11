# =============================================================================
# 4_run_supp_analysis.R
# Fits models, computes emmeans, and builds figures for supplementary-only
# experiments (not included in the main analysis pipeline).
# Output: scripts/intermediate_outputs/supp_analysis_results.RData
#
# Run from the project root directory (where data_analysis.Rproj lives).
# =============================================================================

library(here)
library(lme4)
library(broom.mixed)
library(tidyverse)
library(magrittr)
library(emmeans)
library(zoo)
library(scales)
library(patchwork)

setwd(rprojroot::find_rstudio_root_file())

source("scripts/1_loading_cleaning.R")       # -> choices

styleEnv <- new.env(parent = environment())
source("scripts/custom_themes_and_colour_palettes.R", local = styleEnv)
plotStyle <- list(themeTextEnlarged = styleEnv$custom_theme_text_enlarged)
rm(styleEnv)

suppLabels <- c(
  perpParaPostNatcan_090925 = "Perp./Para. (post-natcan)",
  thickObDiff_210924        = "Thick oblique (diffuser)"
)

# =============================================================================
# Perp/para following natcan (perpParaPostNatcan_090925) — test-only
# Shown alongside natcan6_070925 (same individuals, immediately preceding)
# =============================================================================

# Extract both supp datasets before loading main analysis results, because
# loading analysis_results.RData overwrites `choices` with the filtered version
# that excludes supplementary experiments.
ppPostNatcanData <- choices %>%
  filter(experiment == "perpParaPostNatcan_090925") %>%
  mutate(decision     = as.numeric(decision),
         manipulation = relevel(manipulation, ref = "test"))

thickObDiffDataRaw <- choices %>%
  filter(experiment == "thickObDiff_210924")

# Load main analysis results to get natcan6 emmeans and rolling means.
load("scripts/intermediate_outputs/analysis_results.RData")  # -> emmAll, choices_rolling

mPpPostNatcan <- glmer(
  decision ~ rank_trial + reward_side + (1 | individual),
  data = ppPostNatcanData, family = binomial
)

mPpPostNatcanCoefs <- broom.mixed::tidy(mPpPostNatcan, conf.int = TRUE) %>%
  mutate(experiment = "perpParaPostNatcan_090925", nat_or_art = "artificial")

# Emmeans for perpParaPostNatcan
emmPpPostNatcan <- emmeans(
  mPpPostNatcan, ~ rank_trial,
  cov.keep = "rank_trial", type = "response"
) %>%
  as.data.frame() %>%
  semi_join(distinct(ppPostNatcanData, rank_trial), by = "rank_trial") %>%
  mutate(panel_exp = "perpParaPostNatcan_090925")

# Emmeans for natcan6 test phase (from main analysis)
emmNatcan6 <- emmAll %>%
  filter(experiment == "natcan6_070925", manipulation == "test") %>%
  select(rank_trial, prob, asymp.LCL, asymp.UCL) %>%
  mutate(panel_exp = "natcan6_070925")

# Combined emmeans for top-row figure
topRowEmm <- bind_rows(emmNatcan6, emmPpPostNatcan) %>%
  mutate(panel_exp = factor(panel_exp,
                            levels = c("natcan6_070925", "perpParaPostNatcan_090925")))

# Endpoint points
topRowEndPts <- topRowEmm %>%
  group_by(panel_exp) %>%
  filter(rank_trial == max(rank_trial)) %>%
  ungroup()

# Rolling means — natcan6 from main analysis, perpParaPostNatcan computed fresh
ppPostNatcanRolling <- ppPostNatcanData %>%
  arrange(rank_trial) %>%
  group_by(individual) %>%
  mutate(pcorr = rollmean(decision, k = 3, fill = NA, align = "center")) %>%
  ungroup() %>%
  group_by(rank_trial) %>%
  summarise(pcorr = mean(pcorr, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel_exp = "perpParaPostNatcan_090925")

natcan6Rolling <- choices_rolling %>%
  filter(experiment == "natcan6_070925", manipulation == "test") %>%
  select(rank_trial, pcorr) %>%
  mutate(panel_exp = "natcan6_070925")

topRowRolling <- bind_rows(natcan6Rolling, ppPostNatcanRolling) %>%
  mutate(panel_exp = factor(panel_exp,
                            levels = c("natcan6_070925", "perpParaPostNatcan_090925")))

# Sample size labels
topRowNLabels <- bind_rows(
  choices %>%
    filter(experiment == "natcan6_070925", manipulation == "test") %>%
    summarise(n_wasps = n_distinct(individual), n_choices = n(),
              label = paste0("n = ", n_wasps, " wasps, ", n_choices, " choices"),
              panel_exp = "natcan6_070925"),
  ppPostNatcanData %>%
    summarise(n_wasps = n_distinct(individual), n_choices = n(),
              label = paste0("n = ", n_wasps, " wasps, ", n_choices, " choices"),
              panel_exp = "perpParaPostNatcan_090925")
) %>%
  mutate(panel_exp = factor(panel_exp,
                            levels = c("natcan6_070925", "perpParaPostNatcan_090925")))

topRowPanelLabels <- c(
  natcan6_070925            = "Canopy J",
  perpParaPostNatcan_090925 = "Perp./Para. (post-natcan)"
)

# Figure — two panels side by side
pPpPostNatcan <- ggplot(topRowEmm, aes(x = rank_trial, y = prob)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_point(data = topRowRolling, aes(x = rank_trial, y = pcorr),
             colour = "black", alpha = 0.1) +
  geom_errorbar(data = topRowEndPts,
                aes(x = rank_trial, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 1) +
  geom_point(data = topRowEndPts, aes(x = rank_trial, y = prob)) +
  geom_text(data = topRowNLabels, aes(label = label),
            x = Inf, y = -Inf, hjust = 1.05, vjust = -0.3,
            size = 2.5, inherit.aes = FALSE) +
  labs(x = "Trial", y = "% of decisions for reward") +
  scale_y_continuous(labels = scales::percent, limits = c(0.25, 1)) +
  facet_wrap(~ panel_exp, nrow = 1, scales = "free_x",
             labeller = labeller(panel_exp = topRowPanelLabels)) +
  theme_bw() +
  theme(
    panel.spacing.x  = unit(0, "lines"),
    strip.background = element_blank()
  )

# =============================================================================
# Thick oblique with diffuser (thickObDiff_210924) — test + control
# =============================================================================

panelLevels <- c("no_diffuser", "diffuser_bottom", "diffuser_whole")

panelLabels <- c(
  no_diffuser     = "No diffuser",
  diffuser_bottom = "Diffuser (floor)",
  diffuser_whole  = "Diffuser (floor + sides)"
)

thickObDiffData <- thickObDiffDataRaw %>%
  # Individual 11 has a single diffuser_whole trial at rank_trial 11,
  # far earlier than all other individuals (rank_trial 89–111). Excluded.
  filter(individual != "11") %>%
  mutate(
    decision           = as.numeric(decision),
    manipulation       = relevel(manipulation, ref = "test"),
    diffuser_condition = factor(diffuser_condition,
                                levels = c("no_diffuser", "diffuser_bottom",
                                           "diffuser_whole"))
  )

# Full model — test + control, no diffuser term (used for test-end vs
# control-start contrast and for the control-phase prediction curve)
mThickObDiff <- glmer(
  decision ~ rank_trial * manipulation + reward_side + (1 | individual),
  data = thickObDiffData, family = binomial
)

# Test-phase model with diffuser_condition (used for per-condition emmeans)
mThickObDiffTest <- glmer(
  decision ~ rank_trial * diffuser_condition + reward_side + (1 | individual),
  data = filter(thickObDiffData, manipulation == "test"), family = binomial
)

mThickObDiffCoefs <- broom.mixed::tidy(mThickObDiff, conf.int = TRUE) %>%
  mutate(experiment = "thickObDiff_210924", nat_or_art = "artificial")

# Test-end vs control-start contrast (from full model)
testEnd   <- max(filter(thickObDiffData, manipulation == "test")$rank_trial)
ctrlStart <- 1L

emmContPts <- emmeans(
  mThickObDiff, ~ manipulation * rank_trial,
  at = list(rank_trial = c(ctrlStart, testEnd))
)
emmContPts <- subset(
  emmContPts,
  (manipulation == "control" & rank_trial == ctrlStart) |
  (manipulation == "test"    & rank_trial == testEnd)
)
grid <- emmContPts@grid
emmContPts <- emmContPts[order(grid$manipulation == "test", grid$rank_trial)]

thickObDiffContrast <- summary(
  contrast(emmContPts, list("test_end - control_start" = c(-1, 1))),
  infer = c(TRUE, TRUE)
) %>%
  rename(logOddsRatio = estimate) %>%
  mutate(experiment    = "thickObDiff_210924",
         control_start = ctrlStart,
         test_end      = testEnd)

# Emmeans per diffuser condition (test panels).
# Filter to each condition's observed rank_trial range explicitly — semi_join
# on factor columns can silently fail to filter, leaving all 1:max predictions.
diffCondRanges <- filter(thickObDiffData, manipulation == "test") %>%
  group_by(diffuser_condition) %>%
  summarise(min_rt = min(rank_trial), max_rt = max(rank_trial), .groups = "drop") %>%
  mutate(diffuser_condition = as.character(diffuser_condition))

emmThickObDiffTest <- emmeans(
  mThickObDiffTest, ~ rank_trial * diffuser_condition,
  cov.keep = "rank_trial", type = "response"
) %>%
  as.data.frame() %>%
  mutate(diffuser_condition = as.character(diffuser_condition)) %>%
  left_join(diffCondRanges, by = "diffuser_condition") %>%
  filter(rank_trial >= min_rt, rank_trial <= max_rt) %>%
  select(-min_rt, -max_rt) %>%
  mutate(panel = diffuser_condition)

# Emmeans for control panel (from full model)
emmThickObDiffCtrl <- emmeans(
  mThickObDiff, ~ rank_trial * manipulation,
  cov.keep = "rank_trial", type = "response"
) %>%
  as.data.frame() %>%
  semi_join(
    filter(thickObDiffData, manipulation == "control") %>%
      distinct(manipulation, rank_trial),
    by = c("manipulation", "rank_trial")
  ) %>%
  filter(manipulation == "control") %>%
  mutate(panel = "control")

# Combined emmeans for figure (test panels only)
emmThickObDiffPlot <- emmThickObDiffTest %>%
  select(rank_trial, prob, asymp.LCL, asymp.UCL, panel) %>%
  mutate(panel = factor(panel, levels = panelLevels))

# Endpoint predicted points (last trial of each condition)
thickObDiffEndPts <- emmThickObDiffTest %>%
  group_by(panel) %>%
  filter(rank_trial == max(rank_trial)) %>%
  ungroup() %>%
  select(rank_trial, prob, asymp.LCL, asymp.UCL, panel) %>%
  mutate(panel = factor(panel, levels = panelLevels))

# Rolling means per panel (test phase only)
thickObDiffRolling <- thickObDiffData %>%
  filter(manipulation == "test") %>%
  arrange(rank_trial) %>%
  group_by(individual, diffuser_condition) %>%
  mutate(pcorr = rollmean(decision, k = 3, fill = NA, align = "center")) %>%
  ungroup() %>%
  group_by(rank_trial, diffuser_condition) %>%
  summarise(pcorr = mean(pcorr, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    panel = factor(as.character(diffuser_condition), levels = panelLevels)
  )

# Sample size labels per panel (test phase only)
thickObDiffNLabels <- thickObDiffData %>%
  filter(manipulation == "test") %>%
  group_by(diffuser_condition) %>%
  summarise(
    n_wasps   = n_distinct(individual),
    n_choices = n(),
    label     = paste0("n = ", n_wasps, " wasps, ", n_choices, " choices"),
    .groups = "drop"
  ) %>%
  mutate(panel = factor(as.character(diffuser_condition), levels = panelLevels))

# Figure — 3 panels (one per diffuser condition)
pThickObDiff <- ggplot(emmThickObDiffPlot, aes(x = rank_trial, y = prob)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_point(data = thickObDiffRolling, aes(x = rank_trial, y = pcorr),
             colour = "black", alpha = 0.1) +
  geom_errorbar(data = thickObDiffEndPts,
                aes(x = rank_trial, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 1) +
  geom_point(data = thickObDiffEndPts, aes(x = rank_trial, y = prob)) +
  geom_text(data = thickObDiffNLabels, aes(label = label),
            x = Inf, y = -Inf, hjust = 1.05, vjust = -0.3,
            size = 2.5, inherit.aes = FALSE) +
  labs(x = "Trial", y = "% of decisions for reward") +
  scale_y_continuous(labels = scales::percent, limits = c(0.25, 1)) +
  facet_wrap(~ panel, nrow = 1, scales = "free_x",
             labeller = labeller(panel = panelLabels)) +
  theme_bw() +
  theme(
    panel.spacing.x  = unit(0, "lines"),
    strip.background = element_blank()
  )

# =============================================================================
# Combined figure
# =============================================================================

pThickObDiff   <- pThickObDiff   + theme(plot.margin = margin(t = 5, r = 5, b = 2, l = 5))
pPpPostNatcan  <- pPpPostNatcan  + theme(plot.margin = margin(t = 2, r = 5, b = 5, l = 5))

suppFig <- pThickObDiff / pPpPostNatcan +
  plot_layout(heights = c(1, 1), axis_titles = "collect_y") +
  plot_annotation(tag_levels = "A")

# =============================================================================
# Save
# =============================================================================

save(
  mPpPostNatcanCoefs,
  mThickObDiffCoefs,
  thickObDiffContrast,
  emmThickObDiffPlot,
  thickObDiffRolling,
  thickObDiffNLabels,
  suppLabels,
  suppFig,
  file = "scripts/intermediate_outputs/supp_analysis_results.RData"
)

message("supp_analysis_results.RData saved.")
