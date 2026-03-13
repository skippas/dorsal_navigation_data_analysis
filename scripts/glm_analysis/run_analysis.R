# =============================================================================
# run_analysis.R
# Fits all models and generates emmeans predictions.
# Run this script when data or model specifications change.
# Output: scripts/glm_analysis/analysis_results.RData
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

setwd(rprojroot::find_rstudio_root_file())

# -----------------------------------------------------------------------------
# Load and clean data
# -----------------------------------------------------------------------------

source("scripts/loading_cleaning.R")

styleEnv <- new.env(parent = environment())
source("scripts/custom_themes_and_colour_palettes.R", local = styleEnv)
plotStyle <- list(
  themeTextEnlarged = styleEnv$custom_theme_text_enlarged
)
rm(styleEnv)

choices %<>% filter(
  ! experiment %in% c("thick_oblique_apis", "thickOb_140823", "thickObDiff_210924",
                      "perpPara_240723", "perpParaPostNatcan_090925")
)

choices$decision    <- as.numeric(choices$decision)
choices$manipulation <- relevel(choices$manipulation, ref = "test")

# -----------------------------------------------------------------------------
# Test-phase models (mTest)
# Fit one model per experiment using test-phase data only.
# Produces: mTest, mTestTidy, emmTestAll, emmTestPts
# -----------------------------------------------------------------------------

mTest <- choices %>%
  group_by(experiment) %>%
  filter(n_distinct(manipulation) == 1) %>%
  split(.$experiment, drop = TRUE) %>%
  map(~ glmer(
    decision ~ rank_trial + sideAlt_trial + reward_side + (1 | individual),
    data = .x, family = binomial
  ))

mTestTidy <- map_df(mTest, broom.mixed::tidy, conf.int = TRUE, .id = "experiment") %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  )

emmTestAll <- map2(
  .x = mTest, .y = names(mTest),
  function(m, exp_id) {
    emmeans(m, ~ rank_trial,
            cov.keep = "rank_trial",
            type = "response") %>%
      as.data.frame() %>%
      mutate(experiment = exp_id)
  }
) %>% list_rbind()

emmTestAll <- left_join(
  emmTestAll,
  unique(choices[, c("experiment", "nat_or_art")]),
  by = "experiment"
)

emmTestPts <- emmTestAll %>%
  group_by(experiment) %>%
  mutate(mid_trial = ceiling((min(rank_trial) + max(rank_trial)) / 2)) %>%
  filter(rank_trial == max(rank_trial) | rank_trial == 1 | rank_trial == mid_trial) %>%
  select(-mid_trial)

# -----------------------------------------------------------------------------
# Test-control models (mTc)
# Fit one model per experiment that has both test and control phases.
# Produces: mTc, mTcTidy, emmTcAll, emmTcPts, contTcLn, contTcRsp
# -----------------------------------------------------------------------------

mTc <- choices %>%
  group_by(experiment) %>%
  filter(n_distinct(manipulation) > 1) %>%
  split(.$experiment, drop = TRUE) %>%
  map(~ glmer(
    decision ~ rank_trial * manipulation + sideAlt_trial + reward_side + (1 | individual),
    data = .x, family = binomial
  ))

mTcTidy <- map_df(mTc, broom.mixed::tidy, conf.int = TRUE, .id = "experiment") %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  )

emmPredCont <- map2(
  .x = mTc, .y = names(mTc),
  function(m, exp_id) {
    obs     <- model.frame(m) %>% distinct(manipulation, rank_trial)
    testEnd  <- max(obs[obs$manipulation == "test", ]$rank_trial)
    ctrlStart <- 1

    emmAll <- emmeans(m, ~ rank_trial * manipulation,
                      cov.keep = "rank_trial",
                      type = "response") %>%
      as.data.frame() %>%
      semi_join(obs, by = c("manipulation", "rank_trial")) %>%
      mutate(experiment = exp_id)

    emmCont <- emmeans(m, ~ manipulation * rank_trial,
                       at = list(rank_trial = c(ctrlStart, testEnd)))
    emmCont <- subset(
      emmCont,
      (manipulation == "control" & rank_trial == ctrlStart) |
      (manipulation == "test"    & rank_trial == testEnd)
    )
    grid <- emmCont@grid
    ord  <- order(grid$manipulation == "test", grid$rank_trial)
    emmCont <- emmCont[ord]

    cont <- contrast(emmCont, list("test_end - control_start" = c(-1, 1)))

    contLink <- summary(cont, infer = c(TRUE, TRUE)) %>%
      rename(logOddsRatio = estimate) %>%
      mutate(experiment = exp_id, control_start = ctrlStart, test_end = testEnd)

    contResp <- as.data.frame(summary(cont, type = "response")) %>%
      mutate(experiment = exp_id, control_start = ctrlStart, test_end = testEnd)

    list(emmAll = emmAll, contLink = contLink, contResp = contResp)
  }
)

emmPredCont <- transpose(emmPredCont)
emmPredCont <- map(emmPredCont, list_rbind)
emmTcAll  <- emmPredCont[["emmAll"]]
contTcLn  <- emmPredCont[["contLink"]]
contTcRsp <- emmPredCont[["contResp"]]
rm(emmPredCont)

emmTcAll <- left_join(
  emmTcAll,
  unique(choices[, c("experiment", "nat_or_art")]),
  by = "experiment"
)

contTcLn <- left_join(
  contTcLn,
  unique(choices[, c("experiment", "nat_or_art")]),
  by = "experiment"
)

emmTcPts <- emmTcAll %>%
  group_by(nat_or_art, experiment, manipulation) %>%
  filter(
    (manipulation == "test"    & rank_trial == max(rank_trial)) |
    (manipulation == "control" & rank_trial == 1)
  )

# -----------------------------------------------------------------------------
# Combined emmAll and emmPts (single source of truth for predictions)
# TC experiments: taken from emmTcAll (includes both test and control phases)
# Test-only experiments: taken from emmTestAll (manipulation = "test" added)
# Produces: emmAll, emmPts
# -----------------------------------------------------------------------------

emmAll <- bind_rows(
  emmTcAll,
  emmTestAll %>%
    filter(!experiment %in% unique(emmTcAll$experiment)) %>%
    mutate(manipulation = "test")
) %>%
  mutate(manipulation = factor(manipulation, levels = c("test", "control")))

# All four point types: first, middle, last_test, ctrl_start
# ctrl_start is NA for experiments without a control phase
emmPts <- bind_rows(
  emmAll %>%
    filter(manipulation == "test") %>%
    group_by(experiment, nat_or_art) %>%
    mutate(mid_trial = ceiling((min(rank_trial) + max(rank_trial)) / 2)) %>%
    filter(rank_trial == 1 | rank_trial == mid_trial | rank_trial == max(rank_trial)) %>%
    mutate(trial_pos = case_when(
      rank_trial == 1          ~ "first",
      rank_trial == mid_trial  ~ "middle",
      TRUE                     ~ "last_test"
    )) %>%
    select(-mid_trial) %>%
    ungroup(),
  emmAll %>%
    filter(manipulation == "control", rank_trial == 1) %>%
    mutate(trial_pos = "ctrl_start")
)

# -----------------------------------------------------------------------------
# Rolling mean choices (for figures)
# Produces: choices_rolling
# -----------------------------------------------------------------------------

choices_rolling <- choices %>%
  arrange(rank_trial) %>%
  group_by(individual, experiment, nat_or_art, manipulation) %>%
  mutate(pcorr = rollmean(decision, k = 3, fill = NA, align = "center")) %>%
  ungroup() %>%
  group_by(rank_trial, experiment, nat_or_art, manipulation) %>%
  summarise(pcorr = mean(pcorr), .groups = "drop")

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(
  choices,
  choices_rolling,
  mTest, mTestTidy,
  mTc, mTcTidy, contTcLn, contTcRsp,
  emmAll, emmPts,
  plotStyle,
  file = "scripts/glm_analysis/analysis_results.RData"
)

message("analysis_results.RData saved.")
