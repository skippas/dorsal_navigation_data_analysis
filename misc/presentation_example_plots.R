library(ggplot2)
library(dplyr)

set.seed(42)

n_trials <- 45
n_wasps  <- 14

# --- Simulate binary choices and fit real logistic regressions -------------

simulate_choices <- function(prob_fn, n_trials, n_wasps, seed) {
  set.seed(seed)
  expand.grid(wasp = 1:n_wasps, trial = 1:n_trials) |>
    mutate(
      p      = prob_fn(trial),
      choice = rbinom(n(), 1, p)
    )
}

# Learning: rising logistic starting at 50% at trial 0, reaching ~90% by end
choices_learn  <- simulate_choices(\(t) plogis(0.065 * t), n_trials, n_wasps, 1)
# Chance: flat ~47%, noticeably below 0.5 so the fit line doesn't overlap the dashed line
choices_chance <- simulate_choices(\(t) plogis(-0.12), n_trials, n_wasps, 2)

fit_learn  <- glm(choice ~ trial, data = choices_learn,  family = binomial)
fit_chance <- glm(choice ~ trial, data = choices_chance, family = binomial)

get_predictions <- function(fit, n_trials) {
  nd      <- data.frame(trial = 1:n_trials)
  pred    <- predict(fit, nd, type = "link", se.fit = TRUE)
  nd |>
    mutate(
      prob = plogis(pred$fit),
      lwr  = plogis(pred$fit - 1.96 * pred$se.fit),
      upr  = plogis(pred$fit + 1.96 * pred$se.fit)
    )
}

curve_learn  <- get_predictions(fit_learn,  n_trials)
curve_chance <- get_predictions(fit_chance, n_trials)

# --- Per-trial proportion correct (scatter points) -------------------------

rolling_learn <- choices_learn |>
  group_by(trial) |>
  summarise(pcorr = mean(choice), .groups = "drop")

rolling_chance <- choices_chance |>
  group_by(trial) |>
  summarise(pcorr = mean(choice), .groups = "drop")

# --- Endpoint predicted point ----------------------------------------------

end_learn  <- curve_learn  |> filter(trial == max(trial))
end_chance <- curve_chance |> filter(trial == max(trial))

# --- Shared theme ----------------------------------------------------------

pres_theme <- theme_bw(base_size = 16) +
  theme(
    panel.grid       = element_blank(),
    axis.title       = element_text(size = 14),
    axis.text        = element_text(size = 12),
    strip.background = element_blank(),
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5)
  )

chance_line <- geom_hline(
  yintercept = 0.5, linetype = "dashed", linewidth = 1.4, colour = "grey40"
)

# --- Plot 1: Learning ------------------------------------------------------

p_learn <- ggplot(curve_learn, aes(x = trial)) +
  chance_line +
  geom_line(aes(y = prob), linewidth = 1.2) +
  geom_point(data = rolling_learn, aes(y = pcorr),
             colour = "black", alpha = 0.15, size = 1.8) +
  geom_errorbar(data = end_learn,
                aes(y = prob, ymin = lwr, ymax = upr),
                width = 1.5, linewidth = 0.9) +
  geom_point(data = end_learn, aes(y = prob), size = 3) +
  labs(x = "Trial", y = "% correct choices",
       title = "Wasps can discriminate") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  pres_theme

# --- Plot 2: Chance --------------------------------------------------------

p_chance <- ggplot(curve_chance, aes(x = trial)) +
  chance_line +
  geom_line(aes(y = prob), linewidth = 1.2) +
  geom_point(data = rolling_chance, aes(y = pcorr),
             colour = "black", alpha = 0.15, size = 1.8) +
  geom_errorbar(data = end_chance,
                aes(y = prob, ymin = lwr, ymax = upr),
                width = 1.5, linewidth = 0.9) +
  geom_point(data = end_chance, aes(y = prob), size = 3) +
  labs(x = "Trial", y = "% correct choices",
       title = "Wasps cannot discriminate") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  pres_theme

# --- Simulate control phase (patterns removed, performance drops to ~50%) --

n_ctrl <- 20

# Control choices: flat at ~50%, trials starting from 1 (displayed from 0)
choices_ctrl <- simulate_choices(\(t) plogis(0), n_ctrl, n_wasps, 3)

fit_ctrl <- glm(choice ~ trial, data = choices_ctrl, family = binomial)

curve_ctrl <- get_predictions(fit_ctrl, n_ctrl)

rolling_ctrl <- choices_ctrl |>
  group_by(trial) |>
  summarise(pcorr = mean(choice), .groups = "drop")

end_ctrl <- curve_ctrl |> filter(trial == max(trial))

# --- Sequential combined plots ---------------------------------------------

col_learn  <- "#2ca02c"  # green
col_chance <- "#d62728"  # red

y_scale_combined <- scale_y_continuous(
  labels = scales::percent,
  limits = c(0, 1),
  breaks = seq(0, 1, 0.25)
)

# Step 1: can-discriminate only (green)
p_seq1 <- ggplot(curve_learn, aes(x = trial)) +
  chance_line +
  geom_line(aes(y = prob), colour = col_learn, linewidth = 1.2) +
  geom_point(data = rolling_learn, aes(y = pcorr),
             colour = col_learn, alpha = 0.25, size = 1.8) +
  geom_errorbar(data = end_learn,
                aes(y = prob, ymin = lwr, ymax = upr),
                colour = col_learn, width = 1.5, linewidth = 0.9) +
  geom_point(data = end_learn, aes(y = prob),
             colour = col_learn, size = 3) +
  labs(x = "Trial", y = "% correct choices") +
  y_scale_combined +
  pres_theme

# Step 2: both cases overlaid
p_seq2 <- p_seq1 +
  geom_line(data = curve_chance, aes(x = trial, y = prob),
            colour = col_chance, linewidth = 1.2) +
  geom_point(data = rolling_chance, aes(x = trial, y = pcorr),
             colour = col_chance, alpha = 0.25, size = 1.8) +
  geom_errorbar(data = end_chance,
                aes(x = trial, y = prob, ymin = lwr, ymax = upr),
                colour = col_chance, width = 1.5, linewidth = 0.9) +
  geom_point(data = end_chance, aes(x = trial, y = prob),
             colour = col_chance, size = 3)

# Step 3: control phase only, green, trials from 0
p_seq3 <- ggplot(curve_ctrl, aes(x = trial)) +
  chance_line +
  geom_line(aes(y = prob), colour = col_learn, linewidth = 1.2) +
  geom_point(data = rolling_ctrl, aes(y = pcorr),
             colour = col_learn, alpha = 0.25, size = 1.8) +
  geom_errorbar(data = end_ctrl,
                aes(y = prob, ymin = lwr, ymax = upr),
                colour = col_learn, width = 1.5, linewidth = 0.9) +
  geom_point(data = end_ctrl, aes(y = prob), colour = col_learn, size = 3) +
  labs(x = "Trial", y = "% correct choices") +
  scale_x_continuous(labels = \(x) x - 1) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  pres_theme

# --- Save ------------------------------------------------------------------

ggsave("output/presentation_learning.png",  p_learn,  width = 5, height = 4, dpi = 200)
ggsave("output/presentation_chance.png",    p_chance, width = 5, height = 4, dpi = 200)
ggsave("output/presentation_combined1.png", p_seq1,   width = 5, height = 4, dpi = 200)
ggsave("output/presentation_combined2.png", p_seq2,   width = 5, height = 4, dpi = 200)
ggsave("output/presentation_combined3.png", p_seq3,   width = 6, height = 4, dpi = 200)

message("Saved all five plots to output/")
