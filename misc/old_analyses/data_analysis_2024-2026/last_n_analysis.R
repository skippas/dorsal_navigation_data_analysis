# last_n analysis

# get emmeans of experiments at start and end of experiments
last_n_tbl <- choices_test %>%
  group_by(experiment) %>%
  summarise(
    max_trial = max(rank_trial),
    last_trials = list(
      seq.int(
        max(max_trial - last_n + 1),
        max_trial
      )
    )
  )

avg_last_n_for_experiment <- function(model, exp, trials) {
  
  emm <- emmeans(model, ~ rank_trial, 
                 at = list(rank_trial = trials,
                           experiment = exp))
  
  contrast(emm, 
           method = list(
             avg_last_n = rep(1 / length(trials), length(trials))
           )
  ) %>%
    regrid(transform = "response") %>%
    as.data.frame() %>%
    mutate(experiment = exp)
}

results_last_n <- last_n_tbl %>%
  mutate(
    result = purrr::map2(
      experiment,
      last_trials,
      ~ avg_last_n_for_experiment(
        model = art_glm,
        exp = .x,
        trials = .y
      )
    )
  ) %>%
  tidyr::unnest(result)

# extra random code
avg_last_n <- contrast(
  emmDay1,
  method = list("avg_last_n" = rep(1 / length(last_trials), length(last_trials)))
)

avg_last_n_resp <- avg_last_n %>%
  regrid(avg_last_n,transform = "response")
summary(avg_last_n_resp, infer = TRUE, type = "response")
