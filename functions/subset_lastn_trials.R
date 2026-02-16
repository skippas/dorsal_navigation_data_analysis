# This function finds the last n trials of the TEST phase of the experiment
# is trial > max(trial) - n ?
# also would be nice if the function prints the max trial for each exp and
# the trial range it found

subset_last_n_trials <- function(data, trial_variable) {
  # Filter and convert
  data <- data[data$manipulation == "test", ]
  data$rank_trial <- as.numeric(data$rank_trial)
  
  # Calculate max for each experiment
  max_trials <- tapply(data[[trial_variable]], data$experiment, max)
  print("Max trials by experiment:")
  print(max_trials)
  
  # Create filter condition
  experiment_maxes <- max_trials[data$experiment]
  keep_rows <- data[[trial_variable]] > (experiment_maxes - 15)
  
  return(data[keep_rows, ])
}

