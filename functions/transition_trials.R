# Function to filter trials with alternating sides
filter_alternating_sides <- function(df) {
  df <- df %>%
    arrange(individual, rank_trial) %>% # Ensure the dataframe is ordered by trial_number within each individual
    group_by(individual) %>%
    # does this ignore the NAs? ie. it should be the previous trial for the individual, not the previous trial of the experiment (which they may or may not have participated in)
    mutate(previous_side = lag(reward_side)) %>% 
    filter(is.na(previous_side) | reward_side != previous_side) %>%
    #select(-previous_side) # Remove the helper column
    
    return(df)
}

identify_sideAlt_trials <- function(df) {
  df <- df %>%
    arrange(individual, rank_trial) %>%
    group_by(individual) %>%
    mutate(
      previous_side = lag(reward_side),
      sideAlt_trial = !is.na(previous_side) & reward_side != previous_side
    ) %>%
    ungroup()
  
  return(df)
}