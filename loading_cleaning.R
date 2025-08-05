# ==============================================================================
# WASP BEHAVIORAL DATA PROCESSING PIPELINE
# ==============================================================================
# Purpose: Process and combine wasp behavioral choice data from multiple experiments

# Setup ========================================================================
rm(list = ls())
library(tidyverse)
library(broom)
library(readxl)

# CONFIGURATION ================================================================
# File paths
DATA_PATH_2023 <- "data/raw/2023/data_summer_2023_20230817.xlsx"
DATA_PATH_2024 <- "data/raw/2024/wasp_data_2024.xlsx" 
DATA_PATH_2025 <- "data/raw/2025/data_summer_2025.xlsx"
APIS_PATH <- "data/raw/2024/honeybee_dorsal_landmark_data_2024.xlsx"

# Experiment name mappings
EXPERIMENT_INITIALS <- c(
  "perp_para" = "PP", 
  "perp_para_2025" = "PP25", 
  "thick_oblique" = "THIC",
  "thick_oblique_diff" = "THICd", 
  "thick_oblique_apis" = "THICa", 
  "thin_oblique" = "THIN", 
  "natcan" = "NC"
)

# HELPER FUNCTIONS =============================================================

#' Transform raw Excel data to long format with decision sequences
transform_to_long_format <- function(data) {
  data %>%
    pivot_longer(
      cols = !individual, 
      values_to = "decision", 
      names_to = "trial",
      values_drop_na = TRUE
    ) %>%
    separate_longer_delim(decision, ",") %>%
    group_by(individual, trial) %>% 
    mutate(decision_within_trial = row_number()) %>%
    ungroup()
}

#' Clean and standardize decision data
clean_decisions <- function(data, keep_first_decision_only = TRUE) {
  cleaned_data <- data %>%
    # Remove non-numeric decisions (B, b, B*, etc.)
    filter(!grepl("[^0-9]", decision))
  
  if (keep_first_decision_only) {
    cleaned_data <- cleaned_data %>%
      filter(decision_within_trial == 1)
  }
  
  return(cleaned_data)
}

#' Extract trial information and create choice variables
extract_trial_info <- function(data) {
  data %>%
    mutate(
      reward_side = gsub("[^A-Za-z]", "", trial),
      trial = as.numeric(gsub("[^0-9]+", "", trial)),
      chosen_side = ifelse(
        decision == 1, 
        reward_side, 
        ifelse(reward_side == "L", "R", "L")
      )
    )
}

#' Add experimental metadata columns
add_experiment_metadata <- function(data, date = NULL, manipulation = NULL, 
                                    experiment = NULL, diffuser_condition = "absent") {
  if (!is.null(date)) {
    data <- data %>% mutate(date = date)
  }
  if (!is.null(manipulation)) {
    data <- data %>% mutate(manipulation = manipulation)
  }
  if (!is.null(experiment)) {
    data <- data %>% mutate(experiment = experiment)
  }
  
  data %>% mutate(diffuser_condition = diffuser_condition)
}

#' Complete data processing pipeline for wasp choice data
process_wasp_data <- function(data, ...) {
  data %>%
    transform_to_long_format() %>%
    clean_decisions(keep_first_decision_only = TRUE) %>%
    extract_trial_info() %>%
    add_experiment_metadata(...)
}

#' Read and process a single Excel sheet
read_and_process_sheet <- function(file_path, sheet_name, maxrows = Inf, ...) {
  raw_data <- read_excel(
    file_path, 
    sheet = sheet_name,
    col_types = "text", 
    n_max = maxrows
  )
  
  process_wasp_data(raw_data, ...)
}

# DATA LOADING =================================================================

# Thick oblique 2023 -----------------------------------------------------------
thick_oblique_2023 <- read_and_process_sheet(
  file_path = DATA_PATH_2023,
  sheet_name = "thick_stripe_45_degrees",
  manipulation = "test",
  experiment = "thick_oblique",
  date = "17-08-2023"
) %>%
  filter(individual != "illegible_handwriting")

# Thin oblique 2023 ------------------------------------------------------------
thin_sheets <- tibble(
  sheet = c("15_08_2023_many_oblique_stripes", 
            "16_08_2023_many_oblique_stripes",
            "16_08_2023_many_oblique_ctrl"),
  date = c("15-08-2023", "16-08-2023", "16-08-2023"), 
  manipulation = c("test", "test", "control"),
  maxrows = rep(8, 3)
)

thin_oblique_2023 <- thin_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = DATA_PATH_2023,
    sheet_name = ..1,
    date = ..2,
    experiment = "thin_oblique",
    manipulation = ..3,
    maxrows = ..4
  ))

# Perpendicular vs parallel 2023 ----------------------------------------------
perp_para_sheets <- tibble(
  sheet = c("24_07_2023", "25_07_2023", "26_07_2023", "26_07_2023_control"),
  date = c("24-07-2023", "25-07-2023", "26-07-2023", "26-07-2023"),
  manipulation = c("test", "test", "test", "control"),
  maxrows = c(6, 4, 4, 4)
)

perp_para_2023 <- perp_para_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = DATA_PATH_2023,
    sheet_name = ..1,
    date = ..2,
    experiment = "perp_para",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  filter(individual != "?")

# Naturalistic canopy 2023 ----------------------------------------------------
natcan_sheets <- tibble(
  sheet = c("17_09_2023_holldobler_canopies", "18_09_2023_holldobler_canopies",
            "19_09_2023_holldobler_canopies", "20_09_2023_holldobler_canopies",
            "19_09_2023_holldobler_control"),
  date = c("17-09-2023", "18-09-2023", "19-09-2023", "20-09-2023", "19-09-2023"), 
  manipulation = c("test", "test", "test", "test", "control"),
  maxrows = rep(8, 5)
)

natcan_2023 <- natcan_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = DATA_PATH_2023,
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  # Remove data from 20-09-2023 (ran after control, may have affected performance)
  filter(date != "20-09-2023")

# Thick oblique with diffuser 2024 --------------------------------------------
thick_diff_sheets <- tibble(
  sheet = c("21_09_2024_thick_oblique_stripe", "22_09_2024_thick_oblique_stripe", 
            "22_09_2024_thic_ob_diffuser_bot", "23_09_2024_diffuser_whole_chamb",
            "23_09_2024_control"),
  date = c("21-09-2024", "22-09-2024", "22-09-2024", "23-09-2024", "23-09-2024"),
  manipulation = c("test", "test", "test", "test", "control"),
  diffuser_condition = c("no_diffuser", "no_diffuser", "diffuser_bottom", 
                         "diffuser_whole", "diffuser_whole")
)

thick_oblique_diff_2024 <- thick_diff_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = DATA_PATH_2024,
    sheet_name = ..1,
    date = ..2,
    experiment = "thick_oblique_diff",
    manipulation = ..3,
    diffuser_condition = ..4
  ))

# Perpendicular parallel 2025 -------------------------------------------------
perp_para_2025_sheets <- tibble(
  sheet = c("170725", "180725", "190725_ctrl"),
  date = c("17-07-2025", "18-07-2025", "19-07-2025"),
  manipulation = c("test", "test", "control"),
  maxrows = rep(14, 3)
)

perp_para_2025 <- perp_para_2025_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = DATA_PATH_2025,
    sheet_name = ..1,
    date = ..2,
    experiment = "perp_para_2025",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  # Remove trial 34 data because a mistake was made 
  filter(trial != 34)

# naturalistic canopies 2025

# Honeybee (Apis) 2024 ---------------------------------------------------------
# This dataset has different format - doesn't need pivoting
apis_2024 <- read_excel(APIS_PATH, sheet = "Sheet1", col_types = "text") %>%
  group_by(individual, trial) %>%  
  mutate(
    decision_within_trial = row_number(),
    experiment = "thick_oblique_apis",
    manipulation = "test",
    diffuser_condition = "absent"
  ) %>%  
  filter(decision_within_trial == 1) %>%
  ungroup() %>%
  select(-c(time, notes)) %>%
  mutate(date = gsub("\\.", "-", date),
         trial = as.numeric(trial))


# COMBINE DATASETS =============================================================

choices <- bind_rows(
  thick_oblique_2023,
  thin_oblique_2023,
  perp_para_2023,
  natcan_2023,
  thick_oblique_diff_2024,
  perp_para_2025,
  apis_2024
)

# POST-PROCESSING ==============================================================

# Add trial ranking within individuals
choices <- choices %>%
  group_by(individual, experiment, manipulation) %>%
  mutate(
    trial = as.numeric(trial),
    rank_trial = as.integer(rank(trial))
  ) %>%
  ungroup()

# Add day variable based on experiment dates
choices <- choices %>%
  group_by(experiment) %>%
  mutate(
    date = as.Date(date, format = "%d-%m-%y"),
    day = as.integer(factor(date, levels = sort(unique(date))))
  ) %>%
  ungroup()

# Create unique individual codes across experiments
choices <- choices %>%
  mutate(
    experiment_initials = EXPERIMENT_INITIALS[experiment],
    indiv_code = paste0(experiment_initials, "_", individual)
  ) %>%
  select(-experiment_initials)

# Convert to factors
choices <- choices %>%
  mutate(across(any_of(c("experiment", "manipulation")), as.factor))

# Create stimuli grouping variable
choices <- choices %>%
  mutate(
    stimuli = recode(experiment, "thick_oblique_diff" = "thick_oblique"),
    stimuli = factor(stimuli)
  )

# Clean up environment (keep only final dataset)
rm(list = ls()[ls() != "choices"])

# Summary
cat("Data processing complete!\n")
cat("Final dataset 'choices' contains", nrow(choices), "observations\n")
cat("Experiments:", paste(unique(choices$experiment), collapse = ", "), "\n")