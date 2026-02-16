# TO DO:
# Change individual numbers to individual codes. ie. the column indiv should 
# have codes not numbers (which are ambiguouos between exps)

# ==============================================================================
# WASP BEHAVIORAL DATA PROCESSING PIPELINE
# ==============================================================================
# Purpose: Process and combine wasp behavioral choice data from multiple experiments

# Setup ========================================================================
rm(list = ls())
library(tidyverse)
library(broom)
library(readxl)
library(here)
library(magrittr)

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

# Perpendicular vs parallel 2023 ----------------------------------------------
perpPara_240723_sheets <- tibble(
  sheet = c("perpPara_240723", "perpPara_250723", "perpPara_260723", "perpPara_ctrl_260723"),
  date = c("24-07-2023", "25-07-2023", "26-07-2023", "26-07-2023"),
  manipulation = c("test", "test", "test", "control"),
  maxrows = c(6, 4, 4, 4)
)

perpPara_240723 <- perpPara_240723_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = here('data/raw/2023/perpPara_240723.xlsx'),
    sheet_name = ..1,
    date = ..2,
    experiment = "perpPara_240723",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  filter(individual != "?")


# Thick oblique 2023 -----------------------------------------------------------

thickOb_140823_sheets <- tibble(
  sheet = c("thickOb_140823", "thickOb_150823"),
  date = c("14-08-2023", "15-08-2023"), 
  manipulation = c("test", "test"),
  maxrows = rep(8, 2)
)

thickOb_140823 <- thickOb_140823_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2023/thickOb_140823.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "thickOb_140823",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  filter(individual != "illegible_handwriting")

# Thin oblique 2023 ------------------------------------------------------------
thin_sheets <- tibble(
  sheet = c("thinOb_150823", "thinOb_160823", "thinOb_ctrl_160823"),
  date = c("15-08-2023", "16-08-2023", "16-08-2023"), 
  manipulation = c("test", "test", "control"),
  maxrows = rep(8, 3)
)

thin_oblique_2023 <- thin_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2023/thinOb_150823.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "thinOb_150823",
    manipulation = ..3,
    maxrows = ..4
  ))

# Naturalistic canopy 2023 ----------------------------------------------------
natcan1_170923_sheets <- tibble(
  sheet = c("natcan1_170923", "natcan1_180923", "natcan1_190923",
            "natcan1_200923", "natcan1_ctrl_190923"),
  date = c("17-09-2023", "18-09-2023", "19-09-2023", "20-09-2023", "19-09-2023"), 
  manipulation = c("test", "test", "test", "test", "control"),
  maxrows = rep(8, 5)
)

natcan1_170923 <- natcan1_170923_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2023/natcan1_170923.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan1_170923",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  # Remove data from 20-09-2023 (ran after control, may have affected performance)
  filter(date != "20-09-2023")

# Thick oblique with diffuser 2024 --------------------------------------------
# NOTE: Originally I forgot to include 20_09_2024 data sheet. Perhaps why
# it seems they were immediately above chance? Re-check analysis
thickObDiff_sheets <- tibble(
  sheet = c("thickOb_200924", "thickOb_210924", "thickOb_220924",
            "thickOb_diffBot_220924", "230924_thickObDiffWhole", "thickOb_ctrl_230924"),
  date = c("20-09-2024", "21-09-2024", "22-09-2024",
           "22-09-2024", "23-09-2024", "23-09-2024"),
  manipulation = c("test", "test", "test", "test", "test", "control"),
  diffuser_condition = c("no_diffuser", "no_diffuser", "no_diffuser", 
                         "diffuser_bottom", "diffuser_whole", "diffuser_whole")
)

thickObDiff_210924 <- thickObDiff_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2024/thickObDiff_210924.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "thickObDiff_210924",
    manipulation = ..3,
    diffuser_condition = ..4
  ))

# Honeybee (Apis) 2024 ---------------------------------------------------------
# This dataset has different format - doesn't need pivoting
apis_2024 <- read_excel(
  'data/raw/2024/honeybee_dorsal_landmark_data_2024.xlsx',
  sheet = "Sheet1", col_types = "text") %>%
  group_by(individual, trial) %>%  
  mutate(
    decision_within_trial = row_number(),
    experiment = "thick_oblique_apis",
    manipulation = "test",
    diffuser_condition = "absent"
  ) %>%  
  filter(decision_within_trial == 1) %>%
  ungroup() %>%
#  select(-c(time, notes)) %>% # not sure why that line isnt working
  mutate(date = gsub("\\.", "-", date),
         trial = as.numeric(trial))

# 2025 experiments ------------------------------------------------------------

# perpPara
perpPara_170725_sheets <- tibble(
  sheet = c("perpPara_170725", "perpPara_180725", "perpPara_ctrl_190725"),
  date = c("17-07-2025", "18-07-2025", "19-07-2025"),
  manipulation = c("test", "test", "control"),
  maxrows = rep(14, 3)
)

perpPara_170725 <- perpPara_170725_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/perpPara_170725.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "perpPara_170725",
    manipulation = ..3,
    maxrows = ..4
  )) %>%
  # Remove trial 34 data because a mistake was made 
  filter(trial != 34)

# natcan2 

natcan2_sheets <- tibble(
  sheet = c("natcan2_300725", "natcan2_310725", "natcan2_010825"),
  date = c("30-07-2025", "31-07-2025", "01-08-2025"),
  manipulation = c("test", "test", "test"),
  maxrows = rep(14, 3)
)

natcan2 <- natcan2_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/natcan2_300725.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan2_300725",
    manipulation = ..3,
    maxrows = ..4
  ))

# natcan3 

natcan3_sheets <- tibble(
  sheet = c("natcan3_080825", "natcan3_090825", "natcan3_ctrl_090825"),
  date = c("08-08-2025", "09-08-2025", "09-08-2025"),
  manipulation = c("test", "test", "control"),
  maxrows = rep(14, 3)
)

natcan3 <- natcan3_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/natcan3_080825.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan3_080825",
    manipulation = ..3,
    maxrows = ..4
  ))

# natcan4

natcan4_sheets <- tibble(
  sheet = c("natcan4_180825", "natcan4_190825", "natcan4_200825", "natcan4_200825"),
  date = c("18-08-2025", "19-08-2025", "20-08-2025", "20-08-2025"),
  manipulation = c("test", "test", "test", "control"),
  maxrows = rep(14, 4)
)

natcan4_180825 <- natcan4_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/natcan4_180825.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan4_180825",
    manipulation = ..3,
    maxrows = ..4
  ))

# natcan5

natcan5_sheets <- tibble(
  sheet = c("natcan5_290825", "natcan5_300825"),
  date = c("29-08-2025", "30-08-2025"),
  manipulation = c("test", "test"),
  maxrows = rep(15, 2)
)

natcan5_290825 <- natcan5_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/natcan5_290825.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan5_290825",
    manipulation = ..3,
    maxrows = ..4
  ))

# natcan6

natcan6_sheets <- tibble(
  sheet = c("natcan6_070925", "natcan6_080925"),
  date = c("07-09-2025", "08-09-2025"),
  manipulation = c("test", "test"),
  maxrows = rep(13, 2)
)

natcan6_070925 <- natcan6_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/natcan6_070925.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan6_070925",
    manipulation = ..3,
    maxrows = ..4
  ))

# perpPara directly following natcan6 (same individuals and time of season) 

perpParaPostNatcan_090925 <- 
  read_and_process_sheet(
    file_path = 'data/raw/2025/natcan6_070925.xlsx',
    sheet_name = "perpPara_090925",
    date = c("09-09-2025"),
    manipulation = c("test"),
    maxrows = 13,
    experiment = "perpParaPostNatcan_090925"
  )

# natcan3 repeated

natcan3_190925_sheets <- tibble(
  sheet = c("natcan3_190925", "natcan3_200925", "natcan3_210925",
            "natcan3_220925", "natcan3_ctrl_230925"),
  date = c("19-09-2025", "20-09-2025", "21-09-2025", "22-09-2025", "23-09-2025"),
  manipulation = c("test", "test", "test", "test", "control"),
  maxrows = rep(13, 5)
)

natcan3_190925 <- natcan3_190925_sheets %>%
  pmap_dfr(~ read_and_process_sheet(
    file_path = 'data/raw/2025/natcan3_190925.xlsx',
    sheet_name = ..1,
    date = ..2,
    experiment = "natcan3_190925",
    manipulation = ..3,
    maxrows = ..4
  ))

# brightness discrimination 2025 -----------------------------------------------

brightDiff_250725 <- read_and_process_sheet(
  file_path = 'data/raw/2025/brightDiff_250725.xlsx',
  sheet_name = "brightDiff_250725",
  date = c("08-08-2025"),
  manipulation = c("test"),
  maxrows = 14,
  experiment = "brightDiff_250725"
)

# COMBINE DATASETS =============================================================

choices <- bind_rows(
  thickOb_140823,
  thin_oblique_2023,
  perpPara_240723,
  natcan1_170923,
  thickObDiff_210924,
  perpPara_170725,
  natcan2,
  natcan3,
  natcan4_180825,
  natcan5_290825,
  natcan6_070925,
  perpParaPostNatcan_090925,
  natcan3_190925,
  brightDiff_250725,
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

# identify 'side alternation' trials, where reward side differs from prev trial
source("functions/transition_trials.R")
choices <- identify_sideAlt_trials(choices)

# Add day variable based on experiment dates
choices <- choices %>%
  group_by(experiment) %>%
  mutate(
    date = as.Date(date, format = "%d-%m-%y"),
    day = as.integer(factor(date, levels = sort(unique(date))))
  ) %>%
  ungroup()

# divide stimuli into 'naturalistic' and artificial for comparison
choices %<>% 
  mutate(nat_or_art = ifelse(str_detect(experiment, "natcan"), 
                             "naturalistic", "artificial"))

# Create unique individual codes across experiments: NOTE: some indiv were same across experiments, eg. in perpParaPostNatcan
# choices %<>%
#   mutate(unique_id = paste(experiment, individual, sep = "_"))
# x <-choices %>%
#   mutate(unique_id = group_indices(., experiment, individual))

# Convert to factors
choices <- choices %>%
  mutate(across(any_of(c("experiment", "manipulation", "day")), as.factor))

# Create stimuli grouping variable. Is this necessary???
# choices <- choices %>%
#   mutate(
#     stimuli = recode(experiment, "thick_oblique_diff" = "thick_oblique"),
#     stimuli = factor(stimuli)
#   )

# Clean up environment (keep only final dataset)
rm(list = ls()[ls() != "choices"])

# Summary
cat("Data processing complete!\n")
cat("Final dataset 'choices' contains", nrow(choices), "observations\n")
cat("Experiments:", paste(unique(choices$experiment), collapse = ", "), "\n")