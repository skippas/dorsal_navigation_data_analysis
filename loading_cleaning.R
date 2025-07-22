rm(list = ls())
# get data into appropriate formats
library(tidyverse)
library(broom)

tf_wide_data <- function(data, date = NULL, manipulation = NULL,
                         experiment = NULL, remove_block_trials = FALSE,
                         diffuser_condition = "absent") {
  
  data <- data %>%
    pivot_longer(!individual, values_to = "decision", names_to = "trial",
                 values_drop_na = TRUE) %>%
    separate_longer_delim(decision, ",") %>% # i guess this separates, but creating new rows rather than columns
    group_by(individual, trial) %>% 
    mutate(decision_within_trial = row_number()) %>% # here we get the sequences of decisions within trials each in their own row
    ungroup() %>%
    # Conditionally remove 'B', 'b', or 'B*' from 'decision' <- this doesnt seem conditional?
    filter(!grepl("[^0-9]", decision)) %>%  # [^0-9] matches any character not a digit
    # why do i keep only the first decision within trials below?
    filter(decision_within_trial == "1") %>% # KEEPING ONLY FIRST DECISION WITHIN TRIALS
    mutate(
      reward_side = gsub("[^A-Za-z]", "", trial),
      trial = as.numeric(gsub("[^0-9]+", "", trial)),
      # Creating the third variable 'chosen_side'
      chosen_side = ifelse(decision == 1, reward_side, 
                           ifelse(reward_side == "L", "R", "L")),
      # adding the diffuser paper condition
      diffuser_condition = diffuser_condition
    ) 

  # Conditionally add manipulation, date, experiment columns
  if (!is.null(manipulation)) {
    data <- data %>%
      mutate(manipulation = manipulation)
  }
  if (!is.null(date)) {
    data <- data %>%
      mutate(date = date)
  }
  if (!is.null(experiment)) {
    data <- data %>%
      mutate(experiment = experiment)
  }
  return(data)
}

# Define a function to read and transform each sheet
read_transform <- function(file_path, sheet_name, date = NULL,
                           manipulation = NULL, experiment = NULL, maxrows = Inf,
                           diffuser_condition = "absent",
                           remove_block_trials = FALSE) {
  readxl::read_excel(file_path, sheet = sheet_name,
                     col_types = "text", n_max = maxrows) %>%
    tf_wide_data(date = date, manipulation = manipulation,
                 experiment = experiment,
                 remove_block_trials = remove_block_trials)
}

# thick oblique ####
thick_obliq <- read_transform("data//raw//2023//data_summer_2023_20230817.xlsx", 
               sheet_name = "thick_stripe_45_degrees", manipulation = "test",
               experiment = "thick_oblique", date = "17-08-2023")
thick_obliq <- thick_obliq %>% filter(individual != "illegible_handwriting") 

# thin oblique stripes ####
sheets_info <- tibble(
  file_path = "data//raw//2023//data_summer_2023_20230817.xlsx",
  sheet = c("15_08_2023_many_oblique_stripes", "16_08_2023_many_oblique_stripes",
            "16_08_2023_many_oblique_ctrl"),
  date = c("15-08-2023", "16-08-2023", "16-08-2023"), 
  experiment = rep("thin_oblique", 3),
  manipulation = c("test", "test", "control"),
  maxrows = 8,
  remove_block_trials = TRUE)
  
thin_obliq <- sheets_info %>%
  pmap_dfr(~ read_transform(file_path = ..1, sheet_name = ..2, date = ..3,
                            experiment = ..4, manipulation = ..5, maxrows = ..6,
                            remove_block_trials = ..7))

# para vs perp ####

##### data cleaning perpendic vs parallel (0, 90 deg) exps:

sheets_info <- tibble(
  file_path = "data//raw//2023//data_summer_2023_20230817.xlsx",
  sheet = c("24_07_2023", "25_07_2023", "26_07_2023", "26_07_2023_control"),
  date = c("24-07-2023", "25-07-2023", "26-07-2023", "26-07-2023"),
  experiment = rep("perp_para", 4),
  manipulation = c("test", "test", "test", "control"),
  maxrows = c(6,4,4,4))

perp_para <- sheets_info %>%
  pmap_dfr(~ read_transform(file_path = ..1, sheet_name = ..2, date = ..3,
                            experiment = ..4, manipulation = ..5, maxrows = ..6))

perp_para <- perp_para %>% filter(individual != "?") 

# Naturalistic canopy ####
sheets_info <- tibble(
  file_path = "data//raw//2023//data_summer_2023_20230817.xlsx",
  sheet = c("17_09_2023_holldobler_canopies", "18_09_2023_holldobler_canopies",
            "19_09_2023_holldobler_canopies", "20_09_2023_holldobler_canopies",
            "19_09_2023_holldobler_control"),
  date = c("17-09-2023", "18-09-2023", "19-09-2023", "20-09-2023", "19-09-2023"), 
  experiment = rep("natcan", 5),
  manipulation = c("test", "test", "test","test", "control"),
  maxrows = rep(8,5))

natcan <- sheets_info %>%
  pmap_dfr(~ read_transform(file_path = ..1, sheet_name = ..2, date = ..3,
                            experiment = ..4, manipulation = ..5, maxrows = ..6))

# removing data on this day because it was after I ran a control.
# They may have gotten worse? I should plot it anyway at some point
natcan <- natcan %>% filter(date != "20-09-2023") 

# how to treat multiple decisions per trial. I should look 
# at both wasp trial and wasp decision number.

# 2024 data ####
# read in excel sheets and change format

# List of sheets, dates, and manipulations
sheets_info <- tibble(
  file_path = "data/raw/2024/wasp_data_2024.xlsx",
  sheet = c("21_09_2024_thick_oblique_stripe", "22_09_2024_thick_oblique_stripe", 
            "22_09_2024_thic_ob_diffuser_bot", "23_09_2024_diffuser_whole_chamb",
            "23_09_2024_control"),
  date = c("21-09-2024", "22-09-2024", "22-09-2024", "23-09-2024", "23-09-2024"),
  experiment = rep("thick_oblique_diff", 5), 
  manipulation = c("test", "test", "test", "test", "control"),
  maxrows = rep(Inf, 5),
  diffuser_condition = c("no_diffuser", "no_diffuser", "diffuser_bottom", "diffuser_whole",
                         "diffuser_whole")
)

thick_obliq_diff <- sheets_info %>%
  pmap_dfr(~ read_transform(file_path = ..1, sheet_name = ..2, date = ..3,
                            experiment = ..4, manipulation = ..5,
                            maxrows = ..6, diffuser_condition = ..7))

# load up apis experiment data. function doesnt work because this sheet doesnt
# need to be pivoted
apis <- readxl::read_excel("data/raw/2024/honeybee_dorsal_landmark_data_2024.xlsx",
                           sheet = "Sheet1", col_types = "text")
# label the i-th choice within each trial for each individual and keep only 1st choices
# row_number ranks the decision_within_trial by its order of appearance / entry in the datasheet 
apis <- apis %>%
  group_by(individual, trial) %>%  
  mutate(decision_within_trial = row_number(),
         experiment = "thick_oblique_apis",
         manipulation = "test",
         diffuser_condition = "absent") %>%  
  filter(decision_within_trial == "1") %>%
  ungroup() %>%
  # drop time and decision within trial to make apis df similar to choices df
  select(-c(time, notes)) 
apis$date <- gsub("\\.", "-", apis$date)

# 2025 data ####
sheets_info <- tibble(
  file_path = "data/raw/2025/data_summer_2025.xlsx",
  sheet = c("21_09_2024_thick_oblique_stripe", "22_09_2024_thick_oblique_stripe", 
            "22_09_2024_thic_ob_diffuser_bot"),
  date = c("21-09-2024", "22-09-2024", "22-09-2024"),
  experiment = rep("perp_para", 3), 
  manipulation = c("test", "test", "control"),
  maxrows = rep(Inf, 3)
)

perp_para_2025 <- sheets_info %>%
  pmap_dfr(~ read_transform(file_path = ..1, sheet_name = ..2, date = ..3,
                            experiment = ..4, manipulation = ..5,
                            maxrows = ..6, diffuser_condition = ..7))

# join dataframes ####
choices <- rbind(perp_para, natcan, thin_obliq, thick_obliq, thick_obliq_diff, apis)

# rank trial numbering within individuals
choices <- choices %>% 
  group_by(individual, experiment, manipulation) %>%
  mutate(trial = as.numeric(trial),
         rank_trial = as.integer(rank(trial)))

# create 'day' variable to see the effect of new day on choices
choices <- choices %>%
  group_by(experiment) %>%
  mutate(date = as.Date(date, format = "%d-%m-%y"),
         day = as.integer(factor(date, levels = sort(unique(date)))))

# individuals in different experiments often had same numbers.
# generate unique codenames ensuring they can be distinguished. 
# NB. some individuals were used in multiple experiments, check this and give same codenames.
initials_mapping <- c("perp_para" = "PP", "thick_oblique" = "THIC",
                      "thick_oblique_diff = THICd", "thick_oblique_apis" = "THICa", 
                      "thin_oblique" = "THIN", "natcan" = "NC")
choices <- choices %>%
  mutate(
    experiment_initials = initials_mapping[experiment],
    indiv_code = paste0(experiment_initials, "_", individual)
  ) %>%
  select(-experiment_initials) 
  
choices <- choices %>%
  mutate(across(any_of(c("experiment", "manipulation")), as.factor))

# create new variable 'stimuli' that combines the thick_oblique data
choices <- choices %>%
  mutate(stimuli = recode(experiment, "thick_oblique_diff" = "thick_oblique"),
         stimuli = factor(stimuli))

rm(list = ls()[ls() != "choices"])

