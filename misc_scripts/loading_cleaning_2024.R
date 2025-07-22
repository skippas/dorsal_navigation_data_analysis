# I think the entirety of the logic in this script is now taken care of in loading_cleaning.R
# script. So this script can probably be DELETED.

library(tidyverse)
# read in excel sheets and change format

tf_wide_data <- function(data, date, manipulation) {
  data <- data %>%
    pivot_longer(!individual, values_to = "decision", names_to = "trial",
                 values_drop_na = TRUE) %>%
    mutate(
      trial_number = as.numeric(gsub("[^0-9]+", "", trial)),
      reward_side = gsub("[^A-Za-z]", "", trial),
      first = as.integer(substr(decision, 0, 1)),
      manipulation = manipulation,
      date = date
    ) %>%
  return(data)
}


# Define a function to read and transform each sheet
read_transform <- function(sheet_name, date, manipulation) {
  readxl::read_excel("data_2024//wasp_data_2024.xlsx",
                     sheet = sheet_name, col_types = "text") %>%
    tf_wide_data(date = date, manipulation = manipulation)
}

# List of sheets, dates, and manipulations
sheets_info <- tibble(
  sheet = c("21_09_2024_thick_oblique_stripe", 
            "22_09_2024_thick_oblique_stripe", 
            "22_09_2024_thic_ob_diffuser_bot", 
            "23_09_2024_diffuser_whole_chamb",
            "23_09_2024_control"),
  date = c("21-09-2024", "22-09-2024", "22-09-2024", "23-09-2024", "23-09-2024"),
  manipulation = c("no_diffuser", "no_diffuser", "diffuser_bottom", "diffuser_whole", "control")
)

# Use map2 to read and transform each sheet
thick_obliq <- sheets_info %>%
  pmap_dfr(~ read_transform(..1, ..2, ..3))

# rank trial numbering within individuals
thick_obliq <- thick_obliq %>% 
  group_by(individual) %>%
  mutate(rank_trial = as.integer(rank(trial_number))) 

# Creating the third variable 'chosen_side'
thick_obliq <- thick_obliq %>%
  mutate(chosen_side = ifelse(decision == 1, reward_side, 
                              ifelse(reward_side == "L", "R", "L")))
