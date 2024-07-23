
# First things first, get the data into different format. Code pilfered from 2022 data analysis markdown.

library(tidyverse)
library(broom)
##### data cleaning +-45 degree exps:
# this code could be much simplified if I just fixed the trial numbering in the source excel datasheets, but i'm following the advice (probably incorrectly) to 'never change the source data'. i guess it keeps the mapping from the excel sheet to the fieldwork book. Truthfully,
# I should just fix the numbering in the excel file
# thick stripe ####

thick_obliq <- readxl::read_excel("data//data_summer_2023_20230817.xlsx",
                                   sheet = "thick_stripe_45_degrees", col_types = "text")

thick_obliq <- pivot_longer(thick_obliq, !individual, values_to = "decision", 
                      names_to = "trial", values_drop_na = TRUE)

thick_obliq$trial_number <- as.numeric(gsub("[^0-9]+", "", thick_obliq$trial))
thick_obliq$reward_side <- sub("^\\d+", "", thick_obliq$trial)
thick_obliq$trial <- NULL
thick_obliq$first <- as.integer(substr(thick_obliq$decision, 0, 1))
thick_obliq$experiment <- "thick_oblique"
thick_obliq$experiment_type <- "test"

# thin oblique stripes ####
thin_obliq <- readxl::read_excel("data//data_summer_2023_20230817.xlsx",
                                        sheet = "multi_small_stripes_45_degrees", col_types = "text")
# Because I restarted trial numbers everyday (and days are not separated by sheet), 
# I have to make new column names indicating total trial number (experience)
# Loop through the column names and append sequential column numbers
num_columns <- ncol(thin_obliq)
for (i in 2:num_columns) {
  col_name <- names(thin_obliq)[i]
  new_col_name <- paste(col_name, i-1, sep = "_")
  names(thin_obliq)[i] <- new_col_name
}

thin_obliq <- pivot_longer(
  thin_obliq, !individual, values_to = "decision", 
  names_to = "trial", values_drop_na = TRUE)

thin_obliq$trial_number <- as.numeric(gsub(".*_", "", thin_obliq$trial))
thin_obliq$reward_side <- gsub("[^A-Za-z]", "", thin_obliq$trial)
thin_obliq$trial <- NULL
# Not sure how to handle block trials. ignore or include in experience / trial number variable?
# they will obviously affect learning rate. They may even have outsize effect. for now, I'll remove block trials.
thin_obliq <- thin_obliq[thin_obliq$decision != "B*",]
thin_obliq$first <- as.integer(substr(thin_obliq$decision, 0, 1))
thin_obliq$experiment <- "thin_oblique"
thin_obliq$experiment_type <- "test"

# thin stripes 45 deg control ####
thin_obliq_ctrl <- readxl::read_excel(
  "data//data_summer_2023_20230817.xlsx",
  sheet = "control_multi_small_stripes_45", col_types = "text")
# Because I restarted trial numbers everyday, I have to make new column names indicating total trial number (experience)
# Loop through the column names and append column numbers
num_columns <- ncol(thin_obliq_ctrl) # I don't think this chunk is actually necessary for the control!
for (i in 2:num_columns) {
  col_name <- names(thin_obliq_ctrl)[i]
  new_col_name <- paste(col_name, i-1, sep = "_")
  names(thin_obliq_ctrl)[i] <- new_col_name
}

thin_obliq_ctrl <- pivot_longer(thin_obliq_ctrl, !individual, values_to = "decision", 
                                      names_to = "trial", values_drop_na = TRUE)

thin_obliq_ctrl$trial_number <- 
  as.numeric(gsub(".*_", "", thin_obliq_ctrl$trial))
thin_obliq_ctrl$reward_side <- gsub("[^A-Za-z]", "", thin_obliq_ctrl$trial)
thin_obliq_ctrl$trial <- NULL
thin_obliq_ctrl$first <- as.integer(substr(thin_obliq_ctrl$decision, 0, 1))
thin_obliq_ctrl$experiment <- "thin_oblique"
thin_obliq_ctrl$experiment_type <- "control"

# para vs perp ####

##### data cleaning perpendic vs parallel (0, 90 deg) exps:
perp2407 <- readxl::read_excel("data//data_summer_2023_20230817.xlsx",
                               sheet = "24_07_2023", col_types = "text",
                               n_max = 6)
perp2507 <- readxl::read_excel("data//data_summer_2023_20230817.xlsx",
                               sheet = "25_07_2023", col_types = "text",
                               n_max = 4)
perp2607 <- readxl::read_excel("data//data_summer_2023_20230817.xlsx",
                               sheet = "26_07_2023", col_types = "text",
                               n_max = 4)
perp2607_ctrl <- readxl::read_excel("data//data_summer_2023_20230817.xlsx",
                                    sheet = "26_07_2023_control",
                                    col_types = "text", n_max = 4)

# convert dataframes from wide to long format
perp_para <- list(perp2407, perp2507, perp2607, perp2607_ctrl)
rm(list = c("perp2407", "perp2507", "perp2607", "perp2607_ctrl"))
names(perp_para) <- c("24-07-2023_test", "25-07-2023_test", "26-07-2023_test", "26-07-2023_ctrl")

perp_para <- lapply(perp_para, function(x){
  x %>% pivot_longer(!individual, values_to = "decision", 
                     names_to = "trial", values_drop_na = TRUE)
})

perp_para <- data.table::rbindlist(perp_para, idcol = "date")
perp_para <- separate(perp_para, date, into = c("date", "experiment_type"), sep = "_")
perp_para$experiment <- "perp_para"

perp_para$trial_number <- as.numeric(gsub("[^0-9]+", "", perp_para$trial))
perp_para$reward_side <- gsub("[^A-Za-z]", "", perp_para$trial)

perp_para$trial <- NULL
perp_para$first <- as.integer(substr(perp_para$decision, 0, 1))
# Making similar to other dfs by removing date
perp_para$date <- NULL

# rerank trials. separate ranking for control and test. 
# Ahhhh I see. This is so that individuals don't have gaps missing in their data.
perp_para <- perp_para %>% 
  filter(individual != "?") %>%
  group_by(individual, experiment_type) %>%
  mutate(rank_trial = as.integer(rank(trial_number))) 

# Naturalistic canopy ####

natcan230917 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx", 
                                   sheet = "17_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230918 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                      
                                   sheet = "18_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230919 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                      
                                   sheet = "19_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230920 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                      
                                   sheet = "20_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230919_cont <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                 
                                        sheet = "19_09_2023_holldobler_control",
                                        col_types = "text", n_max = 8)

# pivoting data into long format
# I want a dataframe with the variables: date, individual, trial, choice number within trial and choice
# I also want to export this an excel file 
natcan <- list(natcan230917, natcan230918, natcan230919, natcan230920, natcan230919_cont)
names(natcan) <- c("natcan230917", "natcan230918", "natcan230919", "natcan230920",
                   "natcan230919_cont")
natcan <- lapply(natcan, function(x){
  x %>% pivot_longer(!individual, values_to = "decision", 
                     names_to = "trial_number_side", values_drop_na = T)
})


natcan <- data.table::rbindlist(natcan, idcol = "df")
rm(list = c("natcan230917", "natcan230918", "natcan230919", "natcan230920"))

natcan$trial_number <- as.numeric(gsub("[^0-9]+", "", natcan$trial_number_side))
natcan$reward_side <- sub("^\\d+", "", natcan$trial_number_side)
natcan$trial_number_side <- NULL

natcan <- natcan %>% 
  mutate(experiment = "nat_can",
         experiment_type = "test") 
  
natcan <- natcan %>%
  group_by(individual) %>%
  mutate(rank_trial = rank(trial_number))

natcan <- natcan %>% separate(decision, into = c("first", "decision"),
                              extra = "merge", sep = ",", remove = T) %>%
  mutate(first = as.numeric(first))

# all filtering of data should happen here:
excluded <- natcan %>% 
  filter(is.na(first) | # what was wrong with these NAs?
           df == "natcan230920") 

natcan <- natcan %>% 
  filter(first != "NA",
         # removing data on this day because it was after I ran a control. 
         # They may have gotten worse? I should plot it anyway at some point
         df != "natcan230920") 


# remove df variable to make similar to other dfs
natcan$df <- NULL
natcan$trial <- NULL


# Because some wasps were not present for some trials, we need to make a variable
# that describes the number of trials a specific wasp has undergone, not the overall
# trial number. 
# Another complication is how to treat multiple decisions per trial. I should look 
# at both wasp trial and wasp decision number.
## order the trial number variable from smallest to largest for each individual and start a new counter ranking the trials from smallest to largest for each individual

# join dataframes ####

# join the experiments:
# add a variable 'experiment_phase' that separates between the control and the testing phase of experiments

choices <- rbind(perp_para, natcan, thin_obliq, thin_obliq_ctrl, thick_obliq)
## order the trial number variable from smallest to largest for each individual and start a new counter ranking the trials from smallest to largest for each individual
choices <- choices %>% 
  filter(individual != "illegible_handwriting") %>% # this line removes 1 trial. could just remove this in original df?
  group_by(individual, experiment, experiment_type) %>%
  mutate(rank_trial = rank(trial_number)) 

rm(list = ls()[ls() != "choices"])
