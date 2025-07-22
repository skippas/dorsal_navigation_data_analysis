thin_obliq <- readxl::read_excel("data//raw//2023//data_summer_2023_20230817.xlsx",
                                 sheet = "multi_small_stripes_45_degrees",
                                 col_types = "text")
# trial numbers in sheet restarted each day. So we loop through the column names and append sequential column numbers
num_columns <- ncol(thin_obliq) 
for (i in 2:num_columns) {
  rew_sides <- gsub("[^A-Za-z]", "", colnames(thin_obliq)[i])
  names(thin_obliq)[i] <- paste0(i-1, rew_sides)
}
# remove all block trials (b / B / B*)
# Not sure how to handle block trials. ignore or include in experience / trial number variable?
# they will obviously affect learning rate. They may even have outsize effect. for now, I'll remove block trials.
thin_obliq <- thin_obliq %>%
  mutate(across(everything(), ~ gsub("B\\*?|b", "", .))) # i think there are two cells that have only a b / B. These are trials that are lost from the dataframe for those indivs

# write a new data file to processed data directory
openxlsx::write.xlsx(thin_obliq, file = "data/processed/2023/thin_obliq.xlsx")


