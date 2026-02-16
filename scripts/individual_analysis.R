# individual analysis
library(tidyverse)
rm(list = ls())
source("loading_cleaning.R")
source("custom_themes_and_colour_palettes.R")


# is there a side bias of individuals within experiments? 
# are wasps more likely to develop side bias in absence of overhead stimuli (during control)?
# there's different ways to measure 'side bias' I suppose. Especially in terms of 'granularity'
# you're looking at: you could look across the whole experiment, or across 5 trials, or within a single trial.

# first, I want a simple metric that gives the side bias across the whole experiment
# maybe it makes sense to show that as a ratio. the ratio of L:R for example.
# But I don't care all that much about the 'polarity', ie. whether its left 
# or right thats dominant, rather im just interested in how dominant left or right is.
# the way i might try solve this is by always dividing the larger / dominant side
# by the smaller side. But perhaps theres a different way?


x <- choices %>% group_by(experiment, manipulation, indiv_code) %>%
  count(chosen_side) %>%
  summarise(
    bias_ratio = max(n) / min(n)
  )

# could compare test to control now, within each individual that took part in both
# and also look at the avg across all individuals
# but remember, there are other granularities to look at...
x %>% 
  ggplot(aes(x = manipulation, group = indiv_code, y = bias_ratio))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(experiment), scale = "free_x")
  

