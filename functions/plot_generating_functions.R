# plotting helper functions
# purpose of this script is to make the flow of the main analysis script
# easier to follow by removing all the verbose code needed to generate and 
# (especially) fine-tune figures.

# Define custom theme (outside function)
custom_theme_experiment <- 
  custom_theme_no_x_grid +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Control which experiments are visible and their colors

# experiment_colors = c(
#       "perpPara_170725" = "#1F78B4",
#       "thinOb_150823" = "black",
#       "natcan1_170923" = "black",
#       "natcan2_300725" = "black",
#       "natcan3_080825" = "black",
#       "brightDiff_250725" = "black"
#       )
# 
# alpha_values = c(
#       "perpPara_170725" = 1,   
#       "thinOb_150823" = 0.3,   
#       "natcan1_170923" = 0,          
#       "natcan2_300725" = 0,
#       "natcan3_080825" = 0,
#       "brightDiff_250725" = 0,
#       "natcan4_180825" = 0,
#       "natcan5_290825" = 0 
#       )

alph <- 0.3
generate_plot <- function(data) {
  ggplot(data, aes(
    x = as.factor(trial_range),
    y = mean,
    color = experiment,
    group = experiment))+
    geom_point(position = position_dodge(width = 0.3), size = 3, alpha = alph)+
    geom_line(position = position_dodge(width = 0.3), size = 1, alpha = alph)+
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = alph,
                  position = position_dodge(width = 0.3), width = 0.2)+
    geom_hline(lty = 2, yintercept = 0.5)+
    # scale_color_manual(values = experiment_colors)+
    # scale_alpha_manual(values = alpha_values)+
    scale_y_continuous(labels = scales::percent)+
    labs(y = "% choices for reward in trial bin",
         x = "Trial")+
    custom_theme_experiment
}

# plots of individual performances
plot_ind_perf_across_trials <- function(data){
  
  ggplot(data = data, aes(x = trial_range, y = mean, colour = indiv_code))+
    geom_point(position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.5),
                  width = 0.2)+
    geom_hline(lty = 2, yintercept = 0.5)+
    facet_grid(experiment ~ manipulation, scales = "free_x", space = "free")+
    theme_classic()

}

plot_ind_perf_over_exp <- function(data){
  
  ggplot(data = data, aes(x = indiv_code, y = mean, colour = indiv_code))+
    geom_point(position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.5),
                  width = 0.2)+
    geom_hline(lty = 2, yintercept = 0.5)+
    facet_grid(experiment ~ manipulation, scales = "free_x", space = "free")+
    theme_classic()
  
}


