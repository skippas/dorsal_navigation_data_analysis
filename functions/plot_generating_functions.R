# plotting helper functions
# purpose of this script is to make the flow of the main analysis script
# easier to follow by removing all the verbose code needed to generate and 
# (especially) fine-tune figures.

generate_plot <- function(data, same_axis = F) {
  # below ensures Ntrial / Nwasp text doesnt overlap when diff exps plotted on same axis
  if(same_axis == T) {
    data <- data %>%
      group_by(experiment) %>%
      mutate(
        n_wasp_offset = 0.45 - 0.03 * as.numeric(as.factor(experiment)),
        #n_trial_offset = 0.35 - 0.015 * as.numeric(as.factor(experiment))
      ) %>%
      ungroup()
  } else {
    n_wasp_offset = 0.45
    n_trial_offset = 0.4
  }
  ggplot(data, aes(
    x = as.factor(trial_range),
    y = mean, 
    color = perp_para_vs_other_stimuli,
    alpha = alpha_val,
    group = experiment))+
    geom_point(position = position_dodge(width = 0.3), size = 3)+
    geom_line(position = position_dodge(width = 0.3), size = 1)+
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.3),
                  width = 0.2)+
    geom_hline(lty = 2, yintercept = 0.5)+
    scale_alpha_identity()+
    # geom_text(aes(label = paste0("(", wasp_n, ", ", total,")"),
    #               y = n_wasp_offset), show.legend = FALSE)+
    #geom_text(aes(label = paste("Nt =", total), y = n_trial_offset))+
    #scale_color_viridis_d(labels = experiment_labels) +
    #stim_colour_scale+
    # scale_alpha_manual(values = c("natcan" = 1,
    #                               "thick_oblique" = 0.3,
    #                               "thin_oblique" = 0.3,
    #                               "perp_para" = 0.3),
    #                    guide = "none") +  # Set transparency per level
    scale_y_continuous(labels = scales::percent)+
    labs(y = "% choices for reward in trial bin",
         x = "Trial")+
    custom_theme_no_x_grid+
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.position = "top")
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


