# theme and colour scales
bin_fill <- c('1' = '#27b376', '0' = '#bf212f')
fill_scale <- scale_fill_manual(name = "decision", values = bin_fill)
color_scale <- scale_color_manual(name = "first_decision", values = bin_fill)
grad_scale <- scale_colour_gradient2(low = bin_fill[2], high = bin_fill[1], 
                                     midpoint = 0.5, limit = c(0,1), space = "Lab")

custom_theme_no_y_grid <- theme_grey() +
  theme(
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal grid lines
  )
custom_theme_no_x_grid <- theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor horizontal grid lines
  )
custom_theme_text_enlarged <- theme_bw()+
  theme(
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))

# experiment ID mapping
experiment_labels <- c(perp_para = "perpendicular/\nparallel\n stripes",
                       natcan = "naturalistic\n canopies", 
                       thick_oblique = "thick oblique\n stripes",
                       thin_oblique = "thin oblique\n stripes",
                       thick_oblique_diff = "thick oblique\n stripes (w diffuser)",
                       thick_oblique_apis = "thick oblique\n stripes (honeybee)")

#Create a custom color scale # from joran stack overflow answer
my_colours <- viridisLite::viridis(
  n = length(levels(choices$stimuli)),
  alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
names(my_colours) <- levels(choices$stimuli)
stim_colour_scale <- scale_colour_manual(
  values = my_colours,
  drop = T,
  breaks = c("thick_oblique", "thick_oblique_diff", "thick_oblique_apis", "thin_oblique", "perp_para", "natcan"),
  labels = experiment_labels )

# create a naturalistic vs artificial colour scale


