set.seed(1)  # Set a seed for reproducibility

# Define the number of trials
num_trials <- 30

# Generate the changing probabilities for each variable
prob_1 <- c(seq(0.5, 0.9,  length.out = 20), rep(0.9, 10))
prob_2 <- c(seq(0.5, 0.7, length.out = 20), rep(0.7, 10))

# Simulate the binary outcomes based on the probabilities
outcome_1 <- rbinom(num_trials, 1, prob_1)
outcome_2 <- rbinom(num_trials, 1, prob_2)

# Create a data frame with the outcomes and trial numbers
data <- data.frame(Trial = 1:num_trials, Outcome1 = outcome_1, Outcome2 = outcome_2)

model_1 <- glm(Outcome1 ~ Trial, data = data, family = binomial)
model_2 <- glm(Outcome2 ~ Trial, data = data, family = binomial)

# Generate the range of trial values for prediction
trial_range <- seq(1, num_trials, length.out = 100)

# Predict probabilities using the fitted models
pred_prob_1 <- predict(model_1, newdata = data.frame(Trial = trial_range), type = "response")
pred_prob_2 <- predict(model_2, newdata = data.frame(Trial = trial_range), type = "response")

# Plot the logistic regression curves
#png("exp1_training_results.png")

par(cex.lab = 1.5)
plot(data$Trial, data$Outcome1, type = "n", xlim = c(1, num_trials), ylim = c(0, 1), xlab = "Learning trials", ylab = "Probability of Success")
points(data$Trial, data$Outcome1, pch = 16)
lines(trial_range, pred_prob_1, col = "red", lwd = 2)
lines(trial_range, pred_prob_2, col = "blue", lwd = 2)
legend("right", legend = c("chromatic difference", "achromatic difference"),
       col = c("red", "blue"),
       lwd = 2, pch = 16, cex = 1.5)
dev.off()

elements2<- rnorm(10, mean = 0.8, sd = 0.05)
elements4<- rnorm(10, mean = 0.6, sd = 0.05)
png("exp1_test_results.png", width = 6, height = 6, units = "in", res = 300)
data.frame("proportion_correct_choices" = c(elements2, elements4), 
           "quality_of_difference" = c(rep("chromatic", length(elements2)),
                                    rep("achromatic", length(elements4)))) %>%
  ggplot(aes(x = quality_of_difference, y = proportion_correct_choices))+
  geom_boxplot()+
  theme_minimal()+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20))
dev.off()

# I asked gpt to tell me how to analyze the nocturnal bee vision data that depends 
# on both light level and frequency of the pattern

library(ggplot2)
library(dplyr)

# Generate example data
set.seed(123)
n <- 100  # Number of observations
x1 <- rnorm(n)  # First continuous variable
x2 <- rnorm(n)  # Second continuous variable
interact <- x1 * x2  # Interaction term
prob <- plogis(2 + 3*x1 + 2*x2 + 0.5*interact)  # Probability of success
y <- rbinom(n, 1, prob)  # Binary outcome variable

# Create a data frame
data <- data.frame(y, x1, x2, interact)

# Fit logistic regression model with interaction
model <- glm(y ~ x1 * x2, data = data, family = binomial)

# Print the model summary
summary(model)
# Plot the scatter plot of the data
ggplot(data, aes(x = x1, y = x2, color = as.factor(y))) +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  xlab("x1") +
  ylab("x2") +
  theme_minimal()

# Add decision boundary from logistic regression model
x1_seq <- seq(min(data$x1), max(data$x1), length.out = 100)
x2_seq <- seq(min(data$x2), max(data$x2), length.out = 100)
grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
grid$prob <- predict(model, newdata = grid, type = "response")

ggplot(data, aes(x = x1, y = x2, color = as.factor(y))) +
  geom_point() +
  scale_color_manual(values = c("red", "blue")) +
  geom_contour(data = grid, aes(x = x1, y = x2, z = prob, fill = ..level..),
               breaks = c(0.5, 0.9), alpha = 0.5) +
  scale_fill_gradient(low = "yellow", high = "black") +
  xlab("x1") +
  ylab("x2") +
  theme_minimal()