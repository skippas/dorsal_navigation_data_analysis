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
plot(data$Trial, data$Outcome1, type = "n", xlim = c(1, num_trials), ylim = c(0, 1), xlab = "Learning trials", ylab = "Probability of Success")
points(data$Trial, data$Outcome1, pch = 16)
lines(trial_range, pred_prob_1, col = "red", lwd = 2)
lines(trial_range, pred_prob_2, col = "blue", lwd = 2)
legend("right", legend = c("2 elements", "4 elements"), col = c("red", "blue"), lwd = 2, pch = 16)