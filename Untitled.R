# 'manually' perform contrast / z-test


# compare the performance between the test and control:
predDf <- perpPara_170725 %>% group_by(manipulation) %>%
  summarise(rank_trial = max(rank_trial)) %>%
  mutate(sideAlt_trial = T)

predDf <- cbind(predDf, as.data.frame(
  predict.glm(mod_perpPara, predDf, se.fit = T, type = "link"))[,1:2])

predDf <- transform(predDf,
                    lower_ci = (fit - 1.96 * se.fit),
                    upper_ci = (fit + 1.96 * se.fit)
)
# plogis is the "inverse logit" function
predDf <- predDf %>%
  mutate(across(c(fit, lower_ci, upper_ci), 
                plogis, 
                .names = "{.col}_prob"))

# compare test and control at max of controls trial number and generate stats
newData <- data.frame(
  sideAlt_trial = T, 
  manipulation = c(unique(perpPara_170725$manipulation)),
  rank_trial = c(max(
    perpPara_170725[perpPara_170725$manipulation == "control", "rank_trial"]))
)
pred <- predict(mod_perpPara, newdata = newData, type = "link", se.fit = TRUE, re.form = NA)

# Calculate difference
diff <- pred$fit[1] - pred$fit[2]
se_diff <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
# Test
z <- diff / se_diff
p_value <- 2 * pnorm(-abs(z))
