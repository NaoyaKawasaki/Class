data <- read.csv("data.txt", header = FALSE, sep = ",")

library(estimatr)
library(stargazer)
library(texreg)
library(dplyr)
library(modelsummary)

head(data)
colnames(data) <- c("i", "t", "y", "x") 

TFE <- lm_robust(y ~ x + factor(t),
                 data = data,
                 clusters = i,
                 se_type = "stata")

TWFE_LSDV <- lm_robust(y ~ x + factor(i) + factor(t),
                 data = data,
                 clusters = i,
                 se_type = "stata")

data <- data %>%
  group_by(i) %>%
  mutate(
    y_demean = y - mean(y),
    x_demean = x - mean(x)
  )

TWFE_ED <- lm_robust(y_demean ~ x_demean + factor(t),
              data = data,
              clusters = i,
              se_type = "stata")

data <- data %>%
  group_by(i) %>%
  mutate(
    y_fd = y - lag(y),  # yの1期前との差分
    x_fd = x - lag(x)   # xの1期前との差分
  ) %>%
  ungroup()

TWFE_FD <- lm_robust(y_fd ~ x_fd + factor(t),
                 data = data,
                 clusters = i,
                 se_type = "stata")

msummary(list("(1)" =TFE), stars = c("*" = .1, "**" = .05, "***" = .01), gof_omit='RMSE|AIC|BIC|Log.Lik.')
msummary(list("(2)" = TWFE_LSDV), stars = c("*" = .1, "**" = .05, "***" = .01), gof_omit='RMSE|AIC|BIC|Log.Lik.')
msummary(list("(3)" = TWFE_ED), stars = c("*" = .1, "**" = .05, "***" = .01), gof_omit='RMSE|AIC|BIC|Log.Lik.')
msummary(list("(4)" = TWFE_FD), stars = c("*" = .1, "**" = .05, "***" = .01), gof_omit='RMSE|AIC|BIC|Log.Lik.')
