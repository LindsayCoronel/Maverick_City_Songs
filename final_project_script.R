## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)
library(readxl)


data <- read_delim("raw_data.csv")
mean(data$artist_count)
mean(data$emotion)
mean(data$Tempo)
mean(data$views)
mean(data$)
table(data$emotion)

table(data$Tempo)
sd(data$artist_count)
sd(data$emotion)
min(data$views)
max(data$artist_count)
max(data$views)
table(data$emotion,data$Tempo)
chisq.test(data$emotion,data$Tempo)

ggplot(raw_data, aes(x = data$emotion, y = data$artist_count)) +
  geom_boxplot() +
  labs(title = "Box Plot of Arist Count by Emotion",
       x = "Emotion",
       y = "Artist Count") +
  theme_minimal()

# Perform ANOVA
anova_adapted <- aov(artist_count ~ emotion, data = data)
# Summarize ANOVA results
summary(anova_adapted)
# total SS; TSS
952+766
# get R2
# between/total
# OR between/(between+within)
766/(766+952)

linear_plot <- plot(data$artist_count, data$views)


meany <- mean(raw_data$artist_count)
meanx <- mean(raw_data$views)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")



##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(data$views ~ data$artist_count, data = raw_data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(raw_data$artist_count, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

