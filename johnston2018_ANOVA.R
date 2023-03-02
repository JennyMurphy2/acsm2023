# Load packages ----------------------------------------------------------------
library(afex)
library(emmeans)
library(pastecs)
library(reshape2)
library(tidyverse)

# Load *anterior* data --------------------------------------------------------------------

anterior_data <- read_csv("anterior_reach_data.csv")
head(anterior_data)

# Drop the "pre01" and "pre03" observation - original study only compared pre03 to post scores
anterior_data <- anterior_data %>%
  select(-c("pre01", "pre02"))

# Prepare anterior data -------------------------------------------------------------------
# Convert data to long dataset

anterior_data.long <- anterior_data %>%
  gather(key = "time", value = "score", pre03, post01, post02, post03)
head(anterior_data.long, 3)

# Add intervention column

anterior_data.long <- anterior_data.long %>%
  mutate(intervention =  case_when(
    time %in% c("pre03") ~ "pre_fatigue",
    time %in% c("post01", "post02", "post03") ~ "post_fatigue"
  ))

structure(anterior_data.long)

# Convert categorical anterior_data to factors

anova_anterior_data <- anterior_data.long %>%
  mutate(
    participant = as.factor(participant),
    time = as.factor(time),
    intervention = as.factor(intervention),
    score = as.numeric(score)
  )

head(anova_anterior_data, 3)

# Assumptions - anterior ----------------------------------------------------------------------------
## Summary stats and normality test ------------------------------------------------------

stat.desc(anterior_data$pre03, basic = TRUE, norm = TRUE)
stat.desc(anterior_data$post01, basic = TRUE, norm = TRUE)
stat.desc(anterior_data$post02, basic = TRUE, norm = TRUE)
stat.desc(anterior_data$post03, basic = TRUE, norm = TRUE)

# Descriptives

summary_anterior_data <- anova_anterior_data %>%
  group_by(time, intervention) %>%
  summarise(mean = mean(score),
            sd = sd(score))

## Plots ---------------------------------------------------------------------------

### Histogram -------------------

# Prepare anterior_data
anterior_hist_dat <- anterior_data %>%
  select(pre03, post01, post02, post03)

anterior_hist_dat$id <- 1:nrow(anterior_hist_dat)
anterior_hist_dat <- melt(anterior_hist_dat, id.vars = "id")

# Plot histogram
hist <- ggplot(data = anterior_hist_dat, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Y balance score")
hist

### Q-Q plots -------------------

ggplot(anterior_data, aes(sample = pre03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(anterior_data, aes(sample = post01)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(anterior_data, aes(sample = post02)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(anterior_data, aes(sample = post03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

### Boxplot -------------------

ggplot(anterior_data.long, aes(x = time, y = score)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

# Anterior ANOVA omnibus test ----------------------------------------------------------------------------

anterior_data_afx <- afex::aov_4(
  score ~ time + (time | participant),
  data = anova_anterior_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anterior_data_afx

summary(anterior_data_afx)

anterior_data_emm <- emmeans::emmeans(anterior_data_afx, ~ time, model = "multivariate")

# Anterior post hoc contrasts ----------------------------------------------------------------------------

posthocresults <- pairs(anterior_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults

