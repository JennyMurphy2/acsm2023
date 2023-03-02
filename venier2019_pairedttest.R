# Load packages ----------------------------------------------------------------
library(tidyverse)
library(stats)
library(rstatix)

# Load data --------------------------------------------------------------------
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")
  
#Calculate descriptives for cmj data-------------------------------------
r_cmj <-cor(venier2019replicationcmj$caffeine, venier2019replicationcmj$placebo) #correlation between dependent measures

cmj_diff <- venier2019replicationcmj$caffeine - venier2019replicationcmj$placebo 

cmj_m_diff<-mean(cmj_diff) #mean of difference scores
cmj_s_diff<-sd(cmj_diff) #standard deviation of difference scores
m1_cmj_caff <- mean(venier2019replicationcmj$caffeine)
m2_cmj_pla <- mean(venier2019replicationcmj$placebo)
sd1_cmj_caff <-sd(venier2019replicationcmj$caffeine) #standard deviation of group 1
sd2_cmj_pla <-sd(venier2019replicationcmj$placebo) #standard deviation of group 2
cmj_N <- length(venier2019replicationcmj$participant) #number of pairs


# Convert cmj data to long dataset --------------------------------------
cmjdat.long <- venier2019replicationcmj %>%
  gather(key = "supplement", value = "cmj", caffeine, placebo)
head(cmjdat.long, 3)

# Resolving assumptions for cmj data ------------------------------------
## Checking for outliers on difference score -----------------------------------

venier2019replicationcmj <- venier2019replicationcmj %>% mutate(differences = caffeine - placebo)

venier2019replicationcmj %>%
  identify_outliers(differences)

## Checking for outliers on individual groups ----------------------------------
venier2019replicationcmj %>%
  identify_outliers(caffeine)

venier2019replicationcmj %>%
  identify_outliers(placebo)

ggplot(cmjdat.long, aes(supplement, cmj, color = supplement)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

## Checking distribution ----------------------------------------------------------

ggplot(venier2019replicationcmj, aes(sample = differences)) +
  geom_qq()+
  geom_qq_line()

ggplot(venier2019replicationcmj, aes(differences)) +
  geom_histogram(color="black", fill="white",
                 bins = 20)

## Checking normality ----------------------------------------------------------
venier2019replicationcmj %>% shapiro_test(caffeine)
venier2019replicationcmj %>% shapiro_test(placebo)
venier2019replicationcmj %>% shapiro_test(differences)

# t test for cmj data ---------------------------------------------------
t.test(cmj ~ supplement, cmjdat.long, 
       var.equal = TRUE, alternative = "two.sided", paired = TRUE, conf.level = 0.95)
