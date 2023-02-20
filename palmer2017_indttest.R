# Load packages ----------------------------------------------------------------
library(stats)
library(rstatix)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- read_csv("osi_balance.csv")
head(data)

# Prepare data ----------------

data <- data %>%
  rename(code = "Code",
         age = "Age",
         bodymass = "Mass (kg)",
         height = "Height (cm)",
         osi_score = "Best OSI",
         group = "Group Cat")
  
#Calculate descriptives for squat jump data-------------------------------------

summary <- data %>%
  group_by(group) %>%
  summarise(count = n(),
          mean = mean(osi_score),
          sd = sd(osi_score))

# Resolving assumptions  ------------------------------------
## Checking distribution ---------------------------------------

ggplot(data, aes(osi_score)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(data, aes(group, osi_score, color = group)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

## Checking normality ------------------------------------------

data %>% 
  group_by(group) %>%
  shapiro_test(osi_score) 

## Variances ----------------------------------

var.test(osi_score ~ group, data)

# t test ---------------------------------------------------

t.test(osi_score ~ group, data, 
       var.equal = TRUE, alternative = "two.sided", paired = FALSE, conf.level = 0.95)


