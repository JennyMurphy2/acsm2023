library(TOSTER)
library(MOTE)
library(tidyverse)


# Load datasets ------
data <- read_csv("osi_balance.csv")

# Calculate ES ------

rep_ds <- d.ind.t(m1 = 0.492, m2=0.467, sd1=0.116, sd2=0.161, n1=12, n2=12, a = 0.05)
rep_ds

ori_ds <- 1.05 #reported

# Z-test --------

rep_test <- compare_smd(
  smd1 = rep_ds$d,
  n1 = 24,
  smd2 = ori_ds,
  n2 = 22,
  paired = FALSE,
  alternative = "two.sided")
rep_test

