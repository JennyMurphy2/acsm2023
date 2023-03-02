library(TOSTER)
library(tidyverse)


# Load datasets ------
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")
venierdata <- read_csv("venierdata.csv")

# Calculate mean difference for replication CMJ data -------------------------------------
venier2019replicationcmj$cmj_diff_rep <- venier2019replicationcmj$caffeine - venier2019replicationcmj$placebo #calculate difference score
cmj_meandiff_rep<-mean(venier2019replicationcmj$cmj_diff_rep) #mean of difference scores

# Calculate mean difference for original CMJ data -------------------------------------
venierdata$cmj_diff_ori <- venierdata$cmjcaff - venierdata$cmjpla #calculate difference score
cmj_meandiff_ori<-mean(venierdata$cmj_diff_ori) #mean of difference scores


# CMJ Z-test --------
boot_test_cmj = boot_compare_smd(x1 = venier2019replicationcmj$cmj_diff_rep,
                             x2 = venierdata$cmj_diff_ori,
                             paired = TRUE)

boot_test_cmj

# Table of bootstrapped CIs
knitr::kable(boot_test_cmj$df_ci, digits = 4)



