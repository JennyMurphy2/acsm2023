library(TOSTER)
library(tidyverse)


# Load datasets ------
venier2019replicationcmj <- read_csv("venier2019replicationcmj.csv")
venier2019replicationsj <- read_csv("venier2019replicationsj.csv")
venierdata <- read.csv("venierdata.csv")

# Calculate mean difference for replication squat jump data -------------------------------------
venier2019replicationsj$sj_diff_rep <- venier2019replicationsj$caffeine - venier2019replicationsj$placebo #calculate difference score
squatjump_meandiff_rep <-mean(venier2019replicationsj$sj_diff_rep) #mean of difference scores

# Calculate mean difference for original squat jump data -------------------------------------
venierdata$sj_diff_ori <- venierdata$sjcaff - venierdata$sjpla #calculate difference score
squatjump_meandiff_ori <-mean(venierdata$sj_diff_ori) #mean of difference scores

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


# Squat jump z-test --------
boot_test_sj = boot_compare_smd(x1 = venier2019replicationsj$sj_diff_rep,
                                 x2 = venierdata$sj_diff_ori,
                                 paired = TRUE)

boot_test_sj

# Table of bootstrapped CIs
knitr::kable(boot_test_sj$df_ci, digits = 4)




