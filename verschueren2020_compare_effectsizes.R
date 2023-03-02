library(MOTE)
library(tidyverse)
library(TOSTER)

## Calculating replication partial eta squared using F statistic and df 
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within
#using alpha = 0.1 rather than 0.05 for 90% CI

pes_rep <- eta.F(dfm=1, dfe=29, Fvalue=1.14, a = 0.1)

pes_rep <- as.data.frame(pes_rep) 

pes_rep_df <- pes_rep %>%
  select(c("eta", "etalow", "etahigh")) %>%
  mutate(study_id = c("Replication study"))

# Calculate original study ES for anterior -------------

pes_ori <- eta.F(dfm=1, dfe=12, Fvalue=6.289, a = 0.1) 

pes_ori <- as.data.frame(pes_ori) 

pes_ori_df <- pes_ori %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))

# Analyze the Anterior Reach Effect Sizes ------

pes_ori = pes_ori_df$eta
pes_rep = pes_rep_df$eta

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test <- compare_cor(r1 = rho_ori,
                        df1 = 12,
                        r2 = rho_rep,
                        df2 = 29)
rep_test
