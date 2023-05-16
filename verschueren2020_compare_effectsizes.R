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

# Forest ---------


## Join datasets -----------------
plot <-
  merge(pes_ori_df, pes_rep_df, by = c("eta", "etalow", "etahigh", "study_id"), all = TRUE)

## Plot -----------------------------
ggplot(plot,
       aes(
         y = study_id,
         x = eta,
         xmin = etalow,
         xmax = etahigh
       )) +
  geom_point(size = 10) +
  geom_errorbarh(height = .3) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "", limits = c(-0.1, 1)) +
  scale_y_discrete(name = "") +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 30),
    axis.text.x = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.85),
    panel.background = element_blank()
  )

ggsave(
  "CF_forestplot.png",
  plot = last_plot(),
  device = "png",
  width = NA,
  height = NA,
  dpi = 300,
  limitsize = TRUE,
  bg = '#ffffff'
)