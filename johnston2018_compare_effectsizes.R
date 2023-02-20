library(MOTE)
library(tidyverse)
library(TOSTER)

## Calculating replication partial eta squared using F statistic and df 
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within
#using alpha = 0.1 rather than 0.05 for 90% CI

ant_pes_rep <- eta.F(dfm=1.6, dfe=60.7, Fvalue=6.60, a = 0.1)

ant_pes_rep <- as.data.frame(ant_pes_rep) 

ant_pes_rep_df <- ant_pes_rep %>%
  select(c("eta", "etalow", "etahigh")) %>%
  mutate(study_id = c("Replication study"))

# Calculate original study ES for anterior -------------

ant_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=3.818, a = 0.1) 

ant_pes_ori <- as.data.frame(ant_pes_ori) 

ant_pes_ori_df <- ant_pes_ori %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))

# Analyze the Anterior Reach Effect Sizes ------

pes_ori = ant_pes_ori_df$eta
pes_rep = ant_pes_rep_df$eta

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test <- compare_cor(r1 = rho_ori,
                        df1 = 19,
                        r2 = rho_rep,
                        df2 = 60.75)
rep_test

# Forest plot for anterior reach data ---------

## Labels for anterior forest plot -------------
labelanteriorrep <- "0.148 [0.03, 0.29]"
labelanteriorori <- "0.376 [0.03, 0.57]"

## Join datasets -----------------
anteriorplot <-
  merge(ant_pes_ori_df, ant_pes_rep_df, by = c("eta", "etalow", "etahigh", "study_id"), all = TRUE)

## Plot -----------------------------
ggplot(anteriorplot,
       aes(
         y = study_id,
         x = eta,
         xmin = etalow,
         xmax = etahigh
       )) +
  ggtitle("Partial eta squared [90% CI]") +
  geom_point() +
  geom_errorbarh(height = .1) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "Observed Effect Size", limits = c(-0.1, 1)) +
  scale_y_discrete(name = "") +
  annotate("text",
           x = 0.8,
           y = 2,
           label = labelanteriorrep) +
  annotate("text",
           x = 0.8,
           y = 1,
           label = labelanteriorori) +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.85),
    panel.background = element_blank()
  )

ggsave(
  "forestanterior.png",
  plot = last_plot(),
  device = "png",
  width = NA,
  height = NA,
  dpi = 300,
  limitsize = TRUE,
  bg = '#ffffff'
)
