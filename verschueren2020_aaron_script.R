library(tidyverse)
library(afex)
library(emmeans)
library(stringr)
library(lmerTest)
# Import data ----

df_exp1 <- readxl::read_excel(here::here("data_CMJ.xlsx"),
                       sheet = "data_exp") |>
  mutate(condition = "exp") |>
  pivot_longer(cols = c(starts_with("pre"),
                        starts_with("post"))) |>
  mutate(time = ifelse(str_detect(name,"pre"),"pre","post"))

df_con1 <- readxl::read_excel(here::here("data_CMJ.xlsx"),
                              sheet = "data_con") |>
  mutate(condition = "con") |>
  pivot_longer(cols = c(starts_with("pre"),
                        starts_with("post"))) |>
  mutate(time = ifelse(str_detect(name,"pre"),"pre","post"))

df_all = bind_rows(df_exp1,df_con1)

df_sumall = df_all |>
  group_by(id,time,condition) |>
  summarize(avg = mean(value,na.rm=TRUE),
            max = max(value,na.rm=TRUE),
            .groups = 'drop')

df_wide = df_sumall |>
  pivot_wider(names_from = time,
              values_from = c(avg,max)) |>
  mutate(avg_c = avg_pre - mean(avg_pre))


# Functions ----

logrom_calc = function(paired = FALSE,
                       bias_c = TRUE,
                       vtype = "LS",
                       m1i,
                       sd1i,
                       n1i,
                       m2i,
                       sd2i,
                       n2i,
                       ri = NULL) {
  if (!paired) {
    yi <- log(m1i / m2i)


    ### sample size weighted average of the coefficient of variation in group 1
    mn1wcvi <- .wmean(sd1i / m1i,
                      n1i,
                      na.rm = TRUE)
    ### sample size weighted average of the coefficient of variation in group 2
    mn2wcvi <- .wmean(sd2i / m2i,
                      n2i,
                      na.rm = TRUE)
    ### sample size weighted average of the two CV values

    mnwcvi  <-
      (sum(n1i * (sd1i / m1i)) + sum(n2i * (sd2i / m2i))) / sum((n1i +
                                                                   n2i))

    ### large sample approximation to the sampling variance (does not assume homoscedasticity)
    if (vtype == "LS") {
      vi <-
        sd1i ^ 2 / (n1i * m1i ^ 2) + sd2i ^ 2 / (n2i * m2i ^
                                                   2)
    }
    ### estimator assuming homoscedasticity
    if (vtype == "HO") {
      mi   <- n1i+n2i - 2
      sdpi <- sqrt(((n1i-1)*sd1i^2 + (n2i-1)*sd2i^2)/mi)
      vi <-
        sdpi ^ 2 / (n1i * m1i ^ 2) + sdpi ^ 2 / (n2i * m2i ^
                                                   2)
    }
    ### estimator using the weighted averages of the CV values
    if (vtype == "AV") {
      vi <- mn1wcvi ^ 2 / n1i + mn2wcvi ^ 2 / n2i
    }
    ### estimator using the weighted average of two weighted averages of the CV values
    if (vtype == "AVHO"){
      vi <- mnwcvi ^ 2 * (1 / n1i + 1 / n2i)
    }

  }

  if (paired) {
    yi <- log(m1i / m2i)
    vi <-
      sd1i ^ 2 / (n1i * m1i ^ 2) + sd2i ^ 2 / (n1i * m2i ^ 2) - 2 * ri * sd1i *
      sd2i / (m1i * m2i * n1i)

  }

  if(bias_c){
    J = 0.5 * (sd1i^2 / (n1i * m1i^2) - sd2i^2 / (n2i * m2i^2))
    yi = yi + J

    Jvar = 0.5 * (sd1i^4 / (n1i^2 * m1i^4) - sd2i^4 / (n2i^2 * m2i^4))
    vi = vi + Jvar
  }


  rval = list(
    log_rom = yi,
    var_rom = vi
  )
  return(rval)
}

.wmean = function (x, w, na.rm = FALSE) {
  if (na.rm) {
    i <- !(is.na(x) | is.na(w))
    x <- x
    w <- w
  }
  sum(x * w)/sum(w)
}

# Models -----

aov1 = aov_4(avg ~ time*condition + (condition+time|id),
             data = df_sumall,
             anova_table = list(es = "pes"))
## actually appropriate model
lmer1 = lmer(value ~ time*condition + (1|id),
             data = df_all)
# anova(lmer1) # same result

## actually appropriate test
lmer2 = lmer(avg_post ~ avg_c + condition + (1|id),
             data = df_wide)
em_ancova = emmeans(lmer2,
                    ~ condition)
pairs_ancova = pairs(em_ancova)
# Replication test -----

pes_rep = aov1$anova_table$pes[1]
df_rep = aov1$anova_table$`den Df`[1]
pes_ori = 0.344
df_ori = 12

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test = TOSTER::compare_cor(r2 = rho_ori,
            df2 = df_ori,
            r1 = rho_rep,
            df1 = df_rep)


