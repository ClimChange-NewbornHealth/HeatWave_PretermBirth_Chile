# Code 5: Logit models lw ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Data ---- 

# BW
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData"))
#bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month_hw", ".RData"))

glimpse(bw_data_lw)

## Logit models ---- 
tic()
m1a <- glm(factor(birth_preterm) ~ HW_30C_2d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 

m2a <- glm(factor(birth_preterm) ~ HW_30C_3d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 

m2a <- glm(factor(birth_preterm) ~ HW_30C_4d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 
toc()

tic()
m1b <- glm(factor(birth_preterm) ~ HW_p90_2d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 

m2b <- glm(factor(birth_preterm) ~ HW_p90_3d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 

m3b <- glm(factor(birth_preterm) ~ HW_p90_4d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 
toc()

tic()
m1c <- glm(factor(birth_preterm) ~ HW_p95_2d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) +
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit"))

m2c <- glm(factor(birth_preterm) ~ HW_p95_3d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 

m3c <- glm(factor(birth_preterm) ~ HW_p95_4d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 
toc()

tic()
m1d <- glm(factor(birth_preterm) ~ HW_p99_2d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit"))

m2d <- glm(factor(birth_preterm) ~ HW_p99_3d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 

m3d <- glm(factor(birth_preterm) ~ HW_p99_4d_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 
toc()


m4 <- glm(factor(birth_preterm) ~ HW_EHF_bin + 
  sex + age_group_mom + educ_group_mom + job_group_mom +
  age_group_dad + educ_group_dad + job_group_dad +
  factor(year_nac) + 
  factor(name_com),
data = bw_data_lw, 
family=binomial(link = "logit")) 




