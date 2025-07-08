# Code 6.3: ICC Cox models by municipality (TEST) ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

# Function calculate ICC
calculate_icc <- function(model) {
  var_random_effect <- as.numeric(VarCorr(model))
  var_residual <- pi^2 / 6
  icc <- var_random_effect / (var_random_effect + var_residual)
  return(icc)
}

gc() # Clean memory

dependent_vars <- c("birth_preterm") # , "birth_term", "birth_posterm"

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                  )

control_vars <- c("weeks", "sex", "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "year_nac", "vulnerability")

# Load large table 
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData")) %>% drop_na()

bw_data_lw <- bw_data_lw %>% 
dplyr::select(all_of(c("id", "name_com", dependent_vars, heatwave_vars, control_vars
)))

bw_data_lw$vulnerability <- droplevels(bw_data_lw$vulnerability[bw_data_lw$vulnerability != "Alta"])

# First model and ICC: only random intercept - Null model
formula <- as.formula(paste("Surv(weeks, ", dependent_vars, ") ~ ", "1", 
                              "+ (1 | name_com)")) 

tic()
model_null <- coxme(formula, data = bw_data_lw) 
toc() # 22.53 sec
  
calculate_icc(model_null) # ICC: 0.007529601 -  0.75% explained variance by nested municipality 
as.numeric(VarCorr(model_null)$name_com[1]) # 0.01247966 ~ 0

# Second model and ICC: full model
formula <- as.formula(paste("Surv(weeks, ", dependent_vars, ") ~ ", heatwave_vars[15], 
                            "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability",
                              "+ (1 | name_com)")) 

tic()
model_fit <- coxme(formula, data = bw_data_lw) 
toc() # 188.009 sec ~ 3 min 

model_fit 

calculate_icc(model_fit) # 0.00635293 ~ 0.6% explained variance by nested municipality 
as.numeric(VarCorr(model_fit)$name_com[1]) # 0.01051697~ 0

# Compare with not random effects
formula <- as.formula(paste("Surv(weeks, ", dependent_vars, ") ~ ", heatwave_vars[15], 
                            "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability")) # without RE

tic()
model_fit_wre <- coxph(formula, data = bw_data_lw) 
toc() # 188.009 sec ~ 3 min 

# Comparision models 
AIC(model_fit) # 1578467
AIC(model_fit_wre) # 1578923

BIC(model_fit) # 1579174
BIC(model_fit_wre) #1579380 

