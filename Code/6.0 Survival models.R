# Code 5: Survival models lw ----

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

# Adjust data 
glimpse(bw_data_lw)


## PR COX Models ---- 

dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm", "birth_term", "birth_posterm") # 

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_bin")

fit_cox_model <- function(dependent, predictor) {
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac)"))
  
  # Ajuste del modelo de Cox
  model_fit <- coxph(formula, data = bw_data_lw)
  
  # Extraer resultados con tidy
  results <- tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(estimate = round(estimate, 3), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)
}

# Iterar sobre las combinaciones de dependientes y predictores
results_list <- map(dependent_vars[1], function(dep_var) {
  map(heatwave_vars[1], function(hw_var) {
    fit_cox_model(dep_var, hw_var)
  })
})

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models", ".xlsx"))


