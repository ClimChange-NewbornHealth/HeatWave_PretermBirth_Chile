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

## Logit models -----

# Construímos los vectores para iterar
dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # "birth_term", "birth_posterm"

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_bin")

# Modelo logístico base
logistic_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm", family = binomial(link = "logit"))

# Lista para almacenar los resultados
results_list <- list()

# Función para ajustar el modelo y extraer los coeficientes en OR, errores estándar e intervalos
fit_model <- function(dependent, predictor) {
  # Fórmula
  formula <- as.formula(paste("factor(", dependent, ")", " ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "year_nac")) #  + factor(name_com) "factor(year_nac)"
  
  # Ajuste del modelo
  #model_fit <- logistic_model %>%
  #  fit(formula, data = bw_data_lw)
  
  model_fit <- glm(formula, data = bw_data_lw, binomial(link="logit"))

  # Extraer resultados y calcular OR
  results <- model_fit %>%
    tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%  # OR
    mutate(estimate = round(estimate, 3), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de variables
  return(results)
}

# Iteramos el proceso
results_list <- map(dependent_vars[1], function(dep_var) {
  map_dfr(heatwave_vars[1], function(hw_var) {
    fit_model(dep_var, hw_var)
  })
})

