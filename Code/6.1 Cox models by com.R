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

## PR COX Models by com ---- 

# Eliminar datos faltantes
bw_data_lw <- bw_data_lw %>% drop_na()

# Variables dependientes y predictoras
dependent_vars <- c("birth_preterm")  # Solo una variable dependiente
heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_31C_2d_bin", "HW_31C_3d_bin", "HW_31C_4d_bin", 
                   "HW_32C_2d_bin", "HW_32C_3d_bin", "HW_32C_4d_bin", 
                   "HW_33C_2d_bin", "HW_33C_3d_bin", "HW_33C_4d_bin", 
                   "HW_34C_2d_bin", "HW_34C_3d_bin", "HW_34C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_2d_bin", "HW_EHF_3d_bin", "HW_EHF_4d_bin")

# Función para ajustar el modelo por comuna
fit_cox_model_by_comuna <- function(data, dependent, predictor) {
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac)"))
  
  # Ajustar el modelo de Cox
  model_fit <- coxph(formula, data = data)
  
  # Extraer resultados en formato legible
  results <- tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(estimate = round(estimate, 3), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas identificativas
  
  return(results)
}

tic()
# Iterar sobre comunas y ajustar modelos
results_by_comuna <- bw_data_lw %>%
  #filter(name_com %in% c("Cerrillos", "Cerrillos", "Conchali")) %>% 
  group_split(name_com) %>%  
  map_dfr(function(data_comuna) {
    comuna_name <- unique(data_comuna$name_com)  
    map_dfr(dependent_vars, function(dep_var) {
      map_dfr(heatwave_vars, function(hw_var) {
        tryCatch({
          fit_cox_model_by_comuna(data_comuna, dep_var, hw_var) %>%
            mutate(name_com = comuna_name)  
        }, error = function(e) {
          tibble(
            term = hw_var,
            estimate = NA,
            std.error = NA,
            statistic = NA,
            p.value = NA,
            conf.low = NA,
            conf.high = NA,
            dependent_var = dep_var,
            predictor = hw_var,
            name_com = comuna_name
          )
        })
      })
    })
  })
toc()

# Results
results_by_comuna

writexl::write_xlsx(results_by_comuna, path =  paste0("Output/", "Models/", "Cox_models_by_com", ".xlsx"))

results_filtered <- results_by_comuna %>%
  filter(term %in% heatwave_vars, dependent_var %in% dependent_vars) %>% 
  mutate(
      duration = str_extract(term, "\\d+d"), 
      duration_label = case_when( 
        duration == "2d" ~ "2 days",
        duration == "3d" ~ "3 days",
        duration == "4d" ~ "4 or more days",
        TRUE ~ NA_character_ 
      ),
      duration_label = factor(duration_label, levels = c("2 days", "3 days", "4 or more days")) 
    ) %>% 
  mutate(term = factor(term,
                         levels = c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                                    "HW_31C_2d_bin", "HW_31C_3d_bin", "HW_31C_4d_bin", 
                                    "HW_32C_2d_bin", "HW_32C_3d_bin", "HW_32C_4d_bin", 
                                    "HW_33C_2d_bin", "HW_33C_3d_bin", "HW_33C_4d_bin", 
                                    "HW_34C_2d_bin", "HW_34C_3d_bin", "HW_34C_4d_bin",
                                    "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                                    "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                                    "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
                                    "HW_EHF_2d_bin", "HW_EHF_3d_bin", "HW_EHF_4d_bin"),
                         labels = c("HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
                                    "HW-31ºC 2D", "HW-31ºC 3D", "HW-31ºC 4D",
                                    "HW-32ºC 2D", "HW-32ºC 3D", "HW-32ºC 4D",
                                    "HW-33ºC 2D", "HW-33ºC 3D", "HW-33ºC 4D",
                                    "HW-34ºC 2D", "HW-34ºC 3D", "HW-34ºC 4D",
                                    "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
                                    "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
                                    "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
                                    "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D"
                                  )))

x_limits <- c(0, 2) 

p1 <- ggplot(results_filtered, aes(x = estimate, y = term, color = duration_label)) +
  geom_point(size = 3, shape = 15) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_hline(yintercept = 27.5, color = "gray") +
  geom_hline(yintercept = 24.5, color = "gray") +
  geom_hline(yintercept = 21.5, color = "gray") +
  geom_hline(yintercept = 18.5, color = "gray") +
  geom_hline(yintercept = 15.5, color = "gray") +
  geom_hline(yintercept = 12.5, color = "gray") +
  geom_hline(yintercept = 9.5, color = "gray") +
  geom_hline(yintercept = 6.5, color = "gray") +
  geom_hline(yintercept = 3.5, color = "gray") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_colour_manual(name = "Duration HW:", values = c("#e59866", "#d35400", "#873600")) +
  scale_x_continuous(limits = x_limits) +
  labs(title = NULL,
       x = "HRs and 95% CI", 
       y = "Heatwave Definition") +
  facet_wrap(~name_com, ncol=11, scales = "free_x") +
  theme_light() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 11))

ggsave(p1,
  filename = paste0("Output/", "Models/", "PTB_COX_com", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 40,
  height = 40,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)
