## 6. AFT Models ---- 

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Data ---- 

dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                   #"HW_EHF_TMAX_2d_bin", "HW_EHF_TMAX_3d_bin", "HW_EHF_TMAX_4d_bin",
                   #"HW_EHF_TAD_sev_first", "HW_EHF_TMAX_sev_first"
                  )

control_vars <- c("weeks", "sex", "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "year_nac", "vulnerability")


# LW
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData")) %>% drop_na()

bw_data_lw <- bw_data_lw %>% 
  dplyr::select(all_of(c("id", dependent_vars, heatwave_vars, control_vars
  )))

bw_data_lw$vulnerability <- droplevels(
    bw_data_lw$vulnerability[bw_data_lw$vulnerability != "Alta"]
  )


# Test initial models 
formula <- as.formula(paste("Surv(weeks, ", dependent_vars[2], ") ~ ", heatwave_vars[15], 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability"))

m1 <- survreg(formula, dist = "gaussian", data = bw_data_lw) # Absolute time 
m2 <- survreg(formula, dist = "weibull", data = bw_data_lw) # log-time
m3 <- survreg(formula, dist = "exponential", data = bw_data_lw) # logtime
m4 <- survreg(formula, dist = "logistic", data = bw_data_lw) # logistic -> change survival time
m5 <- survreg(formula, dist = "loglogistic", data = bw_data_lw) # logtime 
m6 <- coxph(formula, data = bw_data_lw)

screenreg(l=list(m1, m2, m3, m4, m5, m6), digits=3)

cox.zph(m6) # Proportional risk assumption it's ok by heatwave variable, but not covariates.

# Decision: use weibull model to compare HR with cox model 

## HR AFT Models LW ---- 
rm(m1, m2, m3, m4, m5, m6)

# Función para ajustar el modelo Weibull AFT y transformar a Hazard Ratios
fit_weibull_aft <- function(dependent, predictor, data) {
  # Fórmula del modelo
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability"))
  
  # Ajuste del modelo Weibull
  model_fit <- survreg(formula, data = bw_data_lw, dist = "weibull")
  
  # Transformar coeficientes a Hazard Ratios
  scale <- model_fit$scale  # Parámetro de escala
  tidy_model <- broom::tidy(model_fit) %>%
    mutate(HR = exp(-estimate / scale),  # Hazard Ratio
           conf.low = exp(-(estimate + 1.96 * std.error) / scale),  # Límite inferior del IC
           conf.high = exp(-(estimate - 1.96 * std.error) / scale),  # Límite superior del IC
           estimate = round(estimate, 3),
           HR = round(HR, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, HR, std.error, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  
  
  return(tidy_model)

  rm(model_fit); gc()
}

# Aplicar la función con manejo de errores
plan(multisession, workers = parallel::detectCores() - 6)
options(future.globals.maxSize = 3 * 1024^3)  # 1.5 GB

combinations <- expand.grid(dependent_vars, heatwave_vars, stringsAsFactors = FALSE)

tic()
weibull_results <- future_lapply(seq_len(nrow(combinations)), function(i) {
  dep_var <- combinations[i, 1]
  hw_var <- combinations[i, 2]
  fit_weibull_aft(dep_var, hw_var, data=bw_data_lw)
})
toc() # time: 1333.067 sec elapsed, 22.21778 min 

# Resultados para los modelos Weibull transformados a HRs
results_aft <- c()
results_aft <- results_aft %>% bind_rows(weibull_results)
rm(weibull_results)
gc()

writexl::write_xlsx(results_aft, path =  paste0("Output/", "Models/", "AFT_models", ".xlsx"))
results_aft <- rio::import(paste0("Output/", "Models/", "AFT_models", ".xlsx"))

# Plots with HW - Effects

results_filtered <- results_aft %>%
  filter(term %in% heatwave_vars, dependent_var %in% dependent_vars[c(1,4)]) %>% 
  rename(estimate=HR)

results_filtered <- results_filtered %>%
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
    filter(term %in% c(
      "HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
      "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
      "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
      "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
      "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
 )) %>% 
    mutate(term=factor(term, 
      levels = c(
      "HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
      "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
      "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
      "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
      "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
    ), 
      labels = c(
      "HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
      "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
      "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
      "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
      "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D"
      )))

results_filtered <- results_filtered %>%
  mutate(estimate=as.numeric(estimate),
         conf.low=as.numeric(conf.low), 
         conf.high=as.numeric(conf.high))

f1 <- results_filtered %>% filter(dependent_var == "birth_preterm") %>% 
    ggplot(aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 2.5, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_hline(yintercept = 12.5, color = "gray") +
    geom_hline(yintercept = 9.5, color = "gray") +
    geom_hline(yintercept = 6.5, color = "gray") +
    geom_hline(yintercept = 3.5, color = "gray") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(name = "Duration HW:", values = c("#e59866", "#d35400", "#873600")) +
    scale_x_continuous(limits = c(0.8, 1.25)) +
    geom_text(aes(x = 1.15, label = paste0(format(round(estimate, 3), nsmall = 2), " (", 
                                                      format(round(conf.low, 3), nsmall = 2), " - ", 
                                                      format(round(conf.high, 3), nsmall = 2), ")")), 
              position = position_dodge(width = 0.75), size = 3, show.legend = FALSE) + 
    labs(title = "A. Preterm (<37 weeks) last week",
         x = "HRs and 95% CI", 
         y = "Heatwave Definition") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))
f1

f2 <- results_filtered %>% filter(dependent_var == "birth_late_preterm") %>% 
    ggplot(aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 2.5, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_hline(yintercept = 12.5, color = "gray") +
    geom_hline(yintercept = 9.5, color = "gray") +
    geom_hline(yintercept = 6.5, color = "gray") +
    geom_hline(yintercept = 3.5, color = "gray") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(name = "Duration HW:", values = c("#e59866", "#d35400", "#873600")) +
    scale_x_continuous(limits = c(0.8, 1.25)) +
    geom_text(aes(x = 1.15, label = paste0(format(round(estimate, 3), nsmall = 2), " (", 
                                                      format(round(conf.low, 3), nsmall = 2), " - ", 
                                                      format(round(conf.high, 3), nsmall = 2), ")")), 
              position = position_dodge(width = 0.75), size = 3, show.legend = FALSE) + 
    labs(title = "B. Late Preterm (34-37 weeks)",
         x = "HRs and 95% CI", 
         y = "Heatwave Definition") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))
f2

# Save plot 
plots_save <- ggarrange(
  f1,
  f2,
  ncol=2, nrow=1, 
  common.legend = TRUE
)

plots_save

ggsave(plots_save,
  filename = paste0("Output/", "Models/", "PTB_AFT_LW", ".png"), 
  res = 300,
  width = 30,
  height = 25,
  units = 'cm',
  scaling = 1.3,
  device = ragg::agg_png)

