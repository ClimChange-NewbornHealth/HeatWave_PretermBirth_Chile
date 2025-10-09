# Code 6.2: Figure Survival models last week and last month ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

dependent_vars <- c("birth_preterm") # , "birth_term", "birth_posterm"

heatwave_vars <- c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                   "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                   "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                   "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin", 
                   "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                  )

# Import and preparation results 
results_cox_lw <- rio::import(paste0("Output/", "Models/", "Cox_models_lw", ".xlsx"))
results_cox_lm <- rio::import(paste0("Output/", "Models/", "Cox_models_lm", ".xlsx"))

#results_cox_lw <- rio::import(paste0("Output/", "Models/", "Cox_models_lw_summer", ".xlsx"))
#results_cox_lm <- rio::import(paste0("Output/", "Models/", "Cox_models_lm_summer", ".xlsx"))

results_cox_lw <- results_cox_lw %>%
  filter(term %in% c(heatwave_vars), 
    dependent_var %in% dependent_vars) %>% 
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

results_cox_lm <- results_cox_lm %>%
  filter(term %in% c(heatwave_vars), 
    dependent_var %in% dependent_vars) %>% 
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
                                      
  # Figure
  f1 <- ggplot(results_cox_lw, aes(x = estimate, y = term, color = duration_label)) +
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
    labs(title = "A. Last week exposure",
         x = "HRs and 95% CI", 
         y = "Heatwave Definition") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))

 f2 <- ggplot(results_cox_lm, aes(x = estimate, y = term, color = duration_label)) +
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
    labs(title = "B. Last month exposure",
         x = "HRs and 95% CI", 
         y = "Heatwave Definition") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))

# Save plot 
plots_save <- ggarrange(
  f1,
  f2,
  ncol=2, nrow=1, 
  common.legend = TRUE
)

plots_save

ggsave(plots_save,
  filename = paste0("Output/", "Models/", "PTB_COX_LM_LW", ".png"), 
  res = 300,
  width = 28,
  height = 23,
  units = 'cm',
  scaling = 1.3,
  device = ragg::agg_png)

