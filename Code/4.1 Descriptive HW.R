# Code 4.1: Descriptive HW ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Data ---- 

# BW
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData")) %>% drop_na()
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month_hw", ".RData")) %>% drop_na()
bw_data_lmu <- bw_data_lm %>% distinct(id, .keep_all = TRUE)
hw_data <- rio::import(paste0(data_out, "hw_data_1980_2021", ".RData"))





# Adjust data 
glimpse(bw_data_lw)
bw_data_lw <- bw_data_lw %>% drop_na()

# Table with HW across time

result <- bw_data_lw %>%
  select(year_nac, ends_with("_count")) %>%  
  group_by(year_nac) %>%                     
  summarise(across(everything(),             
                   list(mean = mean, min = min, max = max),
                   na.rm = TRUE), .groups = "drop") 



data_plot <- bw_data_lw %>%
  select(year_nac, HW_30C_3d_count, HW_p90_3d_count, HW_p95_3d_count, HW_p99_3d_count, HW_EHF_3d_count) %>%
  group_by(year_nac) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>% 
  pivot_longer(cols = -year_nac, names_to = "variable", values_to = "value") %>%
  mutate(variable=factor(variable,
                          levels = c("HW_30C_3d_count", "HW_p90_3d_count", "HW_p95_3d_count", "HW_p99_3d_count", "HW_EHF_3d_count"),
                          labels = c("HW 30C 3D", "HW P90 3D", "HW P95 3D", "HW P99 3D", "HW EHF 3D")))


ggplot(data_plot, aes(x = year_nac, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks=seq(1992, 2020, by=4)) + 
  scale_color_manual(values = brewer.pal(5, "Reds")) +
  labs(
    title = NULL,
    x = "Año",
    y = "Olas de calor promedio",
    color = "HW Definition"
  ) +
  theme_light() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

  ggsave(filename = paste0("Output/", "Descriptives/", "HW_trends", ".png"), # "Preterm_trendsrm1991"
         res = 300,
         width = 20,
         height = 12,
         units = 'cm',
         scaling = 0.8,
         device = ragg::agg_png)

data_plot <- bw_data_lw %>%
  select(name_com, year_nac, HW_30C_3d_count, HW_p90_3d_count, HW_p95_3d_count, HW_p99_3d_count, HW_EHF_3d_count) %>%
  group_by(name_com, year_nac) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>% 
  pivot_longer(cols = -c(name_com, year_nac), names_to = "variable", values_to = "value") %>%
  mutate(variable=factor(variable,
                          levels = c("HW_30C_3d_count", "HW_p90_3d_count", "HW_p95_3d_count", "HW_p99_3d_count", "HW_EHF_3d_count"),
                          labels = c("HW 30C 3D", "HW P90 3D", "HW P95 3D", "HW P99 3D", "HW EHF 3D")))

ggplot(data_plot, aes(x = year_nac, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks=seq(1992, 2020, by=4)) + 
  scale_color_manual(values = brewer.pal(5, "Reds")) +
  labs(
    title = NULL,
    x = NULL,
    y = "Olas de calor promedio",
    color = "HW Definition"
  ) +
  facet_wrap(~name_com, ncol = 5, scales = "free") +
  theme_light() +
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())


ggsave(filename = paste0("Output/", "Descriptives/", "HW_trends_com", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 30,
       height = 30,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)
 
