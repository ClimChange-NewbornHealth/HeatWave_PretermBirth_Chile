# Code 2.1: HW Descriptives ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"
data_sovi <- "Data/Input/SOVI/"

## Data ---- 

# HW 
hw_data <- rio::import(paste0(data_out, "hw_data_1980_2021", ".RData"))
glimpse(hw_data)

refs <- rio::import("Output/Descriptives/Ref_tmin_tmax_com_1980_2021.xlsx") %>% 
  select(codigo_comuna, p90_tmax, p95_tmax, p99_tmax)

## Histogram Percentiles ---- 
per_data <- hw_data %>% 
  group_by(name_com, month, day) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value")

ggplot(per_data, aes(x = value, fill = metric, color = metric)) +
  geom_histogram(position = "identity", alpha = 0.35, binwidth = 0.5) +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    x = "Tº Celcius",
    y = "Frequency"
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
  facet_grid(name_com ~ metric, switch = "y") +
  #facet_wrap(metric, scales = "free", ncol=3) +
  #facet_wrap( ~ name_com, scales = "free", ncol=5) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_text(),
    legend.box = "horizontal",
    legend.spacing.x = unit(0.3, "cm"), 
    legend.spacing.y = unit(0, "cm"), 
    legend.margin   = margin(t = 0, r = 0, b = 0, l = 0),
    strip.background = element_rect(fill = NA, color = "gray"),
    strip.text = element_text(color = "black"),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    strip.background.y = element_rect(fill = NA, color = NA)
  )

ggsave(
  filename = paste0("Output/", "Descriptives/", "Histogram", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 30,
  height   = 50,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)



