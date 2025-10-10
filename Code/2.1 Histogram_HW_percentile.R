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

# References values
com <- chilemapas::codigos_territoriales 
com_suburb <- c(unique(com$codigo_comuna[com$nombre_provincia=="Santiago"]), 13201) 

ref_temp <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  #filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  group_by(com) %>% 
  summarise(p90_tmax=quantile(tmax, probs = 0.90, digits = 2),
            p95_tmax=quantile(tmax, probs = 0.95, digits = 2),
            p99_tmax=quantile(tmax, probs = 0.99, digits = 2)) %>% 
  ungroup()

## Histogram Percentiles All ---- 

#Overall
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(month, day) %>% 
  #filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(metric) %>% 
  summarise(m=mean(value))

g1 <- ggplot(per_data, aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.5) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
  facet_wrap(~ metric, ncol = 3) +
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

# Summer 
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(month, day) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(metric) %>% 
  summarise(m=mean(value))

g2 <- ggplot(per_data, aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.5) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    x = "Tº Celsius",
    y = "Density",
    title = "B. Summer Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
  facet_wrap(~ metric, ncol = 3) +
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

g2

ggarrange(g1, g2, ncol=1)

ggsave(
  filename = paste0("Output/", "Descriptives/", "Histogram_Overall_Summer", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 15,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)


## Histogram Percentiles Municipality P95 ---- 
# Overall
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(name_com, month, day) %>% 
  #filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(name_com, metric) %>% 
  summarise(m=mean(value)) %>% 
  filter(metric == "P95")

per_data %>% 
  filter(metric == "P95") %>% 
  ggplot(aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.5) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~ name_com, ncol = 5, scales = "free") +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    #title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
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
  filename = paste0("Output/", "Descriptives/", "Histogram_P95_Overall_Mun", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 25,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)

# Summer
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(name_com, month, day) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(name_com, metric) %>% 
  summarise(m=mean(value)) %>% 
  filter(metric == "P95")

per_data %>% 
  filter(metric == "P95") %>% 
  ggplot(aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.3) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~ name_com, ncol = 5, scales = "free") +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    #title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
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
  filename = paste0("Output/", "Descriptives/", "Histogram_P95_Summer_Mun", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 25,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)

## Histogram Percentiles Municipality P90 ---- 
# Overall
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(name_com, month, day) %>% 
  #filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(name_com, metric) %>% 
  summarise(m=mean(value)) %>% 
  filter(metric == "P90")

per_data %>% 
  filter(metric == "P90") %>% 
  ggplot(aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.5) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~ name_com, ncol = 5, scales = "free") +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    #title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
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
  filename = paste0("Output/", "Descriptives/", "Histogram_P90_Overall_Mun", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 25,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)

# Summer
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(name_com, month, day) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(name_com, metric) %>% 
  summarise(m=mean(value)) %>% 
  filter(metric == "P90")

per_data %>% 
  filter(metric == "P90") %>% 
  ggplot(aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.3) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~ name_com, ncol = 5, scales = "free") +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    #title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
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
  filename = paste0("Output/", "Descriptives/", "Histogram_P90_Summer_Mun", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 25,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)

## Histogram Percentiles Municipality P99 ---- 
# Overall
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(name_com, month, day) %>% 
  #filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(name_com, metric) %>% 
  summarise(m=mean(value)) %>% 
  filter(metric == "P99")

per_data %>% 
  filter(metric == "P99") %>% 
  ggplot(aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.5) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~ name_com, ncol = 5, scales = "free") +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    #title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
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
  filename = paste0("Output/", "Descriptives/", "Histogram_P99_Overall_Mun", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 25,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)

# Summer
per_data <- hw_data %>% 
  filter(com %in% com_suburb) %>% 
  group_by(name_com, month, day) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  summarise(
    P90 = quantile(tmax, probs = 0.90, digits = 2),
    P95 = quantile(tmax, probs = 0.95, digits = 2),
    P99 = quantile(tmax, probs = 0.99, digits = 2)
  ) %>% 
  pivot_longer(cols = !c(name_com, month, day), 
               names_to = "metric", 
               values_to = "value") %>% 
  ungroup()

per_mean <- per_data %>% 
  group_by(name_com, metric) %>% 
  summarise(m=mean(value)) %>% 
  filter(metric == "P99")

per_data %>% 
  filter(metric == "P99") %>% 
  ggplot(aes(x = value, fill = metric, color = metric)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "identity", alpha = 0.35, binwidth = 0.3) +
  geom_density(linewidth = 0.9, adjust = 1.0, alpha = 0) +
  geom_vline(data = per_mean,
             aes(xintercept = m, color = metric),
             linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~ name_com, ncol = 5, scales = "free") +
  scale_colour_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  scale_fill_manual(name = "Percentile:", values = c("#e59866", "#d35400", "#873600")) +
  labs(
    #x = "Tº Celsius",
    x = NULL,
    y = "Density",
    #title = "A. All Months",
  ) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", big.mark = "")) +
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
  filename = paste0("Output/", "Descriptives/", "Histogram_P99_Summer_Mun", ".png"), 
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 25,
  units    = 'cm',
  scaling  = 1,
  device   = ragg::agg_png
)
