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

table_pt <- bw_data_lw %>% 
  mutate(month = month(date_end_week)) %>% 
  mutate(month=factor(month, 
    levels=c(11, 12, 1:4),
    labels=c("November", "December", "January", "February", "March", "April"))) %>% 
  group_by(com, name_com) %>% 
  summarise(
    
    weeks=mean(weeks, na.rm = TRUE),
    tbw=mean(tbw, na.rm = TRUE),

    birth_preterm_n=sum(birth_preterm, na.rm = TRUE),
    #birth_very_preterm_n=sum(birth_very_preterm, na.rm = TRUE),
    #birth_moderately_preterm_n=sum(birth_moderately_preterm, na.rm = TRUE),
    #birth_late_preterm_n=sum(birth_late_preterm, na.rm = TRUE),
    #birth_term_n=sum(birth_term, na.rm = TRUE),
    #birth_posterm_n=sum(birth_posterm, na.rm = TRUE),

    birth_preterm=mean(birth_preterm, na.rm = TRUE)*100,
    #birth_very_preterm=mean(birth_very_preterm, na.rm = TRUE),
    #birth_moderately_preterm=mean(birth_moderately_preterm, na.rm = TRUE),
    #birth_late_preterm=mean(birth_late_preterm, na.rm = TRUE),
    #birth_term=mean(birth_term, na.rm = TRUE),
    #birth_posterm=mean(birth_posterm, na.rm = TRUE),

    #HW_30C_2d_bin=mean(HW_30C_2d_bin, na.rm = TRUE),
    #HW_p90_2d_bin=mean(HW_p90_2d_bin, na.rm = TRUE),
    #HW_p95_2d_bin=mean(HW_p95_2d_bin, na.rm = TRUE),
    #HW_p99_2d_bin=mean(HW_p99_2d_bin, na.rm = TRUE),
    #HW_EHF_TAD_2d_bin=mean(HW_EHF_TAD_2d_bin, na.rm = TRUE),
    
    #HW_30C_2d_count=mean(HW_30C_2d_count, na.rm = TRUE),
    #HW_p90_2d_count=mean(HW_p90_2d_count, na.rm = TRUE),
    #HW_p95_2d_count=mean(HW_p95_2d_count, na.rm = TRUE),
    #HW_p99_2d_count=mean(HW_p99_2d_count, na.rm = TRUE),

    HW_30C_3d_bin=mean(HW_30C_3d_bin, na.rm = TRUE),
    HW_p90_3d_bin=mean(HW_p90_3d_bin, na.rm = TRUE),
    HW_p95_3d_bin=mean(HW_p95_3d_bin, na.rm = TRUE),
    HW_p99_3d_bin=mean(HW_p99_3d_bin, na.rm = TRUE),
    HW_EHF_TAD_3d_bin=mean(HW_EHF_TAD_3d_bin, na.rm = TRUE),

    HW_30C_3d_count=mean(HW_30C_3d_count, na.rm = TRUE),
    HW_p90_3d_count=mean(HW_p90_3d_count, na.rm = TRUE),
    HW_p95_3d_count=mean(HW_p95_3d_count, na.rm = TRUE),
    HW_p99_3d_count=mean(HW_p99_3d_count, na.rm = TRUE),
    HW_EHF_TAD_3d_count=mean(HW_EHF_TAD_3d_count, na.rm = TRUE)

    #HW_30C_4d_bin=mean(HW_30C_4d_bin, na.rm = TRUE),
    #HW_p90_4d_bin=mean(HW_p90_4d_bin, na.rm = TRUE),
    #HW_p95_4d_bin=mean(HW_p95_4d_bin, na.rm = TRUE),
    #HW_p99_4d_bin=mean(HW_p99_4d_bin, na.rm = TRUE),

    #HW_30C_4d_count=mean(HW_30C_4d_count, na.rm = TRUE),
    #HW_p90_4d_count=mean(HW_p90_4d_count, na.rm = TRUE),
    #HW_p95_4d_count=mean(HW_p95_4d_count, na.rm = TRUE),
    #HW_p99_4d_count=mean(HW_p99_4d_count, na.rm = TRUE)
  )

table_pt


writexl::write_xlsx(table_pt, path =  paste0("Output/", "Descriptives/", "Table_PTB_HW_COM", ".xlsx"))

# Table with HW across time
result <- bw_data_lw %>%
  select(year_nac, ends_with("_count")) %>%  
  group_by(year_nac) %>%                     
  summarise(across(everything(),             
                   list(mean = mean, min = min, max = max),
                   na.rm = TRUE), .groups = "drop") 


data_plot <- bw_data_lw %>%
  select(year_nac, HW_30C_3d_count, HW_p90_3d_count, HW_p95_3d_count, HW_p99_3d_count, HW_EHF_TAD_3d_count) %>%
  group_by(year_nac) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>% 
  pivot_longer(cols = -year_nac, names_to = "variable", values_to = "value") %>%
  mutate(variable=factor(variable,
                          levels = c("HW_30C_3d_count", "HW_p90_3d_count", "HW_p95_3d_count", "HW_p99_3d_count", "HW_EHF_TAD_3d_count"),
                          labels = c("HW 30C 3D", "HW P90 3D", "HW P95 3D", "HW P99 3D", "HW EHF 3D")))


f1 <- ggplot(data_plot, aes(x = year_nac, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks=seq(1992, 2020, by=4)) + 
  scale_color_manual(values = brewer.pal(5, "Reds")) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean number of HW",
    color = "HW Definition"
  ) +
  theme_light() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

f1

ggsave(f1,
       filename = paste0("Output/", "Descriptives/", "HW_trends", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 12,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)

hw_corr <- data_plot %>% 
  pivot_wider(names_from = variable, values_from = value)

correlate(hw_corr[,-1]) %>% writexl::write_xlsx(path =  paste0("Output/", "Descriptives/", "HW_CORRELATE", ".xlsx"))

calc_ttest_p_value <- function(vec_a, vec_b){
  t.test(vec_a, vec_b)$p.value
}

p_val <- colpair_map(hw_corr[,-1], calc_ttest_p_value)


f2a <- ggplot(hw_corr, aes(y = `HW 30C 3D`, x = `HW P90 3D`)) +
  geom_point(color ="#e67e22", size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha=0.3, linewidth=1) +
  stat_cor(method = "pearson", r.digits=2, p.digits = 3, p.accuracy=0.001) + 
  #scale_y_continuous(breaks=1:4, limits = c(1,4)) + 
  theme_light() +
  theme(plot.title = element_text(size = 11, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(color = "black"),
        panel.grid = element_blank()
        ))

f2b <- ggplot(hw_corr, aes(y = `HW P90 3D`, x = `HW P95 3D`)) +
  geom_point(color ="#e67e22", size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha=0.3, linewidth=1) +
  stat_cor(method = "pearson", r.digits=2, p.digits = 3, p.accuracy=0.001) + 
  #scale_y_continuous(breaks=1:4, limits = c(1,4)) + 
  theme_light() +
  theme(plot.title = element_text(size = 11, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(color = "black"),
        panel.grid = element_blank()
        ))

f2c <- ggplot(hw_corr, aes(y = `HW P95 3D`, x = `HW P99 3D`)) +
  geom_point(color ="#e67e22", size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha=0.3, linewidth=1) +
  stat_cor(method = "pearson", r.digits=2, p.digits = 3, p.accuracy=0.001) + 
  #scale_y_continuous(breaks=1:4, limits = c(1,4)) + 
  theme_light() +
  theme(plot.title = element_text(size = 11, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(color = "black"),
        panel.grid = element_blank()
        ))

f2d <- ggplot(hw_corr, aes(y = `HW P99 3D`, x = `HW EHF 3D`)) +
  geom_point(color ="#e67e22", size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha=0.3, linewidth=1) +
  stat_cor(method = "pearson", r.digits=2, p.digits = 3, p.accuracy=0.001) + 
  #scale_y_continuous(breaks=1:4, limits = c(1,4)) + 
  theme_light() +
  theme(plot.title = element_text(size = 11, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(color = "black"),
        panel.grid = element_blank()
        ))

f2 <- ggarrange(
  f2a,
  f2b,
  f2c,
  f2d,
  ncol=4, nrow=1, 
  common.legend = TRUE
)

plots <- ggarrange(
  f1,
  f2,
  ncol=1, nrow=2, 
  common.legend = TRUE
)

plots

ggsave(plots,
  filename = paste0("Output/", "Descriptives/", "HW_trends_by_correlation", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 25,
  height = 17,
  units = 'cm',
  scaling = 0.9,
  device = ragg::agg_png)

data_plot <- bw_data_lw %>%
  select(name_com, year_nac, HW_30C_3d_count, HW_p90_3d_count, HW_p95_3d_count, HW_p99_3d_count, HW_EHF_TAD_3d_count) %>%
  group_by(name_com, year_nac) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>% 
  pivot_longer(cols = -c(name_com, year_nac), names_to = "variable", values_to = "value") %>%
  mutate(variable=factor(variable,
                          levels = c("HW_30C_3d_count", "HW_p90_3d_count", "HW_p95_3d_count", "HW_p99_3d_count", "HW_EHF_TAD_3d_count"),
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
 

# Other plots 

comunas <- unique(hw_data$name_com)

for (comuna in comunas) {
  
  # Filtrar los datos para la comuna actual
  hw_time <- hw_data %>% 
    filter(name_com == comuna) %>% 
    filter(year_month %in% c("11-2020", "12-2020", "01-2021", "02-2021", "03-2021")) %>% 
    group_by(name_com, date) %>% 
    summarise(tmax_mean = mean(tmax)) %>% 
    ungroup()
  
  # Calcular percentiles para la comuna actual
  p90 <- quantile(hw_time$tmax_mean, probs = 0.90, na.rm = TRUE)
  p95 <- quantile(hw_time$tmax_mean, probs = 0.95, na.rm = TRUE)
  c30 <- 30
  
  # Gráfico de temperaturas máximas
  f1 <- hw_time %>% 
    ggplot(aes(y = tmax_mean, x = date)) +
    geom_line(color = "#09557f", alpha = 0.6, size = 0.4) +
    geom_hline(aes(yintercept = p95, linetype = "P95"), color = "#d35400", linewidth = 0.5) +
    geom_hline(aes(yintercept = p90, linetype = "P90"), color = "#e67e22", linewidth = 0.5) +
    geom_hline(aes(yintercept = c30, linetype = "30ºC"), color = "#f39c12", linewidth = 0.5) +
    scale_y_continuous(limits = c(0, 40)) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month", expand = c(0.03, 0.03)) + 
    scale_linetype_manual(values = c("solid", "twodash", "longdash"),
                          guide = guide_legend(reverse = FALSE, nrow = 1)) +
    labs(y = "Temperatura Máxima (ºC)", x = NULL, linetype = NULL, title = NULL) +
    theme_light() +
    theme(legend.position = "top",
          legend.margin = margin(c(0.05, 0.05, 0.05, 0.05)),
          panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          text = element_text(size = 10))
  
  # Agregar filtro para el cálculo de olas de calor por verano
  count_heatwaves <- function(data, threshold_col) {
    # Aplicar run-length encoding para identificar secuencias de olas de calor
    rle_result <- rle(data[[threshold_col]])
    # Contar cuántas secuencias de al menos 3 días consecutivos (valor 1) hay
    num_heatwaves <- sum(rle_result$values == 1 & rle_result$lengths >= 3)
    return(num_heatwaves)
  }

  hw_sum_time <- hw_data %>% 
    filter(name_com == comuna) %>% 
    filter(date >= as.Date("1980-11-01")) %>% 
    filter(month %in% c(11, 12, 1, 2, 3)) %>% 
    group_by(name_com, summer_year = ifelse(month %in% c(11, 12), year, year - 1), month) %>% 
    #mutate(summer_year = ifelse(month %in% c(11, 12), year, year - 1)) %>%  
    group_by(summer_year) %>% 
    summarise(
    hw30_count = count_heatwaves(cur_data(), "HW_30C_3d"),
    hwp90_count = count_heatwaves(cur_data(), "HW_p90_3d"),
    hwp95_count = count_heatwaves(cur_data(), "HW_p95_3d")
  ) %>% 
    ungroup() %>% 
    pivot_longer(cols = !summer_year,
                 names_to = "hw", 
                 values_to = "value") %>% 
    filter(summer_year >= 1980)
  
  # Gráfico de olas de calor por verano
  f2 <- hw_sum_time %>% 
    ggplot(aes(y = value, x = summer_year, fill = hw)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_continuous(expand = c(0.03, 0.03)) +
    scale_fill_manual(values = c("#f39c12", "#e67e22", "#d35400"),
                      name = "Exposición:",
                      labels = c("30º x 3 días", "P90 x 3 días", "P95 x 3 días")) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(y = "Número de olas de calor", x = NULL, linetype = NULL, title = NULL) +
    theme_light() +
    theme(legend.position = "top",
          legend.margin = margin(c(0.05, 0.05, 0.05, 0.05)),
          legend.title = element_text(size = 9), 
          panel.grid = element_blank(),
          plot.title = element_text(size = 10),
          text = element_text(size = 10))
  
  # Combinar los gráficos
  combined_plot <- f2 | f1
  
  # Guardar el gráfico
  ggsave(filename = paste0("Output/", "Descriptives/", "HW_zip/", "comuna_", comuna, "_HW_last_summer_resume.png"), 
         plot = combined_plot,
         res = 300,
         width = 22,
         height = 12,
         units = 'cm', 
         scaling = 0.90,
         device = ragg::agg_png)
}

zip <- unique(bw_data_lw$com)

com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  #select(1:2) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna")

ref_temp <- hw_data %>% 
  filter(month %in% c(11,12,1,2,3)) %>% 
  group_by(com) %>% 
  summarise(p90_tmax=quantile(tmax, probs = 0.90, digits = 2),
            p95_tmax=quantile(tmax, probs = 0.95, digits = 2),
            p99_tmax=quantile(tmax, probs = 0.99, digits = 2), 
            TAD=mean(TAD), 
            EHIsigi_tad=mean(EHIsigi_tad, na.rm=TRUE), 
            EHIaccli_tad=mean(EHIaccli_tad, na.rm=TRUE), 
            EHF_tad=mean(EHF_tad, na.rm=TRUE), 
          ) %>% 
  ungroup()

table_temp <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna") %>% 
  left_join(ref_temp, by=c("codigo_comuna"="com")) %>% 
  filter(codigo_comuna %in% zip)

writexl::write_xlsx(table_temp, "Output/Descriptives/Ref_TMAX_TAD_com_1980_2021.xlsx")

# HW temp data

tmax_tmin_tad_table <- hw_data %>%
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  pivot_longer(
    cols = starts_with("HW_"),
    names_to = "hw_definition",
    values_to = "hw_value"
  ) %>%
  filter(hw_value == 1) %>% 
  group_by(hw_definition) %>% 
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    TAD = mean(TAD, na.rm = TRUE),
    .groups = "drop" 
  )

  writexl::write_xlsx(tmax_tmin_tad_table, "Output/Descriptives/HW_TMIN_TMAX_TAD_com_1980_2021.xlsx")

# MAPS


