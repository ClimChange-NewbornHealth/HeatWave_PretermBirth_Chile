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

    #HW_30C_3d_bin=mean(HW_30C_3d_bin, na.rm = TRUE),
    #HW_p90_3d_bin=mean(HW_p90_3d_bin, na.rm = TRUE),
    #HW_p95_3d_bin=mean(HW_p95_3d_bin, na.rm = TRUE),
    #HW_p99_3d_bin=mean(HW_p99_3d_bin, na.rm = TRUE),
    #HW_EHF_TAD_3d_bin=mean(HW_EHF_TAD_3d_bin, na.rm = TRUE),

    #HW_30C_3d_count=mean(HW_30C_3d_count, na.rm = TRUE),
    #HW_p90_3d_count=mean(HW_p90_3d_count, na.rm = TRUE),
    #HW_p95_3d_count=mean(HW_p95_3d_count, na.rm = TRUE),
    #HW_p99_3d_count=mean(HW_p99_3d_count, na.rm = TRUE),
    #HW_EHF_TAD_3d_count=mean(HW_EHF_TAD_3d_count, na.rm = TRUE)

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

table_pt_aux <- bw_data_lw %>% 
  group_by(com, name_com, year_nac) %>% 
  summarise(
    HW_30C_3d_count=max(HW_30C_3d_count, na.rm = TRUE),
    HW_p90_3d_count=max(HW_p90_3d_count, na.rm = TRUE),
    HW_p95_3d_count=max(HW_p95_3d_count, na.rm = TRUE),
    HW_p99_3d_count=max(HW_p99_3d_count, na.rm = TRUE),
    HW_EHF_TAD_3d_count=max(HW_EHF_TAD_3d_count, na.rm = TRUE)
  ) %>% 
  group_by(com, name_com) %>% 
  summarise(
    HW_30C_3d_count=mean(HW_30C_3d_count, na.rm = TRUE),
    HW_p90_3d_count=mean(HW_p90_3d_count, na.rm = TRUE),
    HW_p95_3d_count=mean(HW_p95_3d_count, na.rm = TRUE),
    HW_p99_3d_count=mean(HW_p99_3d_count, na.rm = TRUE),
    HW_EHF_TAD_3d_count=mean(HW_EHF_TAD_3d_count, na.rm = TRUE)
  ) %>% 
  ungroup()

table_pt <- table_pt %>% 
  left_join(table_pt_aux, by = c("com", "name_com"))

writexl::write_xlsx(table_pt, path =  paste0("Output/", "Descriptives/", "Table_PTB_HW_COM", ".xlsx"))

# Table with HW across time
result <- bw_data_lw %>% 
  group_by(com, name_com, year_nac) %>% 
  summarise(
    HW_30C_3d_count=max(HW_30C_3d_count, na.rm = TRUE),
    HW_p90_3d_count=max(HW_p90_3d_count, na.rm = TRUE),
    HW_p95_3d_count=max(HW_p95_3d_count, na.rm = TRUE),
    HW_p99_3d_count=max(HW_p99_3d_count, na.rm = TRUE),
    HW_EHF_TAD_3d_count=max(HW_EHF_TAD_3d_count, na.rm = TRUE)
  ) %>% 
  ungroup()

result <- result %>%
  select(year_nac, ends_with("_count")) %>%  
  group_by(year_nac) %>%                     
  summarise(across(everything(),             
                   list(mean = mean, min = min, max = max),
                   na.rm = TRUE), .groups = "drop") 


data_plot <- bw_data_lw %>%
  select(com, name_com, year_nac, HW_30C_3d_count, HW_p90_3d_count, HW_p95_3d_count, HW_p99_3d_count, HW_EHF_TAD_3d_count) %>%
  group_by(com, name_com, year_nac) %>% 
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  dplyr::select(!c(com, name_com)) %>% 
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
        legend.text = element_text(size=11),
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
        )

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
        )

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
        )

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
        )

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
            TAD95=quantile(TAD, probs = 0.95, digits = 2, na.rm=TRUE),
            TAD=mean(TAD, na.rm=TRUE), 
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
glimpse(hw_data) 

## Map santiago
america <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")
chile <- ne_states(country = "Chile", returnclass = "sf")
santiago <- chile[chile$name == "Región Metropolitana de Santiago", ]

chile$color <- "gray50"
santiago$color <- "white"

america_centroids <- st_centroid(america) %>% 
  mutate(long = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) |> 
  filter(!iso_a3 %in% c("FLK", "CHL"))

base_map <- ggplot() +
  geom_sf(data = america, fill = "white", color = "black") +
  geom_sf(data = chile, aes(fill = color), color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +  
  geom_text(data = america_centroids, aes(x = long, y = lat, label = iso_a3), size = 4, fontface="bold") + 
  geom_point(aes(x =  st_coordinates(st_centroid(santiago))[1], y =  st_coordinates(st_centroid(santiago))[2]), 
  color = "red", size = 3, shape = 21, fill = "white", stroke = 1.2, alpha=0.5) +
  geom_point(aes(x =  st_coordinates(st_centroid(santiago))[1], y =  st_coordinates(st_centroid(santiago))[2]), 
  color = "red", size = 1, shape = 21, fill = "red") +
  scale_fill_manual(values = c("gray80", "gray50", "white")) +
  #annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.6, tick_height = 0.3) +
  labs(x=NULL, y=NULL, title = "A.") +
  theme_light() +
  theme(
      plot.title = element_text(size = 16),
      legend.position = "none", 
      #axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1), 
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0),
      
    )

zoom_map <- ggplot() +
  geom_sf(data = chile, fill = "gray70", color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +
  coord_sf(xlim = c(-72, -69.5), ylim = c(-34.5, -32.5)) + 
  geom_sf_text(data=santiago, aes(label = name_en, geometry = geometry), size = 5, fontface="bold", color = "black", stat = "sf_coordinates") +
  labs(x=NULL, y=NULL) +
  theme_light() + 
  theme(
      legend.position = "none", 
      plot.title = element_text(size=11, hjust = 0.5),
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size=9),
      axis.text.x = element_text(size=9, angle=45, hjust = 1),
      #axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0)
    )

zoom_map

map_plot <- base_map + inset_element(zoom_map, 
                                        left = -0.1, 
                                        bottom = 0.2, 
                                        right = 0.5, 
                                        top = 0.5)

map_plot

ggsave(
  filename = paste0("Output/Descriptives/MAP_STGO.png"), 
  res = 300,
  width = 20,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 

temp_maxs <- hw_data %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>% 
  group_by(com) %>% 
  summarise(
    p90 = quantile(tmax, probs = 0.90),
    p95 = quantile(tmax, probs = 0.95),
    p99 = quantile(tmax, probs = 0.99),
    tad = mean(TAD, probs = 0.99)
  )

stgo <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  filter(codigo_comuna %in% zip) %>% 
  left_join(temp_maxs, by=c("codigo_comuna"="com")) %>% 
  mutate(id = 1:n()) %>% 
  mutate(id_mun = str_pad(as.integer(factor(id)), width = 2, pad = "0"))

p90 <- stgo %>% 
  ggplot() +
  geom_sf(aes(fill=p90, geometry = geometry), color = "white") +
  geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
  labs(y=NULL, x=NULL, title = "B. 90th percentile") +
  scale_fill_fermenter(palette = "Oranges", direction = 1, 
                       n.breaks = 8, limits = c(20, 35),
                       guide = guide_colorbar(title = "Daily Max. Temp. \n(1991-2019)", barwidth = 7, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "left",
                           reverse = FALSE,
                           label.theme = element_text(angle = 0)) 
                          ) +
  theme_light() +
  theme(
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0),
    strip.background = element_rect(fill = NA, color = "black"),
    strip.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

p90

p95 <- stgo %>% 
  filter(codigo_comuna %in% zip) %>% 
  ggplot() +
  geom_sf(aes(fill=p95, geometry = geometry), color = "white") +
    geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2,  fontface = "bold",  color = "black", stat = "sf_coordinates") +
  labs(y=NULL, x=NULL, title = "C. 95th percentile") +
  scale_fill_fermenter(palette = "Oranges", direction = 1, 
                       n.breaks = 8, limits = c(20, 35),
                       guide = guide_colorbar(title = "Daily Max. Temp. \n(1991-2019)", barwidth = 7, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "left",
                           reverse = FALSE,
                           label.theme = element_text(angle = 0)) 
                        ) +
  theme_light() +
  theme(
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0),
    strip.background = element_rect(fill = NA, color = "black"),
    strip.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

p95

p99 <- stgo %>% 
  filter(codigo_comuna %in% zip) %>% 
  ggplot() +
  geom_sf(aes(fill=p90, geometry = geometry), color = "white") +
    geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
  labs(y=NULL, x=NULL, title = "D. 99th percentile") +
  scale_fill_fermenter(palette = "Oranges", direction = 1, 
                       n.breaks = 8, limits = c(20, 35),
                       guide = guide_colorbar(title = "Daily Max. Temp. \n(1991-2019)", barwidth = 7, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "left",
                           reverse = FALSE,
                           label.theme = element_text(angle = 0)) 
                        ) +
  theme_light() +
  theme(
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0),
    strip.background = element_rect(fill = NA, color = "black"),
    strip.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

p99

tad <- stgo %>% 
  filter(codigo_comuna %in% zip) %>% 
  ggplot() +
  geom_sf(aes(fill=tad, geometry = geometry), color = "white") +
    geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
  labs(y=NULL, x=NULL, title = "A. Mean Temperature") +
  scale_fill_fermenter(palette = "Oranges", direction = 1, 
                       n.breaks = 8, limits = c(14, 22),
                       guide = guide_colorbar(title = "Daily Mean Temp. \n(1991-2019)", barwidth = 7, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "left",
                           reverse = FALSE,
                           label.theme = element_text(angle = 0)) 
                        ) +
  theme_light() +
  theme(
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0),
    strip.background = element_rect(fill = NA, color = "black"),
    strip.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

tad

ggsave(p90,
  filename = paste0("Output/", "Descriptives/", "MAP_P90", ".png"), 
  res = 300,
  width = 15,
  height = 10,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

ggsave(p95,
  filename = paste0("Output/", "Descriptives/", "MAP_P95", ".png"), 
  res = 300,
  width = 15,
  height = 10,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

ggsave(p99,
  filename = paste0("Output/", "Descriptives/", "MAP_P99", ".png"), 
  res = 300,
  width = 15,
  height = 10,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

ggsave(tad,
  filename = paste0("Output/", "Descriptives/", "MAP_TAD", ".png"), 
  res = 300,
  width = 15,
  height = 10,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

plots_save <- (p90 / p95 / p99) + plot_layout(guides = "collect") & 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

plots_save

plots_save <- ggarrange(p90, p95, p99, nrow=3, 
                        common.legend = TRUE,
                        legend = "top"
                      )
plots_save

ggsave(plots_save,
  filename = paste0("Output/", "Descriptives/", "MAP_TMAX", ".png"), 
  res = 300,
  width = 20,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)  
  
id_mun <- stgo |>
  group_by(id_mun, nombre_comuna) |> 
  summarise(n=n()) |> 
  dplyr::select(-n)

id_mun_text <- c(paste(id_mun$id_mun, "-", id_mun$nombre_comuna), "", "", "")
id_mun_matrix <- matrix(id_mun_text, ncol = 4, byrow = FALSE)

text_mun <- tableGrob(id_mun_matrix, theme = ttheme_minimal(
  core = list(fg_params = list(cex = 0.5, hjust = 0, x = 0),
              bg_params = list(fill = "white", col = "white") 
) ,
  padding = unit(c(0.2, 0.2, 0.2, 0.2), "cm") 
))

text_municipality <- textGrob("Municipality:\n", gp = gpar(fontsize = 8, fontface="bold"), hjust = -0.4, x = 0, y = -6)

tab <- ggarrange(
  text_municipality, text_mun,  
  ncol = 1, heights = c(0.02, 1) 
)

tab

ggsave(
  filename = paste0("Output/Descriptives/Stgo_tab.png"), 
  res = 300,
  width = 10,
  height = 6 ,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 

# Map with tad 
p90 <- p90 + theme(plot.margin = margin(1, 1, 1, 1, "pt"))
p95 <- p95 + theme(plot.margin = margin(1, 1, 1, 1, "pt"))
p99 <- p99 + theme(plot.margin = margin(1, 1, 1, 1, "pt"))
tad <- tad + theme(plot.margin = margin(1, 1, 1, 1, "pt"))

plots_save <- ggarrange(tad, p90, p95, p99, 
  nrow=2, ncol = 2,
  common.legend = FALSE,
  legend = "top",
  align = "hv"
)
plots_save

ggsave(plots_save,
  filename = paste0("Output/", "Descriptives/", "MAP_TAD_TMAX", ".png"), 
  res = 300,
  width = 20,
  height = 20,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)  


# HW 

hws <- bw_data_lw %>% 
  group_by(com, name_com, year_nac) %>% 
  summarise(
    hw_30_2d = max(HW_30C_2d_count, na.rm = TRUE),
    hw_30_3d = max(HW_30C_3d_count, na.rm = TRUE),
    hw_30_4d = max(HW_30C_4d_count, na.rm = TRUE),
    
    hw_ehf_2d = max(HW_EHF_TAD_2d_count, na.rm = TRUE),
    hw_ehf_3d = max(HW_EHF_TAD_3d_count, na.rm = TRUE),
    hw_ehf_4d = max(HW_EHF_TAD_4d_count, na.rm = TRUE),
    
    hw_p90_2d = max(HW_p90_2d_count, na.rm = TRUE),
    hw_p90_3d = max(HW_p90_3d_count, na.rm = TRUE),
    hw_p90_4d = max(HW_p90_4d_count, na.rm = TRUE),

    hw_p95_2d = max(HW_p95_2d_count, na.rm = TRUE),
    hw_p95_3d = max(HW_p95_3d_count, na.rm = TRUE),
    hw_p95_4d = max(HW_p95_4d_count, na.rm = TRUE),
    
    hw_p99_2d = max(HW_p99_2d_count, na.rm = TRUE),
    hw_p99_3d = max(HW_p99_3d_count, na.rm = TRUE),
    hw_p99_4d = max(HW_p99_4d_count, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  group_by(com, name_com) %>% 
  summarise(
    hw_30_2d = mean(hw_30_2d, na.rm = TRUE),
    hw_30_3d = mean(hw_30_3d, na.rm = TRUE),
    hw_30_4d = mean(hw_30_4d, na.rm = TRUE),
    
    hw_ehf_2d = mean(hw_ehf_2d, na.rm = TRUE),
    hw_ehf_3d = mean(hw_ehf_3d, na.rm = TRUE),
    hw_ehf_4d = mean(hw_ehf_4d, na.rm = TRUE),
    
    hw_p90_2d = mean(hw_p90_2d, na.rm = TRUE),
    hw_p90_3d = mean(hw_p90_3d, na.rm = TRUE),
    hw_p90_4d = mean(hw_p90_4d, na.rm = TRUE),

    hw_p95_2d = mean(hw_p95_2d, na.rm = TRUE),
    hw_p95_3d = mean(hw_p95_3d, na.rm = TRUE),
    hw_p95_4d = mean(hw_p95_4d, na.rm = TRUE),
    
    hw_p99_2d = mean(hw_p99_2d, na.rm = TRUE),
    hw_p99_3d = mean(hw_p99_3d, na.rm = TRUE),
    hw_p99_4d = mean(hw_p99_4d, na.rm = TRUE)
  )

hws

stgo <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  left_join(hws, by=c("codigo_comuna"="com")) %>% 
  filter(codigo_comuna %in% zip) %>% 
  mutate(id = 1:n()) %>% 
  mutate(id_mun = str_pad(as.integer(factor(id)), width = 2, pad = "0"))

stgo

hws_long <- stgo %>%
  pivot_longer(
    cols = starts_with("hw"), 
    names_to = "heatwave_measure",  
    values_to = "value"  
  )

hws_long <- hws_long %>%
  mutate(heatwave_measure = factor(heatwave_measure, levels = c(
    "hw_30_2d", "hw_30_3d", "hw_30_4d",
    "hw_p90_2d", "hw_p90_3d", "hw_p90_4d",
    "hw_p95_2d", "hw_p95_3d", "hw_p95_4d",
    "hw_p99_2d", "hw_p99_3d", "hw_p99_4d",
    "hw_ehf_2d", "hw_ehf_3d", "hw_ehf_4d"  # Mover EHF al final
  )))

glimpse(hws_long)

hw_mat <- hws_long %>% 
  ggplot() +
  geom_sf(aes(fill = value, geometry = geometry), color = "white") +
  geom_sf_text(aes(label = id_mun, geometry = geometry), size = 1.5, fontface = "bold", color = "black", stat = "sf_coordinates") +
  labs(y=NULL, x=NULL, title = NULL) +
  scale_fill_fermenter(palette = "Oranges", direction = 1, 
                       n.breaks = 8, limits = c(0, 8),
                       guide = guide_colorbar(title = "Number of Heat Waves \n(1991-2019)", barwidth = 7, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "left",
                           reverse = FALSE,
                           label.theme = element_text(angle = 0)) 
                        ) +
  facet_wrap(~heatwave_measure, ncol = 3, nrow = 5, 
           labeller = as_labeller(c("hw_30_2d" = "HW-30ºC 2D", "hw_30_3d" = "HW-30ºC 3D", "hw_30_4d" = "HW-30ºC 4D",
                                    "hw_p90_2d" = "HW-P90 2D", "hw_p90_3d" = "HW-P90 3D", "hw_p90_4d" = "HW-P90 4D",
                                    "hw_p95_2d" = "HW-P95 2D", "hw_p95_3d" = "HW-P95 3D", "hw_p95_4d" = "HW-P95 4D",
                                    "hw_p99_2d" = "HW-P99 2D", "hw_p99_3d" = "HW-P99 3D", "hw_p99_4d" = "HW-P99 4D",
                                    "hw_ehf_2d" = "HW-EHF 2D", "hw_ehf_3d" = "HW-EHF 3D", "hw_ehf_4d" = "HW-EHF 4D"))) +
  theme_light() +
  theme(
    legend.position = "top",   
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.title = element_text(size = 10, hjust = 0),
    #strip.background = element_rect(fill = NA, color = "black"),
    strip.background = element_blank(), 
    strip.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

hw_mat

ggsave(hw_mat,
  filename = paste0("Output/", "Descriptives/", "MAP_HW_MAT", ".png"), 
  res = 300,
  width = 18,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)  


# HEAT MAP Number of HW across time
glimpse(bw_data_lw)

h_service <- rio::import("Data/Input/Health_service_municipality.xlsx") %>% select(-name_com)

heat_table <- bw_data_lw %>% 
  mutate(
    summer_year = case_when(
      month_end_week %in% c(11, 12) ~ paste0(year_end_week),               # Noviembre y Diciembre del mismo año
      month_end_week %in% c(1, 2, 3) ~ paste0(year_end_week - 1),          # Enero, Febrero y Marzo del año siguiente
      TRUE ~ NA_character_                                                 
    )) %>% 
  group_by(com, name_com, summer_year) %>% 
  summarise(
            HW_30C_3d_count=max(HW_30C_3d_count, na.rm = TRUE), 
            HW_p90_3d_count=max(HW_p90_3d_count, na.rm = TRUE), 
            HW_p95_3d_count=max(HW_p95_3d_count, na.rm = TRUE), 
            HW_p99_3d_count=max(HW_p99_3d_count, na.rm = TRUE), 
            HW_EHF_TAD_3d_count=max(HW_EHF_TAD_3d_count, na.rm = TRUE)
            ) %>% 
  ungroup() %>% 
  filter(summer_year != "2020") %>% 
  left_join(h_service, by="com") %>% 
    mutate(
      name_com = fct_reorder(name_com, com), # Ordenar comunas por código postal
      service = factor(service, levels = c("North", "Central", "East", "Southeast", "South", "West")) # Orden geográfico
    )

g1 <- ggplot(heat_table, aes(x = summer_year, y = name_com, fill = HW_30C_3d_count)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(
    colours = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000"), 
    values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7)), # Rescala los valores
    breaks = 0:7, # Valores discretos en la leyenda
    limits = c(0, 7), # Límite del rango
    guide = guide_colorbar(title = "Number of Heatwaves", barwidth = 15, barheight = 0.5)
  ) +
  labs(x = NULL, y = "Municipality", 
        title="A. HW 30 3D") +
  facet_grid(service~., scales = "free", space = "free",  switch = "y") +
  theme_light() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill=NA, color="gray70"), 
    strip.text=element_text(color="black"),
    strip.text.y.left = element_text(angle = 0)
  )

g1

ggsave(g1,
  filename = paste0("Output/", "Descriptives/", "HEATMAP_30C", ".png"), 
  res = 300,
  width = 20,
  height = 14,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

g2 <- ggplot(heat_table, aes(x = summer_year, y = name_com, fill = HW_p90_3d_count)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(
    colours = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000"), 
    values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7)), # Rescala los valores
    breaks = 0:7, # Valores discretos en la leyenda
    limits = c(0, 7), # Límite del rango
    guide = guide_colorbar(title = "Number of Heatwaves", barwidth = 15, barheight = 0.5)
  ) +
  labs(x = NULL, y = "Municipality", 
        title="B. HW P90 3D") +
  facet_grid(service~., scales = "free", space = "free",  switch = "y") +
  theme_light() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill=NA, color="gray70"), 
    strip.text=element_text(color="black"),
    strip.text.y.left = element_text(angle = 0)
  )

g2

ggsave(g2,
  filename = paste0("Output/", "Descriptives/", "HEATMAP_P90", ".png"), 
  res = 300,
  width = 20,
  height = 14,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

g3 <- ggplot(heat_table, aes(x = summer_year, y = name_com, fill = HW_p95_3d_count)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(
    colours = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000"), 
    values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7)), # Rescala los valores
    breaks = 0:7, # Valores discretos en la leyenda
    limits = c(0, 7), # Límite del rango
    guide = guide_colorbar(title = "Number of Heatwaves", barwidth = 15, barheight = 0.5)
  ) +
  labs(x = NULL, y = "Municipality", 
        title="C. HW P95 3D") +
  facet_grid(service~., scales = "free", space = "free",  switch = "y") +
  theme_light() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill=NA, color="gray70"), 
    strip.text=element_text(color="black"),
    strip.text.y.left = element_text(angle = 0)
  )

g3

ggsave(g3,
  filename = paste0("Output/", "Descriptives/", "HEATMAP_P95", ".png"), 
  res = 300,
  width = 20,
  height = 14,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  


g4 <- ggplot(heat_table, aes(x = summer_year, y = name_com, fill = HW_p99_3d_count)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(
    colours = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000"), 
    values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7)), # Rescala los valores
    breaks = 0:7, # Valores discretos en la leyenda
    limits = c(0, 7), # Límite del rango
    guide = guide_colorbar(title = "Number of Heatwaves", barwidth = 15, barheight = 0.5)
  ) +
  labs(x = NULL, y = "Municipality", 
        title="D. HW P99 3D") +
  facet_grid(service~., scales = "free", space = "free",  switch = "y") +
  theme_light() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill=NA, color="gray70"), 
    strip.text=element_text(color="black"),
    strip.text.y.left = element_text(angle = 0)
  )

g4

ggsave(g4,
  filename = paste0("Output/", "Descriptives/", "HEATMAP_P99", ".png"), 
  res = 300,
  width = 20,
  height = 14,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  


g5 <- ggplot(heat_table, aes(x = summer_year, y = name_com, fill = HW_EHF_TAD_3d_count)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(
    colours = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000"), 
    values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7)), # Rescala los valores
    breaks = 0:7, # Valores discretos en la leyenda
    limits = c(0, 7), # Límite del rango
    guide = guide_colorbar(title = "Number of Heatwaves", barwidth = 15, barheight = 0.5)
  ) +
  labs(x = NULL, y = "Municipality", 
        title="E. HW EHF 3D") +
  facet_grid(service~., scales = "free", space = "free",  switch = "y") +
  theme_light() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill=NA, color="gray70"), 
    strip.text=element_text(color="black"),
    strip.text.y.left = element_text(angle = 0)
  )

g5

ggsave(g5,
  filename = paste0("Output/", "Descriptives/", "HEATMAP_EHF", ".png"), 
  res = 300,
  width = 20,
  height = 14,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

plots_save <- ggarrange(g1, g2, g3, g4, g5, nrow=3, ncol=2, common.legend = TRUE)
plots_save

ggsave(plots_save,
  filename = paste0("Output/", "Descriptives/", "HEATMAP_HW", ".png"), 
  res = 300,
  width = 40,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)  

ggsave(ggarrange(g1, g2, nrow=2, ncol=1, common.legend = TRUE),
  filename = paste0("Output/", "Descriptives/", "HEATMAP_HW_row1", ".png"), 
  res = 300,
  width = 30,
  height = 30,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)  

ggsave(ggarrange(g3, g4, nrow=2, ncol=1, common.legend = TRUE),
  filename = paste0("Output/", "Descriptives/", "HEATMAP_HW_row2", ".png"), 
  res = 300,
  width = 30,
  height = 30,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png)  
