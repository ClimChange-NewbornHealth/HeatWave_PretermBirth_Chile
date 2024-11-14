# Code 4: Descriptive analysis lw ----

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
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month_hw", ".RData"))

## Descriptives characteristics -----

bw_data_lmu <- bw_data_lm %>% distinct(id, .keep_all = TRUE)

tab1 <-  bw_data_lw %>% 
   select(tbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_nac, month_nac, 
          name_com,
          birth_preterm,
          birth_very_preterm,      
          birth_moderately_preterm,
          birth_late_preterm,      
          birth_term,             
          birth_posterm,
          HW_30C_2d_bin,           
          HW_p90_2d_bin,           
          HW_p95_2d_bin,           
          HW_p99_2d_bin,           
          HW_30C_2d_count,         
          HW_p90_2d_count,         
          HW_p95_2d_count,         
          HW_p99_2d_count,         
          HW_30C_3d_bin,           
          HW_p90_3d_bin,           
          HW_p95_3d_bin,           
          HW_p99_3d_bin,           
          HW_30C_3d_count,         
          HW_p90_3d_count,         
          HW_p95_3d_count,         
          HW_p99_3d_count,         
          HW_30C_4d_bin,           
          HW_p90_4d_bin,           
          HW_p95_4d_bin,           
          HW_p99_4d_bin,           
          HW_30C_4d_count,         
          HW_p90_4d_count,         
          HW_p95_4d_count,         
          HW_p99_4d_count,         
          HW_EHF_bin,              
          HW_EHF_count
   ) %>% 
  mutate(year_nac=factor(year_nac)) %>% 
  mutate(month_nac=factor(month_nac)) %>% 
   st(,
   digits = 1, 
   out="return", 
   add.median = TRUE,
   fixed.digits = TRUE, 
   simple.kable = FALSE,
   title="",
   numformat = NA) %>% 
   data.frame() 

tab2 <-  bw_data_lmu %>% 
    select(tbw, weeks, sex,  
           age_group_mom, educ_group_mom, job_group_mom,
           age_group_dad, educ_group_dad, job_group_dad, 
           year_nac, month_nac, 
           name_com,
           birth_preterm,
           birth_very_preterm,      
           birth_moderately_preterm,
           birth_late_preterm,      
           birth_term,             
           birth_posterm,
           HW_30C_2d_bin,           
           HW_p90_2d_bin,           
           HW_p95_2d_bin,           
           HW_p99_2d_bin,           
           HW_30C_2d_count,         
           HW_p90_2d_count,         
           HW_p95_2d_count,         
           HW_p99_2d_count,         
           HW_30C_3d_bin,           
           HW_p90_3d_bin,           
           HW_p95_3d_bin,           
           HW_p99_3d_bin,           
           HW_30C_3d_count,         
           HW_p90_3d_count,         
           HW_p95_3d_count,         
           HW_p99_3d_count,         
           HW_30C_4d_bin,           
           HW_p90_4d_bin,           
           HW_p95_4d_bin,           
           HW_p99_4d_bin,           
           HW_30C_4d_count,         
           HW_p90_4d_count,         
           HW_p95_4d_count,         
           HW_p99_4d_count,         
           HW_EHF_bin,              
           HW_EHF_count
    ) %>% 
   mutate(year_nac=factor(year_nac)) %>% 
   mutate(month_nac=factor(month_nac)) %>% 
    st(,
    digits = 1, 
    out="return", 
    add.median = TRUE,
    fixed.digits = TRUE, 
    simple.kable = FALSE,
    title="",
    numformat = NA) %>% 
    data.frame() 
 
 
lista_tab <- list(
    "tab1"=tab1, 
    "tab2"=tab2 
 )
 
writexl::write_xlsx(lista_tab, path =  paste0(data_out,  "Descriptives", ".xlsx"))
 
rm(bw_data_lmu)

## Descriptive analysis ---- 
glimpse(bw_data_lw)

### Preterms -----

# Preterm across time 
table <- bw_data_lw %>% 
  group_by(year_nac, month_nac) %>% 
    summarise(
      tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
      tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
      tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100,
      tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
      tasa_t=mean(birth_term, na.rm=TRUE)*100,
      tasa_post=mean(birth_posterm, na.rm=TRUE)*100,
    ) %>%
  mutate(
    summer_year = case_when(
      month_nac %in% c(11, 12) ~ paste0("Summer-", year_nac),           # Noviembre y Diciembre toman el año actual
      month_nac %in% c(1, 2, 3) ~ paste0("Summer-", year_nac - 1),       # Enero, Febrero y Marzo toman el año anterior
      TRUE ~ NA_character_                                           # Otros meses quedan como NA
    )
  ) %>% 
  relocate(summer_year)

table %>% 
  ggplot(aes(y=tasa_pt, x=factor(month_nac), group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_pt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 18,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_vpt, x=factor(month_nac), group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_vpt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 18,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_mpt, x=factor(month_nac), group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_mpt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 18,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_lpt, x=factor(month_nac), group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_lpt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 18,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_t, x=factor(month_nac), group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_t", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 18,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_post, x=factor(month_nac), group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_post", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 18,
       units = 'cm',
       scaling = 0.8,
       device = ragg::agg_png)


table <- bw_data_lw %>% 
  group_by(year_nac) %>% 
  summarise(
    tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
    tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
    tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100,
    tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
    tasa_t=mean(birth_term, na.rm=TRUE)*100,
    tasa_post=mean(birth_posterm, na.rm=TRUE)*100,
  ) %>% 
  pivot_longer(
    cols=!year_nac, 
    names_to="preterm",
    values_to="prev"
  ) %>% 
  mutate(preterm=case_when(
    preterm=="tasa_vpt" ~ "Very Preterm",
    preterm=="tasa_mpt" ~ "Moderately Preterm",
    preterm=="tasa_lpt" ~ "Late Preterm",
    preterm=="tasa_pt" ~ "Preterm",
    preterm=="tasa_t" ~  "Term",
    preterm=="tasa_post" ~ "Post-term"
  )) %>% 
  mutate(preterm=factor(preterm, levels=c(
    "Very Preterm",
    "Moderately Preterm",
    "Late Preterm",
    "Preterm",
    "Term",
    "Post-term"
  )))

table %>% 
  ggplot(aes(y=prev, x=year_nac)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~preterm, ncol = 2, scales = "free") +
  scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence (per 1.000)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 12,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)

## HW

hw_data <- rio::import(paste0(data_out, "hw_data_1980_2021", ".RData"))

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


table_pt <- bw_data_lw %>% 
  mutate(month = month(date_end_week)) %>% 
  mutate(month=factor(month, 
    levels=c(11, 12, 1:4),
    labels=c("November", "December", "January", "February", "March", "April"))) %>% 
  group_by(name_com) %>% 
  summarise(
    
    weeks=mean(weeks, na.rm = TRUE),
    tbw=mean(tbw, na.rm = TRUE),

    birth_preterm_n=sum(birth_preterm, na.rm = TRUE),
    birth_very_preterm_n=sum(birth_very_preterm, na.rm = TRUE),
    birth_moderately_preterm_n=sum(birth_moderately_preterm, na.rm = TRUE),
    birth_late_preterm_n=sum(birth_late_preterm, na.rm = TRUE),
    birth_term_n=sum(birth_term, na.rm = TRUE),
    birth_posterm_n=sum(birth_posterm, na.rm = TRUE),

    birth_preterm=mean(birth_preterm, na.rm = TRUE),
    birth_very_preterm=mean(birth_very_preterm, na.rm = TRUE),
    birth_moderately_preterm=mean(birth_moderately_preterm, na.rm = TRUE),
    birth_late_preterm=mean(birth_late_preterm, na.rm = TRUE),
    birth_term=mean(birth_term, na.rm = TRUE),
    birth_posterm=mean(birth_posterm, na.rm = TRUE),

    HW_30C_2d_bin=mean(HW_30C_2d_bin, na.rm = TRUE),
    HW_p90_2d_bin=mean(HW_p90_2d_bin, na.rm = TRUE),
    HW_p95_2d_bin=mean(HW_p95_2d_bin, na.rm = TRUE),
    HW_p99_2d_bin=mean(HW_p99_2d_bin, na.rm = TRUE),

    HW_30C_2d_count=mean(HW_30C_2d_count, na.rm = TRUE),
    HW_p90_2d_count=mean(HW_p90_2d_count, na.rm = TRUE),
    HW_p95_2d_count=mean(HW_p95_2d_count, na.rm = TRUE),
    HW_p99_2d_count=mean(HW_p99_2d_count, na.rm = TRUE),

    HW_30C_3d_bin=mean(HW_30C_3d_bin, na.rm = TRUE),
    HW_p90_3d_bin=mean(HW_p90_3d_bin, na.rm = TRUE),
    HW_p95_3d_bin=mean(HW_p95_3d_bin, na.rm = TRUE),
    HW_p99_3d_bin=mean(HW_p99_3d_bin, na.rm = TRUE),

    HW_30C_3d_count=mean(HW_30C_3d_count, na.rm = TRUE),
    HW_p90_3d_count=mean(HW_p90_3d_count, na.rm = TRUE),
    HW_p95_3d_count=mean(HW_p95_3d_count, na.rm = TRUE),
    HW_p99_3d_count=mean(HW_p99_3d_count, na.rm = TRUE),

    HW_30C_4d_bin=mean(HW_30C_4d_bin, na.rm = TRUE),
    HW_p90_4d_bin=mean(HW_p90_4d_bin, na.rm = TRUE),
    HW_p95_4d_bin=mean(HW_p95_4d_bin, na.rm = TRUE),
    HW_p99_4d_bin=mean(HW_p99_4d_bin, na.rm = TRUE),

    HW_30C_4d_count=mean(HW_30C_4d_count, na.rm = TRUE),
    HW_p90_4d_count=mean(HW_p90_4d_count, na.rm = TRUE),
    HW_p95_4d_count=mean(HW_p95_4d_count, na.rm = TRUE),
    HW_p99_4d_count=mean(HW_p99_4d_count, na.rm = TRUE)
  
  )


table <- bw_data_lw %>% 
    group_by(name_com, year_nac, month_nac) %>% 
      summarise(
        tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
        tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
        tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100,
        tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
        tasa_t=mean(birth_term, na.rm=TRUE)*100,
        tasa_post=mean(birth_posterm, na.rm=TRUE)*100,
      ) %>%
    mutate(
      summer_year = case_when(
        month_nac %in% c(11, 12) ~ paste0("Summer-", year_nac),           # Noviembre y Diciembre toman el año actual
        month_nac %in% c(1, 2, 3) ~ paste0("Summer-", year_nac - 1),       # Enero, Febrero y Marzo toman el año anterior
        TRUE ~ NA_character_                                           # Otros meses quedan como NA
      )
    ) %>% 
    relocate(summer_year)
  
  table %>% 
    ggplot(aes(y=tasa_pt, x=factor(month_nac), group = 1)) +
    geom_line(color="#08519c") +
    geom_point(color="#08519c", size=0.5) +
    facet_wrap(~summer_year, ncol = 5, scales="free_x") +
    #scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
    labs(y ="Prevalence", x=NULL) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.background = element_rect(fill=NA, color="black"), 
          strip.text=element_text(color="black"),
          panel.grid = element_blank())
  
  ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_pt", ".png"), # "Preterm_trendsrm1991"
         res = 300,
         width = 25,
         height = 18,
         units = 'cm',
         scaling = 0.8,
         device = ragg::agg_png)
  

table <- bw_data_lw %>% 
  group_by(name_com, year_nac) %>% 
  summarise(
    tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
    tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
    tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100,
    tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
    tasa_t=mean(birth_term, na.rm=TRUE)*100,
    tasa_post=mean(birth_posterm, na.rm=TRUE)*100,
  ) %>% 
  pivot_longer(
    cols=!c(year_nac, name_com),
    names_to="preterm",
    values_to="prev"
  ) %>% 
  mutate(preterm=case_when(
    preterm=="tasa_vpt" ~ "Very Preterm",
    preterm=="tasa_mpt" ~ "Moderately Preterm",
    preterm=="tasa_lpt" ~ "Late Preterm",
    preterm=="tasa_pt" ~ "Preterm",
    preterm=="tasa_t" ~  "Term",
    preterm=="tasa_post" ~ "Post-term"
  )) %>% 
  mutate(preterm=factor(preterm, levels=c(
    "Very Preterm",
    "Moderately Preterm",
    "Late Preterm",
    "Preterm",
    "Term",
    "Post-term"
  )))

table %>% 
  filter(preterm=="Preterm") %>% 
  ggplot(aes(y=prev, x=year_nac)) +
  geom_line(color="#08519c") +
  geom_smooth(method = "lm", color="red") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~name_com, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence (per 1.000)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill=NA, color="black"), 
        strip.text=element_text(color="black"),
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_com", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 30,
       height = 30,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)


writexl::write_xlsx(table_pt, path =  paste0(data_out, "Descriptives/", "Tabla_PT", ".xlsx"))

