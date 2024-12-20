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
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData")) %>% drop_na()
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month_hw", ".RData")) %>% drop_na()
bw_data_lmu <- bw_data_lm %>% distinct(id, .keep_all = TRUE)

## Descriptives characteristics -----
tab1 <-  bw_data_lw %>% 
   select(
          birth_preterm,
          birth_very_preterm,      
          birth_moderately_preterm,
          birth_late_preterm,      
          birth_term, 
          #birth_posterm, 
          tbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_nac, month_nac, 
          sovi, vulnerability,
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
          HW_EHF_TAD_2d_bin,
          HW_EHF_TAD_3d_bin,
          HW_EHF_TAD_4d_bin,
          HW_EHF_TAD_2d_count,
          HW_EHF_TAD_3d_count,
          HW_EHF_TAD_4d_count,
          HW_EHF_TAD_sev_first,
          HW_EHF_TMAX_2d_bin,
          HW_EHF_TMAX_3d_bin,
          HW_EHF_TMAX_4d_bin,
          HW_EHF_TMAX_2d_count,
          HW_EHF_TMAX_3d_count,
          HW_EHF_TMAX_4d_count,
          HW_EHF_TMAX_sev_first
   ) %>% 
   mutate(
    birth_preterm=factor(birth_preterm),
    birth_very_preterm=factor(birth_very_preterm),      
    birth_moderately_preterm=factor(birth_moderately_preterm),
    birth_late_preterm=factor(birth_late_preterm),      
    birth_term=factor(birth_term), 
   ) %>% 
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
    select(
      birth_preterm,
      birth_very_preterm,      
      birth_moderately_preterm,
      birth_late_preterm,      
      birth_term, 
      #birth_posterm, 
      tbw, weeks, sex,  
      age_group_mom, educ_group_mom, job_group_mom,
      age_group_dad, educ_group_dad, job_group_dad, 
      year_nac, month_nac, 
      sovi, vulnerability,
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
      HW_EHF_TAD_2d_bin,
      HW_EHF_TAD_3d_bin,
      HW_EHF_TAD_4d_bin,
      HW_EHF_TAD_2d_count,
      HW_EHF_TAD_3d_count,
      HW_EHF_TAD_4d_count,
      HW_EHF_TAD_sev_first,
      HW_EHF_TMAX_2d_bin,
      HW_EHF_TMAX_3d_bin,
      HW_EHF_TMAX_4d_bin,
      HW_EHF_TMAX_2d_count,
      HW_EHF_TMAX_3d_count,
      HW_EHF_TMAX_4d_count,
      HW_EHF_TMAX_sev_first
    ) %>% 
   mutate(
    birth_preterm=factor(birth_preterm),
    birth_very_preterm=factor(birth_very_preterm),      
    birth_moderately_preterm=factor(birth_moderately_preterm),
    birth_late_preterm=factor(birth_late_preterm),      
    birth_term=factor(birth_term), 
   ) %>% 
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
 
writexl::write_xlsx(lista_tab, path =  paste0("Output/", "Descriptives/",  "Descriptives", ".xlsx"))
 
#rm(bw_data_lmu)

## Descriptive analysis ---- 
glimpse(bw_data_lw)
glimpse(bw_data_lmu)

### Preterms -----

# Preterm across time 
table <- bw_data_lw %>% 
  group_by(year_nac) %>% 
  summarise(
    tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
    tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
    tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
    tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100,
    tasa_t=mean(birth_term, na.rm=TRUE)*100,
    tasa_post=mean(birth_posterm, na.rm=TRUE)*100
  ) %>% 
  pivot_longer(
    cols=!year_nac, 
    names_to="preterm",
    values_to="prev"
  ) %>% 
  mutate(preterm=case_when(
    preterm=="tasa_pt" ~ "Preterm",  
    preterm=="tasa_vpt" ~ "Very Preterm",
    preterm=="tasa_mpt" ~ "Moderately Preterm",
    preterm=="tasa_lpt" ~ "Late Preterm",
    preterm=="tasa_t" ~  "Term",
    preterm=="tasa_post" ~ "Post-term"
  )) %>% 
  mutate(preterm=factor(preterm, levels=c(
    "Preterm",
    "Very Preterm",
    "Moderately Preterm",
    "Late Preterm",
    "Term",
    "Post-term"
  )))

  titles <- c(
    "A. Preterm (<37 weeks)", 
    "B. Very Preterm (28-32 weeks)", 
    "C. Moderately Preterm (32-33 weeks)", 
    "D. Late Preterm (34-37 weeks)", 
    "E. Term (>38 weeks)", 
    "F. Post-term (42-44 weeks)"
  )

figures <- table %>%
    split(.$preterm) %>%  
    map2(titles, ~ ggplot(.x, aes(y = prev, x = year_nac)) +
           geom_line(color = "#08519c") +
           geom_point(color = "#08519c", size = 0.5) +
           geom_smooth(method = "lm", formula = y ~ x + I(x^2), color="gray30", alpha=0.5, linewidth=0.5) +
           labs(
             title = paste0(.y),
             y = "Prevalence (per 100)",
             x = NULL
           ) +
           scale_x_continuous(breaks = seq(1992, 2020, by = 4)) +
            scale_y_continuous(n.breaks = 4) +
           theme_light() +
           theme(
            plot.title = element_text(size = 11, hjust = 0, face = "bold"),
             strip.background = element_rect(fill = NA, color = "black"),
             strip.text = element_text(color = "black"),
             panel.grid = element_blank()
           ))

#wrap_plots(figures, ncol = 2)
do.call(ggarrange, c(figures, list(nrow = 3, ncol = 2, common.legend=TRUE)))

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 17,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)


# Preterm across time by municipality 
table <- bw_data_lw %>% 
  group_by(name_com, year_nac) %>% 
  summarise(
    tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
    tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
    tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
    tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100,
    tasa_t=mean(birth_term, na.rm=TRUE)*100,
    tasa_post=mean(birth_posterm, na.rm=TRUE)*100
  ) %>% 
  pivot_longer(
    cols=!c(year_nac, name_com),
    names_to="preterm",
    values_to="prev"
  ) %>% 
  mutate(preterm=case_when(
    preterm=="tasa_pt" ~ "Preterm (<37 weeks)", 
    preterm=="tasa_vpt" ~ "Very Preterm (28-32 weeks)", 
    preterm=="tasa_mpt" ~ "Moderately Preterm (32-33 weeks)", 
    preterm=="tasa_lpt" ~ "Late Preterm (34-37 weeks)", 
    preterm=="tasa_t" ~  "Term (>38 weeks)", 
    preterm=="tasa_post" ~ "Post-term (42-44 weeks)"
  )) %>% 
  mutate(preterm=factor(preterm, levels=c(
    "Preterm (<37 weeks)", 
    "Very Preterm (28-32 weeks)", 
    "Moderately Preterm (32-33 weeks)", 
    "Late Preterm (34-37 weeks)", 
    "Term (>38 weeks)", 
    "Post-term (42-44 weeks)"
  )))

table %>% 
  filter(preterm=="Preterm (<37 weeks)") %>% 
  ggplot(aes(y=prev, x=year_nac)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color="red", alpha=0.5, linewidth=0.5) +
  facet_wrap(~name_com, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence (per 100)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 11, hjust = 0, color="black"),
        strip.background = element_rect(fill=NA, color=NA), 
        panel.grid = element_blank())

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_com", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 30,
       height = 30,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)


### Preterms by summer -----

table <- bw_data_lw %>% 
  group_by(year_nac, month_nac) %>% 
    summarise(
      tasa_pt=mean(birth_preterm, na.rm=TRUE)*100,
      tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*100,
      tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*100,
      tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*100 
    ) %>%
   mutate(
    summer_year = case_when(
      month_nac %in% c(11, 12) ~ paste0("Summer ", year_nac),               # Noviembre y Diciembre del mismo año
      month_nac %in% c(1, 2, 3) ~ paste0("Summer ", year_nac - 1),          # Enero, Febrero y Marzo del año siguiente
      TRUE ~ NA_character_                                                 
    ),
    year_month = case_when(
      month_nac == 1 ~ paste0("01-", year_nac),
      month_nac == 2 ~ paste0("02-", year_nac),
      month_nac == 3 ~ paste0("03-", year_nac),
      month_nac == 11 ~ paste0("11-", year_nac),
      month_nac == 12 ~ paste0("12", year_nac),
      TRUE ~ NA_character_                                                
    )
  ) %>% 
  #mutate(month_nac_sort = factor(month_nac, levels=c(11, 12, 1, 2, 3))) %>% 
  mutate(month_nac_sort = factor(
    paste0(month_nac, "-", year_nac), 
    levels = c(
      paste0("11", "-", unique(year_nac)),
      paste0("12", "-", unique(year_nac)),
      paste0("1", "-", unique(year_nac)),
      paste0("2", "-", unique(year_nac)),
      paste0("3", "-", unique(year_nac))
    ),
    labels = c(
      paste0("11", "-", unique(year_nac)),
      paste0("12", "-", unique(year_nac)),
      paste0("01", "-", unique(year_nac)),
      paste0("02", "-", unique(year_nac)),
      paste0("03", "-", unique(year_nac))
    )
  )) %>% 
  relocate(summer_year)

table %>% 
  ggplot(aes(y=tasa_pt, x=month_nac_sort, group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  labs(y ="Prevalence (per 100)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 11, hjust = 0, color="black"),
        strip.background = element_rect(fill=NA, color=NA), 
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9, vjust=0.9, size=10) 
      )

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_pt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 28,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)


table %>% 
  ggplot(aes(y=tasa_vpt, x=month_nac_sort, group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  labs(y ="Prevalence (per 100)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 11, hjust = 0, color="black"),
        strip.background = element_rect(fill=NA, color=NA), 
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9, vjust=0.9, size=10) 
      )

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_vpt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 28,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_mpt, x=month_nac_sort, group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  labs(y ="Prevalence (per 100)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 11, hjust = 0, color="black"),
        strip.background = element_rect(fill=NA, color=NA), 
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9, vjust=0.9, size=10) 
      )

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_mpt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 28,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)

table %>% 
  ggplot(aes(y=tasa_lpt, x=month_nac_sort, group = 1)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c", size=0.5) +
  facet_wrap(~summer_year, ncol = 5, scales="free_x") +
  labs(y ="Prevalence (per 100)", x=NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 11, hjust = 0, color="black"),
        strip.background = element_rect(fill=NA, color=NA), 
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9, vjust=0.9, size=10) 
      )

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_summer_prev_lpt", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 25,
       height = 28,
       units = 'cm',
       scaling = 0.9,
       device = ragg::agg_png)
