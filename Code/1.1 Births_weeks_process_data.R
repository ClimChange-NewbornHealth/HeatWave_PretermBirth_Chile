# Code 1.1: Birth exploratorion and preparation ----
rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/Nacimientos/"
data_out <- "Data/Output/"

## Birth data ---- 

# ID file load
file <- "births_1992_2020.RData"

# Open data in R
load(paste0(data_out, file)) 

glimpse(births)

### 1. Fixed cohort bias  -----

# Floor 
table(births$year_week1) 

# births_rm1991 <- births %>% 
#   filter(year_week1>=1992) # Loss 77980 Introduce bias 

length(unique(births$id)) # 2,988,646,213
# Ceiling date_ends_week_gest 2020-12-31 

weeks <- births %>% 
  group_by(date_start_week_gest, date_ends_week_gest, weeks) %>% 
  summarise(n_gestantes=n(),
            min_semana_gestacion=min(weeks),
            max_semana_gestacion=max(weeks), 
            ultimo_nacimiento=max(date_nac) 
          )

write.xlsx(weeks, "Output/Descriptives/Start_ends_gestational_weeks.xlsx")

# Figure trends preterms

table <- births %>% 
  group_by(year_nac) %>% 
  summarise(
    tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*1000,
    tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*1000,
    tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*1000,
    tasa_pt=mean(birth_preterm, na.rm=TRUE)*1000,
    tasa_t=mean(birth_term, na.rm=TRUE)*1000,
    tasa_post=mean(birth_posterm, na.rm=TRUE)*1000,
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
  geom_point(color="#08519c") +
  facet_wrap(~preterm, ncol = 2, scales = "free") +
  scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence (per 1.000)", x=NULL) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 12,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)

# Podrían haber nacido en el úlimo mes 30/04, pero no última semana -> fecha de cohorte 07/04. Todo el resto fuera. 
#rm(births_rm1991)

### 2. Expand gestational weeks  -----
glimpse(births)

# Create week_gest obs
s <- now()
births <- births %>% 
  rowwise() %>%
  mutate(week_gest = list(seq.Date(date_start_week_gest, date_ends_week_gest, by = "week"))) %>%
  unnest(week_gest) %>%
  group_by(id) %>%
  mutate(week_gest_num = paste0(abs(weeks - row_number())),  
         week_gest_num = (weeks) - as.numeric(week_gest_num), 
         date_start_week = (week_gest - (7 * abs(week_gest_num - row_number()))) - weeks(1), #(abs(week_gest_num - row_number())),
         date_start_week = date_start_week + 1,
         date_end_week = week_gest - (7 * abs(week_gest_num - row_number()))
         ) %>% # ,(abs(week_gest_num - row_number())
  group_by(id) %>% 
  distinct(week_gest_num, .keep_all = TRUE) %>% 
  arrange(id, week_gest_num) %>% 
  ungroup() 
e <- now()
e-s # Timer 1.48 hrs

glimpse(births)
save(births, file=paste0(data_out, "births_1992_2020_weeks", ".RData"))


### 3. Only exposition births  -----

#births <- rio::import(paste0(data_out, "births_1992_2020_weeks", ".RData"))

## Define the range of time observation
#dates_range <- function(date) {
#  any(date >= as.Date(paste0(year(date)-1, "-11-01")) & date <= as.Date(paste0(year(date), "-03-31")))
#}

dates_range <- function(dates) {
  all(dates >= as.Date(paste0(year(dates[1]) - 1, "-11-01")) & 
      dates <= as.Date(paste0(year(dates[1]), "-03-31")))
}


## Last month -> Nov, Dic, Ene, Feb, Mar
births_last_month <- births %>%
  group_by(id) %>%  
  filter(week_gest_num > (max(week_gest_num) - 4)) %>%  
  mutate(month_end_week = month(date_end_week)) %>% 
  mutate(year_end_week=year(date_end_week)) %>% 
  filter(all(month_end_week %in% c(11, 12, 1, 2, 3))) %>% 
  ungroup()
 
save(births_last_month, file=paste0(data_out, "births_1992_2020_last_month", ".RData"))

## Last week  -> Nov, Dic, Ene, Feb, Mar
births_last_week <- births %>%
  group_by(id) %>%  
  filter(week_gest_num == max(week_gest_num)) %>%  
  ungroup() %>% 
  mutate(month_end_week=month(date_end_week)) %>%
  mutate(year_end_week=year(date_end_week)) %>%
  filter(month_end_week %in% c(11, 12, 1, 2, 3)) 

save(births_last_week, file=paste0(data_out, "births_1992_2020_last_week", ".RData"))

