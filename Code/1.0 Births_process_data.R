# Code 1: Birth data preparation ----
rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Birth data ---- 

# ID file load
file <- "data_1992_2020.RData"

# Open data in R
load(paste0(data_inp, file)) 

births <- data_1992_2020 %>% janitor::clean_names()

rm(data_1992_2020)

# Explorer data 
glimpse(births) # 7084698 obs 

# Prepare data
### 1. Date 1992-2020 -----
table(births$ano_nac)
table(births$region)

### 2. Construction births date----
births <- births %>% 
  mutate(date_nac = make_date(year = ano_nac, month = mes_nac, day = dia_nac)) %>% # Year, Month, Date 
  filter(region==13) # 2895207

glimpse(births)

### 3. Create week_gest obs ----

start_count <- nrow(births) 

s <- now()

births <- births %>%
  mutate(tbw=if_else(peso==9999, NA_real_, peso),
        weeks=if_else(semanas==99, NA_real_, semanas)) %>% 
  drop_na(date_nac) %>% 
  {
    cat("Observaciones después de drop_na(date_nac):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA: 41
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } %>%     
  drop_na(weeks) %>%  
  {
    cat("Observaciones después de drop_na(semanas):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA 2695
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } %>%     
  drop_na(comuna) %>%  
  {
    cat("Observaciones después de drop_na(comuna):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA 0
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } %>%     
  mutate(id=1:n()) %>% 
  mutate(date_start = date_nac - weeks(weeks-2),
         date_start = date_start - weeks(1), 
         date_end = date_nac) 
  

e <- now()
e-s # Timer 6.4 sec

# Check results

t1 <- births %>%
  group_by(weeks) %>% 
  summarise(min_tbw=min(tbw, na.rm = TRUE), 
            mean_tbw=mean(tbw, na.rm = TRUE), 
            median_tbw=median(tbw, na.rm = TRUE), 
            max_tbw=max(tbw, na.rm = TRUE),  
            n_births=n())

write.xlsx(t1, "Data/Output/Data_weeks_tbw.xlsx")

### 4. Prepare births variables ----

comunas <- chilemapas::codigos_territoriales
comunas <- comunas %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna))

# 2,892,471
births <- births %>%
  mutate(
    size=if_else(talla==99, NA_real_, talla),
    age_mom=if_else(edad_madre==99, NA_real_, edad_madre),
    educ_mom=if_else(nivel_madre==9, NA_real_, nivel_madre),
    job_mom=if_else(activ_madre %in% c(9), NA_real_, activ_madre+1),
    age_dad=if_else(edad_padre==99, NA_real_, edad_padre),
    educ_dad=if_else(nivel_padre==9, NA_real_, nivel_padre),
    job_dad=if_else(activ_padre %in% c(3,9), NA_real_, activ_padre+1)
    ) %>%
  mutate(
    sex=factor(sexo, levels=c(1,2,9), labels=c("Boy", "Girl", "Unknown"))
  ) %>%
  left_join(comunas, by=c("comuna"="codigo_comuna")) %>%
  mutate(ntbw=if_else(weeks<=37, 1, 0)) %>%
  mutate(date_week1 = date_start, 
         year_week1 = year(date_start), 
         month_week1 = month(date_start),
         ym_week1 = as.yearmon(date_start, "%M-%Y")) %>%
  filter(!is.na(weeks)) %>%
  #filter(!is.na(tbw)) %>%
  rename(day_nac=dia_nac,
         month_nac=mes_nac,
         year_nac=ano_nac,
         com=comuna, 
         reg=codigo_region,
         name_reg=nombre_region, 
         date_start_week_gest=date_start,
         date_ends_week_gest=date_end,
         name_reg=nombre_region, 
         name_comuna=nombre_comuna
         ) %>% 
  mutate(
    age_group_mom=case_when(
      age_mom <= 20 ~ 1, 
      age_mom > 20 & age_mom <= 29 ~ 2,
      age_mom >= 30 & age_mom <= 39 ~ 3,
      age_mom >= 40 & age_mom <= 49 ~ 4,
      age_mom >= 50 ~ 5, 
      TRUE ~ 6
    ),
    age_group_mom=factor(age_group_mom, 
                         levels=c(1:6), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50", "Unknown")),
    educ_group_mom = case_when(
      educ_mom == 1 ~ 4, # College
      educ_mom == 2 ~ 3, # Secondary
      educ_mom == 3 ~ 3, # Secondary
      educ_mom == 4 ~ 2, # Primary
      educ_mom == 5 ~ 1, # No educaction 
      TRUE ~ 5, #Unknow
    ), 
    educ_group_mom = factor(educ_group_mom, 
                            levels = c(1:5), 
                            labels = c("Non education", "Primary", "Secondary", "College", "Unknown")),
    job_group_mom = if_else(is.na(job_mom), 4, job_mom), 
    job_group_mom = factor(job_group_mom, levels = c(1,2,3,4), labels=c("Not working", "Employed", "Unemployed", "Unknown"))
  ) %>%
  relocate(age_group_mom, educ_group_mom, job_group_mom, .after=job_mom) %>%
  mutate(
    age_group_dad=case_when(
      age_dad <= 20 ~ 1, 
      age_dad > 20 & age_dad <= 29 ~ 2,
      age_dad >= 30 & age_dad <= 39 ~ 3,
      age_dad >= 40 & age_dad <= 49 ~ 4,
      age_dad >= 50 ~ 5, 
      TRUE ~ 6
    ),
    age_group_dad=factor(age_group_dad, 
                         levels=c(1:6), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50", "Unknown")),
    educ_group_dad = case_when(
      educ_dad == 1 ~ 4, # College
      educ_dad == 2 ~ 3, # Secondary
      educ_dad == 3 ~ 3, # Secondary
      educ_dad == 4 ~ 2, # Primary
      educ_dad == 5 ~ 1, # No educaction 
      TRUE ~ 5, #Unknow
    ), 
    educ_group_dad = factor(educ_group_dad, 
                            levels = c(1:5), 
                            labels = c("Non education", "Primary", "Secondary", "College", "Unknown")),
    job_group_dad = if_else(is.na(job_dad), 4, job_dad), 
    job_group_dad = factor(job_group_dad, levels = c(1,2,3,4), labels=c("Not working", "Employed", "Unemployed", "Unknown"))
  ) %>%
  relocate(age_group_dad, educ_group_dad, job_group_dad, .after=job_dad) %>% 
  select(id, weeks,
         # Other variables 
         date_nac, day_nac, month_nac, year_nac, 
         date_week1, year_week1, month_week1, ym_week1,
         date_start_week_gest, date_ends_week_gest,
         sex, tbw, size,
         age_group_mom,educ_group_mom,job_group_mom,
         age_group_dad,educ_group_dad,job_group_dad,
         )

### 5. Outcome: preterm (2892471) ---- 
births <- births %>% 
  mutate(birth_preterm = if_else(weeks < 37, 1, 0)) %>% 
  mutate(birth_early = if_else(weeks >= 37 & weeks <=38, 1, 0)) %>% 
  mutate(birth_extrem_preterm = if_else(weeks < 28, 1, 0)) %>% 
  mutate(birth_very_preterm = if_else(weeks >= 28 & weeks <32, 1, 0)) %>% 
  mutate(birth_moderate_preterm = if_else(weeks >= 32 & weeks <37, 1, 0))

### 6. Exclusion criteria  -----

# USA criteria: Alexander G, Himes J, Kaufaman R, Mor J, Kogan M. A United States national reference for fetal growth. Obstet Gynecol. 1996;87(2). 

missing <- births %>%
  mutate(
    test = case_when(
      weeks == 20 ~ tbw >= 125 & tbw <= 1250,
      weeks == 22 ~ tbw >= 125 & tbw <= 1375,
      weeks == 23 ~ tbw >= 125 & tbw <= 1500,
      weeks == 24 ~ tbw >= 125 & tbw <= 1625,
      weeks == 25 ~ tbw >= 250 & tbw <= 1750,
      weeks == 26 ~ tbw >= 250 & tbw <= 2000,
      weeks == 27 ~ tbw >= 250 & tbw <= 2250,
      weeks == 28 ~ tbw >= 250 & tbw <= 2500,
      weeks == 29 ~ tbw >= 250 & tbw <= 2750,
      weeks == 30 ~ tbw >= 375 & tbw <= 3000,
      weeks == 31 ~ tbw >= 375 & tbw <= 3250,
      weeks == 32 ~ tbw >= 500 & tbw <= 3500,
      weeks == 33 ~ tbw >= 500 & tbw <= 3750,
      weeks == 34 ~ tbw >= 500 & tbw <= 4000,
      weeks == 35 ~ tbw >= 750 & tbw <= 4500,
      weeks == 36 ~ tbw >= 750 & tbw <= 5000,
      weeks == 37 ~ tbw >= 750 & tbw <= 5500,
      weeks >= 38 ~ tbw >= 1000 & tbw <= 6000,
      TRUE ~ FALSE  
    )
  ) %>%
  filter(!test) %>%
  group_by(weeks) %>%
  summarise(loss_data = n()) %>% 
  ungroup()

# Total missing: 
sum(missing$loss_data) # 1552

write.xlsx(missing, "Data/Output/Data_exclussion_weeks_tbw-USA.xlsx")

# Chile criteria: remove 1% extreme values 

missing2 <- births %>%
  group_by(weeks) %>%
  mutate(
    P2.5 = quantile(tbw, probs = 0.005, na.rm = TRUE),
    P97.5 = quantile(tbw, probs = 0.995, na.rm = TRUE),
    test = tbw >= P2.5 & tbw <= P97.5
  ) %>%
  ungroup() %>%
  filter(!test) %>%
  group_by(weeks) %>%
  summarise(loss_data = n(), .groups = 'drop') %>% 
  ungroup()

# Total missing: 
sum(missing2$loss_data) # 27967

write.xlsx(missing2, "Data/Output/Data_exclussion_weeks_tbw-1percent.xlsx")

# Apply USA criteria 
# Init sample 2,892,471
births <- births %>%
  filter(
    case_when(
      weeks == 20 ~ tbw >= 125 & tbw <= 1250,
      weeks == 22 ~ tbw >= 125 & tbw <= 1375,
      weeks == 23 ~ tbw >= 125 & tbw <= 1500,
      weeks == 24 ~ tbw >= 125 & tbw <= 1625,
      weeks == 25 ~ tbw >= 250 & tbw <= 1750,
      weeks == 26 ~ tbw >= 250 & tbw <= 2000,
      weeks == 27 ~ tbw >= 250 & tbw <= 2250,
      weeks == 28 ~ tbw >= 250 & tbw <= 2500,
      weeks == 29 ~ tbw >= 250 & tbw <= 2750,
      weeks == 30 ~ tbw >= 375 & tbw <= 3000,
      weeks == 31 ~ tbw >= 375 & tbw <= 3250,
      weeks == 32 ~ tbw >= 500 & tbw <= 3500,
      weeks == 33 ~ tbw >= 500 & tbw <= 3750,
      weeks == 34 ~ tbw >= 500 & tbw <= 4000,
      weeks == 35 ~ tbw >= 750 & tbw <= 4500,
      weeks == 36 ~ tbw >= 750 & tbw <= 5000,
      weeks == 37 ~ tbw >= 750 & tbw <= 5500,
      weeks >= 38 ~ tbw >= 1000 & tbw <= 6000,
      TRUE ~ FALSE  
    )
  )
# End sample 2,890,674 - 1797 -> ¿245?
length(unique(births$id))

### 7. Fixed cohort bias  -----

# Floor and ceiling
# Preguntar esto!!!!!!!!!!!! Si tenemos olas de calor desde el 80, será necesario remover estos casos
births <- births %>% 
  filter(year_week1>=1992) # Loss 79400

length(unique(births$id)) # 2,811,274
# Ceiling date_ends_week_gest 2020-12-31 

dates <- births %>% 
  group_by(date_start_week_gest, date_ends_week_gest, weeks) %>% 
  summarise(n=n())

write.xlsx(dates, "Data/Output/Start_ends_gestational_weeks.xlsx")

### 8. Include weeks gest  -----
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
         date_end_week = week_gest - (7 * abs(week_gest_num - row_number()))
         ) %>% # ,(abs(week_gest_num - row_number())
  group_by(id) %>% 
  distinct(week_gest_num, .keep_all = TRUE) %>% 
  arrange(id, week_gest_num) %>% 
  ungroup() 
e <- now()
e-s # Timer 6.4 sec

### 9.  Save new births data ----
glimpse(births)
save(births, file=paste0(data_out, "births_1992_2020", ".RData"))

