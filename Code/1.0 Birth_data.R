# Code 1: Birth data preparation ----

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
glimpse(births) # >7084698 obs 

# Prepare data
### 1. Date 2011-2020 -----
table(births$ano_nac)

### 2. Construction births date----
births <- births %>% 
  mutate(date_nac = make_date(year = ano_nac, month = mes_nac, day = dia_nac)) # Year, Month, Date 

glimpse(births)

### 3. Create week_gest obs ----

start_count <- nrow(births) 

s <- now()

births <- births %>%
  mutate(tbw=if_else(peso==9999, NA_real_, peso),
        weeks=if_else(semanas==99, NA_real_, semanas)) %>% 
  drop_na(date_nac) %>% 
  {
    cat("Observaciones después de drop_na(date_nac):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA: 64
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } %>%     
  drop_na(weeks) %>%  
  {
    cat("Observaciones después de drop_na(semanas):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA 13791
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
e-s # Timer 13 sec

# Check results

t1 <- births %>%
  group_by(weeks) %>% 
  summarise(min_tbw=min(tbw, na.rm = TRUE), 
            mean_tbw=mean(tbw, na.rm = TRUE), 
            median_tbw=median(tbw, na.rm = TRUE), 
            max_tbw=max(tbw, na.rm = TRUE),  
            n_births=n())

write.xlsx(t1, "Data/Output/Data_semanas_peso.xlsx")

### 4. Prepare births variables ----

comunas <- chilemapas::codigos_territoriales
comunas <- comunas |> 
  mutate(codigo_comuna=as.numeric(codigo_comuna))

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
  select(id, tbw, weeks,
         # Other variables 
         date_nac, day_nac, month_nac, year_nac, 
         date_week1, year_week1, month_week1, ym_week1,
         sex, size,
         age_mom,educ_mom,job_mom,
         age_dad,educ_dad,job_dad,
         reg, name_reg, com, name_comuna
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
  relocate(age_group_dad, educ_group_dad, job_group_dad, .after=job_dad)

### 5. Outcome: preterm (7,070,663) ---- 
births <- births |> 
  mutate(birth_preterm = if_else(weeks < 37, 1, 0)) |> 
  mutate(birth_early = if_else(weeks >= 37 & weeks <=38, 1, 0)) |> 
  mutate(birth_extrem_preterm = if_else(weeks < 28, 1, 0)) |> 
  mutate(birth_very_preterm = if_else(weeks >= 28 & weeks <32, 1, 0)) |> 
  mutate(birth_moderate_preterm = if_else(weeks >= 32 & weeks <37, 1, 0))

### 6. Exclusion criteria (7,070,663) -----


## 7.  Save new births data ----
save(births, file=paste0(data_out, "births_1992_2020", ".RData"))

