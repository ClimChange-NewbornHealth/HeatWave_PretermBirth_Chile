rm(list = ls())
library(tidyverse)
library(rio)
library(janitor)
library(zoo)
library(writexl)

# Load data births ----
file <- "BIRTHS/data_1992_2020.RData"
file2 <- "BIRTHS/nacim2019.xlsx"
file3 <- "BIRTHS/nacim2020.xlsx"

# Open and append data ----
load(file) 
births <- data_1992_2020 |> janitor::clean_names(); rm(data_1992_2020)

births19 <- rio::import(file2, sheet="R") %>% janitor::clean_names() 
births20 <- rio::import(file3, sheet="R") %>% janitor::clean_names()

births <- births |> filter(!ano_nac %in% c(2019, 2020))

nrow(births) + nrow(births19) + nrow(births20) # 7084698

births <- births |> 
  bind_rows(births19) |> 
    bind_rows(births20)

table(births$ano_nac)
glimpse(births)
rm(births19)
rm(births20)

# Municipality data ----
comunas <- chilemapas::codigos_territoriales |> 
  mutate(codigo_comuna=as.numeric(codigo_comuna))

# Prepare data ----
births <- births |> 
  mutate(date_nac = make_date(year = ano_nac, month = mes_nac, day = dia_nac))  |> 
  mutate(tbw=if_else(peso==9999, NA_real_, peso),
        weeks=if_else(semanas==99, NA_real_, semanas)) |> 
  drop_na(date_nac) |>     
  drop_na(weeks) |> 
  drop_na(comuna) |>  
  mutate(id=1:n()) |> 
  mutate(date_start = date_nac - weeks(semanas-1),
  date_end = date_nac) |> 
  mutate(
    size=if_else(talla==99, NA_real_, talla),
    age_mom=if_else(edad_madre==99, NA_real_, edad_madre),
    educ_mom=if_else(nivel_madre==9, NA_real_, nivel_madre),
    job_mom=if_else(activ_madre %in% c(9), NA_real_, activ_madre+1),
    age_dad=if_else(edad_padre==99, NA_real_, edad_padre),
    educ_dad=if_else(nivel_padre==9, NA_real_, nivel_padre),
    job_dad=if_else(activ_padre %in% c(3,9), NA_real_, activ_padre+1)
    ) |> 
  filter(age_mom>=12 & age_mom<=50) |> 
  filter(weeks >= 28) |> 
  filter(tipo_parto==1) |> 
  mutate(sex=factor(sexo, levels=c(1,2), labels=c("Boy", "Girl"))) |> 
  left_join(comunas, by=c("comuna"="codigo_comuna")) |> 
  mutate(date_week1 = date_start, 
         year_week1 = year(date_start), 
         month_week1 = month(date_start),
         ym_week1 = as.yearmon(date_start, "%M-%Y")) |> 
  rename(day_nac=dia_nac,
         month_nac=mes_nac,
         year_nac=ano_nac,
         com=comuna, 
         name_com=nombre_comuna, 
         reg=codigo_region,
         name_reg=nombre_region, 
         date_start_week_gest=date_start,
         date_ends_week_gest=date_end
         ) |> 
  mutate(
    age_group_mom=case_when(
      age_mom <= 20 ~ 1, 
      age_mom > 20 & age_mom <= 29 ~ 2,
      age_mom >= 30 & age_mom <= 39 ~ 3,
      age_mom >= 40 & age_mom <= 49 ~ 4,
      age_mom >= 50 ~ 5, 
      TRUE ~ NA_real_
    ),
    age_group_mom=factor(age_group_mom, 
                         levels=c(1:5), 
                         labels=c("<=20", "20-29", "30-39", "40-49", ">=50")),
    educ_group_mom = case_when(
      educ_mom == 1 ~ 4, # College
      educ_mom == 2 ~ 3, # Secondary
      educ_mom == 3 ~ 3, # Secondary
      educ_mom == 4 ~ 2, # Primary
      educ_mom == 5 ~ 1, # No educaction 
      TRUE ~ NA_real_, #Unknow
    ), 
    educ_group_mom = factor(educ_group_mom, 
                            levels = c(1:4), 
                            labels = c("Non education", "Primary", "Secondary", "College")),
    #job_group_mom = if_else(is.na(job_mom), 4, job_mom), 
    job_group_mom = if_else(job_mom==3, 1, job_mom),
    job_group_mom = factor(job_group_mom, levels = c(1,2), labels=c("Not working", "Employed"))
  ) |> 
  relocate(age_group_mom, educ_group_mom, job_group_mom, .after=job_mom) |> 
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
    job_group_dad = if_else(job_dad==3, 1, job_dad),
    job_group_dad = if_else(is.na(job_group_dad), 3, job_group_dad), 
    job_group_dad = factor(job_group_dad, levels = c(1,2,3), labels=c("Not working", "Employed", "Unknown"))
  ) |> 
  relocate(age_group_dad, educ_group_dad, job_group_dad, .after=job_dad) |> 
  select(id, 
         com, name_com, reg, name_reg, 
         weeks,
         # Other variables 
         date_nac, day_nac, month_nac, year_nac, 
         date_week1, year_week1, month_week1, ym_week1,
         date_start_week_gest, date_ends_week_gest,
         sex, tbw, size, 
         age_group_mom,educ_group_mom,job_group_mom,
         age_group_dad,educ_group_dad,job_group_dad,
) |>
 filter(
    case_when(
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

# Outcomes 
births <- births %>% 
  mutate(birth_preterm = if_else(weeks < 37, 1, 0)) %>%
  #mutate(birth_extremely_preterm = if_else(weeks < 28, 1, 0)) %>% 
  mutate(birth_very_preterm = if_else(weeks >= 28 & weeks <32, 1, 0)) %>% 
  mutate(birth_moderately_preterm = if_else(weeks >= 32 & weeks <33, 1, 0)) %>% 
  mutate(birth_late_preterm = if_else(weeks >= 34 & weeks <37, 1, 0)) %>% 
  mutate(birth_term = if_else(weeks >= 37 & weeks <42, 1, 0)) %>% 
  mutate(birth_posterm = if_else(weeks >= 42, 1, 0))

births_V <- births |> filter(reg=="05")

# Save births data ----

save(births, file=paste0("BIRTHS/births_1992_2020", ".RData"))
save(births_V, file=paste0("BIRTHS/births_1992_2020_Region_V", ".RData"))

# Codebook -----

codebook <- tibble(
  Variable = c(
    "id", "com", "name_com", "reg", "name_reg", "weeks", "date_nac", 
    "day_nac", "month_nac", "year_nac", "date_week1", "year_week1", 
    "month_week1", "ym_week1", "date_start_week_gest", "date_ends_week_gest", 
    "sex", "tbw", "size", "age_group_mom", "educ_group_mom", "job_group_mom", 
    "age_group_dad", "educ_group_dad", "job_group_dad", "birth_preterm", 
    "birth_very_preterm", "birth_moderately_preterm", "birth_late_preterm", 
    "birth_term", "birth_posterm"
  ),
  Description = c(
    "Identificador único del nacimiento",
    "Código de la comuna",
    "Nombre de la comuna",
    "Código de la región",
    "Nombre de la región",
    "Número de semanas de gestación",
    "Fecha de nacimiento",
    "Día de nacimiento",
    "Mes de nacimiento",
    "Año de nacimiento",
    "Fecha de la primera semana de gestación",
    "Año de la primera semana de gestación",
    "Mes de la primera semana de gestación",
    "Año-mes de la primera semana de gestación",
    "Fecha de inicio de la última semana gestacional",
    "Fecha de término de la última semana gestacional",
    "Sexo del recién nacido",
    "Peso al nacer (gramos)",
    "Tamaño del recién nacido (cm)",
    "Grupo de edad de la madre",
    "Nivel educativo de la madre",
    "Situación laboral de la madre",
    "Grupo de edad del padre",
    "Nivel educativo del padre",
    "Situación laboral del padre",
    "Nacimiento pretérmino (0=No, 1=Sí)",
    "Nacimiento muy pretérmino (0=No, 1=Sí)",
    "Nacimiento moderadamente pretérmino (0=No, 1=Sí)",
    "Nacimiento tardío pretérmino (0=No, 1=Sí)",
    "Nacimiento a término (0=No, 1=Sí)",
    "Nacimiento postérmino (0=No, 1=Sí)"
  )
)

writexl::write_xlsx(codebook, "BIRTHS/Libro_codigos_data_nacimientos.xlsx")
