# Code 2: Heat Waves (HW) data preparation ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Temp data ---- 

tmax <- rio::import(paste0(data_inp, "CR2MET_tmax_v2.5_day_COM_TS_1980_2021.csv"))

# Adjust long data and time
metadata <- tmax[1:4, 4:ncol(tmax)] %>%
  t() %>%
  as.data.frame() %>%
  rename(com = `1`, lat = `2`, long = `3`, sup = `4`) 

temp <-  tmax[-(2:4), ] %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename("year"="x9999", 
         "month"="x9999_2",
         "day"="x9999_3") %>% 
  pivot_longer(cols = starts_with("x"), 
               names_to = "com", 
               values_to = "tmax") %>% 
  mutate(com=str_remove(com, "x"),
         com=as.numeric(com))

tmax <- temp %>% 
  left_join(metadata, by="com") %>% 
  filter(com >= 13000 & com < 14000) # 52 com ids

tmax <- tmax %>%
    mutate(
      date = as.Date(paste(year, month, day, sep = "-")),
      year_month = format(date, "%m-%Y")
    )

glimpse(tmax)

# Add regional codes 
com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  select(1:2) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna")

tmax <- tmax %>% 
  left_join(com, by=c("com"="codigo_comuna"))

tmax <- tmax %>% 
  relocate(com, name_com, lat, long, sup, date, year_month)

rm(metadata, temp, com)

## HW data ---- 

# Compare tempeture # 1980-01-01 - 2021-12-31
summary(tmax)

ref_tmax <- tmax %>% 
  group_by(com) %>% 
  summarise(t30=30,
            p90=quantile(tmax, probs = 0.90, digits = 2),
            p95=quantile(tmax, probs = 0.95, digits = 2),
            p99=quantile(tmax, probs = 0.99, digits = 2)) %>% 
  ungroup()

table_tmax <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna") %>% 
  left_join(ref_tmax, by=c("codigo_comuna"="com"))

writexl::write_xlsx(table_tmax, "Output/Descriptives/Ref_tmax_com_1980_2021.xlsx")

# Join both tables
tmax <- tmax %>% 
  left_join(ref_tmax, by="com")

# Apply HW definition -----
detect_HW <- function(data){
# Ordenar los datos por fecha para asegurar la secuencia
  data <- data %>% arrange(date)
  
# Crear columnas para cada criterio de ola de calor con tres días consecutivos
data <- data %>%
  mutate(
    # Day with tmax > ref
    HW_day_30C = as.integer(tmax > t30),
    HW_day_p90 = as.integer(tmax > p90),
    HW_day_p95 = as.integer(tmax > p95),
    HW_day_p99 = as.integer(tmax > p99),
    
    # 2 days HW consecutive with run length encoding (RLE)
    HW_30C_2d = +(lag(HW_day_30C, 1) + HW_day_30C >= 2),
    HW_p90_2d = +(lag(HW_day_p90, 1) + HW_day_p90 >= 2),
    HW_p95_2d = +(lag(HW_day_p95, 1) + HW_day_p95 >= 2),
    HW_p99_2d = +(lag(HW_day_p99, 1) + HW_day_p99 >= 2),

    # 3 days HW consecutive with run length encoding (RLE)
    HW_30C_3d = +(lag(HW_day_30C, 2) + lag(HW_day_30C, 1) + HW_day_30C >= 3),
    HW_p90_3d = +(lag(HW_day_p90, 2) + lag(HW_day_p90, 1) + HW_day_p90 >= 3),
    HW_p95_3d = +(lag(HW_day_p95, 2) + lag(HW_day_p95, 1) + HW_day_p95 >= 3),
    HW_p99_3d = +(lag(HW_day_p99, 2) + lag(HW_day_p99, 1) + HW_day_p99 >= 3),

    # 4 days HW consecutive with run length encoding (RLE)
    HW_30C_4d = +(lag(HW_day_30C, 3) + lag(HW_day_30C, 2) + lag(HW_day_30C, 1) + HW_day_30C >= 4),
    HW_p90_4d = +(lag(HW_day_p90, 3) + lag(HW_day_p90, 2) + lag(HW_day_p90, 1) + HW_day_p90 >= 4),
    HW_p95_4d = +(lag(HW_day_p95, 3) + lag(HW_day_p95, 2) + lag(HW_day_p95, 1) + HW_day_p95 >= 4),
    HW_p99_4d = +(lag(HW_day_p99, 3) + lag(HW_day_p99, 2) + lag(HW_day_p99, 1) + HW_day_p99 >= 4)
  ) %>%
  
  # NA with begin serie
  mutate(across(contains("HW_"), ~replace_na(., 0)))

return(data)


}

hw_data <- detect_HW(tmax)
summary(hw_data)

# Apply EHF definition -----

# Funtion to estimate EHIsigi, EHIaccli and EHF with p95 by com 
EHF <- function(data, temp_col, date_col, p95_col, period_days = 30) {
  
  # Verificar si las columnas existen en el conjunto de datos
  required_cols <- c(temp_col, date_col, p95_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Error: Una o más columnas especificadas no existen en los datos.")
  }
  
  # Ordenar los datos por fecha para asegurar la secuencia
  data <- data %>% arrange(.data[[date_col]])
  
  # Calcular EHIsigi, EHIaccli y EHF en un solo paso
  data <- data %>%
    mutate(
      # EHIsigi: promedio de los últimos tres días menos el percentil 95 de cada fila
      EHIsigi = (lag(.data[[temp_col]], 0) + lag(.data[[temp_col]], 1) + lag(.data[[temp_col]], 2)) / 3 - .data[[p95_col]],
      
      # EHIaccli: promedio de los últimos tres días menos el promedio de los 30 días anteriores
      EHIaccli = (lag(.data[[temp_col]], 0) + lag(.data[[temp_col]], 1) + lag(.data[[temp_col]], 2)) / 3 - 
                 rollmean(.data[[temp_col]], period_days, align = "right", fill = NA),
      
      # EHF: producto de EHIsigi y el máximo entre 1 y EHIaccli
      EHF = EHIsigi * pmax(1, EHIaccli)
    )
  
  return(data)
}

hw_data <- EHF(hw_data, "tmax", "date", "p95")

hw_data <- hw_data %>% 
  mutate(HW_EHF=if_else(EHF > 0, 1, 0))

summary(hw_data)

save(hw_data, file=paste0(data_out, "hw_data_1980_2021", ".RData"))
