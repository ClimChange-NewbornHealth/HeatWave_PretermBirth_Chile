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
tmin <- rio::import(paste0(data_inp, "CR2MET_tmin_v2.5_day_COM_TS_1980_2021.csv"))

# Adjust long data and time tmax
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


# Adjust long data and time tmin
metadata <- tmin[1:4, 4:ncol(tmin)] %>%
  t() %>%
  as.data.frame() %>%
  rename(com = `1`, lat = `2`, long = `3`, sup = `4`) 

temp <-  tmin[-(2:4), ] %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename("year"="x9999", 
         "month"="x9999_2",
         "day"="x9999_3") %>% 
  pivot_longer(cols = starts_with("x"), 
               names_to = "com", 
               values_to = "tmin") %>% 
  mutate(com=str_remove(com, "x"),
         com=as.numeric(com))

tmin <- temp %>% 
  left_join(metadata, by="com") %>% 
  filter(com >= 13000 & com < 14000) # 52 com ids

tmin <- tmin %>% 
    mutate(
      date = as.Date(paste(year, month, day, sep = "-")),
      year_month = format(date, "%m-%Y")
    )

glimpse(tmin)

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

tmin <- tmin %>% 
  select(com, year, month, day, tmin, tmin)

temp <- tmax %>% 
  left_join(tmin, by=c("com", "year", "month", "day"))

glimpse(temp)

rm(metadata, tmin, tmax, com)

## HW data ---- 

# Compare tempeture # 1980-01-01 - 2021-12-31
summary(temp)

ref_temp <- temp %>% 
  group_by(com) %>% 
  summarise(t30=30,
            t31=31,
            t32=32,
            t33=33,
            t34=34,
            p90_tmin=quantile(tmin, probs = 0.90, digits = 2),
            p95_tmin=quantile(tmin, probs = 0.95, digits = 2),
            p99_tmin=quantile(tmin, probs = 0.99, digits = 2),
            p90_tmax=quantile(tmax, probs = 0.90, digits = 2),
            p95_tmax=quantile(tmax, probs = 0.95, digits = 2),
            p99_tmax=quantile(tmax, probs = 0.99, digits = 2)) %>% 
  ungroup()

table_temp <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna") %>% 
  left_join(ref_temp, by=c("codigo_comuna"="com")) %>% 
  left_join(dplyr::select(temp, "com", "lat", "long", "sup"), by=c("codigo_comuna"="com"), multiple="first")

table_temp <- table_temp %>% relocate(lat, long, sup, .after=nombre_region)

writexl::write_xlsx(table_temp, "Output/Descriptives/Ref_tmin_tmax_com_1980_2021.xlsx")

# Join both tables
temp <- temp %>% 
  left_join(ref_temp, by="com")

# Apply HW definition -----
detect_HW <- function(data){
# Ordenar los datos por fecha para asegurar la secuencia
  data <- data %>% arrange(date)
  
# Crear columnas para cada criterio de ola de calor con tres días consecutivos
data <- data %>%
  mutate(
    # Day with tmax > ref
    HW_day_30C = as.integer(tmax > t30),
    HW_day_31C = as.integer(tmax > t31),
    HW_day_32C = as.integer(tmax > t32),
    HW_day_33C = as.integer(tmax > t33),
    HW_day_34C = as.integer(tmax > t34),
    HW_day_p90 = as.integer(tmax > p90_tmax),
    HW_day_p95 = as.integer(tmax > p95_tmax),
    HW_day_p99 = as.integer(tmax > p99_tmax),
    HW_day_min_p90 = as.integer(tmin > p90_tmin),
    HW_day_min_p95 = as.integer(tmin > p95_tmin),
    HW_day_min_p99 = as.integer(tmin > p99_tmin),
    
    # 2 days HW consecutive with run length encoding (RLE)
    HW_30C_2d = +(lag(HW_day_30C, 1) + HW_day_30C >= 2),
    HW_31C_2d = +(lag(HW_day_31C, 1) + HW_day_31C >= 2),
    HW_32C_2d = +(lag(HW_day_32C, 1) + HW_day_32C >= 2),
    HW_33C_2d = +(lag(HW_day_33C, 1) + HW_day_33C >= 2),
    HW_34C_2d = +(lag(HW_day_34C, 1) + HW_day_34C >= 2),
    HW_p90_2d = +(lag(HW_day_p90, 1) + HW_day_p90 >= 2),
    HW_p95_2d = +(lag(HW_day_p95, 1) + HW_day_p95 >= 2),
    HW_p99_2d = +(lag(HW_day_p99, 1) + HW_day_p99 >= 2),

    HW_p90_min_2d = +(lag(HW_day_min_p90, 1) + HW_day_min_p90 >= 2),
    HW_p95_min_2d = +(lag(HW_day_min_p95, 1) + HW_day_min_p95 >= 2),
    HW_p99_min_2d = +(lag(HW_day_min_p99, 1) + HW_day_min_p99 >= 2),

    # 3 days HW consecutive with run length encoding (RLE)
    HW_30C_3d = +(lag(HW_day_30C, 2) + lag(HW_day_30C, 1) + HW_day_30C >= 3),
    HW_31C_3d = +(lag(HW_day_31C, 2) + lag(HW_day_31C, 1) + HW_day_31C >= 3),
    HW_32C_3d = +(lag(HW_day_32C, 2) + lag(HW_day_32C, 1) + HW_day_32C >= 3),
    HW_33C_3d = +(lag(HW_day_33C, 2) + lag(HW_day_33C, 1) + HW_day_33C >= 3),
    HW_34C_3d = +(lag(HW_day_34C, 2) + lag(HW_day_34C, 1) + HW_day_34C >= 3),
    HW_p90_3d = +(lag(HW_day_p90, 2) + lag(HW_day_p90, 1) + HW_day_p90 >= 3),
    HW_p95_3d = +(lag(HW_day_p95, 2) + lag(HW_day_p95, 1) + HW_day_p95 >= 3),
    HW_p99_3d = +(lag(HW_day_p99, 2) + lag(HW_day_p99, 1) + HW_day_p99 >= 3),

    HW_p90_min_3d = +(lag(HW_day_min_p90, 2) + lag(HW_day_min_p90, 1) + HW_day_min_p90 >= 3),
    HW_p95_min_3d = +(lag(HW_day_min_p95, 2) + lag(HW_day_min_p95, 1) + HW_day_min_p95 >= 3),
    HW_p99_min_3d = +(lag(HW_day_min_p99, 2) + lag(HW_day_min_p99, 1) + HW_day_min_p99 >= 3),

    # 4 days HW consecutive with run length encoding (RLE)
    HW_30C_4d = +(lag(HW_day_30C, 3) + lag(HW_day_30C, 2) + lag(HW_day_30C, 1) + HW_day_30C >= 4),
    HW_31C_4d = +(lag(HW_day_31C, 3) + lag(HW_day_31C, 2) + lag(HW_day_31C, 1) + HW_day_31C >= 4),
    HW_32C_4d = +(lag(HW_day_32C, 3) + lag(HW_day_32C, 2) + lag(HW_day_32C, 1) + HW_day_32C >= 4),
    HW_33C_4d = +(lag(HW_day_33C, 3) + lag(HW_day_33C, 2) + lag(HW_day_33C, 1) + HW_day_33C >= 4),
    HW_34C_4d = +(lag(HW_day_34C, 3) + lag(HW_day_34C, 2) + lag(HW_day_34C, 1) + HW_day_34C >= 4),
    HW_p90_4d = +(lag(HW_day_p90, 3) + lag(HW_day_p90, 2) + lag(HW_day_p90, 1) + HW_day_p90 >= 4),
    HW_p95_4d = +(lag(HW_day_p95, 3) + lag(HW_day_p95, 2) + lag(HW_day_p95, 1) + HW_day_p95 >= 4),
    HW_p99_4d = +(lag(HW_day_p99, 3) + lag(HW_day_p99, 2) + lag(HW_day_p99, 1) + HW_day_p99 >= 4),

    HW_p90_min_4d = +(lag(HW_day_min_p90, 3) + lag(HW_day_min_p90, 2) + lag(HW_day_min_p90, 1) + HW_day_min_p90 >= 4),
    HW_p95_min_4d = +(lag(HW_day_min_p95, 3) + lag(HW_day_min_p95, 2) + lag(HW_day_min_p95, 1) + HW_day_min_p95 >= 4),
    HW_p99_min_4d = +(lag(HW_day_min_p99, 3) + lag(HW_day_min_p99, 2) + lag(HW_day_min_p99, 1) + HW_day_min_p99 >= 4)
  ) %>%
  
  # NA with begin serie
  mutate(across(contains("HW_"), ~replace_na(., 0)))

return(data)
}

hw_data <- detect_HW(temp)
summary(hw_data)

# Apply EHF definition -----

# Function to estimate EHIsigi, EHIaccli and EHF with p95 by com for n_days
EHF <- function(data, tmax_col, tmin_col, date_col, p_col, n_days = 3, period_days = 30) {
  
  # Verificar columnas
  required_cols <- c(tmax_col, tmin_col, date_col, p_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Error: Una o más columnas especificadas no existen en los datos.")
  }
  
  # Ordenar los datos por fecha para asegurar la secuencia
  data <- data %>% arrange(.data[[date_col]])
  
  # Calcular temperatura media ajustada (tad)
  data <- data %>%
    mutate(
      TAD = (get(tmax_col) + lead(get(tmin_col), default = NA)) / 2
    )
  
  # Calcular TAD_p95 por grupo y agregarlo al marco de datos original
  data <- data %>%
    group_by(com) %>%
    mutate(TAD_p95 = quantile(TAD, probs = 0.95, na.rm = TRUE)) %>%
    ungroup() # Eliminar agrupamiento para cálculos posteriores
  
  # Calcular EHIsigi, EHIaccli y EHF para `TAD` y `tmax`
  data <- data %>%
    mutate(
      # EHIsigi para TAD
      EHIsigi_tad = rowMeans(sapply(0:(n_days - 1), function(i) lag(TAD, i)), na.rm = TRUE) - TAD_p95,
      
      # EHIaccli para TAD
      EHIaccli_tad = rowMeans(sapply(0:(n_days - 1), function(i) lag(TAD, i)), na.rm = TRUE) - 
                     rollmean(TAD, period_days, align = "right", fill = NA),
      
      # EHF para TAD
      EHF_tad = EHIsigi_tad * pmax(1, EHIaccli_tad),
      
      # EHIsigi para tmax
      EHIsigi_tmax = rowMeans(sapply(0:(n_days - 1), function(i) lag(.data[[tmax_col]], i)), na.rm = TRUE) - .data[[p_col]],
      
      # EHIaccli para tmax
      EHIaccli_tmax = rowMeans(sapply(0:(n_days - 1), function(i) lag(.data[[tmax_col]], i)), na.rm = TRUE) - 
                      rollmean(.data[[tmax_col]], period_days, align = "right", fill = NA),
      
      # EHF para tmax
      EHF_tmax = EHIsigi_tmax * pmax(1, EHIaccli_tmax)
    )
  
  return(data)
}


# Iteración para `n_days`
n_days_list <- c(2, 3, 4)

for (n_days in n_days_list) {
  hw_data <- hw_data %>%
    EHF(tmax_col = "tmax", tmin_col = "tmin", date_col = "date", p_col = "p95_tmax", n_days = n_days) %>%
    
    # Calcular binarias para `tad` y `tmax`
    mutate(
      !!paste0("HW_EHF_tad_", n_days, "d") := if_else(EHF_tad > 0, 1, 0),
      !!paste0("HW_EHF_tmax_", n_days, "d") := if_else(EHF_tmax > 0, 1, 0)
    )
}

# Severity (dummy)
quantile(hw_data$EHF_tad, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), na.rm = TRUE)

EHF_p85_tad <- quantile(hw_data$EHF_tad[hw_data$EHF_tad> 0 ], p=0.85, na.rm = TRUE)  
EHF_p85_tmax <- quantile(hw_data$EHF_tad[hw_data$EHF_tmax> 0 ], p=0.85, na.rm = TRUE)  

hw_data <- hw_data %>% 
  mutate(
    EHF_tad_sev = case_when(
      (EHF_tad/EHF_p85_tad) <=0 ~ 1, 
      (EHF_tad/EHF_p85_tad) > 0 & (EHF_tad/EHF_p85_tad)<=1 ~ 2, # "Low"
      (EHF_tad/EHF_p85_tad) > 1 & (EHF_tad/EHF_p85_tad) < 3 ~ 3, # "Severe"
      (EHF_tad/EHF_p85_tad) >= 3 ~ 4 # "Extreme"
    )) %>%
  mutate(
    EHF_tmax_sev = case_when(
      (EHF_tmax/EHF_p85_tmax) <=0 ~ 1, # No HW
      (EHF_tmax/EHF_p85_tmax) > 0 & (EHF_tmax/EHF_p85_tmax)<=1 ~ 2, # "Low"
      (EHF_tmax/EHF_p85_tmax) > 1 & (EHF_tmax/EHF_p85_tmax) < 3 ~ 3, # "Severe"
      (EHF_tmax/EHF_p85_tmax) >= 3 ~ 4 # "Extreme"
    )) %>%
    mutate(
      EHF_tad_sev = factor(EHF_tad_sev, levels=c(1:4), labels=c("Non-HW", "Low", "Severe", "Extreme")),
      EHF_tmax_sev = factor(EHF_tmax_sev, levels=c(1:4), labels=c("Non-HW", "Low", "Severe", "Extreme"))
    )

save(hw_data, file=paste0(data_out, "hw_data_1980_2021", ".RData"))

