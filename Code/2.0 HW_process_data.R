# Code 2: Heat Waves (HW) data preparation ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

# ID file load
tmax <- rio::import(paste0(data_inp, "CR2MET_tmax_v2.5_day_COM_TS_1980_2021.csv"))

## Temp data ---- 

# Open data in R
tx <- rio::import((paste0(data_inp, file1))) # 1979 ->>

# Adjust long data and time
tx <- tx %>% pivot_longer(
  cols = !fechas, 
  names_to = "nombre_comuna",
  values_to="temp_mean" 
) %>% filter(fechas>="1990-01-01")

glimpse(tx)

# Missing values
tx <- tx %>% 
  mutate(year=year(fechas))

missing_summary <- tx %>%
  group_by(nombre_comuna, year) %>%
  summarise(
    n_missing = sum(is.na(temp_mean)),
    total = n(),
    pct_missing = (n_missing / total) * 100
  ) %>% 
  ungroup()

writexl::write_xlsx(missing_summary, "Data/Output/missing_temp.xlsx")

unique(tx$nombre_comuna[is.na(tx$temp_mean)])
unique(tx$year[is.na(tx$temp_mean)])

# Add regional codes 
com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>% 
  select(1:4)

test_com <- unique(tx$nombre_comuna)
name_com <- com$nombre_comuna

setdiff(test_com, name_com)
setdiff(name_com, test_com)

# Homologar los nombres es juntar todo en uno solo. 

## HW data ---- 
# Apply definition


# Función para detectar olas de calor (preliminar)
detectar_olas_calor <- function(data, temp_col, fecha_col, percentil90, percentil95) {
  # Ordenar los datos por fecha
  data <- data %>% arrange(!!sym(fecha_col))
  
  # Crear variables booleanas para cada criterio
  data <- data %>%
    mutate(
      ola_calor_30C = ifelse(!!sym(temp_col) > 30, 1, 0),
      ola_calor_P90 = ifelse(!!sym(temp_col) > percentil90, 1, 0),
      ola_calor_P95 = ifelse(!!sym(temp_col) > percentil95, 1, 0)
    )
  
  # Función auxiliar para detectar 3 días consecutivos de ola de calor
  detectar_consecutivos <- function(vector) {
    rle_result <- rle(vector)
    olas_calor <- rep(0, length(vector))
    for (i in seq_along(rle_result$lengths)) {
      if (rle_result$lengths[i] >= 3 & rle_result$values[i] == 1) {
        olas_calor[seq(sum(rle_result$lengths[1:(i-1)]) + 1, sum(rle_result$lengths[1:i]))] <- 1
      }
    }
    return(olas_calor)
  }
  
  # Aplicar la función de consecutivos a cada criterio
  data <- data %>%
    mutate(
      ola_calor_30C_consecutiva = detectar_consecutivos(ola_calor_30C),
      ola_calor_P90_consecutiva = detectar_consecutivos(ola_calor_P90),
      ola_calor_P95_consecutiva = detectar_consecutivos(ola_calor_P95)
    )
  
  return(data)
}

# Ejemplo de uso
# Asumiendo que tienes un dataframe 'datos_clima' con una columna 'temp_max' y 'fecha'
# Y que ya has calculado los percentiles 90 y 95 para la temperatura máxima (por ejemplo, 32°C y 34°C)
percentil90 <- 32
percentil95 <- 34

resultado <- detectar_olas_calor(datos_clima, "temp_max", "fecha", percentil90, percentil95)

# Visualizar el resultado
head(resultado)


# Función para calcular el EHF
calcular_ehf <- function(data, temp_col, fecha_col, percentil95, period_days = 30) {
  
  # Ordenar los datos por fecha
  data <- data %>% arrange(!!sym(fecha_col))
  
  # Calcular el EHIsigi
  data <- data %>%
    mutate(
      EHIsigi = (lag(!!sym(temp_col), 0) + lag(!!sym(temp_col), 1) + lag(!!sym(temp_col), 2)) / 3 - percentil95
    )
  
  # Calcular el EHIaccli
  data <- data %>%
    mutate(
      EHIaccli = (lag(!!sym(temp_col), 0) + lag(!!sym(temp_col), 1) + lag(!!sym(temp_col), 2)) / 3 -
                 rollmean(!!sym(temp_col), period_days, align = "right", fill = NA)
    )
  
  # Calcular el EHF
  data <- data %>%
    mutate(
      EHF = EHIsigi * pmax(1, EHIaccli),
      ola_calor = ifelse(EHF > 0, 1, 0)
    )
  
  return(data)
}




