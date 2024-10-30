# Code 3: Join Data ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Data ---- 

# HW 
hw_data <- rio::import(paste0(data_out, "hw_data_1980_2021", ".RData"))

# BW
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month", ".RData"))
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week", ".RData"))

## Join test ---- 

ids <- sample(bw_data_lw$id, 100000)

data_test <- bw_data_lw %>%
  filter(id %in% ids)

# Optimize with data.table
setDT(data_test)
setDT(hw_data)

# Preparar las tablas para el join por rango
data_test[, start := date_start_week]
data_test[, end := date_end_week]

# Establecer las fechas como intervalos en `hw_data` para el join
hw_data[, start := date]
hw_data[, end := date]

# Seteamos 
setkey(hw_data, name_com, start, end)
setkey(data_test, name_com, start, end)

# Time estimation 
tic()

result_data <- foverlaps(hw_data, data_test, type = "any", nomatch = 0) %>%
  .[, .(
    HW_30C_bin = as.integer(any(HW_30C > 0, na.rm = TRUE)),
    HW_p90_bin = as.integer(any(HW_p90 > 0, na.rm = TRUE)),
    HW_p95_bin = as.integer(any(HW_p95 > 0, na.rm = TRUE)),
    HW_p99_bin = as.integer(any(HW_p99 > 0, na.rm = TRUE)),
    HW_EHF_bin = as.integer(any(HW_EHF > 0, na.rm = TRUE)),
    
    HW_30C_count = sum(HW_30C, na.rm = TRUE),
    HW_p90_count = sum(HW_p90, na.rm = TRUE),
    HW_p95_count = sum(HW_p95, na.rm = TRUE),
    HW_p99_count = sum(HW_p99, na.rm = TRUE),
    HW_EHF_count = sum(HW_EHF, na.rm = TRUE)
  ), by = .(name_com, id,  date_start_week, date_end_week)]

toc() # Time by 100000 obs: 2.142 sec elapsed 1.4 min
#beepr::beep(8)

# Estimaciones 
((713461/100000)*0.234)
(((713461/100000)*0.234))*4

# Save data
save(bw_data_lm, file=paste0(data_out, "births_1992_2020_last_month_hw", ".RData"))
save(bw_data_lw, file=paste0(data_out, "births_1992_2020_last_week_hw", ".RData"))
