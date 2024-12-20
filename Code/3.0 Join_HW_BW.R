# Code 3: Find and join Data ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"
data_sovi <- "Data/Input/SOVI/"

## Data ---- 

# HW 
hw_data <- rio::import(paste0(data_out, "hw_data_1980_2021", ".RData"))

# BW
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month", ".RData"))
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week", ".RData"))

# Adjust BW with urban Santiago
com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  #select(1:2) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna")

# 33 COM
com_suburb <- c(unique(com$codigo_comuna[com$nombre_provincia=="Santiago"]), 13201) # +13201

bw_data_lm <- bw_data_lm %>% filter(com %in% com_suburb )
bw_data_lw <- bw_data_lw %>% filter(com %in% com_suburb)

# Add vulnerability data 
sovi <- rio::import(paste0(data_sovi, "sovi_datasets", ".RData")) %>% 
  select(-name_comuna)  %>% 
  rename(vulnerability=vulnerablidad) %>% 
    mutate(vulnerability = fct_recode(vulnerability,
      "Low" = "Baja",
      "Medium-low" = "Medio-baja",
      "Medium-high" = "Medio-alta"))

hw_data <- hw_data %>% left_join(sovi, by=c("com"="cod_com"))

## Join Data ---- 

# Optimize with data.table
setDT(bw_data_lm)
setDT(bw_data_lw)
setDT(hw_data)

# Join tables per range
bw_data_lm[, start := date_start_week]
bw_data_lm[, end := date_end_week]

bw_data_lw[, start := date_start_week]
bw_data_lw[, end := date_end_week]

# Interval join `hw_data` 
hw_data[, start := date]
hw_data[, end := date]

# Seteamos 
setkey(hw_data, name_com, start, end)
setkey(bw_data_lm, name_com, start, end)
setkey(bw_data_lw, name_com, start, end)

# Time estimation bw last month 
tic()

hw_data_lm <- foverlaps(hw_data, bw_data_lm, type = "any", nomatch = 0) %>%
  .[, .(
    HW_30C_2d_bin = as.integer(any(HW_30C_2d > 0, na.rm = TRUE)),
    HW_31C_2d_bin = as.integer(any(HW_31C_2d > 0, na.rm = TRUE)),
    HW_32C_2d_bin = as.integer(any(HW_32C_2d > 0, na.rm = TRUE)),
    HW_33C_2d_bin = as.integer(any(HW_33C_2d > 0, na.rm = TRUE)),
    HW_34C_2d_bin = as.integer(any(HW_34C_2d > 0, na.rm = TRUE)),
    HW_p90_2d_bin = as.integer(any(HW_p90_2d > 0, na.rm = TRUE)),
    HW_p95_2d_bin = as.integer(any(HW_p95_2d > 0, na.rm = TRUE)),
    HW_p99_2d_bin = as.integer(any(HW_p99_2d > 0, na.rm = TRUE)),
    
    HW_30C_2d_count = sum(HW_30C_2d, na.rm = TRUE),
    HW_31C_2d_count = sum(HW_31C_2d, na.rm = TRUE),
    HW_32C_2d_count = sum(HW_32C_2d, na.rm = TRUE),
    HW_33C_2d_count = sum(HW_33C_2d, na.rm = TRUE),
    HW_34C_2d_count = sum(HW_34C_2d, na.rm = TRUE),
    HW_p90_2d_count = sum(HW_p90_2d, na.rm = TRUE),
    HW_p95_2d_count = sum(HW_p95_2d, na.rm = TRUE),
    HW_p99_2d_count = sum(HW_p99_2d, na.rm = TRUE),

    HW_30C_3d_bin = as.integer(any(HW_30C_3d > 0, na.rm = TRUE)),
    HW_31C_3d_bin = as.integer(any(HW_31C_3d > 0, na.rm = TRUE)),
    HW_32C_3d_bin = as.integer(any(HW_32C_3d > 0, na.rm = TRUE)),
    HW_33C_3d_bin = as.integer(any(HW_33C_3d > 0, na.rm = TRUE)),
    HW_34C_3d_bin = as.integer(any(HW_34C_3d > 0, na.rm = TRUE)),
    HW_p90_3d_bin = as.integer(any(HW_p90_3d > 0, na.rm = TRUE)),
    HW_p95_3d_bin = as.integer(any(HW_p95_3d > 0, na.rm = TRUE)),
    HW_p99_3d_bin = as.integer(any(HW_p99_3d > 0, na.rm = TRUE)),
    
    HW_30C_3d_count = sum(HW_30C_3d, na.rm = TRUE),
    HW_31C_3d_count = sum(HW_31C_3d, na.rm = TRUE),
    HW_32C_3d_count = sum(HW_32C_3d, na.rm = TRUE),
    HW_33C_3d_count = sum(HW_33C_3d, na.rm = TRUE),
    HW_34C_3d_count = sum(HW_34C_3d, na.rm = TRUE),
    HW_p90_3d_count = sum(HW_p90_3d, na.rm = TRUE),
    HW_p95_3d_count = sum(HW_p95_3d, na.rm = TRUE),
    HW_p99_3d_count = sum(HW_p99_3d, na.rm = TRUE),

    HW_30C_4d_bin = as.integer(any(HW_30C_4d > 0, na.rm = TRUE)),
    HW_31C_4d_bin = as.integer(any(HW_31C_4d > 0, na.rm = TRUE)),
    HW_32C_4d_bin = as.integer(any(HW_32C_4d > 0, na.rm = TRUE)),
    HW_33C_4d_bin = as.integer(any(HW_33C_4d > 0, na.rm = TRUE)),
    HW_34C_4d_bin = as.integer(any(HW_34C_4d > 0, na.rm = TRUE)),
    HW_p90_4d_bin = as.integer(any(HW_p90_4d > 0, na.rm = TRUE)),
    HW_p95_4d_bin = as.integer(any(HW_p95_4d > 0, na.rm = TRUE)),
    HW_p99_4d_bin = as.integer(any(HW_p99_4d > 0, na.rm = TRUE)),
    
    HW_30C_4d_count = sum(HW_30C_4d, na.rm = TRUE),
    HW_31C_4d_count = sum(HW_31C_4d, na.rm = TRUE),
    HW_32C_4d_count = sum(HW_32C_4d, na.rm = TRUE),
    HW_33C_4d_count = sum(HW_33C_4d, na.rm = TRUE),
    HW_34C_4d_count = sum(HW_34C_4d, na.rm = TRUE),
    HW_p90_4d_count = sum(HW_p90_4d, na.rm = TRUE),
    HW_p95_4d_count = sum(HW_p95_4d, na.rm = TRUE),
    HW_p99_4d_count = sum(HW_p99_4d, na.rm = TRUE),

    HW_EHF_TAD_2d_bin = as.integer(any(HW_EHF_tad_2d > 0, na.rm = TRUE)),
    HW_EHF_TAD_3d_bin = as.integer(any(HW_EHF_tad_3d > 0, na.rm = TRUE)),
    HW_EHF_TAD_4d_bin = as.integer(any(HW_EHF_tad_4d > 0, na.rm = TRUE)),
    
    HW_EHF_TAD_2d_count = sum(HW_EHF_tad_2d, na.rm = TRUE),
    HW_EHF_TAD_3d_count = sum(HW_EHF_tad_3d, na.rm = TRUE),
    HW_EHF_TAD_4d_count = sum(HW_EHF_tad_4d, na.rm = TRUE),

    HW_EHF_TMAX_2d_bin = as.integer(any(HW_EHF_tmax_2d > 0, na.rm = TRUE)),
    HW_EHF_TMAX_3d_bin = as.integer(any(HW_EHF_tmax_3d > 0, na.rm = TRUE)),
    HW_EHF_TMAX_4d_bin = as.integer(any(HW_EHF_tmax_4d > 0, na.rm = TRUE)),
    
    HW_EHF_TMAX_2d_count = sum(HW_EHF_tad_2d, na.rm = TRUE),
    HW_EHF_TMAX_3d_count = sum(HW_EHF_tad_3d, na.rm = TRUE),
    HW_EHF_TMAX_4d_count = sum(HW_EHF_tad_4d, na.rm = TRUE),

    HW_EHF_TAD_sev_first = first(EHF_tad_sev[!is.na(EHF_tad_sev)]),
    HW_EHF_TMAX_sev_first = first(EHF_tmax_sev[!is.na(EHF_tmax_sev)]),

    P90_min_2d_bin = as.integer(any(HW_p90_min_2d > 0, na.rm = TRUE)),
    P95_min_2d_bin = as.integer(any(HW_p95_min_2d > 0, na.rm = TRUE)),
    P99_min_2d_bin = as.integer(any(HW_p99_min_2d > 0, na.rm = TRUE)),
    P90_min_3d_bin = as.integer(any(HW_p90_min_3d > 0, na.rm = TRUE)),
    P95_min_3d_bin = as.integer(any(HW_p95_min_3d > 0, na.rm = TRUE)),
    P99_min_3d_bin = as.integer(any(HW_p99_min_3d > 0, na.rm = TRUE)),
    P90_min_4d_bin = as.integer(any(HW_p90_min_4d > 0, na.rm = TRUE)),
    P95_min_4d_bin = as.integer(any(HW_p95_min_4d > 0, na.rm = TRUE)),
    P99_min_4d_bin = as.integer(any(HW_p99_min_4d > 0, na.rm = TRUE)),
    
    P90_min_2d_count = sum(HW_p90_min_2d, na.rm = TRUE),
    P95_min_2d_count = sum(HW_p95_min_2d, na.rm = TRUE),
    P99_min_2d_count = sum(HW_p99_min_2d, na.rm = TRUE),
    P90_min_3d_count = sum(HW_p90_min_3d, na.rm = TRUE),
    P95_min_3d_count = sum(HW_p95_min_3d, na.rm = TRUE),
    P99_min_3d_count = sum(HW_p99_min_3d, na.rm = TRUE),
    P90_min_4d_count = sum(HW_p90_min_4d, na.rm = TRUE),
    P95_min_4d_count = sum(HW_p95_min_4d, na.rm = TRUE),
    P99_min_4d_count = sum(HW_p99_min_4d, na.rm = TRUE)
    
  ), by = .(name_com, id,  date_start_week, date_end_week)]

toc() # Time: 385.12 sec elapsed 

# Time estimation bw last week 

tic()

hw_data_lw <- foverlaps(hw_data, bw_data_lw, type = "any", nomatch = 0) %>%
  .[, .(
    HW_30C_2d_bin = as.integer(any(HW_30C_2d > 0, na.rm = TRUE)),
    HW_31C_2d_bin = as.integer(any(HW_31C_2d > 0, na.rm = TRUE)),
    HW_32C_2d_bin = as.integer(any(HW_32C_2d > 0, na.rm = TRUE)),
    HW_33C_2d_bin = as.integer(any(HW_33C_2d > 0, na.rm = TRUE)),
    HW_34C_2d_bin = as.integer(any(HW_34C_2d > 0, na.rm = TRUE)),
    HW_p90_2d_bin = as.integer(any(HW_p90_2d > 0, na.rm = TRUE)),
    HW_p95_2d_bin = as.integer(any(HW_p95_2d > 0, na.rm = TRUE)),
    HW_p99_2d_bin = as.integer(any(HW_p99_2d > 0, na.rm = TRUE)),
    
    HW_30C_2d_count = sum(HW_30C_2d, na.rm = TRUE),
    HW_31C_2d_count = sum(HW_31C_2d, na.rm = TRUE),
    HW_32C_2d_count = sum(HW_32C_2d, na.rm = TRUE),
    HW_33C_2d_count = sum(HW_33C_2d, na.rm = TRUE),
    HW_34C_2d_count = sum(HW_34C_2d, na.rm = TRUE),
    HW_p90_2d_count = sum(HW_p90_2d, na.rm = TRUE),
    HW_p95_2d_count = sum(HW_p95_2d, na.rm = TRUE),
    HW_p99_2d_count = sum(HW_p99_2d, na.rm = TRUE),

    HW_30C_3d_bin = as.integer(any(HW_30C_3d > 0, na.rm = TRUE)),
    HW_31C_3d_bin = as.integer(any(HW_31C_3d > 0, na.rm = TRUE)),
    HW_32C_3d_bin = as.integer(any(HW_32C_3d > 0, na.rm = TRUE)),
    HW_33C_3d_bin = as.integer(any(HW_33C_3d > 0, na.rm = TRUE)),
    HW_34C_3d_bin = as.integer(any(HW_34C_3d > 0, na.rm = TRUE)),
    HW_p90_3d_bin = as.integer(any(HW_p90_3d > 0, na.rm = TRUE)),
    HW_p95_3d_bin = as.integer(any(HW_p95_3d > 0, na.rm = TRUE)),
    HW_p99_3d_bin = as.integer(any(HW_p99_3d > 0, na.rm = TRUE)),
    
    HW_30C_3d_count = sum(HW_30C_3d, na.rm = TRUE),
    HW_31C_3d_count = sum(HW_31C_3d, na.rm = TRUE),
    HW_32C_3d_count = sum(HW_32C_3d, na.rm = TRUE),
    HW_33C_3d_count = sum(HW_33C_3d, na.rm = TRUE),
    HW_34C_3d_count = sum(HW_34C_3d, na.rm = TRUE),
    HW_p90_3d_count = sum(HW_p90_3d, na.rm = TRUE),
    HW_p95_3d_count = sum(HW_p95_3d, na.rm = TRUE),
    HW_p99_3d_count = sum(HW_p99_3d, na.rm = TRUE),

    HW_30C_4d_bin = as.integer(any(HW_30C_4d > 0, na.rm = TRUE)),
    HW_31C_4d_bin = as.integer(any(HW_31C_4d > 0, na.rm = TRUE)),
    HW_32C_4d_bin = as.integer(any(HW_32C_4d > 0, na.rm = TRUE)),
    HW_33C_4d_bin = as.integer(any(HW_33C_4d > 0, na.rm = TRUE)),
    HW_34C_4d_bin = as.integer(any(HW_34C_4d > 0, na.rm = TRUE)),
    HW_p90_4d_bin = as.integer(any(HW_p90_4d > 0, na.rm = TRUE)),
    HW_p95_4d_bin = as.integer(any(HW_p95_4d > 0, na.rm = TRUE)),
    HW_p99_4d_bin = as.integer(any(HW_p99_4d > 0, na.rm = TRUE)),
    
    HW_30C_4d_count = sum(HW_30C_4d, na.rm = TRUE),
    HW_31C_4d_count = sum(HW_31C_4d, na.rm = TRUE),
    HW_32C_4d_count = sum(HW_32C_4d, na.rm = TRUE),
    HW_33C_4d_count = sum(HW_33C_4d, na.rm = TRUE),
    HW_34C_4d_count = sum(HW_34C_4d, na.rm = TRUE),
    HW_p90_4d_count = sum(HW_p90_4d, na.rm = TRUE),
    HW_p95_4d_count = sum(HW_p95_4d, na.rm = TRUE),
    HW_p99_4d_count = sum(HW_p99_4d, na.rm = TRUE),

    HW_EHF_TAD_2d_bin = as.integer(any(HW_EHF_tad_2d > 0, na.rm = TRUE)),
    HW_EHF_TAD_3d_bin = as.integer(any(HW_EHF_tad_3d > 0, na.rm = TRUE)),
    HW_EHF_TAD_4d_bin = as.integer(any(HW_EHF_tad_4d > 0, na.rm = TRUE)),
    
    HW_EHF_TAD_2d_count = sum(HW_EHF_tad_2d, na.rm = TRUE),
    HW_EHF_TAD_3d_count = sum(HW_EHF_tad_3d, na.rm = TRUE),
    HW_EHF_TAD_4d_count = sum(HW_EHF_tad_4d, na.rm = TRUE),

    HW_EHF_TMAX_2d_bin = as.integer(any(HW_EHF_tmax_2d > 0, na.rm = TRUE)),
    HW_EHF_TMAX_3d_bin = as.integer(any(HW_EHF_tmax_3d > 0, na.rm = TRUE)),
    HW_EHF_TMAX_4d_bin = as.integer(any(HW_EHF_tmax_4d > 0, na.rm = TRUE)),
    
    HW_EHF_TMAX_2d_count = sum(HW_EHF_tad_2d, na.rm = TRUE),
    HW_EHF_TMAX_3d_count = sum(HW_EHF_tad_3d, na.rm = TRUE),
    HW_EHF_TMAX_4d_count = sum(HW_EHF_tad_4d, na.rm = TRUE),

    HW_EHF_TAD_sev_first = first(EHF_tad_sev[!is.na(EHF_tad_sev)]),
    HW_EHF_TMAX_sev_first = first(EHF_tmax_sev[!is.na(EHF_tmax_sev)]),

    P90_min_2d_bin = as.integer(any(HW_p90_min_2d > 0, na.rm = TRUE)),
    P95_min_2d_bin = as.integer(any(HW_p95_min_2d > 0, na.rm = TRUE)),
    P99_min_2d_bin = as.integer(any(HW_p99_min_2d > 0, na.rm = TRUE)),
    P90_min_3d_bin = as.integer(any(HW_p90_min_3d > 0, na.rm = TRUE)),
    P95_min_3d_bin = as.integer(any(HW_p95_min_3d > 0, na.rm = TRUE)),
    P99_min_3d_bin = as.integer(any(HW_p99_min_3d > 0, na.rm = TRUE)),
    P90_min_4d_bin = as.integer(any(HW_p90_min_4d > 0, na.rm = TRUE)),
    P95_min_4d_bin = as.integer(any(HW_p95_min_4d > 0, na.rm = TRUE)),
    P99_min_4d_bin = as.integer(any(HW_p99_min_4d > 0, na.rm = TRUE)),
    
    P90_min_2d_count = sum(HW_p90_min_2d, na.rm = TRUE),
    P95_min_2d_count = sum(HW_p95_min_2d, na.rm = TRUE),
    P99_min_2d_count = sum(HW_p99_min_2d, na.rm = TRUE),
    P90_min_3d_count = sum(HW_p90_min_3d, na.rm = TRUE),
    P95_min_3d_count = sum(HW_p95_min_3d, na.rm = TRUE),
    P99_min_3d_count = sum(HW_p99_min_3d, na.rm = TRUE),
    P90_min_4d_count = sum(HW_p90_min_4d, na.rm = TRUE),
    P95_min_4d_count = sum(HW_p95_min_4d, na.rm = TRUE),
    P99_min_4d_count = sum(HW_p99_min_4d, na.rm = TRUE)
    
  ), by = .(name_com, id,  date_start_week, date_end_week)]

toc() # Time 109.775 sec elapsed

# Join data 

bw_data_lm_joined <- bw_data_lm %>%
  left_join(hw_data_lm, by=c("id", "name_com", "date_start_week", "date_end_week")) %>% 
    left_join(sovi, by=c("com"="cod_com"))
  
bw_data_lw_joined <- bw_data_lw %>% 
  left_join(hw_data_lw, by=c("id", "name_com", "date_start_week", "date_end_week")) %>% 
    left_join(sovi, by=c("com"="cod_com"))

summary(bw_data_lm_joined)
summary(bw_data_lw_joined)

# Save data
save(bw_data_lm_joined, file=paste0(data_out, "births_1992_2020_last_month_hw", ".RData"))
save(bw_data_lw_joined, file=paste0(data_out, "births_1992_2020_last_week_hw", ".RData"))
