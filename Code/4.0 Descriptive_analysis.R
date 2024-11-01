# Code 4: Descriptive analysis ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/HW/"
data_out <- "Data/Output/"

## Data ---- 

# BW
bw_data_lm <- rio::import(paste0(data_out, "births_1992_2020_last_month_hw", ".RData"))
bw_data_lw <- rio::import(paste0(data_out, "births_1992_2020_last_week_hw", ".RData"))

## Descriptive analysis ---- 
glimpse(bw_data_lm)
glimpse(bw_data_lw)