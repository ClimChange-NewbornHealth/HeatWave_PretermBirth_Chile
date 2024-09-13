# Code 2: Heat Waves (HW) data preparation ----
rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Birth data ---- 

# ID file load
file <- ""

# Open data in R
load(paste0(data_inp, file)) 

# PREGUNTAR: NOVIEMBRE A ENERO, NACIDOS o que tuvieron su período gestacional o cual criterio? 