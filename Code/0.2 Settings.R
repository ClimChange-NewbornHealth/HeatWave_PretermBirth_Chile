# Settings ---- 

options(scipen=999)
options(max.print = 99999999)
options(knitr.kable.NA = '', OutDec = ".") 
knitr::opts_chunk$set("ft.shadow" = FALSE)
#rm(list=(ls()))

# Local figures text
#Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8") #LAT
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8") #USA

# Function install/load packages
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

# Apply function common packages 
install_load(c("rio", 
               "janitor", 
               "tidyverse", 
               "openxlsx",
               "chilemapas", 
               "patchwork",
               "sf", 
               "ggpubr", 
               "data.table",
               "vtable",
               "naniar", 
               "visdat", 
               "VIM",
               "rpart", 
               "rpart.plot", 
               "parallel", 
               "profvis", 
               "htmlwidgets",
               "future", 
               "purrr", 
               "furrr",
               "future.apply", 
               "zoo",
               "splines",      
               "dlnm",
               "mgcv",
               "magrittr",
               "r2symbols",
               "plotly",      
               "nlme",
               "ggstatsplot",
               "tidymodels",
               "knitr", 
               "kableExtra",
               "Epi",
               "metR", 
               "mvmeta",
               "ncdf4",
               "tidync", 
               "rix", # Reproducible environments and package
               "tictoc",
               "paletteer"
               ))
