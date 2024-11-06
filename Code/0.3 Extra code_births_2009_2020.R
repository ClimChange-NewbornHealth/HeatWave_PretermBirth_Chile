# Preparación de datos nacimiento - contaminación

births  <- rio::import("Data/Output/births_1992_2020.RData")

births  <- births  %>% select(id, com, name_com, weeks, 
  date_nac, year_nac, date_start_week_gest, date_ends_week_gest) %>% 
  filter(year_nac>=2009)

com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  #select(1:2) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna")

com_suburb <- c(unique(com$codigo_comuna[com$nombre_provincia=="Santiago"]), 13201)

births  <- births  %>% filter(com %in% com_suburb)

save(births, file=paste0("Data/Output/", "births_2009_2020_contaminacion", ".RData"))
write.table(births, file=paste0("Data/Output/", "births_2009_2020_contaminacion", ".txt"), 
            sep=",")