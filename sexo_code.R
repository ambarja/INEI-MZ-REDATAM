library(tidyverse)
library(sf)  
library(magrittr)
source("https://raw.githubusercontent.com/ambarja/utilsR/main/ubicode.R")
source("https://raw.githubusercontent.com/ambarja/utilsR/main/remove_accent_enie.R")

# 1. Preprocessing data ------------------------------------------------
lista_excels <- list.files(".", ".xlsx$")
names_seguro <- c("codigo","mz","hombre","mujer")

clean_xlsx <- function(x){
  data <- readxl::read_xlsx(x) %>% 
    `[`(-c(1:5),-1) %>% 
    set_colnames(names_seguro) %>% 
    mutate(
      codigo = str_extract(mz,pattern = "\\w+")
    ) %>% 
    drop_na(codigo) %>% 
    mutate(codigo = lapply(codigo,ubicode_mz) %>% unlist())
  return(data)
}

lima <- lapply(lista_excels,FUN = clean_xlsx)

final_data <- map_df(lima,.f = as.data.frame) %>% 
  drop_na(codigo,mz) 

final_data <- final_data %>% 
  mutate_all(~replace(., is.na(.), 0))

mz_shp <- st_read(
  "C:/Users/anton/Downloads/LimaDB_Salurbal.gpkg"
) %>% 
  mutate(codigo = paste0(IDCCPP,CODZONA,SUFZONA,CODMZNA,SUFMZNA))

# Final dataset 

edad <- left_join(mz_shp,final_data,"codigo")
orden_names <- edad %>% 
  select(IDMANZANA:DISTRITO,codigo,vivienda:poblacion,edad_0to4:edad_95tomás,
         casa:v5gl02_edited_pm2.5_mean,hombre,mujer,geom)

write_sf(orden_names,"LimaDB_Salurbal.gpkg")
