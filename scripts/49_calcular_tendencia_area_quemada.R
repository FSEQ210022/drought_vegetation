library(terra)
library(fs)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(trend)

# datos almacenados en el computador de escritorio
dir <- '/media/francisco/data_raw/Otros/incendios_monthly'
files <- dir_ls(dir,regexp = 'nc$')

chl <- ne_countries(country = 'chile',scale = 'medium',returnclass = 'sf')
chl_cont <- st_as_sf(chl) |> 
  st_cast('POLYGON') |> 
  slice(-(1:2)) 


arque_anual <- seq_along(files) |> 
  map(\(i){
    arque <- rast(dir_ls(dir)[i]) 

    arque['burned_area'] |> 
      crop(vect(chl_cont)) |>
      mask(vect(chl_cont)) |> 
      sum(na.rm = TRUE) 
  })

arque_anual <- rast(arque_anual)
names(arque_anual) <- 2002:2022
plot(arque_anual)

#Calcular suma de area quemada de todos los años

arque_anual_sum <- app(arque_anual,'sum',na.rm = TRUE)
writeRaster(arque_anual_sum,'~/Documentos/suma_area_quemada-2002-2023.tif')


#Calcular la tendencia de el área quemada entre 2002 y 2022

source('R/trend_func.R')

library(modifiedmk)
nc <- parallel::detectCores()
cl <- snow::makeCluster(nc-2,"SOCK")
snow::clusterExport(cl, list = c('trend_func2','mmky'))
trend <- app(arque_anual,trend_func2,cores = cl)
snow::stopCluster(cl)

writeRaster(trend,'~/Documentos/trend_area_quemada_2002-2022.tif')
