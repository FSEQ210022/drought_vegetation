library(terra)
library(fs)
library(tidyverse)
library(rnaturalearth)
library(sf)

#descarga los límites de chile
chl <- ne_countries(country = 'chile',scale = 'medium',returnclass = 'sf')

#extrae las islas para dejar las geometrías de Chile continetal
chl_cont <- st_as_sf(chl) |> 
  st_cast('POLYGON') |> 
  slice(-(1:2)) 

dir <- '/media/francisco/data_raw/Otros/VNP46A4'
files <- dir_ls(dir,regexp = 'tif$')

out <- files |> 
  map(\(file){
    rast(file) |> 
      crop(vect(chl_cont)) |> 
      mask(vect(chl_cont))
  })
    
nbm_nl <- rast(out)

# Calcular el promedio de todos los años

nbm_nl_mean <- app(nbm_nl,'mean',na.rm = TRUE)
writeRaster(nbm_nl_mean,'~/Documentos/promedio_luces_nocturnas_2012-2023.tif')

#Calcular la tendencia de las luces nocturnas entre 2012 a 2023

source('R/trend_func.R')

library(modifiedmk)

trend <- app(nbm_nl,trend_func2)

writeRaster(trend,'~/Documentos/trend_nasa_marble_luces_nocturnas_2012-2023.tif')
