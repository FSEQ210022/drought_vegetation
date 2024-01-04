# script para extraer valores de indicadores de sequía en las clases de landcover

library(terra)
library(fs)
library(sf)
library(purrr)
library(stringr)
library(tidyverse)
library(glue)

#paleta colores landcover persistencia
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land'))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

# 
pal <- read.csv('data/processed_data/paleta_colores_landcover.csv')
t <- pal$class
names(t) <- pal$Name

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') 
  # project('EPSG:4326',method = 'mode') #for ERA5-Land

dir <- '/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/zcNDVI-6'
files <- dir_ls(dir,regexp = 'tif$')

index <- rast(files)
index <- resample(index,lc)

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> st_transform(32719)

data_index <- map_df(1:5,function(i){
  index_im <- mask(index,macro[i,])
  lc_m <- mask(lc,macro[i,])
  index_df <- zonal(index_im,lc_m,na.rm = TRUE)
  names(index_df) <- c('lyr',str_extract(names(index_df[-1]),'[0-9]{4}-[0-9]{2}-[0-9]{2}'))
  d <- index_df |> 
    pivot_longer(-1) |> 
    rename(date = name) |> 
    mutate(clase = factor(lyr,levels = t, labels = names(t)),
           macro = macro[i,]$macrozona) |> 
    select(macro,date,clase,value) 
  write_rds(d,glue('data/processed_data/timesseries_zcNDVI6_macro_landcover{i}.rds'))
  
})

