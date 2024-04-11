# November 2023
# by frzambra

dir <- '/mnt/md0/raster_procesada/MODIS/'

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

library(terra)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(fs)
library(stars)

chl <- ne_countries(country = 'chile',returnclass = 'sf') |> 
  st_transform(32719)

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  st_transform(32719) |> 
  mutate(macrozona = c('norte chico','norte grande','austral','centro','sur'))

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') 

lc <- project(lc,"EPSG:4326",method = 'mode')
macro <- st_transform(macro,4326)

indices <- c('EDDI','SPI','SPEI','zcSM')
scales <- c(1,3,6,12,24,36)
scales_ndvi <- c(1,3,6,12)

data_index <- map_df(indices,\(index){
  if( index == 'zcNDVI') scales <- scales_ndvi
  
  trends <- map(scales,\(scale){
    
    if (index == 'zcNDVI'){
      dir <- glue::glue('/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/{index}-{scale}')
    } else dir <- glue::glue('/mnt/md0/raster_procesada/ERA5-Land_tiff/sequia/{index}/{index}-{scale}')
    
    files <- dir_ls(dir,regexp = 'tif$')
    index_ts <- rast(files)
    index_ts_r <- resample(index_ts,lc,threads = 50)
    
  
  data_ts <- map_df(1:5,function(i){
    index_m <- mask(index_ts_r,macro[i,])
    lc_m <- mask(lc,macro[i,])
    index_df <- zonal(index_m,lc_m,'median',na.rm = TRUE)
    
    trends_df |> 
      pivot_longer(-1) |> 
      rename(scale = name) |> 
      mutate(clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name),
             macro = macro[i,]$macrozona) |> 
      select(macro,scale,clase,value) 
  })
  return(data_trend)
})
})

# data_t |> 
#   pivot_wider(names_from = scale)
#   write_rds('data/processed_data/trends_indices_varios_macroXlandocver_2001_2023.rds')


