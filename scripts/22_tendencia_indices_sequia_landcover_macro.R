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

indices <- c('EDDI','SPI','SPEI','zcSM')
scales <- c(1,3,6,12,24,36)
scales_ndvi <- c(1,3,6,12)

data_t <- map_df(indices,\(index){
  if( index == 'zcNDVI') scales <- scales_ndvi
  
  trends <- map(scales,\(scale){
    
    if (index == 'zcNDVI'){
      dir <- glue::glue('/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/{index}-{scale}')
    } else dir <- glue::glue('/mnt/md0/raster_procesada/ERA5-Land_tiff/sequia/{index}/{index}-{scale}')
    
    #files <- dir_ls(dir,regexp = paste(2001:2023,collapse='|'))
    files <- dir_ls(dir,regexp = 'tif$')
    index_ts <- rast(files)
    #cores <- parallel::detectCores()-5

    trend <- app(index_ts,\(y){
      df <- data.frame(x = seq_along(y),y=y)
      if(!(is.na(df[,2]) |> all())){
        summary(lm(y~x,df))$coefficients[2,1]
      } else  NA
      })
    return(trend)
  })
  
  trends <- rast(trends)
  names_ind <- if(index == 'zcNDVI') paste0(index,'-',c(1,3,6,12)) else paste0(index,'-',c(1,3,6,12,24,36))
  names(trends) <- names_ind

  if (index != 'zcNDVI') {
    lc <- project(lc,"EPSG:4326",method = 'mode')
    macro <- st_transform(macro,4326)
  }
    
  trends <- resample(trends,lc)
  
  data_trend <- map_df(1:5,function(i){
    trends_m <- mask(trends,macro[i,])
    lc_m <- mask(lc,macro[i,])
    trends_df <- zonal(trends_m,lc_m,'median',na.rm = TRUE)
    #names(trends_df)[2:7] <- paste0(index,'-',names_ind)
    trends_df |> 
      pivot_longer(-1) |> 
      rename(scale = name) |> 
      mutate(clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name),
             macro = macro[i,]$macrozona) |> 
      select(macro,scale,clase,value) 
  })
  return(data_trend)
})

# data_t |> 
#   pivot_wider(names_from = scale)
#   write_rds('data/processed_data/trends_indices_varios_macroXlandocver_2001_2023.rds')


