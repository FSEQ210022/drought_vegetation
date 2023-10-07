# script para extraer valores de indicadores de sequía en las clases de landcover

library(terra)
library(fs)
library(sf)
library(purrr)
library(stringr)
library(tidyverse)

#paleta colores landcover persistencia
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land'))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

# 
pal <- read.csv('data/processed_data/paleta_colores_landcover.csv')
t <- pal$class
names(t) <- pal$Name

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') |> 
  project('EPSG:4326',method = 'mode')

dir <- '/mnt/md0/raster_procesada/ERA5-Land_tiff/sequia/zcSM/zcSM-12'
files <- dir_ls(dir,regexp = 'tif$')

index <- rast(files)
index <- resample(index,lc)

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg')

data_index <- map_df(1:5,function(i){
  index_im <- mask(index,macro[i,])
  lc_m <- mask(lc,macro[i,])
  index_df <- zonal(index_im,lc_m,na.rm = TRUE)
  names(index_df) <- c('lyr',str_extract(names(index_df[-1]),'[0-9]{4}-[0-9]{2}-[0-9]{2}'))
  index_df |> 
    pivot_longer(-1) |> 
    rename(date = name) |> 
    mutate(clase = factor(lyr,levels = t, labels = names(t)),
           macro = macro[i,]$macrozona) |> 
    select(macro,date,clase,value) 
})

write_rds(data_index,'data/processed_data/timesseries_zcSM12_macro_landcover.rds')

data_index |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland')) |> 
  mutate(date = ymd(date),
         macro = fct_relevel(macro, "norte grande", "norte chico",
                                 'zona central','zona sur'),
         clase = fct_relevel(clase, "Forest", "Cropland",
                             'Grassland','Savanna','Shrubland')) |> 
  ggplot(aes(date,value)) +
  geom_ribbon(aes(ymin = pmin(value,0),ymax = 0),fill = 'red',alpha=.7)+
  geom_ribbon(aes(ymin = 0,ymax = pmax(value,0)),fill = 'darkgreen',alpha=.7)+
  geom_smooth(se=FALSE,method ='lm',colour='grey',alpha = .5,linetype = 'dashed') +
  facet_grid(macro~clase) +
  theme_bw()
ggsave('output/figs/timeseries_zcsm12_macro_landcover.png',scale = 2)
