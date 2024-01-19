library(sf)
library(terra)
library(fs)
library(tidyverse)

dir <- '/mnt/data_procesada/data/rasters/Procesados/koppen_geiger_tif/'

#/1991_2020'

#/2041_2070/ssp585'

file1 <- paste0(dir,'1991_2020/koppen_geiger_0p01.tif')
file2 <- paste0(dir,'2071_2099/ssp585/koppen_geiger_0p01.tif')

kp <- c(rast(file1),rast(file2))

chile <- read_sf('data/processed_data/spatial/chile_continental.gpkg')

kp_chile <- crop(kp,chile)
kp_chile <- mask(kp,chile)
kp_chile <- trim(kp_chile)
#writeRaster(kp_chile,'data/processed_data/spatial/koppen_geiger_1991_2020.tif')
library(tmap)

rgb <- coltab(kp_chile)[[1]][2:31,]
paleta_kg <- read_csv2('data/processed_data/legend_koppen-geiger.csv')
labs <- paleta_kg$type
names(labs) <- 1:30
colores <- rgb(rgb[,2],rgb[,3],rgb[,4],rgb[,5],maxColor = 255)
names(colores) <- 1:30

names(kp_chile) <- c('1991-2020','2071-2099 ssp585')

map <- tm_shape(kp_chile) +
  tm_raster(palette = colores,
            style = 'cat',
            labels = labs,
            title = 'Climate (1991-2020)') +
  tm_layout(legend.outside = TRUE)
tmap_save(map,'output/figs/mapa_koppen_presente_futuro.png',scale=1.2)
