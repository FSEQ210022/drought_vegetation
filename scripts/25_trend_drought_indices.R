library(tidyverse)
library(terra)
library(tmap)
library(sf)
library(rnaturalearth)

chl_b <- ne_countries(country='chile',scale = 'medium',returnclass = 'sf')
macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') 

trend_spi <- rast('/mnt/md0/raster_procesada/analysis/trends/trend_SPI_1981-2023.tif')
#names(trend_spei) <- paste0('SPEI-',c(1,3,6,12,24,36))
#writeRaster(trend_spei,'/mnt/md0/raster_procesada/analysis/trends/trend_SPEI_1981-2023_2.tif',overwrite = TRUE)
names(trend_spi) <- paste0('SPI-',c(1,3,6,12,24,36))
map_spi <- tm_shape(trend_spi*100) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend SPI (x 100)',style = 'kmeans') +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('SPI-',c(1,3,6,12,24,36)))
tmap_save(map_spi,'output/figs/trend_raster_SPI_1981-2023.png',asp=.2)

trend_spei <- rast('/mnt/md0/raster_procesada/analysis/trends/trend_SPEI_1981-2023.tif')
#names(trend_spei) <- paste0('SPEI-',c(1,3,6,12,24,36))
#writeRaster(trend_spei,'/mnt/md0/raster_procesada/analysis/trends/trend_SPEI_1981-2023_2.tif',overwrite = TRUE)

map_spei <- tm_shape(trend_spei*100) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend SPEI (x 100)',style = 'kmeans') +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('SPEI-',c(1,3,6,12,24,36)))
tmap_save(map_spei,'output/figs/trend_raster_SPEI_1981-2023.png',asp=.2)

trend_EDDI <- rast('/mnt/md0/raster_procesada/analysis/trends/trend_EDDI_1981-2023.tif')
# names(trend_EDDI) <- paste0('zcNDVI-',c(1,3,6,12,24,36))
# writeRaster(trend_EDDI,'/mnt/md0/raster_procesada/analysis/trends/trend_EDDI_1981-2023_2.tif',overwrite = TRUE)

map_EDDI <- tm_shape(trend_EDDI*100) + 
  tm_raster(palette = 'inferno',midpoint = 0,title = 'Trend EDDI (x 100)',style = 'kmeans') +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('EDDI-',c(1,3,6,12,24,36)))
tmap_save(map_EDDI,'output/figs/trend_raster_EDDI_1981-2023.png',asp=.2)

trend_zcSM <- rast('/mnt/md0/raster_procesada/analysis/trends/trend_zcSM_1981-2023.tif')
# names(trend_zcSM) <- paste0('zcNDVI-',c(1,3,6,12,24,36))
# writeRaster(trend_zcSM,'/mnt/md0/raster_procesada/analysis/trends/trend_zcSM_1981-2023_2.tif',overwrite = TRUE)

map_zcSM <- tm_shape(trend_zcSM*100) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend zcSM (x 100)') +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('zcSM-',c(1,3,6,12,24,36)))
tmap_save(map_zcSM,'output/figs/trend_raster_zcSM_1981-2023.png',asp=.2)

trend_zcNDVI <- rast('/mnt/md0/raster_procesada/analysis/trends/trend_zcNDVI_2001-2023.tif') 
ext <- ext(c(-75.6950024449548, -66.9939929163733, -55.6620008267854, -17.5609987459685)) 
ext <- project(ext,from="EPSG:4326",to="EPSG:32719")
trend_zcNDVI <- crop(trend_zcNDVI,ext)

# names(trend_zcSM) <- paste0('zcNDVI-',c(1,3,6,12,24,36))
# writeRaster(trend_zcSM,'/mnt/md0/raster_procesada/analysis/trends/trend_zcSM_1981-2023_2.tif',overwrite = TRUE)

map_zcNDVI <- tm_shape(trend_zcNDVI) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend zcNDVI',style = 'kmeans') +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col='black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('zcNDVI-',c(1,3,6,12)))
tmap_save(map_zcNDVI,'output/figs/trend_raster_zcNDVI_2001-2023.png',asp=.2)
