library(tidyverse)
library(terra)
library(tmap)
library(sf)
library(rnaturalearth)
library(fs)

chl_b <- ne_countries(country='chile',scale = 'medium',returnclass = 'sf')
macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') 

## Time series of zcNDVI for macrozones
dir <- '/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/zcNDVI-6'

zcndvi <- rast(dir_ls(dir,regexp = 'tif$'))
macro_mask <- rasterize(st_transform(macro,32719),zcndvi[[1]],field ='macrozona')

df_zcndvi <- zonal(zcndvi,macro_mask,fun = 'mean',na.rm = TRUE)
df_zcndvi |> 
  pivot_longer(-macrozona) |> 
  mutate(dates = str_extract(name,'[0-9]{4}-[0-9]{2}-[0-9]{2}'),
         dates = ymd(dates),
         macrozona = fct(macrozona,c('norte grande','norte chico','zona central','zona sur','zona austral'))) |> 
  select(-name) |> 
  drop_na() |> 
  ggplot(aes(dates,value)) +
  #geom_point(size=.2) + 
  #geom_line(size = .2) +
  geom_ribbon(aes(ymin = pmin(value,0),ymax = 0),fill = 'red',alpha=.7)+
  geom_ribbon(aes(ymin = 0,ymax = pmax(value,0)),fill = 'darkgreen',alpha=.7)+
  geom_hline(yintercept = 0,col = 'red') +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",limits = c(as.Date("2000-01-01"),as.Date("2023-04-01")),expand = c(0,0)) +
  geom_smooth(method = 'lm',se = FALSE,alpha=.8,lty = 'dashed',col='grey') +
  labs(y = 'zcNDVI') +
  facet_grid(macrozona~.) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

ggsave('output/figs/temporal_variation_zcNDVI6_macrozonas.png',scale = 1.5,bg = 'white',width=6,height=3)

trend_spi <- rast('/mnt/md0/raster_procesada/analysis/trends/trend_SPI_1981-2023.tif')

macro_ras <- rasterize(macro,trend_spi,field = 'macrozona') 

data_trend_spi_macro <- zonal(trend_spi,macro_ras,'mean',na.rm=TRUE) |> as_tibble()

data_trend_spi_macro |> 
  pivot_longer(-macrozona) |> 
  mutate(value = value*10) |> 
  arrange(value)

#names(trend_spei) <- paste0('SPEI-',c(1,3,6,12,24,36))
#writeRaster(trend_spei,'/mnt/md0/raster_procesada/analysis/trends/trend_SPEI_1981-2023_2.tif',overwrite = TRUE)
names(trend_spi) <- paste0('SPI-',c(1,3,6,12,24,36))
map_spi <- tm_shape(trend_spi*10) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend SPI \n (per decade)',style = 'kmeans') +
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

map_spei <- tm_shape(trend_spei*10) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend SPEI \n (per decade)',style = 'kmeans') +
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

map_EDDI <- tm_shape(trend_EDDI*10) + 
  tm_raster(palette = 'inferno',midpoint = 0,title = 'Trend EDDI \n (per decade)',style = 'kmeans') +
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

map_zcSM <- tm_shape(trend_zcSM*10) + 
  tm_raster(palette = '-inferno',midpoint = 0,title = 'Trend zcSM \n (per decade)') +
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

map_zcNDVI <- tm_shape(trend_zcNDVI[[3]]) + 
  tm_raster(palette = 'RdYlGn',midpoint = 0,title = 'Trend zcNDVI \n (per year)',style = 'kmeans') +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col='black') +
  #tm_facets() +
  tm_layout(panel.labels = paste0('zcNDVI-',6),legend.outside = TRUE)
tmap_save(map_zcNDVI,'output/figs/trend_raster_zcNDVI6_2001-2023.png')


mapa1 <- tmap_arrange(map_zcNDVI,map_spi,widths = c(.2,.8))
tmap_save(mapa1,'output/figs/trend_raster_zcNDVI6_SPIs.png',asp=.3)
