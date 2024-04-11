library(tidyverse)
library(terra)
library(tmap)
library(sf)
library(rnaturalearth)
library(fs)

chl_b <- ne_countries(country='chile',scale = 'medium',returnclass = 'sf')
macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  mutate(macrozona = c('Norte Chico','Norte Grande','Austral','Centro','Sur'))

## Time series of zcNDVI for macrozones
dir <- '/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/zcNDVI-6'
zcndvi <- rast(dir_ls(dir,regexp = 'tif$'))

#persistencia de landcover
lc_pers <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') 
#eliminar los que no corresponde a los 5 principales landcover
lc_pers[lc_pers %in% c(5,7:10)] <- NA
lc_pers <- resample(lc_pers,zcndvi)

#aplicar mascara de landcover permanenete 
lc_pers <- resample(lc_pers,zcndvi,method = 'mode')
zcndvi <- mask(zcndvi,lc_pers)

macro_mask <- rasterize(st_transform(macro,32719),zcndvi[[1]],field ='macrozona')

df_zcndvi <- zonal(zcndvi,macro_mask,fun = 'mean',na.rm = TRUE)
df_zcndvi |> 
  pivot_longer(-macrozona) |> 
  mutate(dates = str_extract(name,'[0-9]{4}-[0-9]{2}-[0-9]{2}'),
         dates = ymd(dates),
         macrozona =str_to_title(macrozona),
         macrozona = fct(macrozona,c('Norte Grande','Norte Chico','Centro','Sur','Austral'))) |> 
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

ggsave('output/figs/temporal_variation_zcNDVI6_macrozonas.png',scale = 2,bg = 'white',width=6,height=3)

#Mapa de tendencia Mann-Kendall

#breaks para escalas

breaks <- seq(-0.06,.06,length.out = 6)

library(fs)
dir <- '/mnt/md0/raster_procesada/analysis/trends/'
files <- dir_ls(dir,regexp = 'mann.*SPI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_spi <- rast(files)
trend_spi <- app(trend_spi,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

ind_ord <- scales |> as.numeric() |> order()
trend_spi <- subset(trend_spi,ind_ord)
names(trend_spi) <- paste('SPI',scales[ind_ord],sep ='-')

df_spi <- terra::extract(trend_spi,macro,fun = mean, na.rm = TRUE) |> 
  mutate(macro = macro$macrozona) |> 
  select(-ID) |> 
  pivot_longer(-macro) |> 
  mutate(scale = str_remove(name,'SPI-') |> as.numeric())

map_spi <- tm_shape(trend_spi*10) + 
  tm_raster(palette = 'inferno',
            midpoint = 0,
            title = 'Trend SPI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow=1) +
  tm_layout(panel.labels = paste0('SPI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_spi,'output/figs/trend_raster_SPI_1981-2023.png',asp=.2)

## SPEI

files <- dir_ls(dir,regexp = 'mann.*SPEI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_spei <- rast(files)
trend_spei <- app(trend_spei,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

ind_ord <- scales |> as.numeric() |> order()
trend_spei <- subset(trend_spei,ind_ord)
names(trend_spei) <- paste('SPEI',scales[ind_ord],sep ='-')

df_spei <- terra::extract(trend_spei,macro,fun = mean, na.rm = TRUE) |> 
  mutate(macro = macro$macrozona) |> 
  select(-ID) |> 
  pivot_longer(-macro) |> 
  mutate(scale = str_remove(name,'SPEI-') |> as.numeric())

map_spei <- tm_shape(trend_spei*10) + 
  tm_raster(palette = 'inferno',
            midpoint = 0,
            title = 'Trend SPEI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow = 1) +
  tm_layout(panel.labels = paste0('SPEI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_spei,'output/figs/trend_raster_SPEI_1981-2023.png',asp=.2)

## EDDI
files <- dir_ls(dir,regexp = 'mann.*EDDI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_eddi <- rast(files)
trend_eddi <- app(trend_eddi,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

ind_ord <- scales |> as.numeric() |> order()
trend_eddi <- subset(trend_eddi,ind_ord)
names(trend_eddi) <- paste('EDDI',scales[ind_ord],sep ='-')

df_eddi <- terra::extract(trend_eddi,macro,fun = mean, na.rm = TRUE) |> 
  mutate(macro = macro$macrozona) |> 
  select(-ID) |> 
  pivot_longer(-macro) |> 
  mutate(scale = str_remove(name,'EDDI-') |> as.numeric())

map_EDDI <- tm_shape(trend_eddi*10) + 
  tm_raster(palette = '-inferno',
            midpoint = 0,
            title = 'Trend EDDI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow = 1) +
  tm_layout(panel.labels = paste0('EDDI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_EDDI,'output/figs/trend_raster_EDDI_1981-2023.png',asp=.2)

## zcSM
files <- dir_ls(dir,regexp = 'mann.*zcSM.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_zcsm <- rast(files)
trend_zcsm <- app(trend_zcsm,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

ind_ord <- scales |> as.numeric() |> order()
trend_zcsm <- subset(trend_zcsm,ind_ord)
names(trend_zcsm) <- paste('zcSM',scales[ind_ord],sep ='-')

df_zcsm <- terra::extract(trend_zcsm,macro,fun = mean, na.rm = TRUE) |> 
  mutate(macro = macro$macrozona) |> 
  select(-ID) |> 
  pivot_longer(-macro) |> 
  mutate(scale = str_remove(name,'zcSM-') |> as.numeric())

map_zcSM <- tm_shape(trend_zcsm*10) + 
  tm_raster(palette = 'inferno',
            midpoint = 0,
            title = 'Trend SSI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(macro) + 
  tm_borders(col='white') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('SSI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_zcSM,'output/figs/trend_raster_zcSM_1981-2023.png',asp=.2)

## zcNDVI
dir <- '/mnt/md0/raster_procesada/analysis/trends/'
files <- dir_ls(dir,regexp = 'mann.*zcNDVI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_zcndvi <- rast(files)
trend_zcndvi <- app(trend_zcndvi,\(x){
  i <- seq(1,2,2)
  x[i]*x[i+1]
})

ind_ord <- scales |> as.numeric() |> order()
trend_zcndvi <- subset(trend_zcndvi,ind_ord)
names(trend_zcndvi) <- paste('zcNDVI',scales,sep ='-')
trend_zcndvi <- mask(trend_zcndvi,st_transform(macro,crs(trend_zcndvi)))
trend_zcndvi <- trim(trend_zcndvi) 

#aplicar mascara de landcover persistente
lc_pers <- crop(lc_pers,trend_zcndvi)
lc_pers <- resample(lc_pers,trend_zcndvi,method = 'near')
trend_zcndvi <- mask(trend_zcndvi,lc_pers)

map_zcNDVI <- tm_shape(trend_zcndvi*10) + 
  tm_raster(palette = 'RdYlGn',midpoint = 0,title = 'Trend zcNDVI \n (per decade)',style = 'kmeans',colorNA = 'grey',textNA = 'No vegetation/change' ) +
  tm_shape(macro) + 
  tm_borders(col='black') +
  tm_shape(chl_b) + 
  tm_borders(col='black') +
  #tm_facets(nrow = 1) +
  tm_layout(legend.outside = TRUE,
            legend.text.size = .5)
tmap_save(map_zcNDVI,'output/figs/trend_raster_zcNDVI6_2001-2023.png',scale=1)


mapa1 <- tmap_arrange(map_zcNDVI,map_spi,widths = c(.2,.8))
tmap_save(mapa1,'output/figs/trend_raster_zcNDVI6_SPIs.png',asp=.3)

## Visualización trend por macrozona
## 
data_df <- bind_rows(df_spi,df_spei,df_eddi,df_zcsm)

data_df |> 
  mutate(index =str_remove(name,'-.*'),
         macro = fct_relevel(macro,'Norte Grande','Norte Chico','Centro','Sur','Austral'),
         index = case_when(index == 'zcSM' ~ 'SSI',
                         .default = index)) |> 
  ggplot(aes(scale,value*10,color=index)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d(name = 'Drought index') +
  scale_y_continuous(breaks= seq(-0.05,0.05,length.out=9)) +
  scale_x_continuous(breaks = c(1,3,6,12,24,36)) +
  facet_grid(.~macro,scale = 'free') +
  labs(y = 'Trend per decade',x= 'Time scales') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        legend.position = 'bottom')
ggsave('output/figs/trend_macrozone_drought_indices.png',width =10,height=3)
