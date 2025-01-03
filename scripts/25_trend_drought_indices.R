library(tidyverse)
library(terra)
library(tmap)
library(sf)
library(rnaturalearth)
library(fs)

chl_b <- ne_countries(country='chile',scale = 'medium',returnclass = 'sf')
ecoregions <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg')

## Time series of zcNDVI for macrozones
dir <- '/home/rstudio/discoB/processed/MODIS/zcNDVI/zcNDVI-6'
zcndvi <- rast(dir_ls(dir,regexp = 'tif$'))

#persistencia de landcover
lc_pers <- rast('/home/rstudio/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/IGBP80_reclassified.tif') 
#eliminar los que no corresponde a los 5 principales landcover
lc_pers[lc_pers %in% c(5,7:10)] <- NA
lc_pers <- resample(lc_pers,zcndvi)

#aplicar mascara de landcover permanenete 
lc_pers <- resample(lc_pers,zcndvi,method = 'mode')
zcndvi <- mask(zcndvi,lc_pers)

eco_mask <- rasterize(st_transform(ecoregions,32719),zcndvi[[1]],field ='ECO_NAME')

df_zcndvi <- zonal(zcndvi,eco_mask,fun = 'mean',na.rm = TRUE)
df_zcndvi |> 
  pivot_longer(-ECO_NAME) |>
  filter(ECO_NAME != "Rock and Ice" ) |> 
  mutate(dates = str_extract(name,'[0-9]{4}-[0-9]{2}-[0-9]{2}'),
         dates = ymd(dates),
         ECO_NAME = fct(ECO_NAME,
                        levels = 
                          c("Atacama desert","Central Andean dry puna",
                            "Southern Andean steppe","Chilean Matorral",
                            "Valdivian temperate forests","Magellanic subpolar forests",
                            "Patagonian steppe"))) |> 
  select(-name) |> 
  drop_na() |> 
  ggplot(aes(dates,value)) +
  #geom_point(size=.2) + 
  #geom_line(size = .2) +
  geom_ribbon(aes(ymin = pmin(value,0),ymax = 0),fill = 'red',alpha=.7)+
  geom_ribbon(aes(ymin = 0,ymax = pmax(value,0)),fill = 'darkgreen',alpha=.7)+
  geom_hline(yintercept = 0,col = 'red') +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",limits = c(as.Date("2000-07-01"),as.Date("2023-04-01")),expand = c(0,0)) +
  geom_smooth(method = 'lm',se = FALSE,alpha=.8,lty = 'dashed',col='grey') +
  labs(y = 'zcNDVI') +
  facet_wrap(ECO_NAME~.,ncol=1) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(hjust=0))

ggsave('output/figs/temporal_variation_zcNDVI6_ecoregiones.png',scale = 2,bg = 'white',width=6,height=3)

#Mapa de tendencia Mann-Kendall

#breaks para escalas

breaks <- seq(-0.06,.06,length.out = 6)

library(fs)
dir <- '/home/rstudio/discoB/processed/analysis/trends/'
files <- dir_ls(dir,regexp = 'mann.*mod_SPI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_spi <- rast(files)
trend_spi <- app(trend_spi,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

ind_ord <- scales |> as.numeric() |> order()
trend_spi <- subset(trend_spi,ind_ord)
trend_spi <- mask(trend_spi,chl_b)

names(trend_spi) <- paste('SPI',scales[ind_ord],sep ='-')

df_spi <- terra::extract(trend_spi,ecoregions,fun = mean, na.rm = TRUE) |> 
  mutate(ECO_NAME = ecoregions$ECO_NAME) |> 
  select(-ID) |> 
  pivot_longer(-ECO_NAME) |> 
  mutate(scale = str_remove(name,'SPI-') |> as.numeric())

tmap_options(check.and.fix = TRUE)

map_spi <- tm_shape(trend_spi*10) + 
  tm_raster(palette = 'inferno',
            midpoint = 0,
            title = 'Trend SPI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(ecoregions) + 
  tm_borders(col='black',lty ='dashed') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow=1) +
  tm_layout(panel.labels = paste0('SPI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_spi,'output/figs/trend_raster_SPI_2000-2023.png',asp=.2)

## SPEI

files <- dir_ls(dir,regexp = 'mann.*mod_SPEI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_spei <- rast(files)
trend_spei <- app(trend_spei,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

trend_spei <- mask(trend_spei,chl_b)

ind_ord <- scales |> as.numeric() |> order()
trend_spei <- subset(trend_spei,ind_ord)
names(trend_spei) <- paste('SPEI',scales[ind_ord],sep ='-')

df_spei <- terra::extract(trend_spei,ecoregions,fun = mean, na.rm = TRUE) |> 
  mutate(ECO_NAME = ecoregions$ECO_NAME) |> 
  select(-ID) |> 
  pivot_longer(-ECO_NAME) |> 
  mutate(scale = str_remove(name,'SPEI-') |> as.numeric())

map_spei <- tm_shape(trend_spei*10) + 
  tm_raster(palette = 'inferno',
            midpoint = 0,
            title = 'Trend SPEI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(ecoregions) + 
  tm_borders(col='black',lty='dashed') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow = 1) +
  tm_layout(panel.labels = paste0('SPEI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_spei,'output/figs/trend_raster_SPEI_2000-2023.png',asp=.2)

## EDDI
files <- dir_ls(dir,regexp = 'mann.*mod_EDDI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_eddi <- rast(files)
trend_eddi <- app(trend_eddi,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

trend_eddi <- mask(trend_eddi,chl_b)
ind_ord <- scales |> as.numeric() |> order()
trend_eddi <- subset(trend_eddi,ind_ord)
names(trend_eddi) <- paste('EDDI',scales[ind_ord],sep ='-')

df_eddi <- terra::extract(trend_eddi,ecoregions,fun = mean, na.rm = TRUE) |> 
  mutate(ECO_NAME = ecoregions$ECO_NAME) |> 
  select(-ID) |> 
  pivot_longer(-ECO_NAME) |> 
  mutate(scale = str_remove(name,'EDDI-') |> as.numeric())

map_EDDI <- tm_shape(trend_eddi*10) + 
  tm_raster(palette = '-inferno',
            midpoint = 0,
            title = 'Trend EDDI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(ecoregions) + 
  tm_borders(col='black',lty='dashed') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow = 1) +
  tm_layout(panel.labels = paste0('EDDI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_EDDI,'output/figs/trend_raster_EDDI_2000-2023.png',asp=.2)

## SETI
files <- dir_ls(dir,regexp = 'mann.*mod_zcET.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_seti <- rast(files)
trend_seti <- app(trend_seti,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

trend_seti <- mask(trend_seti,st_transform(chl_b,crs(trend_seti)))
ind_ord <- scales |> as.numeric() |> order()
trend_seti <- subset(trend_seti,ind_ord)
names(trend_seti) <- paste('SETI',scales[ind_ord],sep ='-')

df_seti <- terra::extract(trend_seti,ecoregions,fun = mean, na.rm = TRUE) |> 
  mutate(ECO_NAME = ecoregions$ECO_NAME) |> 
  select(-ID) |> 
  pivot_longer(-ECO_NAME) |> 
  mutate(scale = str_remove(name,'SETI-') |> as.numeric())

map_SETI <- tm_shape(trend_seti*10) + 
  tm_raster(palette = '-inferno',
            midpoint = 0,
            title = 'Trend SETI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(ecoregions) + 
  tm_borders(col='black',lty='dashed') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets(nrow = 1) +
  tm_layout(panel.labels = paste0('SETI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_SETI,'output/figs/trend_raster_SETI_2000-2023.png',asp=.2)

## zcSM
files <- dir_ls(dir,regexp = 'mann.*mod_SSI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_zcsm <- rast(files)
trend_zcsm <- app(trend_zcsm,\(x){
  i <- seq(1,12,2)
  x[i]*x[i+1]
})

trend_zcsm <- mask(trend_zcsm,chl_b)
ind_ord <- scales |> as.numeric() |> order()
trend_zcsm <- subset(trend_zcsm,ind_ord)
names(trend_zcsm) <- paste('zcSM',scales[ind_ord],sep ='-')

df_zcsm <- terra::extract(trend_zcsm,ecoregions,fun = mean, na.rm = TRUE) |> 
  mutate(ECO_NAME = ecoregions$ECO_NAME) |> 
  select(-ID) |> 
  pivot_longer(-ECO_NAME) |> 
  mutate(scale = str_remove(name,'zcSM-') |> as.numeric())

map_zcSM <- tm_shape(trend_zcsm*10) + 
  tm_raster(palette = 'inferno',
            midpoint = 0,
            title = 'Trend SSI \n (per decade)',
            style = 'cont',
            breaks = breaks) +
  tm_shape(ecoregions) + 
  tm_borders(col='black',lty='dashed') +
  tm_shape(chl_b) + 
  tm_borders(col = 'black') +
  tm_facets() +
  tm_layout(panel.labels = paste0('SSI-',c(1,3,6,12,24,36)),
            panel.label.bg.color = 'white')
tmap_save(map_zcSM,'output/figs/trend_raster_SSI_2000-2023.png',asp=.2)

## zcNDVI
dir <- '/home/rstudio/discoB/processed/analysis/trends/'
files <- dir_ls(dir,regexp = 'mann.*mod_zcNDVI.*tif$')
scales <- str_extract(basename(files),'-.*\\.') |> str_remove('-') |> str_remove('\\.')

trend_zcndvi <- rast(files[4])
trend_zcndvi <- app(trend_zcndvi,\(x){
  i <- seq(1,2,2)
  x[i]*x[i+1]
})

trend_zcndvi <- mask(trend_zcndvi,st_transform(chl_b,crs(trend_zcndvi)))

ind_ord <- scales |> as.numeric() |> order()
trend_zcndvi <- subset(trend_zcndvi,ind_ord)
names(trend_zcndvi) <- 'zcNDVI-6'
#trend_zcndvi <- mask(trend_zcndvi,st_transform(ecoregions,crs(trend_zcndvi)))
#trend_zcndvi <- trim(trend_zcndvi) 

#aplicar mascara de landcover persistente
lc_pers <- crop(lc_pers,trend_zcndvi)
lc_pers <- resample(lc_pers,trend_zcndvi,method = 'near')
trend_zcndvi <- mask(trend_zcndvi,lc_pers)

trend_zcndvi[trend_zcndvi>=0] <- 1
trend_zcndvi[trend_zcndvi<0] <- -1

map_zcNDVI <- tm_shape(trend_zcndvi) + 
  tm_raster(palette = c('darkred','darkgreen'),
            midpoint = 0,title = 'Trend zcNDVI',style = 'cat',colorNA = 'white',textNA = NULL,
            labels = c('Negative','Positive')) +
  tm_shape(ecoregions) + 
  #tm_add_legend('fill',title = '',labels = c('Negative','Positive')) +
  tm_borders(col='black',lty='dashed') +
  tm_shape(chl_b) + 
  tm_borders(col='black') +
  #tm_facets(nrow = 1) +
  tm_layout(legend.outside = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.2,
            legend.width = .8,
            frame = FALSE)
tmap_save(map_zcNDVI,'output/figs/trend_raster_zcNDVI6_2000-2023_v2.png',scale=1)

mapa1 <- tmap_arrange(map_zcNDVI,map_zcNDVI,widths = c(.2,.8))
tmap_save(mapa1,'output/figs/trend_raster_zcNDVI6_SPIs.png',asp=.3)

## Visualización trend por macrozona
## 
data_df <- bind_rows(df_spi,df_spei,df_eddi,df_zcsm,df_seti)

data_df |> 
  filter(ECO_NAME != "Rock and Ice") |> 
  mutate(index =str_remove(name,'-.*'),
         index = case_when(index == 'zcSM' ~ 'SSI',
                           .default = index),
         ECO_NAME = fct(ECO_NAME,
                        levels = 
                          c("Atacama desert","Central Andean dry puna",
                            "Southern Andean steppe","Chilean Matorral",
                            "Valdivian temperate forests","Magellanic subpolar forests",
                            "Patagonian steppe"))) |> 
  ggplot(aes(scale,value*10,color=index)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d(name = 'Drought index') +
  scale_y_continuous(breaks= seq(-0.05,0.05,length.out=9)) +
  scale_x_continuous(breaks = c(1,3,6,12,24,36)) +
  facet_grid(.~ECO_NAME,scale = 'free') +
  labs(y = 'Trend per decade',x= 'Time scales') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        legend.position = 'bottom')
ggsave('output/figs/trend_macrozone_drought_indices.png',width =10,height=3,scale = 1.5)
