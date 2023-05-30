library(terra)
library(sf)
library(fs)
library(tmap)
library(stringr)
library(dplyr)
library(forcats)

dir <- '/mnt/raster_raw/GRACE/'

chl <- read_sf('data/processed_data/spatial/chile_continental.gpkg')
macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') 

files <- dir_ls(paste0(dir,'TELLUS_GRAC_L3_GFZ_RL06_LND_v04'),regexp = 'tif$')
files_gf <- dir_ls(paste0(dir,'TELLUS_GRFO_L3_GFZ_RL06.1_LND_v04'),regexp = 'tif$')

chl <- rnaturalearth::ne_countries(country = 'chile')
grace <- rast(c(files,files_gf))

grace <- crop(grace,chl)
ind <- seq(1,nlyr(grace),2)
grace <- grace[[ind]]

dates <- as.Date(str_extract(str_split_i(names(grace),'-',3),'[0-9]{7}'),
        '%Y%j')
#cuenca <- cuenca |> st_transform(4326) 

df <- terra::extract(grace,macro,fun=mean,na.rm=TRUE)
df$macro <- macro$macrozona

library(ggplot2)
library(tidyr)

df |> select(-ID) |> 
  pivot_longer(-macro) |>
  mutate(dates=as.Date(str_extract(str_split_i(name,'\\.',3),'[0-9]{7}'),'%Y%j'),
         macro = fct_relevel(macro, "norte grande", "norte chico",
                             'zona central','zona sur')) |> 
  select(-name) |> 
  relocate(dates,.before= 'value') |> 
  ggplot(aes(dates,value,col=as.factor(macro))) +
  geom_point(size=.2,alpha=.4) +
  ggtitle('TELLUS_GRACE-FO_L3_GFZ_RL06_LND_v04') +
  labs(y='Equivalent Water Thickness (cm)') +
  scale_color_brewer(name = 'macrozona',palette = 'Set1') +
  #geom_line(lwd=.3) +
  geom_smooth(se = FALSE) +
  #facet_grid(macro~.,scales = 'free') + 
  theme_bw()
ggsave('output/figs/TELLUS_GRACE-FO_L3_GFZ_RL06_LND_v04.png',scale=1.5)


plot(c(as.numeric(df)[-1],as.numeric(df)[-1]),type='l')
dir_ls('~/Descargas',regexp = 'gpkg$')

map <- tm_shape(grace) +
  tm_raster(style ='cont',palette = 'RdYlBu')+
  tm_shape(chl) + 
  tm_borders() +
  tm_facets(nrow=1,ncol=1) 
  
tmap_animation(map, filename = "grace.mp4", 
                 width=300, height = 1200, delay = 100, outer.margins = 0)
