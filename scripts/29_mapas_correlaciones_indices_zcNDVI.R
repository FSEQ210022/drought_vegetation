library(terra)
library(sf)
library(tidyverse)
library(fs)
library(tmap)
library(basemaps)
library(rnaturalearth)

dir <- '/mnt/md0/raster_procesada/analysis/correlations/'
files <- dir_ls(dir,type = 'file')

chl <- ne_countries(country = 'chile',returnclass = 'sf') |> st_transform(32719)

cors <- rast(files)
cors_i <- subset(cors,seq(1,8,2))
cors_r <- subset(cors,seq(2,8,2))

set_defaults(map_token = "pk.eyJ1IjoiZnJ6YW1icmEiLCJhIjoiY2tqdmw5Z3QxMDZyZjJydG54M2RobWMyeSJ9.rl8_KzhiKaV0wgsLL2Y1WQ")
bm <- basemap_raster(chl,map_service = 'osm',map_type="streets")

names(cors_i) <- 1:4

# map_i <- tm_shape(bm) + 
#   tm_rgb() + 
map_i <- tm_shape(bm) + 
  tm_rgb() +
  tm_shape(cors_i) + 
  tm_raster(style = 'cat',
            labels = as.character(c(1,3,6,12,24,36)),
            palette = viridis::magma(6),
            title =  'Time-scale \n(months)') +
  # tm_shape(chl) +
  # tm_borders() +
  tm_facets(nrow = 1) + 
  tm_layout(
    panel.labels = c('EDDI','SPEI','SPI','zcSM'),
            )
names(cors_r) <- 1:4

# map_r <- tm_shape(bm) + 
#   tm_rgb() +
map_r <- tm_shape(bm) + 
  tm_rgb() +
  tm_shape(cors_r) +
  tm_raster(style='equal',palette = viridis::viridis(5, option = "B"),title = 'r',legend.hist = TRUE) +
  tm_facets(free.scales	= FALSE) + 
  tm_layout(
    panel.labels = c('EDDI','SPI','SPEI','zcSM'),
    legend.hist.bg.color = 'lightgrey',
    legend.outside = FALSE,
    legend.hist.width = .5,
    legend.hist.height = .15,
    legend.format = list(digits = 1)
  )

mapU <- tmap_arrange(map_i,map_r,nrow = 2)
tmap_save(map_i,'output/figs/mapa_cor_selec_indices_zcNDVI6.png',asp = .25)
tmap_save(map_r,'output/figs/mapa_cor_r_indices_zcNDVI6.png',asp = .25)
