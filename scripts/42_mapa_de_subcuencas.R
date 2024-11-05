library(sf)
library(tmap)
library(tidyverse)
library(rnaturalearth)

chl <- ne_countries(country= 'chile',scale = 'small',returnclass = 'sf') |> 
  st_transform(32719)
bb <- st_bbox(chl)
bb[1] <- -550000
subcuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg')
macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  st_transform(32719)
subcuencas <- st_crop(subcuencas,bb)

subcuencas2 <- st_intersection(subcuencas,macro)
macro$macrozona <- c('Norte Chico','Norte Grande','Austral','Centro','Sur')
map <- tm_shape(subcuencas) + 
  tm_borders() +
  tm_shape(macro) +
  tm_text('macrozona',size=.6,xmod = -3) +
  tm_borders(col='red') +
  tm_layout(frame = FALSE)
tmap_save(map,'output/figs/mapa_subcuencas.png',scale=1.7)

library(units)
subcuencas2 <- subcuencas2 |> 
  mutate(area = st_area(geom) |> set_units(km2))

subcuencas2 |> 
  st_drop_geometry() |> 
  group_by(macrozona) |> 
  summarize(n = n(),max_area = max(area),med_area = median(area), min_area = min(area))
