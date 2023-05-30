library(terra)
library(rnaturalearth)
library(sf)
library(stars)

chl <- ne_countries(country='chile',returnclass = 'sf') |> 
  st_transform(4326)

library(ncdf4)
dir_name <- 'GRC_TELLUS_JPL_GRAC-GRFO_MASCON_CRI_GRID_RL06.1_V3' 
gr <- nc_open(paste0('/mnt/raster_raw/GRACE/',dir_name,'/GRCTellus.JPL.200204_202302.GLO.RL06.1M.MSCNv03CRI.nc'))

dates <- ncvar_get(gr,'time') |> as.Date(origin = "2002-01-01")-15

grace_sf <- rast(paste0('/mnt/raster_raw/GRACE/',dir_name,'/GRCTellus.JPL.200204_202302.GLO.RL06.1M.MSCNv03CRI.nc'),subds = 'scale_factor')
grace <- rast(paste0('/mnt/raster_raw/GRACE/',dir_name,'/GRCTellus.JPL.200204_202302.GLO.RL06.1M.MSCNv03CRI.nc'),subds = 'lwe_thickness')
grace_sf <- resample(grace_sf,grace)

grace <- grace*grace_sf

names(grace) <- dates

crs(grace) <- 'EPSG:4326'
grace <- rotate(grace)
grace_chl <- crop(grace,chl)
grace_chl <- mask(grace_chl,chl)

plot(grace_chl)
plot(chl,add=TRUE)

library(tmap)

tm_shape(grace_chl) +
  tm_raster(style='cont') +
  tm_shape(chl) +
  tm_borders() +
  tm_facets()

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') 

df <- terra::extract(grace_chl,macro,fun=mean,na.rm=TRUE)
df$macro <- macro$macrozona

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(forcats)
library(lubridate)

df |> select(-ID) |> 
  pivot_longer(-macro) |>
  mutate(dates=ymd(substr(name,2,11)),
         macro = fct_relevel(macro, "norte grande", "norte chico",
                             'zona central','zona sur')) |> 
  dplyr::select(-name) |> 
  relocate(dates,.before= 'value') |> 
  ggplot(aes(dates,value,col=as.factor(macro))) +
  geom_point(size=.2,alpha=.4) +
  #geom_line(lwd=.3) +
  ggtitle(dir_name)+
  labs(y='Equivalent Water Thickness (cm)') +
  scale_color_brewer(name = 'macrozona',palette = 'Set1') +
  geom_smooth(se = FALSE) +
  #facet_grid(macro~.,scales = 'free') + 
  theme_bw()
ggsave(paste0('output/figs/',dir_name,'Solutions_macrozonas.png'),scale=1.5)
