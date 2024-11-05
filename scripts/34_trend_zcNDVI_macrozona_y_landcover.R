library(tidyverse)
library(fs)

#Trend zcNDVi-6 por amcrozona y landcover 

#datos para landcover
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

dir <- '/mnt/md0/raster_procesada/analysis/trends'
files <- dir_ls(dir,regexp = 'mann_kendall.*zcNDVI-6.*tif$')

trends <- rast(files)[[2]]
names(trends) <- 'zcNDVI-6'



ecoregions <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_transform(32719)

# ahora por tipo de landcover

lc_pers <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') 
#nieve permanente
lc_pers <- resample(lc_pers,trends,method = 'near')
lc_pers[lc_pers %in% c(5,7:10)] <- NA

data <-  map_df(1:5,\(i){
  macro_n <- ecoregions[i,]
  trends_n <- mask(trends,macro_n)
  lc_pers_n <- mask(lc_pers,macro_n)
  trends_df <- zonal(trends_n,lc_pers_n,'mean',na.rm = TRUE)
  trends_df |> 
    mutate(ECO_NAME = macro_n$ECO_NAME) |> 
    select(-lyr.1)
})

data_final <- data |> 
  pivot_wider(names_from = ECO_NAME,values_from = `zcNDVI-6`) |> 
  arrange(macro) |> 
  select(c(1,6,5,4,3,2)) 

data |> 
  ggplot(aes(clase,macro,fill = `zcNDVI-6`)) + 
  geom_tile() +
  scale_y_discrete(limits=rev) +
  scale_fill_viridis_c(name = 'Trend of zcNDVI') + 
  theme_bw()
