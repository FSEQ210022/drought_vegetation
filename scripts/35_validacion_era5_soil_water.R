library(tidyverse)
library(terra)
library(sf)
library(agvAPI)
library(fs)

data_sm_tb <- read_rds('data/processed_data/data_sm_2022_garces.rds') |> 
  #filter(depth == 30) |> 
  mutate(datetime =ymd_hm(datetime)) |> 
  group_by(date = floor_date(datetime,'1 day'),serial) |> 
  summarize(SM_g = mean(SM,na.rm = TRUE)) |> 
  mutate(date = as.character(ymd(date)))

dir_era_sm <- '/mnt/md0/raster_procesada/ERA5-Land_tiff/clima/volumetric_soil_water/daily'

files_era <- dir_ls(dir_era_sm,regexp = '2022.*tif$')
dates_era <- str_extract(files_era,'[0-9]{4}-[0-9]{2}-[0-9]{2}')
sm_era <- lapply(seq_along(files_era),\(i){
  out <- app(rast(files_era[i],lyrs=1:3),mean)
  names(out) <- dates_era[i]
  out
})
sm_era <- rast(sm_era)

est_garces_sf <- estaciones_garces |> 
  filter(tipo == 'Humedad_Suelo') |> 
  st_as_sf(coords = c('lon','lat'),crs=4326)
         
data_sm <- terra::extract(sm_era,est_garces_sf)

dates <- seq(ymd("20220101"),ymd("20221231"),by='1 day')
data_sm <- cbind(st_drop_geometry(est_garces_sf[,'serial']),data_sm)
#names(data_sm)[3:367] <- as.character(dates)

data_unida_sm <- data_sm |> 
  select(-ID) |> 
  pivot_longer(-serial,names_to = 'date',values_to = 'SM_era5') |> 
  left_join(data_sm_tb) |> 
  mutate(SM_g = SM_g/100,date = ymd(date)) 
  
library(yardstick)
metrics_sm <- data_unida_sm |> 
  mutate(num_mes = month(date)) |> 
  filter(num_mes %in% c(4:10)) |> 
  group_by(serial) |> 
  summarize(CC = cor(SM_era5,SM_g),
            rmse = rmse_vec(SM_era5,SM_g),
            mae =mae_vec(SM_era5,SM_g),
            bias = sum(SM_era5)/sum(SM_g)) 

metrics_sm |> 
  pivot_longer(-serial) |> 
  ggplot(aes(name,value)) + 
  geom_jitter(alpha=.7) +
  geom_violin(fill=NA,color = 'darkblue') +
  geom_boxplot(fill = NA,col = 'darkgreen') +
  facet_wrap(name~.,scale = 'free') + 
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank())  
ggsave('output/figs/metrics_validation_sm_era.png',scale=1.5)

metrics_sm |> 
  summarize(across(2:5,\(x) mean(x)))
  
library(tmap)
library(rnaturalearth)
chl <- ne_countries(country = 'chile',scale = 'small',returnclass = 'sf')

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg')
mapa_est <- tm_shape(macro) +
  tm_borders() +
  tm_shape(est_garces_sf) +
  tm_dots() 
tmap_save(mapa_est,'output/figs/mapa_estaciones_humedad.png')
