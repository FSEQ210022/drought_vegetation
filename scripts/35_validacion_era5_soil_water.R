library(tidyverse)
library(terra)
library(sf)
library(agvAPI)
library(fs)

data_sm_tb <- read_rds('data/processed/data_sm_2022.rds') |> 
  #filter(depth == 30) |> 
  mutate(datetime =ymd_hm(datetime)) |> 
  group_by(day = floor_date(datetime,'1 day'),serial) |> 
  summarize(SM = mean(SM,na.rm = TRUE)) |> 
  mutate(day = as.character(ymd(day)))

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

rmse <- function(x,y) {
  sqrt(mean((x-y)^2))
  }

data_sm |> 
  select(-ID) |> 
  pivot_longer(-serial,names_to = 'day') |> 
  right_join(data_sm_tb) |> 
  mutate(SM = SM/100,date = ymd(day)) |> 
  ggplot() +
  geom_point(aes(date,SM),colour='blue') + 
  geom_line(aes(date,SM),colour='blue') + 
  geom_point(aes(date,value),colour='red') + 
  geom_line(aes(date,value),colour='red')  +
  facet_wrap(serial~.)

data_sm |> 
  select(-ID) |> 
  pivot_longer(-serial,names_to = 'day') |> 
  right_join(data_sm_tb) |> 
  mutate(SM = SM/100,date = ymd(day)) |> 
  group_by(serial) |> 
  summarize(r = cor(value,SM),
            rmse = rmse(value,SM)) |>
  ggplot(aes(serial,rmse)) + 
  geom_col()

  ggplot(aes(value,SM)) + 
  geom_point() +
  facet_wrap(serial~.) + 
  theme_bw()

  ##borrar
  ##
  
  load('~/Descargas/Aconcagua_TOMST_TempAire.RData')
  temps_acon |> 
    filter(sensor_name == 'TMS_TMSmoisture') |> 
    select(1:7) |> 
    group_by(Sitio,Profundidad) |> 
    summarize(across(everything(),\(x) unique(x))) |> 
    write_rds('data/processed_data/metadata_tomsT4.rds')
  
  temps_acon |> 
    filter(sensor_name == 'TMS_TMSmoisture') |> 
    select(Sitio,Profundidad,Fecha2,value) |> 
    group_by(Sitio,Fecha2) |> 
    summarize(value = mean(value)) |> 
    write_rds('data/processed_data/datos_tomsT4_raw_promedio_x_sitio.rds')
  