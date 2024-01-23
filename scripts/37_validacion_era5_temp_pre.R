## Validación datos ERA5-Land

library(RSQLite)
pool <- dbConnect(RPostgres::Postgres(),
                  dbname = "shiny",
                  host = Sys.getenv("HOST"),
                  user = "shiny",
                  password = Sys.getenv("SHINY_PSQL_PWD"))



data <- tbl(pool, "estaciones_datos")

library(tidyverse)
data_val <- data |> 
  select(station_id,fecha_hora,temp_promedio_aire,precipitacion_horaria) |> 
  filter(fecha_hora >= "2015-01-01") |> 
  collect()

data_sel <- data_val |> 
  tidyr::complete(station_id,fecha_hora,fill = list(temp_promedio_aire =NA)) |> 
  # agrupar por station_id y mes-año
  dplyr::group_by(station_id, 
                  mes = lubridate::floor_date(fecha_hora,'month')) |>
  #calcular el promedio de temperatura y la cantidad de NAs por estación y mes-año
  dplyr::summarize(temp = mean(temp_promedio_aire,na.rm = TRUE),
                   prop_NAs = sum(is.na(temp_promedio_aire))/n()) |> 
  #filtrar los meses que tienen menos de 20% de NAs
  dplyr::filter(prop_NAs < .2)

data_sel |> 
  ggplot(aes(as.factor(mes),as.factor(station_id),fill=temp)) +
  geom_tile() +
  scale_fill_viridis_c() +
  #scale_x_discrete() +
  #scale_x_date(date_breaks = "1 year",date_labels = '%Y-%b',expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

emas_sel <- data_sel |> 
  ungroup() |> 
  tidyr::complete(station_id,mes,fill = list(temp = NA)) |> 
  dplyr::group_by(station_id) |> 
  dplyr::summarize(prop_NAs = sum(is.na(temp))/n()) |> 
  dplyr::filter(prop_NAs < .2) |> 
  dplyr::pull(station_id)

## Funcion de filtro

filt_out <- function(x,c,...){
  M <- .6745*(x - median(x,...))/mad(x,...)
  
  x[M > c] <- NA 
  x[M < -c] <- NA
  
  return(x)
}

## Temperatura
## 
data_temp <- data_val |> 
  filter(station_id %in% emas_sel) |> 
  dplyr::group_by(station_id) |>
  #aplica el filtro para cada estación
  dplyr::mutate(
    dplyr::across(temp_promedio_aire,\(x) filt_out(x,1.7,na.rm = TRUE),.names ="temp")
  ) |> 
  select(fecha_hora,temp)

data_temp |> 
  ggplot(aes(as.factor(fecha_hora),as.factor(station_id),fill = temp)) +
  geom_tile() +
  theme_bw()

emas_sel <- data_temp |> distinct(station_id) |> pull(station_id)

library(agrometR)
library(fs)
library(sf)

data_agro <- estaciones_agromet |> 
  filter(ema %in% emas_sel) |> 
  select(ema,institucion,latitud,longitud) |> 
  st_as_sf(coords = c('longitud','latitud'),crs=4326)

dir <- '/mnt/md0/raster_procesada/ERA5-Land_tiff/clima/2m_mean_temperature/monthly/'

files <- dir_ls(dir,regexp = 'tif$')

library(terra)
temp <- rast(files)

data_agro_df <- terra::extract(temp,data_agro) |> 
  cbind(data_agro |> st_drop_geometry() |> select(ema)) |> 
  pivot_longer(-c(ID,ema)) |> 
  mutate(dates =ymd(str_extract(name,'[0-9]{4}-[0-9]{2}-[0-9]{2}')),
         temp = value - 273.15) |> 
  select(-name,-value,-ID)

ubrmse <- function(obs,est){
  sqrt(sum(((obs-mean(obs))-(est-mean(est)))^2)/length(obs))
}

data_temp |> 
  group_by(station_id,dates = floor_date(fecha_hora,'1 month')) |> 
  summarise(temp = mean(temp,na.rm = TRUE)) |> 
  left_join(data_agro_df,by = c('dates','station_id' = 'ema')) |> 
  drop_na() |> 
  summarise(
    ubrmse = ubrmse(temp.x,temp.y),
    mae = mae_vec(temp.x,temp.y),
    cor = cor(temp.x,temp.y)
  ) |> 
  filter(ubrmse < 4) |>
  summarize(across(ubrmse:cor,\(x) mean(x)))
  
  
  ggplot(aes(ubrmse)) + 
  geom_boxplot()
  geom_col(position = 'dodge')
  
## Precipitacion
## 
data_pre <- data_val |> 
  filter(station_id %in% emas_sel) |> 
    dplyr::group_by(station_id) |>
    #aplica el filtro para cada estación
    filter(between(precipitacion_horaria,0,200)) 
    # dplyr::mutate(
    #   dplyr::across(precipitacion_horaria,\(x) filt_out(x,5,na.rm = TRUE),.names ="pre")
    # ) |> 
    # select(fecha_hora,pre)
    # 

emas_sel <- data_pre |> distinct(station_id) |> pull(station_id)

dir <- '/mnt/md0/raster_procesada/ERA5-Land_tiff/clima/total_precipitation/monthly/'

files <- dir_ls(dir,regexp = 'tif$')

library(terra)
pre <- rast(files)

data_pre_df <- terra::extract(pre,data_agro) |> 
  cbind(data_agro |> st_drop_geometry() |> select(ema)) |> 
  pivot_longer(-c(ID,ema)) |> 
  mutate(dates =ymd(str_extract(name,'[0-9]{4}-[0-9]{2}-[0-9]{2}')),
         pre = value*1000) |> 
  select(-name,-value,-ID)

ubrmse <- function(obs,est){
  sqrt(sum(((obs-mean(obs))-(est-mean(est)))^2)/length(obs))
}

data_pre |> 
  group_by(station_id,dates = floor_date(fecha_hora,'1 month')) |> 
  summarise(pre = sum(precipitacion_horaria,na.rm = TRUE)) |> 
  left_join(data_pre_df,by = c('dates','station_id' = 'ema')) |> 
  drop_na() |> 
  summarise(
     mae = mae_vec(pre.y,pre.x),
     rmse = rmse_vec(pre.y,pre.x),
     bias =sum(pre.y)/sum(pre.x),
     cor  = cor(pre.y,pre.x)
   ) |> 
  filter(bias < 10) |> 
  ungroup() |> 
  summarize(across(mae:cor,\(x) mean(x,na.rm = TRUE)))
  ggplot(aes(.estimate)) + 
  geom_boxplot()
geom_col(position = 'dodge')

