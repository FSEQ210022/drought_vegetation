library(terra)
library(sf)

meta <- read_rds('data/processed_data/metadata_tomsT4.rds')

meta_sf <- meta |> 
  group_by(Sitio) |> 
  summarize(lon = mean(lon_wgs84),
            lat = mean(lat_wgs84)) |> 
  st_as_sf(coords = c('lon','lat'),crs = 4326)

dir <- '/home/francisco/Descargas/SoilGRID250m'

files_clay <- dir_ls(paste0(dir,'/clay_chl'),regexp = 'tif$')

clay_100 <- rast(files_clay[-2])
clay_100 <- app(clay_100,mean)

clay_df <- terra::extract(clay_100,meta_sf) |> 
  cbind(meta_sf |> st_drop_geometry() |> select(Sitio))

clay_df <- clay_df |> 
  mutate(mean = mean/10) |> 
  rename(clay = mean)

files_sand <- dir_ls(paste0(dir,'/sand_chl'),regexp = 'tif$')

sand_100 <- rast(files_sand[-2])
sand_100 <- app(sand_100,mean)

sand_df <- terra::extract(sand_100,meta_sf) |> 
  cbind(meta_sf |> st_drop_geometry() |> select(Sitio))

sand_df <- sand_df |> 
  mutate(mean = mean/10) |> 
  rename(sand = mean)

files_silt <- dir_ls(paste0(dir,'/silt_chl'),regexp = 'tif$')

silt_100 <- rast(files_silt[-2])
silt_100 <- app(silt_100,mean)

silt_df <- terra::extract(silt_100,meta_sf) |> 
  cbind(meta_sf |> st_drop_geometry() |> select(Sitio))

silt_df <- silt_df |> 
  mutate(mean = mean/10) |> 
  rename(silt = mean)

data_soil <- bind_cols(sand_df,clay_df,silt_df) |> 
  select(3,5,8,2) |> 
  rename(sitio = 1) |> 
  as_tibble()

iden_soil <- function(v){
  d <- dist(rbind(v,mc_data_vwc_parameters[,6:8]))
  n <- d |> as.matrix() |> _[-1,1] |> which.min() |> as.numeric()
  mc_data_vwc_parameters$soiltype[n]
}

data_soil$type <- apply(data_soil[,-1],1,iden_soil)

data_toms <- read_rds('data/processed_data/datos_tomsT4_raw_promedio_x_sitio.rds')

data_toms <- data_toms |> mutate(sensor = 'moist')

data_final <- full_join(data_toms,data_soil,by = c('Sitio' = 'sitio'))
mc_calc_vwc(data_final,moist_sensor = 'moist',localities = 'Sitio',soiltype = 'type')

data_final |> 
  select(Fecha2,Sitio,value,sensor,type) |> 
  mutate(Fecha2 = as_datetime(Fecha2)) |> 
  mc_read_wide()
