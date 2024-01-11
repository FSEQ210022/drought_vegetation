library(fs)
library(sf)
library(terra)
library(tidyverse)
library(geodata)

## Extraer tabla de correlaciones para macrozonas

elev <- geodata::elevation_30s('chile',path = tempdir()) |> 
  project('EPSG:32719')

dir <- '/mnt/md0/raster_procesada/analysis/correlations'
files <- dir_ls(dir,regexp = 'tif$')

elev2 <- resample(elev,rast(files[1]))
mask <- elev2
mask[mask<1500] <- 1
mask[mask>=1500] <- NA

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  st_transform(32719)


d <- map_df(files,\(file){
  index <- rast(file)
  index <- mask(index,mask)
  df_scales <- terra::extract(index[[1]],macro,fun =\(x){
    table(x) |> sort(decreasing = TRUE) -> x
    as.numeric(names(x[1]))
  })
  
  df_r <- terra::extract(index[[2]],macro,mean,na.rm=TRUE)
  name_index <- str_remove_all(str_extract(file,'_[:alpha:]{4}_'),'_')
  data <- cbind(index = name_index ,
                st_drop_geometry(macro['macrozona']),
                time_scales = df_scales[,-1],
                r = df_r[,-1])
  data
}) |> as_tibble()

#guardar data.frame con los valores promedio de las correlaciones para cada indice en las macrozonas de Chile

d |> 
  mutate(index= recode(index, time='SPI'),
         time_scales = as.character(time_scales),
         time_scales = factor(time_scales,labels =  c("6","12","24","36"))) |> 
  arrange(desc(r)) |> 
  mutate(r2 = r^2) |> 
  write_rds('data/processed_data/df_correlaciones_indices_con_zcNDEVI6_macrozonas.rds')


## Extraer tabla de tendencias para macrozonas}

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') 
mask <- project(mask,"EPSG:4326")

dir <- '/mnt/md0/raster_procesada/analysis/trends'
files <- dir_ls(dir,regexp = 'mann_kendall.*tif$')
indices <- str_remove(str_remove(str_extract(files,'l_.*\\.'),'l_'),'\\.')
indices <- indices[-19]
trends <- rast(names(files)[-19])
trends <- trends[[seq(2,48,2)]]
names(trends) <- indices
mask <- resample(mask,trends)
trends <- mask(trends,mask)

df_scales_1 <- terra::extract(trends[[7:24]],macro,fun =\(x){
  table(x) |> sort(decreasing = TRUE) -> x
  as.numeric(names(x[1]))
})

df_scales_2 <- terra::extract(trends[[1:6]],macro,fun =\(x){
  table(x) |> sort() -> x
  as.numeric(names(x[1]))
})

#zcNDVI
trends <- rast(names(files)[19])
trends <- trends[[2]]
names(trends) <- 'zcNDVI-6'
trends <- resample(trends,mask)
macro <- st_transform(macro,32719)
df_scales_3 <- terra::extract(trends,macro,fun =\(x){
  table(x) |> sort(decreasing = TRUE) -> x
  as.numeric(names(x[1]))
})

df_scales <- bind_cols(st_drop_geometry(macro),df_scales_1,df_scales_2,df_scales_3)

#guardar data.frame con los valores promedio de las tendencias (mann-kendall) para cada indice en las macrozonas de Chile
df_scales |> 
  select(-starts_with('ID')) |> 
  pivot_longer(-c(macrozona)) |> 
  rename(index = name,trend = value) |> 
  write_rds('data/processed_data/df_trends_indices_macrozona.rds')



