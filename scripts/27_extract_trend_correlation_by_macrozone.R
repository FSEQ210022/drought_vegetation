library(fs)
library(sf)
library(terra)
library(tidyverse)
library(geodata)

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
andes <- read_sf('~/Descargas/andes-mountains_1256.geojson') |> 
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

d |> 
  mutate(index= recode(index, time='SPI'),
         time_scales = as.character(time_scales),
         time_scales = factor(time_scales,labels =  c("6","12","24","36"))) |> 
  arrange(desc(r)) |> 
  mutate(r2 = r^2)
