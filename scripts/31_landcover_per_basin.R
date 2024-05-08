# Analyzing landcover persistent (80%) per zone
# August 2021
# by frzambra

library(sf)
library(terra)
library(rnaturalearth)
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land'))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

# 
pal <- read.csv('data/processed_data/paleta_colores_landcover.csv')
t <- pal$class
names(t) <- pal$Name

chl <- ne_countries(country = 'chile',returnclass = 'sf') |> 
  st_transform(32719)

dir <- '/mnt/md0/raster_procesada/MODIS/'

igbp <- rast(list.files(paste0(dir,'IGBP.MCD12Q1.061'),pattern='*.tif$',full.names=TRUE))
igbp <- crop(igbp,chl)

classes <- matrix(c(1:17,rep(1,5),rep(2,2),rep(3,2),4,5,6,7,6,8,9,10),ncol=2)

igbpRecl <- classify(igbp,rcl=classes,include.lowest = TRUE)
igbpRecl[igbpRecl == 0] <- NA

cuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg')

data <- terra::extract(igbpRecl,cuencas)
data <- data |> 
  repair_names() |> 
  cbind(cuencas[data$ID,'COD_SUBC']) |> 
  select(-geom,-ID) |> 
  relocate("COD_SUBC",.before = "Land_Cover_Type_1") 
names(data) <- c('cuenca',2001:2022)

library(dplyr)
library(tidyr)
library(ggplot2)

data |> 
  pivot_longer(-cuenca,values_to = 'LC_type',names_to = 'year') |> 
  filter(LC_type %in% c(1:4,6,9)) |>  
  mutate(class2 = LC_type,
         year = as.numeric(year),
         cuenca = factor(cuenca),
         LC_type = factor(LC_type,levels=paleta$class,labels=paleta$Name)) -> data2 

data2 |> 
  group_by(cuenca) |>  
  filter(year == 2002) |>  
  summarize(ntot = n()) ->dataSum

data2 |> 
  group_by(cuenca,year,LC_type) |>  
  summarise(n=n()) |> 
  left_join(dataSum,by='cuenca') |>  
  mutate(prop = n/ntot,sup_km2=n*0.2146587) -> data3

saveRDS(data3,'data/processed_data/timeseries_landcover_cuencas_LCclass_2001-2022.rds')  

#linear trend Mann-Kendall
source('R/trend_func.R')
library(trend)

data3 |> 
  ungroup() |>  
  group_by(cuenca,LC_type) |>  
  #summarize(trend  = as.numeric(tidy(lm(sup_km2~year,data))[2,2])) |> 
  summarize(trend = trend_func(prop,start = c(2001,1),end =c(2022,1),frecuency=1)[2]) |> 
  pivot_wider(names_from = 'LC_type',values_from=3) |> 
  saveRDS('data/processed_data/trends_landcover_cuencas_2001-2022.rds')  
