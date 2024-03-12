library(terra)
library(fs)
library(rnaturalearth)
library(sf)

dir <- '/mnt/md0/raster_procesada/Landcover'
dir2 <- '/mnt/md0/raster_procesada/MODIS/IGBP.MCD12Q1.061'
lc_chile <- rast(dir_ls(dir,recurse = TRUE,type = 'file',regexp = 'tif$'))

lc_modis <- rast(dir_ls(dir2,regexp = '2013|2014'))
lc_modis <- crop(lc_modis,ext(lc_chile))

lc_chile <- resample(lc_chile,lc_modis,method = 'mode')

classes <- matrix(c(1:17,rep(2,5),rep(4,2),rep(4,2),3,5,1,8,1,10,9,6),ncol=2)

lc_modis_r <- classify(lc_modis,rcl=classes,include.lowest = TRUE)

lc2<- c(lc_modis_r,lc_chile)

chl <- ne_countries(country = 'chile',scale = 'small',returnclass = 'sf') |> 
  st_transform(32719)
samples <- st_sample(chl,1000)

df <- terra::extract(lc2,vect(samples))
names(df) <- c('ID','IGBP2013','IGBP2014','LC_CHILE')

f <- function(x){
  if(substr(x,2,2) == '0') substr(x,1,2) else substr(x,1,1)
}

library(tidyverse)

data <- df |> 
  filter(IGBP2013 != 5 & IGBP2014 != 5) |> 
  mutate(LC_CHILE_2 = substr(LC_CHILE,1,2)) |> 
  rowwise(LC_CHILE_2) |> 
  mutate(LC_CHILE_2 = f(LC_CHILE_2) |> as.numeric()) |> 
  select(-ID,-LC_CHILE) |> 
  ungroup() |> 
  mutate(LC_CHILE_2 = ifelse(LC_CHILE_2 == 0, 6,LC_CHILE_2),
         LC_CHILE_2 = ifelse(LC_CHILE_2 == 80, 8,LC_CHILE_2)) |> 
  mutate(across(everything(),\(x) fct(sort(as.character(x)))))

library(yardstick)
conf_mat(data,LC_CHILE_2,IGBP2014)  
accuracy(data,LC_CHILE_2,IGBP2014)  
f_meas(data,LC_CHILE_2,IGBP2014)  

accuracy(data,LC_CHILE_2,IGBP2013)  
f_meas(data,LC_CHILE_2,IGBP2013)  


roc_auc(data,truth = LC_CHILE_2,IGBP2013)  

data |> distinct(LC_CHILE_2)
data |> distinct(IGBP2013)
