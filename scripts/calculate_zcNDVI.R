# Script to calculate zcNDVI
# by frzambra
# April 2022

product <- 'NDVI.MOD13Q1.006/'

dir <- paste0('/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/',product)
dir.ph <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/Phenology/'

library(stars)
library(terra)
library(sf)
library(snow)

pol <- st_read('data/spatial/chile_continental.gpkg')
pol <- st_transform(pol,32719)
lf <- list.files(dir,pattern='*.tif$', full.names=TRUE)
#
ysize = 150
sliceY <- seq(3580,22015,ysize)

source('R/functions/cumRast.R')
source('R/functions/getPerPixel.R')
source('R/functions/smoothNDVI.R')

sos <- st_as_stars(disagg(rast(file.path(dir.ph,'SOS.Chile.S1.500m.tif')),2))
eos <- st_as_stars(disagg(rast(file.path(dir.ph,'EOS.Chile.S1.500m.tif')),2))

cl <- makeCluster(11,"SOCK")

for (i in 1:length(sliceY)){
  rasterio <- list(nXOff = 6088, nYOff = sliceY[i]+1, nXSize = 3081, nYSize = ysize)
  
  ndvi <- read_stars(lf, along ='dates',RasterIO = rasterio)
  sosC <- st_crop(sos,ndvi)
  eosC <- st_crop(eos,ndvi)
  
  dates <- as.Date(substr(lf,67,74),"%Y%j")
  
  message('Suavizado NDVI...')
  NDVI_smooth <- st_apply(ndvi,1:2,smoothNDVI,n=5,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  NDVI_smooth <- st_as_stars(NDVI_smooth) 
  NDVI_smooth <- merge(split(NDVI_smooth,'dates'),name='dates')
  esNDVI <- c(c(sosC,eosC,along = 'dates'),NDVI_smooth, along = 'dates')
  
  rm(NDVI_smooth)
  gc()
  
  message('Acmulado NDVI..')
  cNDVI <- st_apply(esNDVI,1:2,cumRast,dates = dates, step = 16,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  cNDVI <- merge(split(cNDVI,'dates'),name='dates')
  
  rm(esNDVI)
  gc()
  
  message('Obteniendo cNDVI en SOS..')
  secNDVI <- c(c(sosC,eosC,along ='dates'),cNDVI,along='dates')
  soscNDVI <- st_apply(secNDVI,1:2,getPerPixel,dates = dates, at='eos',lead=0,step = 16,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  soscNDVI <- merge(split(soscNDVI,'dates'),name='dates')
  
  rm(secNDVI)
  gc()
  
  message('Scale cNDVI en SOS..')
  zcNDVI <- st_apply(soscNDVI,1:2,function(x){scale(x)[,1]},CLUSTER = cl,PROGRESS = TRUE,.fname = 'zcNDVI')
  zcNDVI <- merge(split(zcNDVI,'zcNDVI'),name='zcNDVI')

  
  saveRDS(zcNDVI,paste0('~/.starstemp/zcNDVI_',i,'.rds'))
  rm(zcNDVI)
  gc()
}

stopCluster(cl)
