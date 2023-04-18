
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

sos <- st_as_stars(disagg(rast(file.path(dir.ph,'SOS.Chile.S1.500m.tif')),2))
eos <- st_as_stars(disagg(rast(file.path(dir.ph,'EOS.Chile.S1.500m.tif')),2))

cl <- makeCluster(11,"SOCK")

lapply(1:length(sliceY),function(i){
  ndvi <- read_stars(paste0('~/.starstemp/.smoothNDVI/NDVI_smooth',i,'.tif'),proxy = TRUE)
  #ndvi <- st_as_stars(ndvi)
  sosC <- st_crop(sos,ndvi)
  eosC <- st_as_stars(st_crop(eos,ndvi))
  
  dates <- as.Date(substr(lf,67,74),"%Y%j")
  
  esNDVI <- c(c(sosC,eosC,along = 'dates'),ndvi, along = 'dates')
  rm(ndvi)
  gc()
  
  message('Acmulado NDVI..')
  cNDVI <- st_apply(ndvi,1:2,cumRast,dates = dates, step = 16,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  cNDVI <- merge(split(cNDVI,'dates'),name='dates')
})