# Script to calculate smooth NDVI
# by frzambra
# April 2022

product <- 'NDVI.MOD13A3.061/'

dir <- paste0('/mnt/md0/raster_procesada/MODIS/',product)

library(stars)
library(terra)
library(sf)
library(snow)

source('R/functions/smoothNDVI.R')

pol <- st_read('data/spatial/chile_continental.gpkg')
pol <- st_transform(pol,32719)
lf <- list.files(dir,pattern='*.tif$', full.names=TRUE)
#
ysize = 500
sliceY <- seq(3580,22015,ysize)

source('R/functions/smoothNDVI.R')

cl <- makeCluster(11,"SOCK")

lapply(1:length(sliceY),function(i){
  rasterio <- list(nXOff = 6088, nYOff = sliceY[i]+1, nXSize = 3081, nYSize = ysize)
  
  ndvi <- read_stars(lf, along ='dates',RasterIO = rasterio)
  dates <- as.Date(substr(lf,67,74),"%Y%j")
  
  message('Suavizado NDVI...')
  NDVI_smooth <- st_apply(ndvi,1:2,smoothNDVI,n=5,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  NDVI_smooth <- st_as_stars(NDVI_smooth) 
  NDVI_smooth <- merge(split(NDVI_smooth,'dates'),name='dates')
  #saveRDS(NDVI_smooth,paste0('~/.starstemp/.smoothNDVI/NDVI_smooth',i,'.rds'))
  write_stars(NDVI_smooth,paste0('~/.starstemp/.smoothNDVI/NDVI_smooth',i,'.tif'),
              type = 'Int16',options =c('co'='COMPRESS=DEFLATE'))
  rm(NDVI_smooth)
  gc()
})

stopCluster(cl)
