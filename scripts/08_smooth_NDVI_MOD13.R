#calcular anonalía de vegetación para Chile 1km

library(sf)
library(fs)
library(terra)
library(rnaturalearth)
library(stringr)
library(lubridate)

dir_in <- '/mnt/md0/raster_procesada/MODIS/NDVI.MOD13A3.061/'

files <- dir_ls(dir_in,regexp = '2023')
dates <- as.Date(str_extract(files,'[0-9]{7}'),'%Y%j')

# test with stars
library(stars)

#ex <- st_bbox(c(xmin=197232,xmax=200300.1,ymin=5962741,ymax=5964844),crs=st_crs(32719))
# ex <- st_bbox(c(xmin=179099.465504802, xmax=652137.031027275, ymin=6129136.91515384, ymax=6541528.63894266),crs = st_crs(32719))
# ndvi_c <- st_crop(ndvi,ex)

#NDVI_smooth <- st_apply(X = ndvi_c,MARGIN = 1:2,FUN = \(x) smoothNDVI(y=x,x=as.numeric(dates),n=5), .fname = 'dates')

sliceY <- seq(895,5504,length.out=4)

library(tictoc)
library(snow)

#lapply(seq_along(sliceY)[-1],function(i){

for (i in seq_along(sliceY)[1:3]){
  rasterio <- list(nXOff = 1522, nYOff = sliceY[i]+1, nXSize = 771, nYSize = 1536)
  ndvi <- read_stars(files, along ='dates',RasterIO = rasterio)
  ndvi <- st_set_dimensions(ndvi,'dates', values= dates)
  tic()
  cl <- makeCluster(80,"SOCK")
  clusterExport(cl, list = c('dates','smoothNDVI'))
  NDVI_smooth <- st_apply(X = ndvi,MARGIN = 1:2,FUN = \(x) smoothNDVI(y=x,x=as.numeric(dates),n=5),PROGRESS = TRUE, CLUSTER = cl, .fname = 'dates')
  toc()
  stopCluster(cl)
  NDVI_smooth <- aperm(NDVI_smooth,c(2,3,1))
  saveRDS(NDVI_smooth,paste0(tempdir(),'/ndvism_',i,'.rds'))
  rm(ndvi,NDVI_smooth)
  gc()
}

#mosaico de 

files <- dir_ls(tempdir(),regexp = 'rds$')
m <- lapply(files,function(name){
  readRDS(name)
})
ndvism <- do.call(st_mosaic,m)

yj <- format(dates,'%Y%j')
dir_out <- '/mnt/md0/raster_procesada/MODIS_derived/NDVI_loess.MOD13A3.061/'

new_names <- paste0(dir_out,'chl_',yj,'_1_km_monthly_NDVI_loess.tif')

lapply(7:dim(ndvism)[3],\(i){
  write_stars(ndvism[,,,i],dsn = new_names[i],type = 'Int16')
})



