#calcular anonalía de vegetación para Chile 1km

library(sf)
library(fs)
library(terra)
library(rnaturalearth)
library(stringr)
library(lubridate)

dir_in <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/NDVI.MOD13A3.061/'

files <- dir_ls(dir_in)
dates <- as.Date(str_extract(files,'[0-9]{7}'),'%Y%j')

indxs <- lapply(1:12,\(i) which(month(dates) == i))
ndvi <- rast(files)

plot(ndvi[[1]])
#ex <- drawExtent()
ex <- ext(c(197232,200300.1,5962741,5964844))
# class      : Extent 
# xmin       : 197232 
# xmax       : 275300.1 
# ymin       : 5962741 
# ymax       : 6079844 

# test with terra
ndvi_c <- crop(ndvi,ex)

library(parallel)
cl <- makeCluster(8)
clusterExport (cl, varlist = c('eddi','pe'))

zNDVI <- lapply(indxs, \(ind){
  app(ndvi[[ind]],fun= \(x) eddi(x)$EDDI,cores = cl)
})

# test with stars
library(stars)
ndvi_st <- st_as_stars(ndvi)
ndvi_st <- st_set_dimensions(ndvi_st,'band', values= dates)

cl <- makeCluster(11,"SOCK")
clusterExport(cl, varlist = c('dates','smoothNDVI'))
NDVI_smooth <- st_apply(X = ndvi_st,MARGIN = 1:2,FUN = \(x) smoothNDVI(x,y=as.numeric(dates),n=5),PROGRESS = TRUE, CLUSTER = cl, .fname = 'dates')
