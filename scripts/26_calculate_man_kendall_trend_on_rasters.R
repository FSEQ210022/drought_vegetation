library(trend)
library(terra)
library(sf)
library(fs)
library(glue)
library(purrr)

source('R/trend_func.R')
dir <- '/mnt/md0/raster_procesada/MODIS/'

indices <- c('EDDI','SPI','SPEI','zcSM','zcNDVI')
scales <- c(1,3,6,12,24,36)
scales_ndvi <- c(1,3,6,12)

map(indices,\(index){
  if( index == 'zcNDVI') scales <- scales_ndvi
  
  map(scales,\(scale){
      if (index == 'zcNDVI'){
        dir <- glue::glue('/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/{index}-{scale}')
      } else dir <- glue::glue('/mnt/md0/raster_procesada/ERA5-Land_tiff/sequia/{index}/{index}-{scale}')
      
      files <- dir_ls(dir,regexp = 'tif$')
      index_ts <- rast(files)
      cl <- snow::makeCluster(80,"SOCK")
      snow::clusterExport(cl, list = c('trend_func','mk.test','sens.slope'))
      trend <- app(index_ts,trend_func,cores = cl,start = c(2002,2),end =c(2023,10))
      snow::stopCluster(cl)
      writeRaster(trend,glue('/mnt/md0/raster_procesada/analysis/trends/trend_mann_kendall_{index}-{scale}.tif'),overwrite = TRUE)
      
    })
})
