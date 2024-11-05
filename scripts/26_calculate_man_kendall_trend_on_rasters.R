library(trend)
library(terra)
library(sf)
library(fs)
library(glue)
library(purrr)
library(modifiedmk)

source('R/trend_func.R')
dir <- '/mnt/md0/raster_procesada/MODIS/'

indices <- c('EDDI','SPI','SPEI','zcSM','zcNDVI','zcET')
scales <- c(1,3,6,12,24,36)

map(indices,\(index){
  if( index == 'zcNDVI') scales <- c(1,3,6,12)
  if (index == 'zcET') scales <- 12
  
  map(scales,\(scale){
      if (index == 'zcNDVI' | index == 'zcET'){
        dir <- glue::glue('/mnt/md0/raster_procesada/MODIS_derived/{index}/{index}-{scale}')
      } else dir <- glue::glue('/mnt/md0/raster_procesada/ERA5-Land_tiff/sequia/{index}/{index}-{scale}')
      
      files <- dir_ls(dir,regexp = paste0('(',paste(2000:2023,collapse = '|'),').*tif$'))
      index_ts <- rast(files)
      cl <- snow::makeCluster(80,"SOCK")
      snow::clusterExport(cl, list = c('trend_func2','mmky'))
      trend <- app(index_ts,trend_func2,cores = cl)
      snow::stopCluster(cl)
      writeRaster(trend,glue('/mnt/md0/raster_procesada/analysis/trends/trend_mann_kendall_mod_{index}-{scale}.tif'),overwrite = TRUE)
      
    })
})
