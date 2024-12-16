library(trend)
library(terra)
library(sf)
library(fs)
library(glue)
library(purrr)
library(modifiedmk)
library(purrr)

source('R/trend_func.R')

indices <- c('EDDI','SPI','SPEI','SSI','zcNDVI','zcET','zcPET')

scales <- c(1,3,6,12,24,36)

map(indices,\(index){
  if( index == 'zcNDVI') scales <- c(1,3,6,12)
  #if (index == 'zcET') scales <- 12
  
  map(scales,\(scale){
      if (index %in% c('zcNDVI','zcET','zcPET')){
        dir <- glue::glue('/home/rstudio/discoB/processed/MODIS/{index}/{index}-{scale}')
      } else dir <- glue::glue('/home/rstudio/discoB/processed/ERA5-Land/sequia_2000-2023/monthly/{index}/{index}-{scale}')
      
      files <- dir_ls(dir,regexp = paste0('(',paste(2000:2023,collapse = '|'),').*tif$'))
      index_ts <- rast(files)
      cl <- snow::makeCluster(80,"SOCK")
      snow::clusterExport(cl, list = c('trend_func2','mmky'))
      trend <- app(index_ts,trend_func2,cores = cl)
      snow::stopCluster(cl)
      writeRaster(trend,glue('/home/rstudio/discoB/processed/analysis/trends/trend_mann_kendall_mod_{index}-{scale}.tif'),overwrite = TRUE)
      
    })
})
