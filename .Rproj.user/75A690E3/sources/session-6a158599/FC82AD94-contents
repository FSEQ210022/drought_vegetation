# Script to convert MODIS product from .hdf to tif and make the mosaic for continental Chile
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# November 2019

source('R/dates2Update.R')
source('R/hdf4ToTif.R')

mosaic_modisFromHDF <- function(dir_in,dir_out,lyr){
  
  dirs <- fs::dir_ls(dir_in)
  datesUpdate <- dates2Update(dir_out,dirs)

  names(datesUpdate) |> 
    purrr::map(safely(hdf4ToTif),dir_out,crs="EPSG:32719",band=lyr)
}
