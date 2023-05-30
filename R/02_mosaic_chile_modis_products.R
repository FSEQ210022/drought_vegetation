# Script to convert MODIS product from .hdf to tif and make the mosaic for continental Chile
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# November 2019

require(purrr)
#Product MOD16A2

source('R/functions/dates2Update.R')
source('R/functions/hdf4ToTif.R')

# ET band=1
dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD44B.006'
list.files(dir)

dirs <- list.files(dir,full.names=TRUE)
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/Perc_NonVeg.MOD44B.006/'

datesUpdate <- dates2Update(dir.out,dirs)

file.path(dir,datesUpdate,'/') %>% map(function(dir){
  hdf4ToTif(dir,dir.out,crs="EPSG:32719",band=3)
})

# ET band=1
dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD16A2.006'
list.files(dir)

dirs <- list.files(dir,full.names=TRUE)
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/ET.MOD16A2.006/'

datesUpdate <- dates2Update(dir.out,dirs)

file.path(dir,datesUpdate,'/') %>% map(function(dir){
  hdf4ToTif(dir,dir.out,crs="EPSG:32719",band=1)
})

# PET band=3
dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD16A2.006'
list.files(dir)

dirs <- list.files(dir,full.names=TRUE)
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/PET.MOD16A2.006/'

datesUpdate <- dates2Update(dir.out,dirs)

file.path(dir,datesUpdate,'/') %>% map(function(dir){
  hdf4ToTif(dir,dir.out,crs="EPSG:32719",band=3)
})

#Product MOD13Q1

dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD13Q1.061/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/NDVI.MOD13Q1.061/'
dirs <- list.files(dir,full.names=TRUE)

datesUpdate <- dates2Update(dir.out,dirs)

file.path(dir,datesUpdate,'/') %>% map(function(dir){
  hdf4ToTif(dir,dir.out,crs="EPSG:32719")
})

#Product MOD13A1

dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD13A1.006/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/NDVI.MOD13A1.006/'
dirs <- list.files(dir,full.names=TRUE)

datesUpdate <- dates2Update(dir.out,dirs)

file.path(dir,datesUpdate,'/') %>% map(function(dir){
  hdf4ToTif(dir,dir.out,crs="EPSG:32719")
})

#Product MOD13A2

dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD13A2.006/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/NDVI.MOD13A2.006/'
dirs <- list.files(dir,full.names=TRUE)

datesUpdate <- dates2Update(dir.out,dirs)

file.path(dir,datesUpdate,'/') %>% map(function(dir){
  hdf4ToTif(dir,dir.out,crs="EPSG:32719")
})

#Product MCD12Q1.061

dir <- '/mnt/raster_raw/MODIS/MCD12Q1.061/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/IGBP.MCD12Q1.061/'
dirs <- list.files(dir,full.names=TRUE)

dirs %>% map(function(dir){
  hdf4ToTif(dir,dir.out,band=1,crs="EPSG:32719")
})

#Product MCD12Q2.061

# Cycle number
dir <- '/mnt/raster_raw/MODIS/MCD12Q2.061/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/NumCycles.MCD12Q2.061/'
dirs <- list.files(dir,full.names=TRUE)

dirs %>% map(function(dir){
  hdf4ToTif(dir,dir.out,band=1,crs="EPSG:32719")
})

# Greenup
dir <- '/mnt/raster_raw/MODIS/MCD12Q2.061/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/Greenup.MCD12Q2.061/'
dirs <- list.files(dir,full.names=TRUE)

dirs %>% map(function(dir){
  hdf4ToTif(dir,dir.out,band=2,crs="EPSG:32719")
})

# Dormancy
dir <- '/mnt/raster_raw/MODIS/MCD12Q2.061/'
dir.out <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/Dormancy.MCD12Q2.061/'
dirs <- list.files(dir,full.names=TRUE)

dirs %>% map(function(dir){
  hdf4ToTif(dir,dir.out,band=8,crs="EPSG:32719")
})

