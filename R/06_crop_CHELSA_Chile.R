# Script to crop CHELSA for Chile
# by frzambra
# June 2022
library(stars)
library(fs)
library(glue)
library(stringr)
library(lubridate)
library(purrr)
library(terra)

dir <- '/mnt/HDD4TB_2/data/rasters/raw/monthly/'

files <- dir_ls(glue('{dir}pre/'),regexp = '*.tif$')
dir_out <- '/mnt/HDD4TB_2/data/rasters/Procesados/CHELSA_v2.1/monthly/'

map(files,function(file){
  pre <- read_stars(file,
                  RasterIO = list(nXOff = 12518, nYOff = 12180, nXSize = 1113, nYSize = 4619))

  mon_yr <- str_extract(str_split(file,'/',simplify = TRUE)[,9],'[0-9]{2}_[0-9]{4}')
  date <- str_remove_all(dmy(glue('01_{mon_yr}')),'-')
  prec <- tryCatch(suppressWarnings(st_as_stars(pre)),
                   error = function(e) message(glue('File {file} con error')))
  if (class(prec) != 'character' & !is.null(prec))
    writeRaster(rast(prec),glue('{dir_out}pet/CHELSA_pet_v2.1_{date}.tif'),overwrite = TRUE)
})
  
makeName <- function(file,grxpr='[0-9]{2}_[0-9]{4}'){
  spl <- str_split(file,'/',simplify = TRUE)
  name <- str_extract(spl[,dim(spl)[2]],grxpr)
  
  nam <- tryCatch(
    str_remove_all(dmy(glue('01_{name}')),'-'),
    warning = function(w) 'warning'
    )
  if (nam == 'warning') return(name) else return(nam)  
}

#  llenado imagenes con error

names1 <- map_chr(names(files),makeName)
names2 <- map_chr(names(dir_ls(glue('{dir_out}/pre/'))),makeName,grxpr = '[0-9]{8}')

setdiff(names1,names2)

pre <- rast(dir_ls(glue('{dir_out}/pre/'))[375:376])
pre <- app(pre,fun='mean')

writeRaster(pre,glue('{dir_out}/pre/CHELSA_pr_v2.1_20100401.tif'))
