library(sf)
library(rnaturalearth)
library(terra)
library(fs)
library(purrr)

chl <- ne_countries(country='chile',scale = 'medium',returnclass = 'sf') |> 
  st_transform(32719)

dir <- '/mnt/discoB/processed/MODIS/ET.MOD16A2GF.061_monthly/'
files <- dir_ls(dir,regexp = 'tif$')

ndvi <- rast('/mnt/discoB/processed/MODIS/NDVI.MOD13A3.061/chl_2016245_1_km_monthly_NDVI.tif') |> 
  mask(chl) |> 
  trim()

dir_out <- '/mnt/discoB/processed/MODIS/ET.MOD16A2GF.061_monthly_1km/'
map(files,\(file){
  rast(file) |> 
    mask(chl) |> 
    trim() |> 
    resample(ndvi) |> 
    writeRaster(filename = paste0(dir_out,basename(file[1])))
})


