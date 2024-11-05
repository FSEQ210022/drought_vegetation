library(blackmarbler)
library(geodata)
library(tidyterra)
library(ggplot2)

token<- Sys.getenv('token_nasa')

chl <- gadm('chl',path = tempdir())

nasa_ln <- lapply(2012:2023,\(year){
  bm_raster(roi_sf = chl,
            product_id = "VNP46A4",
            date = year,
            bearer = token,
            check_all_tiles_exist = FALSE)
})

nasa_ln[[1]] <- crop(nasa_ln[[1]],ext(nasa_ln[[2]])) 

nasa_ln_join <- rast(nasa_ln)
nasa_ln_join <- mask(nasa_ln_join,chl)

dir_out <- '/mnt/md0/raster_raw/VNP46A4/'

lapply(seq_along(2012:2023),\(i){
  writeRaster(subset(nasa_ln_join,i),paste0(dir_out,'VNP46A4_Chile_',names(nasa_ln_join[[i]]),'.tif'))
})