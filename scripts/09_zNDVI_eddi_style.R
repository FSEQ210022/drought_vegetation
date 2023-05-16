

### anomalia

library(fs)
library(stars)
library(stringr)
library(lubridate)
dir_in <- '/mnt/md0/MODIS/NDVI_loess.MOD13A3.061/'

files <- dir_ls(dir_in)
dates <- as.Date(str_extract(files,'[0-9]{7}'),'%Y%j')

yj <- format(dates,'%Y%j')
dir_out <- '/mnt/md0/MODIS/zNDVI_loess.MOD13A3.061/'

new_names <- paste0(dir_out,'chl_',yj,'_1_km_monthly_zNDVI_loess.tif')

indxs <- lapply(1:12,\(i) which(month(dates) == i))

for (i in seq_along(indxs)){
  library(snow)
  
  ndvi <- read_stars(files[indxs[[i]]], along ='dates')
  ndvi <- st_set_dimensions(ndvi,'dates', values= dates[indxs[[i]]])
  
  cl <- makeCluster(80,"SOCK")
  clusterExport(cl, list = c('eddi','pe'))
  zNDVI <- st_apply(X = ndvi,MARGIN = 1:2,FUN = \(x) eddi(x)$EDDI,PROGRESS = TRUE, CLUSTER = cl, .fname = 'dates')
  stopCluster(cl)
  zNDVI <- aperm(zNDVI,c(2,3,1))
  
  lapply(1:dim(zNDVI)[3],\(j){
    write_stars(zNDVI[,,,j],dsn = new_names[indxs[[i]]][j])
    })
}
