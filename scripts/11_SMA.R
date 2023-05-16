

### anomalia

library(fs)
library(stars)
library(stringr)
library(lubridate)
dir_in <- '/mnt/md0/SoilMoisture_1km_Chile/regresion/swc_chl_0-100cm_monthly/'

files <- dir_ls(dir_in)
dates <- as.Date(str_extract(files,'[0-9]{8}'),'%Y%m%d')

yj <- format(dates,'%Y%j')
dir_out <- '/mnt/md0/SoilMoisture_1km_Chile/SMA_chl_0-100cm_monthly//'

new_names <- paste0(dir_out,'chl_',yj,'_1_km_monthly_SMA.tif')

indxs <- lapply(1:12,\(i) which(month(dates) == i))

for (i in seq_along(indxs)){
  library(snow)
  
  sma <- read_stars(files[indxs[[i]]], along ='dates')
  sma <- st_set_dimensions(sma,'dates', values= dates[indxs[[i]]])
  
  cl <- makeCluster(80,"SOCK")
  clusterExport(cl, list = c('eddi','pe'))
  sma <- st_apply(X = sma,MARGIN = 1:2,FUN = \(x) eddi(x)$EDDI,PROGRESS = TRUE, CLUSTER = cl, .fname = 'dates')
  stopCluster(cl)
  sma <- aperm(sma,c(2,3,1))
  
  lapply(1:dim(sma)[3],\(j){
    write_stars(sma[,,,j],dsn = new_names[indxs[[i]]][j])
  })
}
