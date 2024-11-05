library(fs)
library(terra)
library(tidyverse)

dir <- '/mnt/discoB/processed/MODIS/ET.MOD16A2GF.061/'
files <- dir_ls(dir,regexp = 'tif$')
dates <- as.Date(str_extract(files,'[0-9]{7}'),'%Y%j')

et <- rast(files)

ind <- paste0(year(dates),str_pad(month(dates),3,'left',pad='0'))

system.time(
  et_mes <- tapp(et,ind,sum,na.rm = TRUE)
)

dir_out <- '/mnt/discoB/processed/MODIS/ET.MOD16A2GF.061_monthly/'
dates_mes <- seq(ymd("20000101"),ymd("20231201"),by='1 month') |> format("%Y%m%d")

lapply(seq_along(dates_mes),\(i){
  writeRaster(subset(et_mes,i),paste0(dir_out,'ET.MOD16A2GF.061_monthly_',dates_mes[i],'.tif'),overwrite = TRUE)
})

