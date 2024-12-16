library(fs)
library(terra)
library(tidyverse)

dir <- '/home/rstudio/discoB/processed/MODIS/PET.MOD16A2GF.061/'
files <- dir_ls(dir,regexp = 'tif$')
dates <- as.Date(str_extract(files,'[0-9]{7}'),'%Y%j')

et <- rast(files)

ind <- paste0(year(dates),str_pad(month(dates),3,'left',pad='0'))

cl <- parallel::makeCluster(80,"SOCK")
tictoc::tic()
et_mes <- tapp(et,ind,sum,na.rm = TRUE,cores = cl)
tictoc::toc()
parallel::stopCluster(cl)

dir_out <- '/home/rstudio/discoB/processed/MODIS/PET.MOD16A2GF.061_monthly/'
dates_mes <- seq(ymd("20000101"),ymd("20231201"),by='1 month') |> format("%Y%m%d")

lapply(34:288,\(i){
  writeRaster(subset(et_mes,i),paste0(dir_out,'PET.MOD16A2GF.061_monthly_',dates_mes[i],'.tif'),overwrite = TRUE)
})

