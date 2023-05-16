
library(stars)
library(fs)
library(glue)
library(terra)
library(stringr)

dir_out <- '/mnt/md0/CHELSA_v2.1/monthly/'

lf_pet <- dir_ls(glue('{dir_out}/pet'),regexp = 'tif$')
lf_pr <- dir_ls(glue('{dir_out}/pre'),regexp = 'tif$')[2:484]

#rasterio <- list(nXOff = 1522, nYOff = sliceY[i]+1, nXSize = 771, nYSize = 1536)

#pet <- read_stars(lf_pet, along = 'dates',RasterIO = rasio)
pet <- read_stars(lf_pet, along = 'dates')

#pre <- read_stars(lf_pr, along = 'dates',RasterIO = rasio)
pre <- read_stars(lf_pr, along = 'dates')

D <- pre
names(D) <- 'diff'
D$diff <- 0.01*pre$CHELSA_pr_v2.1_19790201.tif - pet$CHELSA_pet_v2.1_19790201.tif

library(snow)

dates <- format(as.Date(str_extract(lf_pet,'[0-9]{8}'),'%Y%m%d'),'%Y%j')

speis <- lapply(c(1,3,6,12,24),function(scale){
  cl <- makeCluster(80,"SOCK")
  clusterExport(cl, list = c('eddi','pe','calc_spei'))
  spei <- st_apply(D,1:2,calc_spei,scale = scale,CLUSTER = cl,PROGRESS = TRUE)
  stopCluster(cl)
  spei <- aperm(spei,c(2,3,1))
  
  lapply(1:dim(spei)[3],\(j){
    write_stars(spei[,,,j],dsn = glue('{dir_out}/SPEI_nonpara/SPEI-{scale}/CHELSA_v2.1_SPEI{scale}_chile_{dates[j]}.tif'))
  })
  rm(spei)
  gc()
})




