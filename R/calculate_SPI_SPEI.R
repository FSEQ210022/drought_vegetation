
library(stars)
library(fs)
library(glue)
library(terra)

dir_out <- '/mnt/md0/CHELSA_v2.1/monthly/'

lf_pet <- dir_ls(glue('{dir_out}/pet'),regexp = 'tif$')
lf_pr <- dir_ls(glue('{dir_out}/pre'),regexp = 'tif$')[2:484]

rasterio <- list(nXOff = 1522, nYOff = sliceY[i]+1, nXSize = 771, nYSize = 1536)

rasio <- c(nxOff = 1,nyOff=1,nXSize = 100, nYSize=100)
pet <- read_stars(lf_pet, along = 'dates',RasterIO = rasio)
             
pre <- read_stars(lf_pr, along = 'dates',RasterIO = rasio)

D <- pre - 0.01*pet
D <- st_as_stars(D)

library(SPEI)
library(snow)
library(glue)
speiRast <- function(x,scale){
  as.numeric(spei(x,scale,na.rm = TRUE)$fitted)
}

library(snow)
library(parallel)
cl <- makeCluster(11, type = "SOCK")
clusterEvalQ(cl,c(library(SPEI)))

scales <- c(3,6,9,12,24)
library(purrr)

map(scales,function(scale){
  cl <- makeCluster(11, type = "SOCK")
  clusterEvalQ(cl,c(library(SPEI)))
  
  system.time(
    spei <- st_apply(D,1:2,speiRast,scale=scale, CLUSTER = cl,PROGRESS = TRUE,.fname = 'time')
  )
  saveRDS(spei,glue('SPEI-{scale}_chile_chelsa_v2.1_layer_top.rds'))
  stopCluster()
  rm(spei)
  gc()
})

###end
et <- rast(dir_ls(glue('{dir_out}/pet')))
pre <- rast(dir_ls(glue('{dir_out}/pre'))[2:484])

crs(et) <- crs(pre)
D <- pre-et
´D <- st_as_stars(D)
