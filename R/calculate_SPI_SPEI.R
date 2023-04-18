
library(stars)
library(fs)
library(glue)
library(terra)
dir_out <- '/mnt/HDD4TB_2/data/rasters/Procesados/CHELSA_v2.1/monthly/'

rasio <-  list(nYOff = 1, nYSize = 1000)

et <- read_stars(dir_ls(glue('{dir_out}/pet')), along = 'time',proxy = TRUE,
                 RasterIO = rasio)
                 
pre <- read_stars(dir_ls(glue('{dir_out}/pre'))[2:484], along = 'time',proxy = TRUE,
                  RasterIO = rasio)

D <- pre-et
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
