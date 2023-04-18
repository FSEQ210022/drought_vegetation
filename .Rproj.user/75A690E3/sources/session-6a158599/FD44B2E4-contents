#Script to calculate the acumulatted value between SOS and EOS
# using {stars}

type <- 'ET'

dir.et <- paste0('/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/',type,'.MOD16A2.006/')
dir.ph <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/Phenology/'

library(stars)
library(sf)

pol <- st_read('data/spatial/gpkg/chile_continental.gpkg')
pol <- st_transform(pol,32719)
lf <- list.files(dir.et,pattern='*.tif$', full.names=TRUE)
et <- read_stars(lf[1])

sliceY <- seq(1790,10837,501)

i<- 1

source('R/functions/cumRast.R')
source('R/functions/getPerPixel_v2.R')

lapply(c(19),function(i){
  rasterio <- list(nXOff = 3044, nYOff = sliceY[i]+1, nXSize = 1541, nYSize = 500)
  
  et <- read_stars(lf, along ='dates',RasterIO = rasterio)
  #et <- merge(et,name = 'dates')
  
  sos <- read_stars(file.path(dir.ph,'SOS.Chile.S1.500m.tif'),RasterIO = rasterio)
  eos <- read_stars(file.path(dir.ph,'EOS.Chile.S1.500m.tif'),RasterIO = rasterio)
  
  esET <- c(c(sos,eos,along='dates'),et,along = 'dates')
  
  dates <- as.Date(substr(lf,65,71),"%Y%j")
  
  library(snow)
  
  cl <- makeCluster(11, type = "SOCK")
  system.time(
    cET <- st_apply(esET,1:2,cumRast,dates = dates, step = 8,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  )
  
  cET <- merge(split(cET,'dates'),name='dates')
  
  system.time(
    secET <- c(c(sos,eos,along ='dates'),cET,along='dates'),
  )
  
  inds <- lapply(sprintf("%03d",seq(1,365,8)),function(i) grep(paste0('[0-9]{4}',i),lf))
  
  soscET <- st_apply(secET,1:2,getPerPixel,dates = dates, at='eos',lead=0,step = 8,CLUSTER = cl,PROGRESS = TRUE,.fname = 'dates')
  
  soscET <- merge(split(soscET,'dates'),name='dates')
  
  zcET <- st_apply(soscET,1:2,function(x){
   scale(x)[,1]
  },.fname = 'zcET')
  
  zcET <- merge(split(zcET,'zcET'),name='zcET')
  
  saveRDS(zcET,paste0('~/Descargas/stars/zcET_',i,'.rds'))
  rm(cET,eos,esET,soscET,et)
  gc()
})
