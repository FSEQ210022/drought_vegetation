# Script o mosaic stars chunks
#

lf <- list.files('~/.starstemp/',full.names = TRUE)

product <- 'zcNDVI.MOD13Q1.006.chl'
library(stars)
season <- paste0(2001:2021,'-',2002:2022)
dir.out <- paste0('/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/',product)

lapply(1:20,function(i){
  m <- lapply(lf,function(name){
    readRDS(name)[,,,i]
  })


  m <- do.call(st_mosaic,m)

  write_stars(m,dsn=paste0(dir.out,'/',product,'.',season[i],'.tif'))
})


