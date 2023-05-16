
library(fs)
library(stars)
library(snow)
library(tictoc)

dir <- '/mnt/md0/SoilMoisture_1km_Chile/regresion/'

lyrs <- paste0('swc_chl_layer',1:3,'_monthly')

swlyr1 <- dir_ls(paste0(dir,lyrs[1]))
swlyr2 <- dir_ls(paste0(dir,lyrs[2]))
swlyr3 <- dir_ls(paste0(dir,lyrs[3]))

dir_out <- '/mnt/md0/SoilMoisture_1km_Chile/regresion/swc_chl_0-100cm_monthly/'

new_names <- paste0(dir_out,str_split_i(swlyr1,'/',7))

lapply(seq_along(swlyr1),\(i){
  sm <- read_stars(c(swlyr1[i],swlyr2[i],swlyr3[i]),along = 'layer')
  sm <- st_set_dimensions(sm,'layer',values = c('layer1','layer2','layer3'))
  tic()
  sm_mean <- st_apply(sm,1:2,mean)
  write_stars(sm_mean,dsn = new_names[i])
  toc()
})
