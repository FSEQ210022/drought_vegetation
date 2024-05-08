library(terra)
library(tidyverse)
library(fs)
library(rnaturalearth)

dir <- '/mnt/md0/raster_procesada/MODIS/'
dir2 <- '/mnt/md0/raster_procesada/MODIS_derived/zcNDVI/'

chl <- ne_countries(country = 'chile',scale = 'small',returnclass = 'sf') |> 
  st_transform(32719)

files_npp <- dir_ls(paste0(dir,'NPP.MOD17A3HGF.061/'),regexp = 'tif$')

npp <- rast(files_npp)
npp <- crop(npp,chl)
npp[npp >30000] <- NA

#detrend NPP

# npp_det <- app(npp,\(val){
#   df <- data.frame(x=1:length(val),y=val)
#   df <- na.omit(df)
#   
#   if(nrow(df) > 10){
#     mod <- lm(y~x,df)
#     out <- df$y-mod$fitted.values
#   } else out <- rep(NA,length(val))
#   out
# },cores=80)

#remove year 2014
npp <- npp[[-14]]

out <- lapply(c(1,3,6,12),\(scale){
  files_zcndvi <- dir_ls(paste0(dir2,glue::glue('zcNDVI-{scale}/')),regexp = 'tif$')
    ind <- basename(files_zcndvi) |> 
    str_extract('[0-9]{4}-[0-9]{2}-[0-9]{2}') |> 
    ymd() |> 
    month() |> 
    (\(x) which(x == 12))()
  
  zcndvi <- rast(files_zcndvi[ind][-1])
  zcndvi <- zcndvi[[-14]] 
  zcndvi <- crop(zcndvi,chl)
  
  npp <- resample(npp,zcndvi)
  
  x <- c(zcndvi,npp)
  r <-  app(x,fun =\(x){
    df <- data.frame(X = x[1:22], Y=x[23:length(x)])
    df <- na.omit(df)
    if(nrow(df) > 10){
      mod <- lm(Y~X,df)
      r <- cor(df$X,df$Y)
      r2 <- summary(mod)$r.squared
      pvalue <- summary(mod)$coefficients[2,4]
      sign <- ifelse(pvalue < 0.05,1,0)
      out <- c(r,r2,pvalue,sign)
    } else out <- c(NA,NA,NA,NA)
    
    return(out)
    #cor(x[1:284],x[285:length(x)],use = 'na.or.complete')
  },cores = 60)
  return(r)
})

r2 <- lapply(out, \(x) {
  m <- x[[4]]
  m[m == 0] <- NA
  mask(x[[2]],m)
  })
r2 <- rast(r2)

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif')
lc_res <- resample(lc,r2,method='mode')

r2_land <- zonal(r2,lc_res,na.rm = TRUE)

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') 

r2_land |> 
  mutate(landcover = paleta$Name) |> 
  write_rds('data/processed_data/r2_npp_vs_zcndvi.rds')

r2_land |> 
  mutate(landcover = paleta$Name) |> 
  dplyr::filter(landcover %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  dplyr::select(6,2:5) |>
  set_names(c('landcover',1,3,6,12)) |> 
  pivot_longer(-landcover,names_to = 'scale') |> 
  mutate(scale = factor(scale,levels = c('1','3','6','12'))) |> 
  ggplot(aes(scale,landcover,fill = value)) + 
  geom_tile() +
  geom_text(aes(label = round(value,2))) +
  scale_fill_viridis_c(name = 'r-squared') + 
  theme_bw() + 
  theme(axis.title = element_blank())
ggsave('output/figs/heatmap_r2_npp_vs_zcndvi_per_landcover.png',scale=2)  

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> st_transform(32719)
df_cov <- extract(cov,macro)

df_cov |> 
  ggplot(aes(factor(ID),lyr.1)) + geom_boxplot()

df_zcndvi <- extract(zcndvi,macro,fun='mean',na.rm = TRUE)

df_zcndvi |> 
  pivot_longer(-ID) |> 
  mutate(ID = factor(ID)) |> 
  ggplot(aes(ID,value)) + geom_boxplot()

library(tmap)

map <- tm_shape(r2) + 
  tm_raster(style = 'cont',palette = 'viridis',title='r-squared') + 
  tm_shape(chl) + 
  tm_borders() +
  tm_layout(panel.labels = c('zcNDVI-1','zcNDVI-3','zcNDVI-6','zcNDVI-12'))
tmap_save(map,'output/figs/map_r2_NPP_vs_zcNDVI.png',scale=1)
