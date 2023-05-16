library(tmap)
library(fs)
library(sf)
library(stars)
library(terra)
library(stringr)

dir_spei <- '/mnt/md0/CHELSA_v2.1/monthly/SPEI_nonpara/SPEI-12/'
dir_sma <- '/mnt/md0/SoilMoisture_1km_Chile/SMA_chl_0-100cm_monthly/'
dir_zndvi <- '/mnt/md0/MODIS/zNDVI_loess.MOD13A3.061/'

lf_spei <- dir_ls(dir_spei,regexp = 'tif$')[253:479]
lf_sma <- dir_ls(dir_sma,regexp = 'tif$')[230:456]
lf_zndvi <- dir_ls(dir_zndvi,regexp = 'tif$')

chl_sf <- vect('data/processed_data/spatial/chile_continental.gpkg') |> 
  project('EPSG:32719')

dates <- as.Date(str_extract(lf_spei,'[0-9]{7}'),'%Y%j')

lapply(seq_along(lf_spei),function(i){
  
  zndvi <- rast(lf_zndvi[i])
  zndvi <- mask(zndvi,chl_sf)
  spei <- rast(lf_spei[i])
  spei <- mask(spei,project(chl_sf,'EPSG:4326'))
  sma <- rast(lf_sma[i])
  
  library(basemaps)
  bm <- basemap_terra(chl_sf,map_service = 'carto',map_type = 'light')
  
  df_text <- data.frame(lat=-20,lon=-74,text = dates[i]) |> st_as_sf(coords = c('lon','lat'),crs=4326)
  
  m1 <- tm_shape(bm)+
    tm_rgb()+
    tm_shape(spei) +
    tm_raster(style ='fixed',palette = 'RdYlBu',breaks=c(-3,-2,-1,0,1,2,3),title = 'SPEI-12',legend.is.portrait = TRUE) +
    tm_shape(df_text) + 
    tm_text('text',size=1.5)+
    tm_logo(file ='~/Descargas/logo.png',position = 'left',height=5) +
    #tm_credits("fecha",position = c('left','top'))+
    tm_layout(legend.outside = TRUE,frame = FALSE,legend.outside.position = 'left')
  
  m2 <-  tm_shape(bm)+
    tm_rgb()+
    tm_shape(sma) +
    tm_raster(style ='fixed',palette = 'RdYlBu',breaks=c(-3,-2,-1,0,1,2,3),title = 'SMA',legend.is.portrait = TRUE) +
    #tm_logo(file ='~/Descargas/logo.png',position = 'left',height=2) +
    tm_layout(legend.outside = TRUE,frame = FALSE,legend.outside.position = 'left')
  
  m3 <-  tm_shape(bm)+
    tm_rgb()+
    tm_shape(zndvi) +
    tm_raster(style ='fixed',palette = 'RdYlGn',breaks=c(-2,-1.5,-0.5,0.5,1.5,2),title = 'zNDVI',legend.is.portrait = TRUE) +
    #tm_logo(file ='~/Descargas/logo.png',position = 'left',height=2) +
    tm_layout(legend.outside = TRUE,frame = FALSE,legend.outside.position = 'left')
  
  map <- tmap_arrange(m1,m2,m3)
  tmap_save(map,paste0(tempdir(),'/frame_',i,'.png'),width=16,height=9,scale=5)
})

system(glue::glue('ffmpeg -framerate 6 -i {tempdir()}/frame_%d.png -pix_fmt yuv420p sequia_chile.mp4'))
