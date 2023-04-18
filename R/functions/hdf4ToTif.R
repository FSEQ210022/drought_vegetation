# Script of convert HDF4 MODIS to GeoTiff
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# July 2019

hdf4ToTif <- function(dir.in,dir.out,band=1,crs="EPSG:4326",ext=NULL,...){
  
  require(purrr)
  require(gdalUtils)
  require(raster)
  require(stringr)
  
  files <- list.files(dir.in,full.names=TRUE)
  
  folder <- tmpDir(create = TRUE)
  
  1:8 %>% map(function(i){
    tryCatch(
      {
      gdal_translate(get_subdatasets(files[i])[band],
                     dst_dataset = paste0(folder,'MODIS_',i,'.tif'),
                     a_src = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
},
      error = function(e){
        message('Hay un error en un archivo en la carpeta')
        message(dir.in)
        return(NA)
      }
    )
  })
  
  name <- get_subdatasets(files[1])[band]
  pre <- strsplit(get_subdatasets(files[3])[band],":")[[1]][5]
  pre <- gsub(" ","_",pre)
  newName <- paste0('chl_',str_extract(name,'[0-9]{7}'),'_',pre)
  
  if (!is.null(crs)){
    # mosaic_rasters(list.files(folder,pattern='*.tif$',full.names=TRUE),
    #                dst_dataset = paste0(folder,'mosaico.tif'))
    r <- raster(list.files(folder,pattern='*.tif$',full.names=TRUE)[1])
    gdalwarp(list.files(folder,pattern='*.tif$',full.names=TRUE),
             dstfile=paste0(dir.out,newName,'.tif'),
             co = "COMPRESS=DEFLATE",
             t_srs=crs,
             tr = res(r),
             verbose = TRUE,
             overwrite=TRUE,...)
  } else {
    gdalwarp(list.files(folder,pattern='*.tif$',full.names=TRUE),
             dstfile=paste0(dir.out,newName,'.tif'),
             co = "COMPRESS=DEFLATE",
             verbose = TRUE,
             overwrite=TRUE,...)
  }
  file.remove(list.files(folder,full.names=TRUE))
  unlink(folder)
}

  
  
