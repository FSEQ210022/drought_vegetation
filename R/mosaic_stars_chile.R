# Script

library(stars)
library(sf)

dir <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/MOD13Q1.006/'

dirs <- list.files(dir,full.names = TRUE)

r <- lapply(dirs,function(dir){
  lf <- list.files(dir,full.names = TRUE,pattern = '.hdf$')
  
  q <- lapply(lf,function(file){
    ndvi <- read_stars(gdal_subdatasets(file)[[1]])
    st_warp(ndvi[pol],crs = st_crs(4326))
    #write_stars(ndvi_cl,'ndvi_chile.tif',type = 'UInt16',options =c('co'='COMPRESS=NONE'))
  })
  
  })
  meta <- lapply(lf,gdal_subdatasets)
  ndvi_list <- lapply(sapply(meta,'[[',1),read_stars,proxy = TRUE)
  ndvi_cl <- do.call(st_mosaic,ndvi_list)
  ndvi_rep <- st_transform_proj(ndvi_cl,crs = st_crs(4326))
  write_stars(ndvi_cl,'ndvi_chile.tif',type = 'UInt16',options =c('co'='COMPRESS=NONE'))
})

pol <- st_read('data/spatial/chile_continental.gpkg')
pol <- st_transform(pol,st_crs(ndvi_cl))
ndvi <- read_stars(gdal_subdatasets(lf[grep('.h12v12.',lf)])[[1]])
new <- ndvi[pol]

rasterio <- list(nXOff = 1537, nYOff = 3600, nXSize = 3264, nYSize = 1201)
ndvi <- read_stars(gdal_subdatasets(lf[1])[[1]],RasterIO = rasterio)


# x 1537 4800 -7783654  231.656 unnamed    NA   NULL [x]
# y 3600 4800 -1111951 -231.656 unnamed    NA   NULL [y]

x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"),proxy = TRUE)
x1 = x[,100:200,100:200,]
x2 = x[,150:300,150:300,]

plot(st_mosaic(x1,x2))

