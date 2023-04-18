
base_url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/cogs/p05/'

library(fs)
library(terra)

ls <- RCurl::getURL(base_url,dirlistonly = TRUE) |>
  XML::htmlParse() |> 
  XML::xpathSApply("//a/@href") 

years <- ls[grepl('[0-9]{4}',ls)]

attr(years, "names") <- NULL
files <- years[42:43] |> 
  purrr::map(function(yr){
    l <- RCurl::getURL(paste0(base_url,yr),dirlistonly = TRUE) |>
      XML::htmlParse() |> 
      XML::xpathSApply("//a/@href")
    attr(l, "names") <- NULL
    paste0(yr,l[grepl('\\d+.{3}.cog',l)])
  }) |> unlist()

library(stars)
ext_io <- list(nXOff = 2087,nYOff = 1350,nXSize = 186, nYSize = 651)

dir_out <- '/mnt/HDD4TB_2/data/rasters/Procesados/CHIRPS_v2/daily'
purrr::map(files,function(file){
  chr <- read_stars(paste0("/vsicurl/",base_url,file),RasterIO = ext_io)
  write_stars(chr,file.path(dir_out,stringr::str_replace(stringr::str_remove(file,'[0-9]{4}/'),'cog','tif')))
})

## terra version 1.2.13

ras <- rast(cog.url)

library(stars)

st <- read_stars(cog.url,driver = 'COG')

htmlContent <- RCurl::getURL(url,dirlistonly = TRUE)
htmlTree <- XML::htmlParse(htmlContent)
links <- XML::xpathSApply(htmlTree, "//a/@href")

chr <- rast(paste0("/vsicurl/",url,links[6:10]))


library(sf)
library(stars)
ext_io <- list(nXOff = 2087,nYOff = 1350,nXSize = 186, nYSize = 651)
chr <- read_stars(paste0("/vsicurl/",url,links[6:100]),along = 'date',RasterIO = ext_io)
chl <- read_sf('/mnt/HDD4TB_2/data/vectorial/limites_Chile/cl_continental_geo.shp')

chr_c <- st_crop(chr,st_bbox(chl))

######

url <- '/vsigzip//vsicurl/https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/tifs/p05/2022/chirps-v2.0.2022.07.05.tif.gz'

pre <- read_stars(url,RasterIO = ext_io)

pre[pre < 0] <- NA
plot(pre)
pre
rast('/vsizip//test.tif.gz')

# testing download tif and COG

dir <- tempdir()
url <- '/vsicurl/https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/cogs/p05/1981/chirps-v2.0.1981.01.01.cog'

#terra
library(tictoc)
tic()
writeRaster(crop(rast(paste0("/vsicurl/",url,links[6:20])),vect(chl)),file.path(dir,'test.tif'),overwrite = TRUE)
toc()

tic()
write_stars(read_stars(paste0("/vsicurl/",url,links[6:20]),RasterIO = ext_io),along = 'date',file.path(dir,'test.tif'))
toc()