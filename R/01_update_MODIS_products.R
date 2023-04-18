# Script to download MODIS data from htps
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# July 2018

library(RCurl)
library(XML)
library(httr)
library(curl)

type <- c('MOLA','MOLT','MOTA')[2]
product <- 'MOD13Q1.061'
dir.out <- '/mnt/HDD4TB_2/data/rasters/raw/MODIS/'

URL <- paste0("https://e4ftl01.cr.usgs.gov/",type,"/",product,'/')

htmlContent <- getURL(URL)
htmlTree <- htmlParse(htmlContent)
links <- xpathSApply(htmlTree, "//a/@href")

datesWeb <- as.character(unlist(regmatches(links,gregexpr("[0-9]{4}.[0-9]{2}.[0-9]{2}",links))))

if (dir.exists(file.path(dir.out,product))){
  datesDir <- list.files(file.path(dir.out,product))
  } else {
    dir.create(file.path(dir.out,product))
    datesDir <- character(0)
}

datesUpdate <- datesWeb[!datesWeb %in% datesDir]

lapply(datesUpdate, function(dir){
  htmlContent2 <- getURL(paste0(URL,dir,'/'))
  htmlTree2 <- htmlParse(htmlContent2)
  links2 <- xpathSApply(htmlTree2, "//a/@href")
  
  files <- unlist(regmatches(links2,gregexpr(paste0(substr(product,1,7),".*hdf$"),links2)))
  filesInd <- grep(files,pattern='.*(h11v10|h11v11|h11v12|h12v12|h12v13|h13v13|h13v14|h14v14).*.hdf$')
  files <- files[filesInd]
  dir.create(paste0(dir.out,product,'/',dir,'/'))
  
  lapply(files,function(name){
    tryCatch(
    GET(url = paste0(URL,dir,'/',name),
        config = httr::config(connecttimeout = 120),
        authenticate("frzambra@gmail.com","29H1kc12"),
        write_disk(paste0(dir.out,product,'/',dir,'/',name),
                   overwrite=TRUE),
        progress()),
    error = function(e) print(paste('Error:',e))
    )
    # curl_download(paste0(URL,dir,'/',name),
    #               destfile = paste0(dir.out,product,'/',dir,'/',name),
    #               handle = curl::handle_setopt(
    #   handle = h,
    #   httpauth = 1,
    #   userpwd = "frzambra@gmail.com:29H1kc12"
    # ))
  })
})

