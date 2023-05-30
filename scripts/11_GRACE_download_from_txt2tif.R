library(RCurl)
library(XML)
library(httr)
library(curl)

url <- 'https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/'
htmlContent <- getURL(url)
htmlTree <- htmlParse(htmlContent)
links <- xpathSApply(htmlTree, "//a/@href")

file <- '/mnt/raster_raw/GRACE/7496993242-download.txt'

library(readr)
library(stringr)

df_files <- read_delim(file,col_names = FALSE)
df_files <- readLines(file)


files_tif <- df_files[grepl('*.tif$',df_files)]
names <- str_split_i(files_tif,'/',6)

dir_out <- '/mnt/raster_raw/GRACE/TELLUS_GRFO_L3_GFZ_RL06.1_LND_v04/'

lapply(seq_along(files_tif),function(i){
  download.file(files_tif[i],destfile = paste0(dir_out,names[i]))
})
