# Script to download MODIS data from htps
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# July 2018

library(RCurl)
library(XML)
library(httr)
library(curl)

type <- c('MOLA','MOLT','MOTA')[3]
product <- 'MCD12Q1.061'
dir_out <- '/home/rstudio/raw/MODIS/'

updt_modis(type,product,dir_out)
