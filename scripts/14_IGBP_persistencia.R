# Analyzing IGBP landcover for Chile
# by Francisco Zambrano (frzambra@gmail.com)
# April 2021

dir <- '/mnt/discoB/processed/MODIS/'
dir.out <- '/mnt/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/'

library(terra)
library(rnaturalearth)
library(fs)
library(sf)

chl <- ne_countries(country='chile',returnclass = 'sf') |> 
  st_transform(32719)

files <- dir_ls(paste0(dir,'IGBP.MCD12Q1.061'),regexp = '*.tif$')
igbp <- rast(files)
igbp <- crop(igbp,chl)

# calculate the most common IGBP class for the 19 years
# and the persistance (%)

igbpfun1 <- function(px){
  if (!all(is.na(px))){
    lv <- sort(table(px),decreasing = TRUE)[1]
    perc <- lv/length(px)
    lclass <- ifelse(px[length(px)] == as.numeric(names(lv)),1,0)
    class <- as.numeric(names(lv))
    out <- c(perc*100,class,lclass)
  } else out <- c(NA,NA,NA)
}

#due to a conflict with {parallel} this line must be executed 
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

system.time(
  igbpPC <- app(igbp,fun=igbpfun1,cores=80)                 
)
writeRaster(igbpPC,paste0(dir.out,'IGBP_perc_class_lastClass.tif'),
            wopt=list(datatype='INT1U'),overwrite = TRUE)

# selecting the IGBP class that remains for >X% of the years

prop <- 80
selct <- function(x,prop){
  out <- NA
  if(!is.na(x[1])){
    out <- ifelse(x[1] < prop, NA,x[2])
  }
  out
}

igbpProp <- app(igbpPC,fun=selct,cores = 80,prop=80)                   

# reclassify with {terra}

classes <- matrix(c(1:17,rep(1,5),rep(2,2),rep(3,2),4,5,6,7,6,8,9,10),ncol=2)

igbpRecl <- classify(igbpProp,rcl=classes,include.lowest = TRUE)
igbpRecl[igbpRecl == 0] <- NA

writeRaster(igbpRecl,paste0(dir.out,'IGBP',prop,'_reclassified.tif'),
            wopt=list(datatype='INT1U'))

# landcover del último año (2023)

igbp2022 <- classify(igbp[[22]],rcl=classes,include.lowest = TRUE)
igbp2022[igbp2022 == 0] <- NA
writeRaster(igbp2022,paste0(dir.out,'IGBP_2023_reclassified.tif'),
            wopt=list(datatype='INT1U'))
