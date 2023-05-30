# Analyzing IGBP landcover for Chile
# by Francisco Zambrano (frzambra@gmail.com)
# May 2023

dir <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/'
dir.out <- '/media/francisco/HDD4TB_2/data/rasters/Proyectos/Chile-ecosystem-change-drought/Landcover_IGBP/'

library(terra)
library(fs)
library(rnaturalearth)
library(sf)

chl <- ne_countries(country='chile',returnclass = 'sf') |> 
  st_transform(32719)

files <- dir_ls(paste0(dir,'IGBP.MCD12Q1.061'),regexp = '*.tif$')
igbp <- rast(files)
igbp <- crop(igbp,chl)

# calculate the most common IGBP class for the n years
# and the persistance (%)

igbpfun1 <- function(px){
  lv <- sort(table(px),decreasing = TRUE)[1]
  if (!is.na(lv)){
    perc <- lv/length(px)
    lclass <- ifelse(px[length(px)] == as.numeric(names(lv)),1,0)
    class <- as.numeric(names(lv))
    out <- c(perc*100,class,lclass)
  } else out <- rep(NA,3)
}

#due to a conflict with {parallel} this line must be executed 
#parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

system.time(
  igbpPC <- app(igbp,fun=igbpfun1,cores=5)                 
)
writeRaster(igbpPC,paste0(dir.out,'IGBP_perc_class_lastClass.tif'),
            wopt=list(datatype='INT1U'),overwrite = TRUE)

# selecting the IGBP class that remains for >X% of the years

prop <- 1
selct <- function(x){
  if(x[1] < prop) NA else x[2]
}

igbpProp <- app(igbpPC,fun=selct)                   

# reclassify with {terra}

classes <- matrix(c(1:17,rep(1,5),rep(2,2),rep(3,2),4,5,6,7,6,8,9,10),ncol=2)

igbpRecl <- classify(igbpProp,rcl=classes,include.lowest = TRUE)
igbpRecl[igbpRecl == 0] <- NA

writeRaster(igbpRecl,paste0(dir.out,'IGBP',prop*100,'_reclassified.tif'),
            wopt=list(datatype='INT1U'))
