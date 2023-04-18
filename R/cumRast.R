

cumRast <- function(x,dates,step,get = 'all'){
    NAasZero = TRUE
    
   #x <- as.vector(x)
   # index for the pixel where are values (not NA)
   years = max(unique(lubridate::year(dates)))-min(unique(lubridate::year(dates)))+1
   sos <- x[1]
   eos <- x[2]
   x <- x[3:length(x)]
   nl <- length(x)
   
  #dates of the MOD13 product between 2000 and 2002
  times <- round(365/step)
  dates2s <- as.vector(sapply(2002:2004,function(year) seq(as.Date(paste0(year,"-01-01")),as.Date(paste0(year,"-12-31")),by=step)))
  indGS <- intersect(which(dates2s >= sos),which(dates2s + step <= eos))
  indAll <- matrix(sapply(0:(years-1),function(l) indGS+times*l),ncol=years)
  cRaster <- rep(NA, length(x))

  if (all(!is.na(as.numeric(indAll)))){
    # indAll[which(indAll[,years-1] > nl),years-1] <- NA
    # indAll[which(indAll[,years] > nl),years] <- NA
    indAll[indAll > nl] <- NA
    isc <- as.numeric(indAll)
    isc <- isc[!is.na(isc)]

    if (NAasZero){ #wsto hay que modificarlo solo resulve un caso especifico
      x[x > 32700] <- 0
      x[is.na(x)] <- 0
    }
    cRaster[isc] <- as.numeric(apply(indAll,2,FUN=function(ix) cumsum(x[ix])))[1:length(isc)]
  }
  
    if (get[1] == 'eos'){
      y <- as.numeric(get[2])
      ix <- as.numeric(indAll[dim(indAll)[1],1:y])
      cRaster <- cRaster[ix]
    }
  return(cRaster)
}

