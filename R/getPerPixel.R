# Function to get the cNDVi at specified perido through the season
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# January 2020

getPerPixel <- function(x,dates,at='eos',lead=0,step=16,...){
  require(lubridate)
  
  years = max(unique(lubridate::year(dates)))-min(unique(lubridate::year(dates)))+1
  sos <- x[1]
  eos <- x[2]
  x <- x[3:length(x)]
  nl <- length(x)
  
  #dates of the MOD13 product between 2000 and 2002
  times <- round(365/step)
  
  dates2s <- as.vector(sapply(2002:2004,function(year) seq(as.Date(paste0(year,"-01-01")),as.Date(paste0(year,"-12-31")),by=step)))
  
    # indGS <- lapply(1:length(vSOS),function(j) intersect(which(dates2s >= vSOS[j]),which(dates2s <= vEOS[j])))
    indGS <- intersect(which(dates2s >= sos),which(dates2s + step <= eos))
    indAll <- matrix(sapply(0:(years-1),function(l) indGS+times*l),ncol=years)
    
    indAll <- sapply(0:(years-1),function(l) indGS+times*l)
      
        if (lead >= length(indGS) | length(indGS) == 1){
          ind2Get <- rep(NA,length(indGS))
        } else {
          if (at == 'eos'){
            ind2Get <- indAll[length(indGS)-lead,]
          } else if (at =='sos') {
            ind2Get <- indAll[1+lead,]
          }
        }
        
      ind2Get <- ind2Get[ind2Get<nl]
      
      vals2get <- rep(NA, years)
      
      #if (all(!is.na(as.numeric(ind2Get)))){
      if (!length(ind2Get) == 0 ){
        vals2get[1:length(ind2Get)] <- x[ind2Get]
      }
      return(vals2get)
    }

  
  
