# function to make the smoothing of NDVI time series
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# December 2019

smoothNDVI <- function(x,n=5){
  
  # x<-addLayer(x,subset(x,(nlayers(x)-1):(nlayers(x)-n)))
  # x<-addLayer(subset(x,((1+n):2)),x)
  
  x <- c(x[(n+1):2],x,x[(length(x)-1):(length(x)-n)])
  
  fun <- function(x){
    lowess(x,f=7/length(x),iter=10)$y
    #loess(y~x,data = data.frame(x=1:length(x),y=x),span =7/length(x))$fitted
  }
  
  smooth <- fun(x)
  smooth <- smooth[(n+1):(length(smooth)-n)]
  
  return(smooth)
}

