# function to make the smoothing of NDVI time series
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# December 2019

smoothNDVI <- function(x,y,n=5){
  
  # x<-addLayer(x,subset(x,(nlayers(x)-1):(nlayers(x)-n)))
  # x<-addLayer(subset(x,((1+n):2)),x)
  
  x <- c(x[(n+1):2],x,x[(length(x)-1):(length(x)-n)])
  y <- c(y[(n+1):2],y,y[(length(y)-1):(length(y)-n)])
  
  df <- data.frame(x=x,y=y)
  df.lo <- loess(y~x,df)
  df$y.pred <- predict(df.lo,df)
    #lowess(x,f=7/length(x),iter=10)$y
    #loess(y~x,data = data.frame(x=1:length(x),y=x),span =7/length(x))$fitted

  df_out <- df[(n+1):(nrow(df)-n),]
  
  return(df_out$y.pred)
}

