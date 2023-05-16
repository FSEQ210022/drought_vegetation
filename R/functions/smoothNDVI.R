# function to make the smoothing of NDVI time series
# by Francisco Zambrano Bigiarini (frzambra@gmail.com)
# December 2019

smoothNDVI <- function(y,x,n=5){

  x <- c((x[1]-n):(x[1]-1),x,(x[length(x)]+1):(x[length(x)]+n))
  y <- c(y[(n+1):2],y,y[(length(y)-1):(length(y)-n)])
  
    df <- data.frame(x=x,y=y)
    
    df.lo <- tryCatch(
      loess(y~x,df,span=7/length(x)),
      error = function(err) NA)
    
    if (length(df.lo) >1){
      if (!all(is.na(df.lo$residuals))) y.pred <- predict(df.lo,df) 
    } else y.pred <- rep(NA,nrow(df))
    
    if (exists('y.pred')) y.pred <- y.pred[(n+1):(nrow(df)-n)] else y.pred <- rep(NA,nrow(df)-2*n)
    y.pred
}


 # y <- runif(100)
 # y[sample(1:100,100)] <- NA
 # x <- 1:100
 # smoothNDVI(y,x) |> length()
