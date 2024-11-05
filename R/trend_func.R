trend_func <- function(x,start = c(1981,1),end = c(2023,11),frecuency =12){
  x <- as.numeric(x)
  t <- ts(x,start=start,end = end,frequency = frecuency)
  
  if (!all(is.na(t))){
    t <- imputeTS::na_mean(t)
    res <- mk.test(t)
    mk_res <- ifelse(res$p.value < 0.05,1,NA)
    ss <- sens.slope(t)
    mk_ss <- ifelse(ss$p.value < 0.05,ss$estimates,NA)
    out <- c(mk_res,mk_ss)
  } else out <- c(NA,NA)
  return(out)
}

trend_func2 <- function(x){
  x <- as.numeric(x)
  
  p <- sum(!is.na(x))/length(x)
  
  if (p > 0.5){
    res <- mmky(x)
    mk_res <- ifelse(res[2] < 0.05,1,NA)
    mk_ss <- ifelse(res[2] < 0.05,res[7],0)
    out <- c(mk_res,mk_ss)
  } else out <- c(NA,NA)
  return(out)
}