trend_func <- function(x){
  x <- as.numeric(x)
  
  if (!all(is.na(x))){
    t <- ts(x,start=c(1981,1),end = c(2023,11),frequency = 12)
    res <- mk.test(na.omit(t))
    mk_res <- ifelse(res$p.value < 0.05,1,NA)
    ss <- sens.slope(na.omit(t))
    mk_ss <- ifelse(ss$p.value < 0.05,ss$estimates,0)
    out <- c(mk_res,mk_ss)
  } else out <- c(NA,NA)
  return(out)
}