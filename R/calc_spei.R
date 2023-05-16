calc_spei <- function(x,scale){
  nas <- is.na(x)
  
  if (length(x) >= scale){
    indxs <- lapply(length(x):scale,\(n,scale) n:(n-(scale-1)),scale=scale)
    vals <- sapply(indxs,\(ix) sum(x[ix],na.rm = TRUE))
    vals <- na.omit(vals)
    out <- eddi(vals)$EDDI
    out[nas] <- NA
    out <- rev(out)
  } 
  out
}
