pe <- function(x){
  r <- rank(x)
  (r-0.33)/(length(x)+0.33)
}

eddi <- function(x){
  C0 = 2.515517
  C1 = 0.802853
  C2 = 0.010328
  d1 = 1.432788
  d2 = 0.189269
  d3 = 0.001308
  pe <- pe(x)
  
  data <- data.frame(x=x,pe=pe)
  
  out <- data |> 
    dplyr::mutate(W = dplyr::case_when(
      pe <= .5 ~ sqrt(-2*log(1-pe)),
      pe > .5 ~ sqrt(-2*log(pe))
    ),
    EDDI = W - (C0+C1*W+C2*W^2)/(1+d1*W+d2*W^2+d3*W^3),
    EDDI = dplyr::case_when(
      pe <= .5 ~ EDDI,
      pe > .5 ~ -EDDI)
    ) 
  
  return(out)
}

library(ggplot2)

ggplot(out) +
  geom_density(aes(EDDI)) +
  geom_density(aes(zscore))

ggplot(out) +
  geom_density(aes(x))

ggplot(out) +
  geom_point(aes(x,EDDI))

ggplot(out) +
  geom_point(aes(x,zscore))
