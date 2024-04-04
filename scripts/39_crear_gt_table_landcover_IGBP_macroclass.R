library(gt)
library(readr)

tabla <- read_csv('data/raw_data/tabla_landcover.csv')

tabla |> 
  gt() |> 
  cols_width(Name~100,
             Description~px(400)) |> 
  gtsave('output/figs/tabla_landcover_macroclass.png')
