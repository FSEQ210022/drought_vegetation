library(sf)
library(tidyverse)

scuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg')
chl <- ne_countries(country = 'chile',scale='medium',returnclass = 'sf') 

data_trend_lc <- read_rds('data/processed_data/trends_landcover_cuencas_2001-2023.rds') |> 
  pivot_longer(-cuenca) |> 
  rename(landcover = value,class = name) |> 
  #replace_na(list(landcover = 0)) |> 
  mutate(class = case_when(class == 'Barren land' ~ 'Barren Land',
                           .default = class)) 
  #pivot_wider(names_from = class,values_from = landcover)


library(tmap)
data_scuen <- left_join(scuencas,data_trend_lc,by=c('COD_SUBC'='cuenca'))

  tm_shape(data_scuen |> drop_na()) + 
  tm_fill(col = 'landcover',name = 'Trend of area',legend.is.portrait = FALSE) + 
  tm_facets(by = 'class',nrow=1,free.scales = FALSE) +
  tm_shape(chl) + 
  tm_borders(col ='black') +
  tm_layout(panel.label.bg.color = 'white',
            legend.outside = TRUE,
            legend.position = c('center','bottom')
            )
          