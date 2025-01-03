library(sf)
library(rnaturalearth)

eco2017 <- read_sf('data/raw_data/ecoregiones_chl.gpkg') |> 
  st_make_valid()
chl <- ne_countries(country = 'chile',scale = 'medium',returnclass = 'sf')

bb <- st_bbox(chl)
bb[1] <- -77

bb_pol <- bb |> st_as_sfc() 
eco2017_chl <- st_intersection(eco2017,chl) 
plot(eco2017_chl["ECO_NAME"])

write_sf(eco2017_chl,'data/processed_data/spatial/ecoregiones_2017.gpkg')
