library(sf)

cuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg') 

cuencas <- cuencas |> 
  st_transform(4326) |> 
  bind_cols(Latitude = st_coordinates(st_centroid(cuencas))[,2])

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg')

cuencas_macro <- st_union(cuencas,macro)

cuencas_macro <- cuencas_macro |> 
  st_drop_geometry() |> 
  select(COD_SUBC,macrozona,Latitude)

library(readr)
write_rds(cuencas_macro,'data/processed_data/cuencas_macrozona.rds')
