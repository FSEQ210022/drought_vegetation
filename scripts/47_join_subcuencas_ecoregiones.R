library(sf)

cuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg') 

cuencas <- cuencas |> 
  st_transform(4326) |> 
  bind_cols(Latitude = st_coordinates(st_centroid(cuencas))[,2])

ecoregiones <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_make_valid()

cuencas_eco <- st_union(cuencas,ecoregiones)

cuencas_eco <- cuencas_eco |> 
  st_drop_geometry() |> 
  select(COD_SUBC,ECO_NAME,Latitude)

library(readr)
write_rds(cuencas_eco,'data/processed_data/cuencas_ecoregiones.rds')
