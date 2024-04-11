## Extraer tabla de tendencias para macrozonas}
library(sf)
library(terra)
library(fs)

cuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg') 

dir <- '/mnt/md0/raster_procesada/analysis/trends'
files <- dir_ls(dir,regexp = 'mann_kendall.*tif$')
indices <- str_remove(str_remove(str_extract(files,'l_.*\\.'),'l_'),'\\.')
indices <- indices[-(19:22)]
trends <- rast(names(files)[-(19:22)])
trends <- trends[[seq(2,48,2)]]
names(trends) <- indices

df_scales_1 <- terra::extract(trends[[7:24]],cuencas,fun =\(x){
  table(x) |> sort(decreasing = TRUE) -> x
  as.numeric(names(x[1]))
})

df_scales_2 <- terra::extract(trends[[1:6]],cuencas,fun =\(x){
  table(x) |> sort() -> x
  as.numeric(names(x[1]))
})

#zcNDVI
trends <- rast(names(files)[22])
trends <- trends[[2]]
names(trends) <- 'zcNDVI-6'
cuencas <- st_transform(cuencas,32719)
df_scales_3 <- terra::extract(trends,cuencas,fun =\(x){
  table(x) |> sort(decreasing = TRUE) -> x
  as.numeric(names(x[1]))
})

df_scales <- cbind(st_drop_geometry(cuencas),df_scales_1,df_scales_2,df_scales_3) |> 
  select(-starts_with('ID')) 

sel_cuen <- df_scales |> 
  group_by(COD_SUBC) |> 
  summarize(n =n()) |> 
  filter(n == 1 ) |> 
  pull(COD_SUBC)

df_scales <- df_scales |> 
  filter(COD_SUBC %in% sel_cuen) |> 
  select(-c(1,3:5)) |> 
  mutate(COD_SUBC = fct(COD_SUBC)) 

#guardar data.frame con los valores promedio de las tendencias (mann-kendall) para cada indice en las macrozonas de Chile
df_scales |> 
  pivot_longer(-c(COD_SUBC)) |> 
  rename(index = name,trend = value) |> 
  unnest(trend) |> 
  write_rds('data/processed_data/df_trends_indices_cuencas.rds')

