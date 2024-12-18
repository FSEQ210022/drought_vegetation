## Extraer tabla de tendencias para macrozonas}
library(sf)
library(terra)
library(fs)
library(tidyverse)

cuencas <- read_sf('data/processed_data/spatial/subcuencas_bna.gpkg') 

dir <- '/home/rstudio/discoB/processed/analysis/trends'
files <- dir_ls(dir,regexp = 'mann_kendall_mod.*tif$')

# En primer lugar para los indices que se calculan de ERA5-Land
files1 <- files[1:24]
indices <- str_remove(basename(files1),'trend_mann_kendall_mod_')

trends <- rast(files1)

ind <- sapply(1:length(files1),\(i) rep(i,2)) |> as.numeric()
trends <- tapp(trends,ind,prod)
names(trends) <- indices

# Extrae la tendencia para cada indice. Reemplaza los NaN por 0. Ausencia de tendencia.
df_scales_1 <- terra::extract(trends,cuencas,mean,na.rm = TRUE) |> 
  mutate(across(everything(),\(x) replace_na(x,0)))

# Ahora los índices derivados de MODIS (ET y PET)
files2 <- files[c(25:30,35:40)]
indices <- str_remove(basename(files2),'trend_mann_kendall_mod_*.tif')

trends <- rast(files2)

# cargar la capa de landcover persistente para aplicar sobre el índice
lc_pers <- rast('/home/rstudio/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/IGBP80_reclassified.tif') 
lc_pers <- resample(lc_pers,trends) 

trends <- mask(trends,lc_pers)
ind <- sapply(1:length(files2),\(i) rep(i,2)) |> as.numeric()
trends <- tapp(trends,ind,prod)
names(trends) <- indices

df_scales_2 <- terra::extract(trends,cuencas,mean,na.rm = TRUE) |> 
  mutate(across(everything(),\(x) replace_na(x,0)))

# Ahora los índices derivados de MODIS (zcNDVI)
files3 <- files[31:34]
indices <- str_remove(basename(files3),'trend_mann_kendall_mod_')

# cargar la capa de landcover persistente para aplicar sobre el índice
lc_pers <- rast('/home/rstudio/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/IGBP80_reclassified.tif') 
lc_pers <- resample(lc_pers,trends) 

trends <- rast(files3)
trends <- mask(trends,lc_pers)

ind <- sapply(1:length(files3),\(i) rep(i,2)) |> as.numeric()
trends <- tapp(trends,ind,prod)
names(trends) <- indices

df_scales_3 <- terra::extract(trends,cuencas,mean,na.rm = TRUE) |> 
  mutate(across(everything(),\(x) replace_na(x,0)))

# Tendencia de área quemada

trend_area_quemada <- rast(paste0(dir,'/trend_area_quemada_2002-2022.tif')) |> 
  prod()

df_scales_4 <- terra::extract(trend_area_quemada,cuencas,mean,na.rm = TRUE) |> 
  mutate(across(everything(),\(x) replace_na(x,0))) |> 
  rename(trend_area_quemada = prod)

# Tendencia luces nocturnas (NASA Marble)

trend_luces_nocturnas <- rast(paste0(dir,'/trend_nasa_marble_luces_nocturnas_2012-2023.tif')) |> 
  prod()

df_scales_5 <- terra::extract(trend_luces_nocturnas,cuencas,mean,na.rm = TRUE) |> 
  mutate(across(everything(),\(x) replace_na(x,0))) |> 
  rename(trend_luces_nocturnas = prod)

# Variable de densidad vial

# Tendencia luces nocturnas (NASA Marble)

densidad_vial <- rast(paste0(dir,'/../densidad_vial_chile.tif'))

df_scales_6 <- terra::extract(densidad_vial,cuencas,mean,na.rm = TRUE) |> 
  mutate(across(everything(),\(x) replace_na(x,0))) |> 
  rename(densidad_vial = 2)


# Unir todas las tendencias y predictores
df_scales <- cbind(st_drop_geometry(cuencas),
                   df_scales_1,df_scales_2,
                   df_scales_3,df_scales_4,
                   df_scales_5,df_scales_6) |> 
  select(-starts_with('ID')) 

sel_cuen <- df_scales |> 
  select(-1,-3,-4,-5) |> 
  group_by(COD_SUBC) |> 
  summarize(n =n()) |> 
  filter(n == 1 ) |> 
  pull(COD_SUBC)

df_scales <- df_scales |>
  select(-1,-3,-4,-5) |> 
  filter(COD_SUBC %in% sel_cuen) |> 
  mutate(COD_SUBC = fct(COD_SUBC)) 

#guardar data.frame con los valores promedio de las tendencias (mann-kendall) para cada indice en las macrozonas de Chile
df_scales |> 
  pivot_longer(-c(COD_SUBC)) |> 
  rename(index = name,trend = value) |> 
  unnest(trend) |> 
  mutate(index = str_remove(index,'.tif')) |> 
  write_rds('data/processed_data/df_trends_indices_cuencas.rds')

