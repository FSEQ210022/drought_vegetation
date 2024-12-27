library(terra)
library(sf)
library(tidyverse)
library(fs)
library(tmap)
library(basemaps)
library(rnaturalearth)
library(glue)

ecoregions <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_transform(32719)

#persistencia de landcover
lc <- rast('/home/rstudio/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/IGBP80_reclassified.tif')
lc[lc %in% c(5,7:10)] <- NA

dir <- '/home/rstudio/discoB/processed/analysis/correlations/'
files <- dir_ls(dir,type = 'file',regexp = 'tif$')

#reordenar indices
files <- files[c(3,1,2,5,4)]
chl <- ne_countries(country = 'chile',scale='medium',returnclass = 'sf') |> st_transform(32719)

chl <- chl |> st_geometry() |> st_cast('POLYGON') |> _[-c(1,2)] 

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg')
cors <- files |> 
  lapply(\(file) {
    c(resample(rast(file)[[1]],lc,method = 'near'),
      resample(rast(file)[[2]],lc))
  })

cors <- rast(cors)
cors <- mask(cors,lc)
cors_i <- subset(cors,seq(1,10,2))
cors_r <- subset(cors,seq(2,10,2))

# 1. Buscar coordenadas de puntos con valores de correlacionaes ----

# mayores a 0.8

cors_r1 <- prod(subset(cors_r,c(1,3:5))> 0.8)
cors_r1[cors_r1 == 0] <- NA

#celdas que tienen valores igual a 1
ids <- which(values(cors_r1) == 1)
df1 <- xyFromCell(cors_r1,ids)

set.seed(678)
coords1 <- df1[sample(nrow(df1),3),] |> 
  data.frame() |> 
  st_as_sf(coords = c('x','y'),crs = 32719) |> 
  mutate(correlacion = 'strong positive correlation')

# entre -0.1 a 0.1
cors_r2 <- prod(subset(cors_r,c(1,3:5)) > -0.2 & subset(cors_r,c(1,3:5)) < 0.2)
cors_r2[cors_r2 == 0] <- NA

#celdas que tienen valores igual a 1
ids <- which(values(cors_r2) == 1)
df2 <- xyFromCell(cors_r2,ids)

set.seed(678)
coords2 <- df2[sample(nrow(df2),3),] |> 
  data.frame() |> 
  st_as_sf(coords = c('x','y'),crs = 32719) |> 
  mutate(correlacion = 'no correlation')

# menor a -0.8
cors_r3 <- prod(subset(cors_r,c(1,3:5)) < -0.3)
cors_r3[cors_r3 == 0] <- NA

#celdas que tienen valores igual a 1
ids <- which(values(cors_r3) == 1)
df3 <- xyFromCell(cors_r3,ids)

set.seed(678)
coords3 <- df3[sample(nrow(df3),3),] |> 
  data.frame() |> 
  st_as_sf(coords = c('x','y'),crs = 32719) |> 
  mutate(correlacion = 'strong negative correlation')

coords <- bind_rows(coords1,coords2,coords3)

df_scales <- terra::extract(cors_i,coords) |> 
  set_names(c('ID','SPI','EDDI','SPEI','SETI','SSI')) |> 
  mutate(across(2:6,\(x) c(1,3,6,12,24,36)[x]))

#paleta de colores landcover
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

df_clase <- terra::extract(lc,coords) |> 
  mutate(across(2,\(x) paleta$Name[x]))

df1 <- cbind(df_clase,df_scales) |>
  select(-ID) |> 
  drop_na()
  
dir_indices1 <- '/home/rstudio/discoB/processed/ERA5-Land/sequia_2000-2023/monthly/'
dir_indices2 <- '/home/rstudio/discoB/processed/MODIS/'

data <- 2:6 |> 
  map_df(\(i){
    index <- names(df1)[i]
    
    1:nrow(df1) |> 
      map_df(\(j){
        scale <- df1[index][j,1] 
        if (index != 'SETI'){
          files <- dir_ls(glue('{dir_indices1}{index}/{index}-{scale}'))
        } else {
          files <- dir_ls(glue('{dir_indices2}{index}/{index}-{scale}'))
        }
        
        r <- rast(files)
        dates <- ymd(str_extract(files,'[0-9]{4}-[0-9]{2}-[0-9]{2}'))
        coords <- coords |> st_transform(crs(r))
        ind_sc <- glue('{index}-{scale}')
        df_ex <- terra::extract(r,coords[j,]) |> 
          pivot_longer(-ID) |> 
          mutate(dates = dates,.before = value) |> 
          select(-ID,-name) |> 
          mutate( punto =  j,
                  nivel_correl = coords$correlacion[j],
                  indice = ind_sc,
                  clase = df1[j,1])
      })
  })

files <- dir_ls(glue('{dir_indices2}/zcNDVI/zcNDVI-6/'))
dates <- ymd(str_extract(files,'[0-9]{4}-[0-9]{2}-[0-9]{2}'))
zcndvi <- rast(files)

data_zcndvi <- terra::extract(zcndvi,coords) |> 
  bind_cols(clase = df1[,1]) |> 
  mutate(punto = 1:9) |> 
  select(-ID) |> 
  pivot_longer(-c(clase,punto)) |> 
  mutate(dates = rep(dates,9),.before = value) |> 
  select(-name) |>
  mutate(indice = 'zcNDVI') |> 
  filter(dates <= "2023-12-01")
    
data_unida <- data |> 
  filter(between(dates,ymd("2000-02-01"),ymd("2023-12-01"))) |>
  left_join(data_zcndvi,by = c('dates','punto','clase'))

data_unida |> 
  ggplot(aes(dates,value.x,color = clase)) +
  geom_point(size = .3) + 
  geom_line(size = .3) +
  geom_line(aes(dates,value.y),col='red') +
  scale_color_manual(values = colors,name = 'Landcover class') +
  facet_wrap(nivel_correl~indice.x+punto) +
  theme_bw()

library(tmap)
tm_shape(ecoregions) + 
  tm_polygons(col = 'ECO_NAME',alpha = .6) + 
  tm_shape(coords) +
  tm_dots(col = 'correlacion',size=.2,palette = viridis::magma(4)) + 
  tm_layout(legend.outside = TRUE)
