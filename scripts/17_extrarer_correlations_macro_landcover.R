# script para extraer valores de indicadores de sequía en las clases de landcover

library(terra)
library(fs)
library(sf)
library(purrr)

#paleta colores landcover persistencia
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land'))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

# 
pal <- read.csv('data/processed_data/paleta_colores_landcover.csv')
t <- pal$class
names(t) <- pal$Name

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif')

dir <- '/mnt/md0/raster_procesada/analysis/correlations/'
files <- dir_ls(dir,regexp = 'tif$')

cors <- rast(files)
cors <- resample(cors,lc)

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  st_transform(32719)

library(tidyverse)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cors_i <- subset(cors,seq(1,8,2))
data_ind <- map_df(1:5,function(i){
  cors_im <- mask(cors_i,macro[i,])
  lc_m <- mask(lc,macro[i,])
  cors_df <- zonal(cors_im,lc_m,getmode)
  names(cors_df)[2:5] <- c('EDDI','SPI','SPEI','zcSM')
  cors_df |> 
    pivot_longer(-1) |> 
    rename(indice = name) |> 
    mutate(clase = factor(lyr.1,levels = t, labels = names(t)),
           macro = macro[i,]$macrozona) |> 
    select(macro,indice,clase,value) 
})

data_ind_4gt <- data_ind |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(value = ceiling(value),
         value = factor(value,levels = 1:6,labels = c(1,3,6,12,24,36)),
         macro = fct_relevel(macro, "norte grande", "norte chico",
                             'zona central','zona sur')) |> 
  pivot_wider(names_from = indice,values_from = value) |> 
  arrange(macro) |> 
  #mutate(row=row_number()) |> 
  pivot_longer(-c(clase,macro))  |> 
  mutate(clase = str_to_title(clase)) |> 
  pivot_wider(names_from=c(clase, name), values_from=value) 

## extraer valores de correlación en dónde los índices obtuvieron mayor correlación
  
cors_i <- subset(cors,seq(1,8,2))
cors_r <- subset(cors,seq(2,8,2))

data_r <- map_df(1:5,function(e){
  cors_rm <- mask(cors_r,macro[e,])
  cors_im <- mask(cors_i,macro[e,])
    
  lc_m <- mask(lc,macro[e,])
  cors_df <- zonal(cors_im,lc_m,getmode)
  names(cors_df)[2:5] <- c('eddi','spi','spei','zcsm')
    
  # cors_df |> 
  #   pivot_longer(-1) |> 
  #   rename(indice = name) |> 
  #   mutate(clase = factor(lyr.1,levels = t, labels = names(t)),
  #          macro = macro[e,]$macrozona) |> 
  #   select(macro,indice,clase,value) 
    
  o <- map_df(cors_df$lyr.1,\(i){
    o1 <- map_dbl(1:4,\(j){
      lc_mi <- ifel(lc_m == i,lc_m,NA) 
      m1 <- mask(cors_im[[j]],lc_mi)
      m <- mask(m1,m1,round(cors_df[i,1+j]),inverse = TRUE) 
      zonal(cors_rm[[j]],m,mean,na.rm = TRUE)[2] |> as.numeric()
    })
    matrix(o1,ncol=4) |> data.frame() |> 
      mutate(clase = factor(i,levels = t,labels = names(t)),zone = macro$macrozona[e]) |> 
      relocate(clase, .before = X1) |> 
      relocate(zone, .before = clase)
  })
o
})

names(data_r)[3:6] <-  c('EDDI','SPI','SPEI','zcSM')

data_r_4gt <- data_r |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  arrange(zone) |> 
  #mutate(row=row_number()) |> 
  pivot_longer(-c(clase,zone))  |> 
  mutate(clase = str_to_title(clase),
         value = value^2) |> #value^2 for r-squared 
  pivot_wider(names_from=c(clase, name), values_from=value) |> 
  rename_with(\(x) str_c(x,'_r'),-zone)

tabla_gt <- cbind(data_ind_4gt,data_r_4gt |>  mutate(zone = fct_relevel(zone, "norte grande", "norte chico",
                                                                  'zona central','zona sur')) |> arrange(zone)) |> 
  select(-zone) |> 
  rename(zone = macro)

library(gt)

#tabla_gt[1,2] <- NA
tabla_gt |> 
  select(1,22:25,18:21,10:13,6:9,2:5,46:49,38:41,34:37,30:33,26:29) |> 
  #select(-starts_with('Barren')) |> 
  gt() %>% 
  #opt_stylize(style = 6, color = 'gray')
  data_color(
    columns = 22:41,
    target_columns =2:21,
    palette = 'RdBu',
    na_color = 'white',
    domain = c(0,.8)  
    ) |> 
  tab_spanner_delim(
    delim="_"
  ) %>% 
  fmt_missing(
    columns=everything(),
    missing_text=""
  ) |> 
  fmt_number() |> 
  cols_hide(columns = 22:41) |> 
  tab_footnote(
    footnote = html(local_image('output/figs/leyenda_tabla_correlaciones_macro_suelo.png',height = 50))) |> 
  gtsave('output/figs/tabla_r_cor_macro_indice.png',expand=20,vwidth=1200)
