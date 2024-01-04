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

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif')

dir <- '/mnt/md0/raster_procesada/analysis/correlations/'
files <- dir_ls(dir,regexp = 'tif$')

cors <- rast(files)
cors <- resample(cors,lc)

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  st_transform(32719) |> 
  mutate(macrozona = c('norte chico','norte grande','austral','centro','sur'))

library(tidyverse)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#obtener los indices y escalas de tiempo en dónde se alcanza la máxima correlación
cors <- subset(cors,seq(1,8,2))
data_ind <- map_df(1:5,function(i){
  cors_m <- mask(cors,macro[i,])
  lc_m <- mask(lc,macro[i,])
  cors_df <- zonal(cors_m,lc_m,getmode)
  names(cors_df)[2:5] <- c('eddi','spi','spei','zcsm')
  cors_df |> 
    pivot_longer(-1) |> 
    rename(indice = name) |> 
    mutate(clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name),
           macro = macro[i,]$macrozona) |> 
    select(macro,indice,clase,value) 
})

data_ind |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(value = ceiling(value),
         value = factor(value,levels = 1:6,labels = c(1,3,6,12,24,36)),
         macro = fct_relevel(macro, "norte grande", "norte chico",
                             'centro','sur','austral')) |> 
  pivot_wider(names_from = indice,values_from = value) |> 
  arrange(macro) |> 
  #mutate(row=row_number()) |> 
  pivot_longer(-c(clase,macro))  |> 
  mutate(clase = str_to_title(clase)) |> 
  pivot_wider(names_from=c(clase, name), values_from=value) |> 
  gt() %>% 
  #opt_stylize(style = 6, color = 'gray')
  tab_spanner_delim(
    delim="_"
  ) %>% 
  fmt_missing(
    columns=everything(),
    missing_text=""
  ) |> 
  gtsave('output/figs/tabla_best_cor_macro_indice.png',expand=20,vwidth=1200)

  gt(groupname_col = "macro") |> 
  opt_stylize(style = 6, color = 'gray')

#obtener los valores de correlación para los indices y escalas de tiempo en dónde se alcanza la máxima correlación
  
cors <- subset(cors,seq(2,8,2))
data_r <- map_df(1:5,function(i){
  cors_m <- mask(cors,macro[i,])
  lc_m <- mask(lc,macro[i,])
  cors_df <- zonal(cors_m,lc_m,na.rm = TRUE)
  names(cors_df)[2:5] <- c('eddi','spi','spei','zcsm')
  cors_df |> 
    pivot_longer(-1) |> 
    rename(indice = name) |> 
    mutate(clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name),
           macro = macro[i,]$macrozona) |> 
    select(macro,indice,clase,value) 
})

data_r |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(macro = fct_relevel(macro, "norte grande", "norte chico",
                             'centro','sur','austral')) |> 
  pivot_wider(names_from = indice,values_from = value) |> 
  write_rds('data/processed_data/df_best_correlation_by_index_macro.rds')

data |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(date = ymd(date),
         macro = fct_relevel(macro, "norte grande", "norte chico",
                                 'zona central','zona sur')) |> 
  ggplot(aes(date,zcndvi_3,colour = clase)) +
  geom_point(size = .2) + 
  geom_line(lwd=.2) +
  scale_color_manual(values = as.character(colors),labels = paleta$Name) +
  scale_x_date(date_breaks = '2 years',date_labels = '%Y',expand = c(0,0)) +
  facet_grid(macro~.,scale = 'free') +
  theme_bw()
    
