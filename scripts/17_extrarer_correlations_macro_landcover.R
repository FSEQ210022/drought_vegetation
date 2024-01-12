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
cors1 <- subset(cors,seq(1,8,2))
data_ind <- map_df(1:5,function(i){
  cors_m <- mask(cors1,macro[i,])
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

data_ind_4gt <- data_ind |> 
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
  pivot_wider(names_from=c(clase, name), values_from=value) 

#obtener los valores de correlación para los indices y escalas de tiempo en dónde se alcanza la máxima correlación
  
cors2 <- subset(cors,seq(2,8,2))
data_r <- map_df(1:5,function(i){
  cors_m <- mask(cors2,macro[i,])
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

data_r_4gt <- data_r |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(macro = fct_relevel(macro, "norte grande", "norte chico",
                             'centro','sur','austral')) |> 
  mutate(clase = str_to_title(clase),
         value = value^2) |> #value^2 for R2
  pivot_wider(names_from = indice,values_from = value) |> 
  pivot_longer(-c(clase,macro))  |> 
  mutate(clase = str_to_title(clase)) |> 
  pivot_wider(names_from=c(clase, name), values_from=value) |> 
  rename_with(\(x) str_c(x,'_r'),-macro)

tabla_gt <- full_join(data_ind_4gt,data_r_4gt) 

library(gt)

#tabla_gt[1,2] <- NA
tabla_gt |> 
  select(1,22:25,18:21,10:13,6:9,2:5,
         46:49,38:41,34:37,30:33,26:29
         ) |> 
  gt() %>% 
  #opt_stylize(style = 6, color = 'gray')
  data_color(
    columns = 22:41,
    target_columns =2:21,
    palette = viridis::inferno(20),
    na_color = 'white',
    alpha = .8,
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
  gtsave('output/figs/tabla_r_cor_macro_indice.png')

#crear legenda para incluir en la tabla
plot <- data_r_4gt |> 
  pivot_longer(-macro) |> 
  ggplot(aes(name,value,color=value)) + 
  geom_point() + 
  scale_color_viridis_c(option = 'inferno',name = 'r-squared',alpha = .8) + theme(legend.position = 'bottom')

ggpubr::get_legend(plot) |> 
  ggpubr::as_ggplot() |> 
  ggsave(filename = 'output/figs/leyenda_tabla_correlaciones_macro_suelo.png')


  