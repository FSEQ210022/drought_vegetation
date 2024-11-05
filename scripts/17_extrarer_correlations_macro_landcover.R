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

eco <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_transform(32719) |> 
  filter(ECO_NAME != "Rock and Ice") |> 
  mutate(ECO_NAME = fct(ECO_NAME,levels = c("Atacama desert","Chilean Matorral","Valdivian temperate forests","Magellanic subpolar forests","Patagonian steppe"))) 

library(tidyverse)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#obtener los indices y escalas de tiempo en dónde se alcanza la máxima correlación
cors1 <- subset(cors,seq(1,8,2))
data_ind <- map_df(1:5,function(i){
  cors_m <- mask(cors1,eco[i,])
  lc_m <- mask(lc,eco[i,])
  cors_df <- zonal(cors_m,lc_m,getmode)
  names(cors_df)[2:5] <- c('EDDI','SPI','SPEI','SSI')
  cors_df |> 
    pivot_longer(-1) |> 
    rename(indice = name) |> 
    mutate(clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name),
           eco = eco[i,]$ECO_NAME) |> 
    select(eco,indice,clase,value) 
})

data_ind_4gt <- data_ind |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(value = ceiling(value),
         value = factor(value,levels = 1:6,labels = c(1,3,6,12,24,36))
         ) |> 
  pivot_wider(names_from = indice,values_from = value) |> 
  arrange(eco) |> 
  #mutate(row=row_number()) |> 
  pivot_longer(-c(clase,eco))  |> 
  mutate(clase = str_to_title(clase)) |> 
  pivot_wider(names_from=c(clase, name), values_from=value) 

#obtener los valores de correlación para los indices y escalas de tiempo en dónde se alcanza la máxima correlación
  
cors2 <- subset(cors,seq(2,8,2))
data_r <- map_df(1:5,function(i){
  cors_m <- mask(cors2,eco[i,])
  lc_m <- mask(lc,eco[i,])
  cors_df <- zonal(cors_m,lc_m,na.rm = TRUE)
  names(cors_df)[2:5] <- c('EDDI','SPI','SPEI','SSI')
  cors_df |> 
    pivot_longer(-1) |> 
    rename(indice = name) |> 
    mutate(clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name),
           eco = eco[i,]$ECO_NAME) |> 
    select(eco,indice,clase,value) 
})

data_r_4gt <- data_r |> 
  filter(clase %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(clase = str_to_title(clase),
         value = value^2) |> #value^2 for R2
  pivot_wider(names_from = indice,values_from = value) |> 
  pivot_longer(-c(clase,eco))  |> 
  mutate(clase = str_to_title(clase)) |> 
  pivot_wider(names_from=c(clase, name), values_from=value) |> 
  rename_with(\(x) str_c(x,'_r'),-eco)

tabla_gt <- full_join(data_ind_4gt,data_r_4gt,by = 'eco') 
# tabla_gt <- tabla_gt |> mutate(macrozone = str_to_title(macro)) |> 
#   select(-macro) |> relocate(macrozone)

library(gt)

tabla_gt$Forest_EDDI[1] <- NA
tabla_gt$Forest_EDDI_r[1] <- NA
tabla_gt$Forest_SPI[1] <- NA
tabla_gt$Forest_SPI_r[1] <- NA
tabla_gt$Forest_SPEI[1] <- NA
tabla_gt$Forest_SPEI_r[1] <- NA
tabla_gt$Forest_SSI[1] <- NA
tabla_gt$Forest_SSI_r[1] <- NA

tabla_gt$Shrubland_SPI[5] <- NA
tabla_gt$Shrubland_SPI_r[5] <- NA

tabla_gt |> 
  rename(Ecoregion = eco) |> 
  dplyr::select(1,22:25,18:21,10:13,6:9,2:5,
         46:49,42:45,34:37,30:33,26:29
         ) |> 
  gt() %>% 
  #opt_stylize(style = 6, color = 'gray')
  data_color(
    columns = 22:41,
    target_columns =2:21,
    palette = rev(viridis::inferno(20)),
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
  pivot_longer(-eco) |> 
  ggplot(aes(name,value,color=value)) + 
  geom_point() + 
  scale_color_viridis_c(option = 'inferno',
                        name = 'r-squared',
                        direction = -1,
                        alpha = .8) + theme(legend.position = 'bottom')

ggpubr::get_legend(plot) |> 
  ggpubr::as_ggplot() |> 
  ggsave(filename = 'output/figs/leyenda_tabla_correlaciones_macro_suelo.png')


  