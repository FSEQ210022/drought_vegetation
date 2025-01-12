# script para extraer valores de indicadores de sequía en las clases de landcover

library(terra)
library(fs)
library(sf)
library(tidyverse)

#paleta colores landcover persistencia
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') 
# |> 
#   dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land'))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

lc <- rast('/home/rstudio/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/IGBP80_reclassified.tif')

dir <- '/home/rstudio/discoB/processed/analysis/correlations/'
files <- dir_ls(dir,regexp = 'tif$')

cors <- files |> 
  lapply(\(file) {
    c(resample(rast(file)[[1]],lc,method = 'near'),
      resample(rast(file)[[2]],lc))
  })

cors <- rast(cors)
eco <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_transform(32719) |> 
  filter(ECO_NAME != "Rock and Ice") |> 
  mutate(ECO_NAME = fct(ECO_NAME,levels = 
                          c("Atacama desert","Central Andean dry puna",
                            "Southern Andean steppe","Chilean Matorral",
                            "Valdivian temperate forests","Magellanic subpolar forests",
                            "Patagonian steppe"))) 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#obtener los indices y escalas de tiempo en dónde se alcanza la máxima correlación
cors1 <- subset(cors,seq(1,10,2))
data_ind <- map_df(1:7,function(i){
  cors_m <- mask(cors1,eco[i,])
  lc_m <- mask(lc,eco[i,])
  cors_df <- zonal(cors_m,lc_m,getmode)
  names(cors_df)[2:6] <- c('EDDI','SPEI','SPI','SSI','SETI')
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
  
cors2 <- subset(cors,seq(2,10,2))
data_r <- map_df(1:7,function(i){
  cors_m <- mask(cors2,eco[i,])
  lc_m <- mask(lc,eco[i,])
  cors_df <- zonal(cors_m,lc_m,na.rm = TRUE)
  names(cors_df)[2:6] <- c('EDDI','SPEI','SPI','SSI','SETI')
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
         value = value) |> #correlation
  pivot_wider(names_from = indice,values_from = value) |> 
  pivot_longer(-c(clase,eco))  |> 
  mutate(clase = str_to_title(clase)) |> 
  pivot_wider(names_from=c(clase, name), values_from=value) |> 
  rename_with(\(x) str_c(x,'_r'),-eco)

# data_ind_4gt <-  data_ind_4gt |> pivot_longer(-eco) |> pivot_wider(names_from = eco,values_from = value)
# data_r_4gt <- data_r_4gt |> pivot_longer(-eco) |> pivot_wider(names_from = 'eco',values_from = 'value')
# data_r_4gt$name <- str_remove(data_r_4gt$name,'_r')
# names(data_r_4gt)[2:8] <- paste0(names(data_r_4gt)[2:8],'_r')

tabla_gt <- full_join(data_ind_4gt,data_r_4gt,by = 'eco') 
# tabla_gt <- tabla_gt |> mutate(macrozone = str_to_title(macro)) |> 
#   select(-macro) |> relocate(macrozone)

library(gt)

tabla_gt$Forest_SPEI_r[6] <- NA
tabla_gt$Savanna_SSI_r[1] <- NA
tabla_gt$Shrubland_EDDI_r[7] <- NA
tabla_gt$Shrubland_SETI_r[7] <- NA

tabla_gt |> 
  rename(`Ecoregion` = eco) |> 
  dplyr::select(1,22:31,12:16,7:12,2:6,
                52:61,42:46,37:41,32:36) |> 
  gt() %>% 
  #opt_stylize(style = 6, color = 'gray')
  data_color(
    columns = 27:51,
    target_columns =2:26,
    palette = 'RdBu',
    na_color = 'white',
    alpha = 1,
    domain = c(-0.75,.75)  
  ) |> 
  tab_spanner_delim(
    delim="_"
  ) %>% 
  sub_missing(
    columns=everything(),
    missing_text=""
  ) |> 
  fmt_number() |> 
  cols_hide(columns = 27:51) |> 
  tab_footnote(
    footnote = html(local_image('output/figs/leyenda_tabla_correlaciones_macro_suelo.png',height = 50))) |> 
  gtsave('output/figs/tabla_r_cor_macro_indice.png')

#crear legenda para incluir en la tabla
plot <- tibble(name = 1:50,value = seq(-0.75,0.75,length.out = 50)) |> 
  ggplot(aes(name,value,color=value)) + 
  geom_point() + 
  scale_colour_gradientn(
    name = 'Pearson \ncorrelation ',
    colors = RColorBrewer::brewer.pal(10,'RdBu'),
    breaks = seq(-0.75,0.75,.35)) +
  theme(legend.text = element_text(size=7),
        legend.position = 'bottom')

ggpubr::get_legend(plot) |> 
  ggpubr::as_ggplot() |> 
  ggsave(filename = 'output/figs/leyenda_tabla_correlaciones_macro_suelo.png',scale=1)


  