library(tidyverse)
library(tidymodels)
library(probably)

cuenca_eco <- read_rds('data/processed_data/cuencas_ecoregiones.rds')

# 1.  leer archivos de variable importancia ----
files2 <- dir_ls('data/processed_data',regexp = 'tabla_vip*.')

data2 <- map_df(files2,read_rds)

#paleta colores landcover persistencia

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(Name = case_when(Name == 'Barren land' ~ "Barren Land",
                          .default = Name))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
#attr(colors,'names') <- paleta$class
#paleta$Name[6] <- 'Barren_land'
attr(colors,'names') <- paleta$Name

scales <- c(1,3,6,12,24,36)
data_var_imp <- data2 |> 
  group_by(ecoregion) |> 
  mutate(rel_imp = scales::rescale(rel_imp)) |> 
  mutate(Variable = 
           case_when(
             Variable == 'zcET-1' ~ 'SETI-1',
             Variable == 'zcET-3' ~ 'SETI-3',
             .default = Variable),
         Variable = factor(Variable,
                           levels = rev(c(paste0('SPI-',scales),
                                          paste0('EDDI-',scales),
                                          paste0('SPEI-',scales),
                                          paste0('SETI-',scales),
                                          paste0('SSI-',scales),
                                          "Burned area","Road density","Nigh Lights",
                                          "area_quemada_sum","luces_nocturnas_avg"
                           )))
  ) |> 
  #right_join(data_max_r2,by = c('ecoregion','type')) |> 
  group_by(ecoregion,type) |> 
  slice_max(rel_imp,n=5) |> 
  mutate(ecoregion = factor(ecoregion,levels = 
                              c("Atacama desert","Central Andean dry puna",
                                "Southern Andean steppe","Chilean Matorral",
                                "Valdivian temperate forests","Magellanic subpolar forests",
                                "Patagonian steppe"))) |> 
  arrange(ecoregion) |> 
  select(Variable,type,ecoregion,rel_imp)


# 2 leer datos de tendencias por landcover ---- 

data_trend_lc <- read_rds('data/processed_data/trends_landcover_cuencas_2001-2023.rds') |> 
  pivot_longer(-cuenca) |> 
  rename(landcover = value,class = name) |> 
  replace_na(list(landcover = 0)) |> 
  mutate(class = case_when(class == 'Barren land' ~ 'Barren Land',
                           .default = class))

# 3. Leer datos de tendencias por índice de sequía

data_trend_di <- read_rds('data/processed_data/df_trends_indices_cuencas_2000-2023.rds') |>
  mutate(index = case_when(
    index == 'zcSM-1' ~ 'SSI-1',
    index == 'zcSM-3' ~ 'SSI-3',
    index == 'zcSM-6' ~ 'SSI-6',
    index == 'zcSM-12' ~ 'SSI-12',
    index == 'zcSM-24' ~ 'SSI-24',
    index == 'zcSM-36' ~ 'SSI-36',
    .default = index),
    index =case_when(
        index == 'zcET-1' ~ 'SETI-1',
        index == 'zcET-3' ~ 'SETI-3',
        index == 'zcET-6' ~ 'SETI-12',
        index == 'zcET-12' ~ 'SETI-12',
        index == 'zcET-24' ~ 'SETI-24',
        index == 'zcET-36' ~ 'SETI-36',
        .default = index
      )
  ) |> 
  replace_na(list(trend = 0)) |> 
  rename(cuenca = COD_SUBC )


# 4 unir los set de datos ----

data_model <- data_trend_di |> 
  full_join(data_trend_lc) |> 
  full_join(cuenca_eco,by = c('cuenca' = 'COD_SUBC')) |> 
  right_join(data_var_imp,by =c('index' = 'Variable','class' = 'type','ECO_NAME' = 'ecoregion')) |> 
  rename(eco = ECO_NAME) |> 
  relocate(eco,.after=cuenca) |> 
  drop_na() |> 
  select(-Latitude) 

# 4 Paleta de colores para landcover
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(Name = case_when(Name == 'Barren land' ~ "Barren Land",
                          .default = Name))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
#attr(colors,'names') <- paleta$class
#paleta$Name[6] <- 'Barren_land'
attr(colors,'names') <- paleta$Name

library(broom.helpers)

data_model |> 
  mutate(
    type = case_when(class == 'Barren_land' ~ 'Barren Land',
                     .default = class),
    type = factor(type,levels=paleta$Name),
    eco = factor(eco,levels = 
                         c("Atacama desert","Central Andean dry puna",
                           "Southern Andean steppe","Chilean Matorral",
                           "Valdivian temperate forests","Magellanic subpolar forests",
                           "Patagonian steppe")),
    index = .clean_backticks(index),
    index = factor(index,
                  levels = c(paste0('SPI-',c(1,3,6,12,24,36)),
                             paste0('EDDI-',c(1,3,6,12,24,36)),
                    paste0('SPEI-',c(1,3,6,12,24,36)),
                    paste0('SETI-',c(1,3,6,12,24,36)),
                    paste0('SSI-',c(1,3,6,12,24,36)),
                           "luces_nocturnas_avg","area_quemada_sum")
                  )) -> data_var_parc

data_var_parc |>  
  group_by(cuenca) |> 
  mutate(trend = scale(trend) |> as.numeric(),
         landcover = scale(landcover)) |> 
  mutate(index = str_remove(str_extract(index, "^\\D+"),'-'),
         index =factor(index,levels = c('SPEI','SETI','SSI', 'luces_nocturnas_avg','area_quemada_sum'),
                       labels = c('SPEI','SETI','SSI', 'Night Lights(*)','Burned Area(*)'))) |> 
  #filter(indices %in% c('SPI-6','SPI-36','EDDI-6','EDDI-36','SSI-6','SSI-36')) |>
  ggplot(aes(trend,landcover,color = class)) + 
  #geom_point(size=.02,alpha = .6) + 
  #geom_line(size = .02,alpha = .6) + 
  geom_smooth(se = FALSE) +
  #scale_x_continuous(expand = c(0,0),labels = function(x) x*1000) +
  facet_grid(index~eco,scales = 'free') +
  #scale_y_continuous(transform ='sqrt') +
  scale_color_manual(name = 'Landcover class',
                    values=colors) +
  geom_hline(yintercept = 0,col='red',lty ='dashed') +
  geom_vline(xintercept = 0,col='red',lty ='dashed') +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = 'Variable value (scaled)',y = 'Land cover trend (scaled)') +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        #axis.title.y = element_blank(),
        legend.position ='bottom')
ggsave('output/figs/RF_most_important_variables_trend_LC_vs_trend_DI.png',height = 7,width=12,scale = 1)

#SPI

data_var_parc |>   
  filter(str_detect(indices,"^SPI")) |> 
  filter(indices != 'SPI-1') |> 
  ggplot(aes(val_index,trend_lc,color = type)) + 
  #geom_point(size=.02,alpha = .6) + 
  #geom_line(size = .02,alpha = .6) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(expand = c(0,0),labels = function(x) x*1000) +
  facet_grid(macrozona~indices,scales = 'free') +
  scale_y_continuous(trans='sqrt') +
  scale_color_manual(name = 'Landcover class',
                     values=colors) +
  geom_hline(yintercept = 0,col='red',lty ='dashed') +
  geom_vline(xintercept = 0,col='red',lty ='dashed') +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = expression(paste('Drought index trend (x',10^{-3},')')),y = expression(paste('Land cover trend (',sqrt(x),')'))) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        #axis.title.y = element_blank(),
        legend.position ='bottom')
ggsave('output/figs/RF_most_important_variables_trend_LC_vs_trend_SPI.png',height = 7,width=12,scale = 1.1)

#SPEI

data_var_parc |>   
  filter(str_detect(indices,"^SPEI")) |> 
  #filter(indices != 'SPI-1') |> 
  ggplot(aes(val_index,trend_lc,color = type)) + 
  #geom_point(size=.02,alpha = .6) + 
  #geom_line(size = .02,alpha = .6) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(expand = c(0,0),labels = function(x) x*1000) +
  facet_grid(macrozona~indices,scales = 'free') +
  scale_y_continuous(trans='sqrt') +
  scale_color_manual(name = 'Landcover class',
                     values=colors) +
  geom_hline(yintercept = 0,col='red',lty ='dashed') +
  geom_vline(xintercept = 0,col='red',lty ='dashed') +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = expression(paste('Drought index trend (x',10^{-3},')')),y = expression(paste('Land cover trend (',sqrt(x),')'))) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        #axis.title.y = element_blank(),
        legend.position ='bottom')
ggsave('output/figs/RF_most_important_variables_trend_LC_vs_trend_SPEI.png',height = 7,width=12,scale = 1.1)

#EDDI
#
data_var_parc |>   
  filter(str_detect(indices,"^EDDI")) |> 
  #filter(indices != 'SPI-1') |> 
  ggplot(aes(val_index,trend_lc,color = type)) + 
  #geom_point(size=.02,alpha = .6) + 
  #geom_line(size = .02,alpha = .6) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(expand = c(0,0),labels = function(x) x*1000) +
  facet_grid(macrozona~indices,scales = 'free') +
  scale_y_continuous(trans='sqrt') +
  scale_color_manual(name = 'Landcover class',
                     values=colors) +
  geom_hline(yintercept = 0,col='red',lty ='dashed') +
  geom_vline(xintercept = 0,col='red',lty ='dashed') +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = expression(paste('Drought index trend (x',10^{-3},')')),y = expression(paste('Land cover trend (',sqrt(x),')'))) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        #axis.title.y = element_blank(),
        legend.position ='bottom')
ggsave('output/figs/RF_most_important_variables_trend_LC_vs_trend_EDDI.png',height = 7,width=12,scale = 1.1)

# SSI

data_var_parc |>   
  filter(str_detect(indices,"^SSI")) |> 
  #filter(indices != 'SPI-1') |> 
  ggplot(aes(val_index,trend_lc,color = type)) + 
  #geom_point(size=.02,alpha = .6) + 
  #geom_line(size = .02,alpha = .6) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(expand = c(0,0),labels = function(x) x*1000) +
  facet_grid(macrozona~indices,scales = 'free') +
  scale_y_continuous(trans='sqrt') +
  scale_color_manual(name = 'Landcover class',
                     values=colors) +
  geom_hline(yintercept = 0,col='red',lty ='dashed') +
  geom_vline(xintercept = 0,col='red',lty ='dashed') +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = expression(paste('Drought index trend (x',10^{-3},')')),y = expression(paste('Land cover trend (',sqrt(x),')'))) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        #axis.title.y = element_blank(),
        legend.position ='bottom')
ggsave('output/figs/RF_most_important_variables_trend_LC_vs_trend_SSI.png',height = 7,width=12,scale = 1.1)


