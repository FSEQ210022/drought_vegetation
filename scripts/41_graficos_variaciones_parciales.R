library(tidyverse)
library(tidymodels)
library(probably)

cuenca_macro <- read_rds('data/processed_data/cuencas_macrozona.rds')

data_trend_lc <- read_rds('data/processed_data/trends_landcover_cuencas_2001-2022.rds') |> 
  pivot_longer(-cuenca) |> 
  rename(landcover = value,class = name) |> 
  replace_na(list(landcover = 0)) |> 
  pivot_wider(names_from = 'class',values_from = 'landcover')

data_trend_di <- read_rds('data/processed_data/df_trends_indices_cuencas.rds') |>
  mutate(index = case_when(
    index == 'zcSM-1' ~ 'SSI-1',
    index == 'zcSM-3' ~ 'SSI-3',
    index == 'zcSM-6' ~ 'SSI-6',
    index == 'zcSM-12' ~ 'SSI-12',
    index == 'zcSM-24' ~ 'SSI-24',
    index == 'zcSM-36' ~ 'SSI-36',
    .default = index)
  ) |> 
  replace_na(list(trend = 0)) |> 
  pivot_wider(names_from = 'index',values_from = 'trend') |> 
  rename(cuenca = COD_SUBC )

data_model <- data_trend_di |> 
  full_join(data_trend_lc) |> 
  full_join(cuenca_macro,by = c('cuenca' = 'COD_SUBC')) |> 
  relocate(macrozona,.after=cuenca) |> 
  relocate(Latitude,.after=macrozona) |> 
  drop_na()

d1 <- pivot_longer(data_model,`SPEI-1`:`EDDI-6`,names_to ='indices',values_to ='val_index') 
d2 <- pivot_longer(data_model,Shrubland:Forest,names_to ='type',values_to = 'trend_lc') 

data <- full_join(d1,d2,by=c('cuenca','macrozona')) |> 
  select(cuenca:macrozona,indices,type,val_index,type,trend_lc)

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(Name = case_when(Name == 'Barren land' ~ "Barren Land",
                          .default = Name))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
#attr(colors,'names') <- paleta$class
#paleta$Name[6] <- 'Barren_land'
attr(colors,'names') <- paleta$Name

library(broom.helpers)

data |> 
  mutate(
    type = case_when(type == 'Barren_land' ~ 'Barren Land',
                     .default = type),
    type = factor(type,levels=paleta$Name),
    macrozona = factor(macrozona,levels=c("norte grande", "norte chico",
                                          'zona central','zona sur','zona austral'),
                       labels = c('Norte Grande','Norte Chico','Centro','Sur','Austral')),
    indices = .clean_backticks(indices),
    trend_lc = case_when(macrozona == "Norte Grande" & (type == "Forest" | type == "Cropland" | type == "Savanna") ~ NA,
                     macrozona == "Norte Chico" & type == "Forest" ~ NA,
                     macrozona == "Sur" & type == "Shrubland" ~ NA,
                     macrozona == "Austral" & type == "Cropland" ~ NA,
                     .default = trend_lc)
  ) |> 
  mutate(indices =
           factor(indices,
                  levels = c(
                    paste0('SPI-',c(1,3,6,12,24,36)),
                    paste0('EDDI-',c(1,3,6,12,24,36)),
                    paste0('SPEI-',c(1,3,6,12,24,36)),
                    paste0('SSI-',c(1,3,6,12,24,36))
                  ))) -> data_var_parc

data_var_parc |>   
  filter(indices %in% c('SPI-6','SPI-36','EDDI-6','EDDI-36','SSI-6','SSI-36')) |> 
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
ggsave('output/figs/RF_most_important_variables_trend_LC_vs_trend_DI.png',height = 7,width=12,scale = 1.1)

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


