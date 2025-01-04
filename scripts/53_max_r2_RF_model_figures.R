library(tidyverse)
library(fs)

files <- dir_ls('data/processed_data',regexp = 'r-squared*.')

data <- map_df(files,read_rds)

data_max_r2 <- data |> 
  group_by(ecoregion,type) |> 
  slice_max(mean,n=1) |> 
  filter(ecoregion != 'Rock and Ice') |> 
  mutate(scale =factor(scale,levels = c(1,3,6,12,24,36)),
         type = case_when(
           type == 'Barren_land' ~ 'Barren Land',
           .default = type))

data_max_r2 |> 
  ggplot(aes(type,mean,color=scale)) +
  geom_point(aes(shape = ecoregion),size=3) +
  scale_color_brewer(palette = 'RdYlBu',name = 'Time scale') +
  scale_shape_manual(values = 1:7,name = 'Ecoregion') +
  scale_size_discrete(name = 'Time scale') +
  labs(y = 'r-squared')+
  theme_bw() +
  theme(axis.title.x = element_blank())
ggsave('output/figs/comparison_r-squared_between_scales_of_drought_indices.png',scale = 1.5)    

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
data2 |> 
  group_by(ecoregion) |> 
  mutate(rel_imp = scales::rescale(rel_imp)) |> 
  mutate(Variable = 
           case_when(
             Variable == 'zcET-1' ~ 'SETI-1',
             .default = Variable),
         Variable = factor(Variable,
                           levels = rev(c(paste0('SPI-',scales),
                                      paste0('EDDI-',scales),
                                      paste0('SPEI-',scales),
                                      paste0('SETI-',scales),
                                      paste0('SSI-',scales),
                                      "Burned area","Road density","Nigh Lights"
                                      )))
         ) |> 
  right_join(data_max_r2,by = c('ecoregion','type')) |> 
  group_by(ecoregion,type) |> 
  slice_max(rel_imp,n=2) |> 
  mutate(ecoregion = factor(ecoregion,levels = 
                              c("Atacama desert","Central Andean dry puna",
                                "Southern Andean steppe","Chilean Matorral",
                                "Valdivian temperate forests","Magellanic subpolar forests",
                                "Patagonian steppe"))) |> 
  arrange(ecoregion) |> 
  ggplot(aes(Variable,rel_imp)) +
  geom_col(aes(fill = type),position = position_dodge2(preserve = "single")) +
  #geom_errorbar(aes(ymin = rel_imp-std.err,ymax = rel_imp+std.err),position = position_dodge2(preserve = "single")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = 'Landcover class',
                    values=colors) +
  labs(y = 'Relative variable importance') +
  coord_flip() +
  guides(fill = guide_legend(nrow = 1)) +
  facet_grid(.~fct_inorder(ecoregion)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'bottom')
ggsave('output/figs/bars_relative_importance_RF_scale_max_r2.png',height = 3,width=12,scale = 1)
