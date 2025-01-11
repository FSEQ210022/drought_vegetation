library(tidyverse)
library(fs)

files <- dir_ls('data/processed_data',regexp = 'r-squared*.')

data <- map_df(files,read_rds)

data_max_r2 <- data |> 
  group_by(ecoregion,type) |> 
  slice_max(mean,n=2) |> filter(ecoregion == 'Chilean Matorral' & type == 'Savanna')
  filter(ecoregion != 'Rock and Ice') |> 
  mutate(scale =factor(scale,levels = c(1,3,6,12,24,36)),
         type = case_when(
           type == 'Barren_land' ~ 'Barren Land',
           .default = type))

data_max_r2 |> 
  ggplot(aes(type,mean,color=scale)) +
  geom_point(aes(shape = ecoregion),size=3) +
  scale_color_brewer(palette = 'RdYlBu',name = 'Time scale') +
  scale_shape_manual(values = c(8,13,15,17:19,21),name = 'Ecoregion') +
  scale_size_discrete(name = 'Time scale') +
  labs(y = 'r-squared')+
  theme_bw() +
  theme(axis.title.x = element_blank())
ggsave('output/figs/comparison_r-squared_between_scales_of_drought_indices.png',scale = 1)    

files2 <- dir_ls('data/processed_data',regexp = 'tabla_vip*.')

scales <- c(1,3,6,12,24,36)

data2 <- map_df(scales,\(sc){
  read_rds(glue("data/processed_data/tabla_vip_scale_{sc}.rds")) |> 
    mutate(scale = sc)
  })

#paleta colores landcover persistencia

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(Name = case_when(Name == 'Barren land' ~ "Barren Land",
                          .default = Name))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
#attr(colors,'names') <- paleta$class
#paleta$Name[6] <- 'Barren_land'
attr(colors,'names') <- paleta$Name


data2 |>
  group_by(type) |> 
  mutate(rel_imp = scales::rescale(rel_imp),
         scale = factor(scale,levels = c(1,3,6,12,24,36))) |> 
  mutate(Variable = 
           case_when(
             Variable == 'zcET-1' ~ 'SETI-1',
             Variable == 'zcET-3' ~ 'SETI-3',
             Variable == 'Nigh Lights' ~ 'Night lights',
             Variable == "area_quemada_sum"~"Burned area(*)",
             Variable == "luces_nocturnas_avg" ~ "Night lights(*)",
             .default = Variable),
         Variable = factor(Variable,
                           levels = rev(c(paste0('SPI-',scales),
                                      paste0('EDDI-',scales),
                                      paste0('SPEI-',scales),
                                      paste0('SETI-',scales),
                                      paste0('SSI-',scales),
                                      "Burned area","Burned area(*)","Road density","Night lights",
                                      "Night lights(*)"
                                      
                                      )))
         ) |> 
  right_join(data_max_r2,by = c('ecoregion','type','scale')) |> 
  group_by(ecoregion,type) |> 
  slice_max(n=1,mean) |> 
  slice_max(rel_imp,n=2) |> 
  mutate(ecoregion = factor(ecoregion,levels = 
                              c("Atacama desert","Central Andean dry puna",
                                "Southern Andean steppe","Chilean Matorral",
                                "Valdivian temperate forests","Magellanic subpolar forests",
                                "Patagonian steppe"))) |> 
  arrange(ecoregion) |> 
  group_by(type,ecoregion) |> 
  # slice_max(n=1,mean) |> 
  # group_by(Variable,ecoregion,type) |> 
  # summarize(Mean  = mean(Mean,na.rm = TRUE),
  #           rel_imp = mean(rel_imp,na.rm = TRUE)) |> 

  ggplot(aes(Variable,rel_imp)) +
  geom_col(aes(fill = type),position = position_dodge2(preserve = "single")) +
  #geom_errorbar(aes(ymin = rel_imp-std.err,ymax = rel_imp+std.err),position = position_dodge2(preserve = "single")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = 'Landcover class',
                    values=colors) +
  labs(y = 'Relative variable importance') +
  coord_flip() +
  guides(fill = guide_legend(nrow = 2)) +
  facet_wrap(.~ecoregion,ncol=3,scales = 'free') +
  scale_y_continuous(breaks = seq(0,0.9,.3),
                     limits = c(0,1),
                     expand = c(0,0)) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        #panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.7,.15))
ggsave('output/figs/bars_relative_importance_RF_scale_max_r2.png',height = 7,width=10,scale = 1)
