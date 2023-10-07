# Analyzing landcover persistent (80%) per zone
# August 2021
# by frzambra

library(sf)
library(terra)

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land'))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

# 
pal <- read.csv('data/processed_data/paleta_colores_landcover.csv')
t <- pal$class
names(t) <- pal$Name

lc <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') |> 
  project('EPSG:4326')

macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg')

LC_data <- terra::extract(lc,macro)

library(tidyverse)

#paletta landcover
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

LC_data |> 
  mutate(LC_type = factor(lyr.1,levels=paleta$class,labels=paleta$Name),
         zone = factor(ID,levels=1:5,labels=macro$macrozona),
         zone = fct_relevel(zone, "norte grande", "norte chico",
                                           'zona central','zona sur')
         ) |> 
  select(LC_type,zone) |> 
  na.omit() |>  
  group_by(zone,LC_type)|> 
  summarize(ntot = n()) -> summ_data2

summ_data2 |> 
  group_by(zone) |>  
  summarize(totZone = sum(ntot)) |>  
  left_join(summ_data2,by='zone') |>  
  mutate(propClusZon = ntot/totZone) |>  
  ggplot() +
  geom_col(aes(x=zone,y=propClusZon,fill=LC_type)) +
  scale_x_discrete(limits = rev(levels(summ_data2$zone))) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,1,.1), 
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0.01,0)) +
  scale_fill_manual(name = 'Landcover class',
                    values=colors) +
  #coord_polar(start = 0) +
  #facet_grid(zone~.) +
  theme_bw() +
  #coord_flip() +
  theme(
    plot.margin = margin(5, 0, 45, 0, "pt"),
    legend.position = 'bottom',
    #axis.text = element_blank(),
    #axis.text.x = element_text(vjust=10),
    axis.title = element_blank(),
    #panel.grid = element_blank(),
    #panel.grid.minor = element_blank(),
    #plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) -> plotZone
ggsave('output/figs/LC_pers80_per_macrozone.png',width=15,height = 10,scale=.5)  

