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

ecoregions <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  filter(ECO_NAME != "Rock and Ice") |> 
  mutate(ECO_NAME = fct(ECO_NAME,levels = c("Atacama desert","Chilean Matorral","Valdivian temperate forests","Magellanic subpolar forests","Patagonian steppe")))
  

LC_data <- terra::extract(lc,ecoregions,bind = TRUE)

library(tidyverse)

#paletta landcover
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

LC_data |> 
  mutate(LC_type = factor(lyr.1,levels=paleta$class,labels=paleta$Name),
         ECO_NAME = factor(as.character(ID),labels = c("Atacama desert", "Chilean Matorral", "Magellanic subpolar forests","Patagonian steppe", "Valdivian temperate forests"))) |> 
  select(LC_type,ECO_NAME) |> 
  na.omit() |>  
  group_by(ECO_NAME,LC_type)|> 
  summarize(ntot = n()) -> summ_data2
write_rds(summ_data2,'data/processed_data/surface_landcover_macrozone.rds')
#tabla con superficie
#
library(gt)
summ_data2 |> 
  mutate(surface = 21.5*ntot*0.01) |> 
  select(-ntot) |> 
  mutate(surface = case_when(surface <=20~NA,
                           .default = surface)) |> 
  pivot_wider(names_from = LC_type,values_from=surface) |> 
  rename(Ecoregions = ECO_NAME) |> 
  ungroup() |> 
   select(1,7:6,4:2,5) |> 
  gt() |> 
  grand_summary_rows(
    columns = 2:7,
    fmt = ~ fmt_number(., decimal = 0, use_seps = TRUE),
    fns = list(
      label = 'Total',fn ="sum"
    )) |> 
  fmt_number(decimals = 0) |> 
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) |> 
  tab_header(
    title = md("Surface  [km<sup>2</sup>]"),
  ) |> 
  gt::gtsave('output/figs/table_surface_landcover_macrozone.png')

summ_data2 |> 
  group_by(ECO_NAME) |>  
  summarize(totZone = sum(ntot)) |>  
  left_join(summ_data2,by='ECO_NAME') |>  
  mutate(propClusZon = ntot/totZone) |>  
  ggplot() +
  geom_col(aes(x=ECO_NAME,y=propClusZon,fill=LC_type)) +
  scale_x_discrete(limits = rev(levels(summ_data2$ECO_NAME))) +
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
    plot.margin = margin(3, 3, 3, 3, "pt"),
    legend.position = 'bottom',
    #axis.text = element_blank(),
    #axis.text.x = element_text(vjust=10),
    axis.title = element_blank(),
    #panel.grid = element_blank(),
    #panel.grid.minor = element_blank(),
    #plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) + 
  guides(fill = guide_legend(nrow = 1)) -> plotZone
ggsave('output/figs/LC_pers80_per_macrozone.png',width=7,height = 4,scale=1.5)  

