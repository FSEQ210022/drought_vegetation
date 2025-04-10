# Analyzing landcover variation through Chile
# August 2021
# by frzambra

dir <- '/home/rstudio/discoB/processed/MODIS/'

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

library(terra)
library(rnaturalearth)
library(sf)

chl <- ne_countries(country = 'chile',returnclass = 'sf') |> 
  st_transform(32719)

igbp <- rast(list.files(paste0(dir,'IGBP.MCD12Q1.061'),pattern='*.tif$',full.names=TRUE))
igbp <- crop(igbp,chl)

classes <- matrix(c(1:17,rep(1,5),rep(2,2),rep(3,2),4,5,6,7,6,8,9,10),ncol=2)

igbpRecl <- classify(igbp,rcl=classes,include.lowest = TRUE)
igbpRecl[igbpRecl == 0] <- NA

ecoregions <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_transform(32719) 

data <- terra::extract(igbpRecl,ecoregions)
names(data) <- c('zone',2001:2023)

library(dplyr)
library(tidyr)
library(ggplot2)

data |> 
  pivot_longer(-zone,values_to = 'LC_type',names_to = 'year') |> 
  filter(LC_type %in% c(1:4,6,9)) |>  
  mutate(class2 = LC_type,
         year = as.numeric(year),
         eco = factor(zone,labels = ecoregions$ECO_NAME),
         LC_type = factor(LC_type,levels=paleta$class,labels=paleta$Name)) -> data2 

data2 |> 
  group_by(eco) |>  
  filter(year == 2002) |>  
  summarize(ntot = n()) ->dataSum

data2 |> 
  dplyr::select(-zone) |> 
  group_by(eco,year,LC_type) |>  
  summarise(n=n()) |> 
  left_join(dataSum,by='eco') |>  
  mutate(prop = n/ntot,sup_km2=n*0.2146587) -> data3

saveRDS(data3,'data/processed_data/timeseries_landcover_ecoregion_LC_class_2001-2023.rds')  

#Figura variación landocver type por ecoregion entre 2001-2023
#
data3 |> 
  filter(eco != 'Rock and Ice') |> 
  mutate(eco = factor(eco,
                   levels = 
                     c("Atacama desert","Central Andean dry puna",
                       "Southern Andean steppe","Chilean Matorral",
                       "Valdivian temperate forests","Magellanic subpolar forests",
                       "Patagonian steppe"))) |> 
  ggplot(aes(year,prop,fill = LC_type)) +
  geom_col(position = 'fill') +
  facet_grid(.~eco) +
  #scale_x_discrete(limits = rev(levels(summ_data2$ECO_NAME))) +
  scale_x_continuous(breaks= 2001:2023,expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,.1), 
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0.01,0)) +
  scale_fill_manual(name = 'Landcover class',
                    values=colors) +
  theme_bw() +
  theme(
    plot.margin = margin(3, 3, 3, 3, "pt"),
    legend.position = 'bottom',
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90,size=5),
    strip.background = element_rect(fill='white')
  ) + 
  guides(fill = guide_legend(nrow = 1)) 
ggsave('output/figs/landcover_variation_per_ecoregions_2001-2023.png',scale=1.7,width=7, height=2)

#plot
data3  |>  
  ggplot(aes(year,prop)) +
  geom_point() + 
  geom_line()+
  geom_smooth(method = 'lm',alpha=0.8,se = FALSE,linetype = 'dashed',col='darkblue') +
  scale_fill_manual(name = 'Landcover class',values=colors) +
  scale_x_continuous(breaks = seq(2000,2023,2),expand=c(0,0)) +
  labs(y=expression(paste('Surface [',km^2,']'))) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = .1),
  #                    expand = c(0,0)) +
  facet_wrap(LC_type~eco,scales ='free_y') +
  theme_bw() +
  theme(axis.title.x = element_blank())

#linear trend Mann-Kendall
library(trend)
source('R/trend_func.R')

data3 |> 
  ungroup() |>  
  group_by(eco,LC_type) |>  
  #summarize(trend  = as.numeric(tidy(lm(sup_km2~year,data))[2,2])) |> 
  summarize(trend = trend_func(sup_km2,start = c(2001,1),end =c(2022,1),frecuency=1)[2]) |> 
  pivot_wider(names_from = 'LC_type',values_from=3) |> 
  ungroup() |> 
  mutate(ECO_NAME = ecoregions$ECO_NAME) |> 
  saveRDS('data/processed_data/trends_landcover_2001-2023.rds')  
