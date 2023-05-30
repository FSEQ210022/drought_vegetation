# Analyzing landcover variation through central Chile
# August 2021
# by frzambra

dir <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/'

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv',sep='\t')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

library(terra)
library(fs)
pol <- vect('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  project('EPSG:32719')

igbp <- rast(dir_ls(paste0(dir,'IGBP.MCD12Q1.061'),regexp ='*.tif$'))
igbp <- crop(igbp,pol)

classes <- matrix(c(1:17,rep(1,5),rep(2,2),rep(3,2),4,5,6,7,6,8,9,10),ncol=2)

igbpRecl <- classify(igbp,rcl=classes,include.lowest = TRUE)
igbpRecl[igbpRecl == 0] <- NA

data <- terra::extract(igbpRecl,pol)
names(data) <- c('macrozona',2001:2021)

library(dplyr)
library(tidyr)
library(ggplot2)

data |>  
  pivot_longer(-macrozona,values_to = 'LC_type',names_to = 'year') |> 
  filter(LC_type %in% c(1:4,6,9)) |>  
  mutate(class2 = LC_type,year = as.numeric(year),
         zone = factor(macrozona,levels=1:5,labels=pol$macrozona),
         LC_type = factor(LC_type,levels=paleta$class,labels=paleta$Name)) -> data2 
data2  |>  
  group_by(zone) |>  
  filter(year == 2002)  |>  
  summarize(ntot = n()) -> dataSum

data2 |>  
  group_by(zone,year,LC_type) |>  
  summarise(n=n()) |> 
  left_join(dataSum,by='zone') |> 
  mutate(prop = n/ntot,sup_km2=n*0.2146587) -> data3

saveRDS(data3,'data/rds/timeseries_landcover_zone_LCclass_2001-2019.rds')  

#plot
data3  |>  #filter(LC_type == 'Grassland') %>% 
  group_by(LC_type,zone) |> 
  mutate(sup_km2 = scale(sup_km2)) |> 
  ggplot(aes(year,sup_km2)) +
  geom_point() + 
  geom_line()+
  geom_smooth(method = 'lm',alpha=0.8,se = FALSE,linetype = 'dashed',col='darkblue') +
  scale_fill_manual(name = 'Landcover class',values=colors) +
  scale_x_continuous(breaks = seq(2000,2019,2),expand=c(0,0)) +
  labs(y=expression(paste('Surface [',km^2,']'))) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = .1),
  #                    expand = c(0,0)) +
  facet_grid(zone~LC_type,scales ='free') +
  theme_bw() +
  theme(axis.title.x = element_blank())

#linear trend
data3 %>% ungroup() %>% 
  nest_by(zone,LC_type) %>% 
  summarize(trend  = as.numeric(tidy(lm(sup_km2~year,data))[2,2])) %>% 
  pivot_wider(names_from = 'LC_type',values_from=3) %>% 
  saveRDS(.,'data/rds/trends_landcover_2001-2019.rds')  
