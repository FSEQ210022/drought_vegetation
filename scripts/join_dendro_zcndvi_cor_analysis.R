# Leer dendrocronologia
# June 2022
# by frzambra

library(readxl)
library(fs)
library(sf)
library(stars)
library(dplyr)
library(tidyr)
library(broom)

infoDendro <- read_xlsx(dir_ls('data/',recurse = TRUE)[5],2)

denCoor <- st_as_sf(infoDendro,coords =c('Lon','Lat'),crs =4326)
denCoor <- st_transform(denCoor,32719)
dir <- '/mnt/HDD4TB_2/data/rasters/Procesados/MODIS/zcNDVIeos.MOD13Q1.006.chl'

zcndvi <- read_stars(dir_ls(dir,regexp = '*.tif$'))
zcndvi <- merge(zcndvi)

data <- st_extract(zcndvi,st_buffer(denCoor,1000),FUN = mean,na.rm = TRUE)

data_zcndvi <- data$X |> 
  as.data.frame() |> 
  setNames(2001:2021) |> 
  cbind(st_drop_geometry(denCoor)) |> 
  pivot_longer(1:21,names_to='years',values_to='zcndvi') |> 
  mutate(years = as.numeric(years))

#data dendro

data_dendro <- read_xlsx(dir_ls('data/',recurse = TRUE)[5],1) |> 
  pivot_longer(2:29,names_to='sites',values_to = 'PC') |> 
  rename(years = yrs) |> 
  filter(years >= 2002)  

dataJ <- left_join(data_zcndvi,data_dendro,by = c('sites','years'))  

#correlacion

res_reg <- dataJ |>
  drop_na() |> 
  nest_by(sites)  |>  
  mutate(model = list(lm(zcndvi ~ PC, data = data))) %>% 
  summarise(glance(model)) |> 
  arrange(desc(adj.r.squared))

library(ggplot2)
library(ggridges)
data_dendro |> 
  ggplot(aes(years,PC,linetype = sites)) + 
  geom_point() +
 # geom_line() +
  scale_x_continuous(breaks = 2002:2014) +
  theme_bw()
  facet_grid(sites~.,scales ='free_y')

data_dendro |> 
  ggplot(aes(x=PC,y=as.factor(sites))) +
  geom_density_ridges() +
  theme_bw()
ggsave('output/plots/density_dendro.png',scale=2,width = 3)

dataJ |> 
  drop_na() |> 
  ggplot(aes(zcndvi,scale(PC))) +
  geom_point() +
  geom_abline(slope=1) +
  facet_wrap(~sites,ncol=4,scales = 'free')
ggsave('output/plots/zcndvi_vs_PC.png',scale=2)

dataJ |> 
  pivot_longer(4:5) |> 
  group_by(name) |> 
  summarize(value =scale(value)) +
  ggplot(aes(years,value,colour = name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~sites,ncol=4,scales='free')

res_reg |> 
  ggplot(aes(sites,adj.r.squared)) +
  geom_col() +
  theme_bw()
ggsave('output/plots/zcndvi_vs_PC_bars.png',scale=2)

#mapa de als regresiones
library(tmap)
tmap_mode('view')
res_reg |> 
  select(1:6) |>
  left_join(denCoor,by = 'sites') |> 
  st_as_sf() |> 
  tm_shape() +
  tm_dots(col ='r.squared',style = 'cont')
  

