library(tidyverse)
library(fs)

files <- dir_ls('data/processed_data/',regexp = 'r-squared_.*')

scales <- c(1,3,12,24,36,6)
data <-  map_df(files,read_rds)

data |> 
  rename(name = type,
         value = mean) |> 
  group_by(ecoregion,name) |> 
  slice_max(n=1,value) |> 
  filter(ecoregion != 'Rock and Ice') |> 
  mutate(scale = factor(scale,levels = c(1,3,6,12,24,36)),
         name = case_when(name == 'Barren_land' ~ 'Barren Land',
                          .default = name)) |> 
  ggplot(aes(name,value,color=scale)) +
  geom_point(aes(shape = ecoregion),size = 3) +
  scale_shape_discrete(name = 'Ecoregion') +
  scale_color_brewer(name = 'Time scale',palette = 'RdBu') +
  labs(y = expression(R^2))+
  theme_bw() +
  theme(axis.title.x = element_blank())
ggsave('output/figs/comparison_r-squared_between_scales_of_drought_indices.png',scale = 1.5)      
