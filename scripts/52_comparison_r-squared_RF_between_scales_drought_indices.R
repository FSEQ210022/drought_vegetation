library(tidyverse)
library(fs)

files <- dir_ls('data/processed_data/',regexp = 'r-squared_.*')

scales <- c(12,24,36,6)
data <-  seq_along(scales) |> 
  map_df(\(i){
  read_rds(files[i]) |> 
      pivot_longer(-ecoregion) |> 
      mutate(scale = scales[i])
  })

data |> 
  group_by(ecoregion,name) |> 
  slice_max(n=1,value) |> 
  filter(ecoregion != 'Rock and Ice') |> 
  mutate(scale = factor(scale,levels = c(6,12,24,36))) |> 
  ggplot(aes(name,value,color=scale)) +
  geom_point(aes(shape = ecoregion),size = 3) +
  scale_shape_discrete(name = 'Ecoregion') +
  scale_color_brewer(name = 'Scale',palette = 'RdBu') +
  labs(y = 'r-squared')+
  theme_bw() +
  theme(axis.title.x = element_blank())
ggsave('output/figs/comparison_r-squared_between_scales_of_drought_indices.png',scale = 1.5)      
