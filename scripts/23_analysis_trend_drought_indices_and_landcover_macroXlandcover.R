library(tidyverse)
library(tidymodels)

data_trend_lc <- read_rds('data/processed_data/trends_landcover_2001-2022.rds') |> 
  pivot_longer(-zone) |> 
  rename(landcover = value,macro = zone,class = name)

data_trend_di <- read_rds('data/processed_data/trends_indices_macroXlandocver_2001_2023.rds') |> 
  mutate(macro = str_to_title(macro)) |> 
  rename(class = clase)

data_ana <- data_trend_di |> 
  full_join(data_trend_lc) |> 
  drop_na()

paleta <- read_csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name
colors[3] <- 'red'

#cargar la superficie de landcover ṕor macro class para relativizar las superficies

lc_surf <- read_rds('data/processed_data/surface_landcover_macrozone.rds') |> 
  set_names(c('macro','class','surf_total'))

#buscando una relación con visualización
#
data_ana |> 
  left_join(lc_surf) |> 
  mutate(landcover_r = landcover) |> 
  select(-surf_total) |> 
  pivot_longer(-c(1:2,landcover,landcover_r)) |> 
  mutate(
    index = str_extract(name,'[[:alpha:]]+'),
    scale = str_extract(name,'[0-9]+'),
    landcover = landcover_r,
    scale_t =fct(scale,levels =as.character(c(1,3,6,12,24,36))),
    macro = fct(macro,levels = c('Norte Grande','Norte Chico','Centro','Sur','Austral')),
    class = fct(class,levels = c('Forest','Cropland','Grassland','Savanna','Shrubland','Barren land')),
    landcover = case_when(
      macro == 'Norte Grande' & class %in% c('Forest', 'Cropland','Savanna') ~ NA,
      macro == 'Norte Chico' & class == 'Forest' ~ NA,
      macro == 'Sur' & class %in% c('Shrubland') ~ NA,
      macro == 'Austral' & class == 'Cropland' ~ NA,
      .default = landcover
    ),
    index = case_when(
      index == 'zcSM' ~ 'SSI',
      .default = index)) |>  
  filter(index != 'zcNDVI' & 
           name %in% c('EDDI-6','EDDI-36',
                        'SPI-6','SPI-36',
                       'zcSM-6','zcSM-36')) |> 
  mutate(name = factor(name,
                     levels = c('SPI-6','EDDI-6','zcSM-6',
                                'SPI-36','EDDI-36','zcSM-36'),
                    labels = c('SPI-6','EDDI-6','SSI-6',
                               'SPI-36','EDDI-36','SSI-36'))) |> 
  ggplot(aes(value*10,landcover,color = class,shape = macro)) +
  geom_point() + 
  geom_hline(yintercept = 0,color = 'red',linetype = 'dashed',alpha = .6) +
  geom_vline(xintercept = 0,color = 'red',linetype = 'dashed',alpha = .6) +
  labs(x = 'Trend of drought index (per decade)',
       y = 'Relative trend of land cover change') +
  scale_color_manual('class',values = colors,name = 'Land cover') +
  #scale_x_sqrt() +
  scale_shape('Land cover') +
  facet_wrap(.~name,ncol=3,nrow=2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))
ggsave('output/figs/points_landcover_drought_indices_trend_and_time_scale.png',scale =1.2,width=10,height = 6)

data_model <- data_ana[,c(1:6,9:12,15:18,21:24,29,31)] |> 
  left_join(lc_surf) |> 
  mutate(landcover = landcover/surf_total) |> 
  select(-surf_total) |> 
  pivot_longer(-c(1:2,landcover)) |> 
  pivot_wider(names_from = name,values_from = value) 

data_model <- data_model |> 
  #filter(macro %in% c('Norte Chico','Centro')) |> 
  #select(-starts_with('EDDI')) |> 
  mutate(
    #landcover = cut(landcover,5),
    #landcover = cut(landcover,quantile(landcover,c(0,1/3,2/3,1)),
    #                include.lowest = TRUE),
         class = factor(class),
         macro = factor(macro)) 

df_split <- initial_split(data_model,prop =.9)
df_train <- training(df_split)
df_test <- testing(df_split)

#random forest

rf_spec <- rand_forest(trees = 1000, mode = "regression") |> 
  set_args(importance = 'impurity')
rf_wflow <- workflow(landcover ~ ., rf_spec)
rf_fit <- fit(rf_wflow, df_train)

 augment(rf_fit, new_data = df_train) %>% 
   metrics(truth = landcover, estimate = .pred)
   
#conf_mat(truth = landcover, estimate = .pred_class)

 # 
augment(rf_fit, new_data = df_train) %>%
   conf_mat(truth = landcover, estimate = .pred_class) %>%
   autoplot(type = "heatmap")



library(vip)
rf_fit |> 
  extract_fit_parsnip() |> 
  vip(geom = "point",num_features = 20) + 
  labs(title = "Random forest variable importance") 

data_model |> 
  pull(landcover)

## Resampling
## 

data_folds <- vfold_cv(df_train, v = 5)

rf_wf <- 
  workflow() |> 
  add_model(rf_spec) |> 
  add_formula(landcover~.)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi()
}

ctrl_imp <- control_grid(extract = get_rf_imp)

rf_fit_resample <-
  rf_wf |> 
  fit_resamples(data_folds,control = ctrl_imp)

rf_fit_resample |> 
  select(id, .extracts) %>%
  unnest(.extracts) %>%
  unnest(.extracts) %>%
  group_by(Variable) %>%
  summarise(Mean = mean(Importance),
            Variance = sd(Importance)) %>%
  slice_max(Mean, n = 15) %>%
  ggplot(aes(Mean, reorder(Variable, Mean))) +
  geom_errorbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
  geom_point(aes(Mean)) +
  labs(x = "Variable importance", y = NULL) +
  theme_bw()
ggsave('outputs/fig_errorbar_resample_random_forest_trends.png')
collect_metrics(rf_fit_resample)

