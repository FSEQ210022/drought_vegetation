library(tidyverse)
library(tidymodels)

data_trend_lc <- read_rds('data/processed_data/trends_landcover_cuencas_2001-2022.rds') |> 
  pivot_longer(-cuenca) |> 
  rename(landcover = value,class = name) |> 
  replace_na(list(landcover = 0)) |> 
  pivot_wider(names_from = 'class',values_from = 'landcover')

data_trend_di <- read_rds('data/processed_data/df_trends_indices_cuencas.rds') |>
  mutate(index = case_when(
    index == 'zcSM-1' ~ 'SSI-1',
    index == 'zcSM-3' ~ 'SSI-3',
    index == 'zcSM-6' ~ 'SSI-6',
    index == 'zcSM-12' ~ 'SSI-12',
    index == 'zcSM-24' ~ 'SSI-24',
    index == 'zcSM-36' ~ 'SSI-36',
    .default = index)
  ) |> 
  replace_na(list(trend = 0)) |> 
  pivot_wider(names_from = 'index',values_from = 'trend') |> 
  rename(cuenca = COD_SUBC )

data_ana <- data_trend_di |> 
  full_join(data_trend_lc) |> 
  drop_na()

#data_model <- data_ana[,c(1:6,9:12,15:18,21:24,29,31)]
data_model <- data_ana 
   # mutate(
   #   `zcNDVI-6` = cut(`zcNDVI-6`,5)
   # )
#     # landcover = cut(landcover,quantile(landcover,c(0,1/3,2/3,1)),
#     #                 include.lowest = TRUE),
#          class = factor(class),
#          macro = factor(macro))

data_model <- data_model |> 
  rename('Barren_land' = `Barren land`) |> 
  select(-cuenca)

#random forest
vars <- c("Shrubland",   "Savanna",     "Grassland",   "Barren_land", "Cropland",    "Forest")

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi()
}

# para tendencia en usos de suelo
 
df <- map(1:6,\(i){
  t <- which(vars == vars[[i]])
  df_split <- initial_split(data_model |> select(-vars[-t]),prop =.8)
  df_train <- training(df_split)
  df_test <- testing(df_split)
  
  rf_spec <- rand_forest(trees = 1000, mode = "regression") |>
    set_args(importance = 'impurity')
  
  rf_wflow <- workflow(as.formula(paste0(vars[i],'~ .')), rf_spec)
  # rf_fit <- fit(rf_wflow, df_train)
  # 
  #  augment(rf_fit, new_data = df_train) %>%
  #    metrics(as.name(vars[i]),.pred)
  # 
  #  augment(rf_fit, new_data = df_test) %>%
  #    metrics(vars[i],.pred)
  #  
  # library(vip)
  # rf_fit |> 
  #   extract_fit_parsnip() |> 
  #   vip(geom = "point",num_features = 20) + 
  #   labs(title = "Random forest variable importance") 
  
  ## Resampling
  ## 
  
  data_folds <- vfold_cv(df_train, v = 10)
  
  rf_wf <- 
    workflow() |> 
    add_model(rf_spec) |> 
    add_formula(as.formula(paste0(vars[i],'~ .')))
  
  ctrl_imp <- control_grid(extract = get_rf_imp)
  
  rf_fit_resample <-
    rf_wf |> 
    fit_resamples(data_folds,control = ctrl_imp)

  #data.frame para guardar metricas
  df_met <- collect_metrics(rf_fit_resample) |> 
    mutate(type = vars[i]) |> 
    select(-.estimator,-.config)
  
  df_var_imp <- rf_fit_resample |> 
    select(id, .extracts) %>%
    unnest(.extracts) %>%
    unnest(.extracts) %>%
    group_by(Variable) %>%
    summarise(Mean = mean(Importance),
              Variance = sd(Importance))
  df_var_imp |>   
    slice_max(Mean, n = 15) %>%
    ggplot(aes(Mean, reorder(Variable, Mean))) +
    geom_errorbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
    geom_point(aes(Mean)) +
    labs(x = "Variable importance", y = NULL) +
    annotate("text",x=max(df_var_imp$Mean),y=1,label =paste0('rsq=',round(df_met$mean[2],2))) +
    theme_bw()
  ggsave(paste0('output/figs/fig_errorbar_resample_random_forest_trends_',vars[i],'.png'))
  df_var_imp <- df_var_imp |> 
    slice_max(Mean, n = 5) |> 
    mutate(type = vars[i])
  list(df_met,df_var_imp)
})

#crear tablas
#

tabla <- map_df(1:6,\(i){
  pluck(df,i,2)
})

tabla2 <- map_df(1:6,\(i){
  pluck(df,i,1)
})

tabla2 <- tabla2 |> 
  filter(.metric == 'rsq') |> 
  select(-std_err,-n) |> 
  pivot_wider(names_from = type,values_from = mean) |> 
  select(.metric,Forest,Cropland,Grassland, Savanna,Shrubland,Barren_land )

head <- paste0(names(tabla2[2:7]),'\n (rsq=',round(tabla2[1,2:7],2),')')
head <- c('Position',head)

library(gt)
library(broom.helpers)
tabla |> 
  mutate(Variable = .clean_backticks(Variable))  |>
  select(-Mean,-Variance) |> 
  mutate(pos = rep(1:5,6)) |> 
  pivot_wider(names_from = type,values_from = Variable) |> 
  select(pos,Forest,Cropland,Grassland, Savanna,Shrubland,Barren_land ) |> 
  set_names(head) |> 
  gt() |> 
  cols_align(align = 'center') |> 
  gt::gtsave('output/figs/table_importance_trends_landcover_vs_drought.png')

# para zcNDVI

df_split <- initial_split(data_model,prop =.8)
df_train <- training(df_split)
df_test <- testing(df_split)

rf_spec <- rand_forest(trees = 1000, mode = "regression") |>
  set_args(importance = 'impurity')
# lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
#   set_engine("glmnet")

rf_wflow <- workflow(as.formula(`zcNDVI-6`~.), rf_spec)
rf_fit <- fit(rf_wflow, df_train)

 augment(rf_fit, new_data = df_train) %>%
   metrics(as.name(vars[i]),.pred)

 augment(rf_fit, new_data = df_test) %>%
   metrics(vars[i],.pred)

 library(vip)
 rf_fit |>
   extract_fit_parsnip() |>
   vip(geom = "point",num_features = 20) +
   labs(title = "Random forest variable importance")
