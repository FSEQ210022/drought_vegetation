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

data_model <- data_ana[,c(1:6,9:12,15:18,21:24,29,31)]
data_model <- data_model |> 
  #select(-starts_with('EDDI')) |> 
  mutate(
    #landcover = cut(landcover,5),
    landcover = cut(landcover,quantile(landcover,c(0,1/3,2/3,1)),
                    include.lowest = TRUE),
         class = factor(class),
         macro = factor(macro))

df_split <- initial_split(data_model,prop =.9)
df_train <- training(df_split)
df_test <- testing(df_split)

#random forest

rf_spec <- rand_forest(trees = 1000, mode = "classification") |> 
  set_args(importance = 'impurity')
rf_wflow <- workflow(landcover ~ ., rf_spec)
rf_fit <- fit(rf_wflow, df_train)

 augment(rf_fit, new_data = df_train) %>%
   conf_mat(truth = landcover, estimate = .pred_class)

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

