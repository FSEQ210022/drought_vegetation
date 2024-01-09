library(tidyverse)
library(tidymodels)

data_trend_lc <- read_rds('data/processed_data/trends_landcover_2001-2022.rds') |> 
  mutate(zone = fct_recode(zone,'norte grande' = 'big north' , 'norte chico' = 'little north','centro' = 'central', 'sur' = 'south' ,'austral' = 'austral')) |> 
  pivot_longer(-zone) |> 
  rename(landcover = value,macro = zone,clase = name)

data_trend_di <- read_rds('data/processed_data/trends_indices_macroXlandocver_2001_2023.rds')

data_ana <- data_trend_di |> 
  full_join(data_trend_lc) |> 
  drop_na()

data_ana |>
  mutate(across(3:31,\(x) {
    ifelse(x<0,0,1)
    })) -> ou
  
#análisis de cluster
library(dbscan)
library(NbClust)

res <- sapply(1:10,\(i){
  kmeans(data_ana[,3:31],i)$tot.withinss  
})

res.nbclust <- NbClust(data_ana[,3:31], distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "ward.D2", index ="dunn")
factoextra::fviz_nbclust(res.nbclust) + theme_minimal() + ggtitle("NbClust's optimal number of clusters")

data_ana <- as.data.frame(data_ana)
row.names(data_ana) <- paste0(data_ana$macro,'_',data_ana$clase)

data_cluster <- data_ana[,1:2]

hclust_eddi <-  data_ana[,3:8,drop = FALSE] |> scale() |>   dist() |> hclust(method = 'ward.D2')

hclust_spi <-  data_ana[,9:14,drop = FALSE] |> scale() |>   dist() |> hclust(method = 'ward.D2')

hclust_spei <-  data_ana[,15:20,drop = FALSE] |> scale() |>   dist() |> hclust(method = 'ward.D2')

hclust_zcsm <-  data_ana[,21:26,drop = FALSE] |> scale() |>   dist() |> hclust(method = 'ward.D2')

hclust_zcndvi <-  data_ana[,27:30,drop = FALSE] |> scale() |>   dist() |> hclust(method = 'ward.D2')

hclust_land <-  data_ana[,31,drop = FALSE] |> scale() |>   dist() |> hclust(method = 'ward.D2')

data_cluster <- data_cluster |> 
  mutate(eddi = cutree(hclust_eddi,4),
         spi  = cutree(hclust_spi,4),
         spei = cutree(hclust_spei,4),
         zcsm = cutree(hclust_zcsm,4),
         zcndvi = cutree(hclust_zcndvi,4),
         land = cutree(hclust_land,4))

data_cluster |> 
  pivot_longer(4:8) |> 
  ggplot(aes(macro,clase,fill = as.factor(value))) + 
  geom_tile() + 
  facet_wrap(~name) +
  theme_bw()
  
# con DBScan
#
data_db <- data_ana[,1:2]

db_eddi <-  data_ana[,3:8,drop = FALSE] |> scale() |> dbscan(1.5)
db_spi <-  data_ana[,9:14,drop = FALSE] |> scale() |> dbscan(1.5)
db_spei <-  data_ana[,15:20,drop = FALSE] |> scale() |> dbscan(1)
db_zcsm <-  data_ana[,21:26,drop = FALSE] |> scale() |> dbscan(1)
db_zcndvi <-  data_ana[,27:30,drop = FALSE] |> scale() |> dbscan(1)
db_land <-  data_ana[,31,drop = FALSE] |> dbscan(150)

data_db <- data_db |> 
  mutate(eddi = db_eddi$cluster,
         spi  = db_spi$cluster,
         spei = db_spei$cluster,
         zcsm = db_zcsm$cluster,
         zcndvi = db_zcndvi$cluster,
         land = db_land$cluster)

data_db |> 
  pivot_longer(3:8) |> 
  ggplot(aes(macro,clase,fill = as.factor(value))) + 
  geom_tile() + 
  facet_wrap(~name) +
  theme_bw()






 
factoextra::fviz_dend(hcl, cex = 0.8, lwd = 0.8, k = 4,
          rect = TRUE,
          k_colors = "jco",
          rect_border = "jco",
          rect_fill = TRUE,
          type = "phylogenic")

clusters <- dbscan(apply(data_ana[,3:31],2,scale),.70)

data_ana$clus <- clusters$cluster

data_ana |> 
  filter(clus == 0)

apply(data_ana[,3:31],2,scale) |> cor() |> dist() |> 
  hclust(method = 'ward.D2') |> plot()

pca <- prcomp(data_ana[,3:31])

plot(pca$rotation[,'PC3'])

set.seed(1234)

data_model <- data_ana[,c(1:6,9:12,15:18,21:24,29,31)]
data_model <- data_model |> 
  #select(-starts_with('EDDI')) |> 
  mutate(landcover = cut(landcover,quantile(landcover)),
         clase = factor(clase),
         macro = factor(macro))

df_split <- initial_split(data_model,prop =.9)
df_train <- training(df_split)
df_test <- testing(df_split)

df_recipe <- recipe(landcover~., data = data_model) 

#building model
tree <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

#workflow
tree_wf <- workflow() %>%
  add_recipe(df_recipe) %>%
  add_model(tree) %>%
  fit(df_train) #results are found here 

tree_fit <- tree_wf %>% 
  pull_workflow_fit()

library(rpart)
library(rpart.plot)
rpart.plot(tree_fit$fit)

tree_wf |> 
  predict(df_test)


#random forest

rf_spec <- rand_forest(trees = 1000, mode = "classification") |> 
  set_args(importance = 'impurity')
rf_wflow <- workflow(landcover ~ ., rf_spec)
rf_fit <- fit(rf_wflow, df_train)

# augment(rf_fit, new_data = df_train) %>%
#   conf_mat(truth = landcover, estimate = .pred_class)
# 
# augment(rf_fit, new_data = df_train) %>%
#   conf_mat(truth = landcover, estimate = .pred_class) %>%
#   autoplot(type = "heatmap")

library(vip)
rf_fit |> 
  extract_fit_parsnip() |> 
  vip(geom = "point",num_features = 20) + 
  labs(title = "Random forest variable importance") 
