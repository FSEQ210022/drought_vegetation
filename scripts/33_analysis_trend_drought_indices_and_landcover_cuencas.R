library(tidyverse)
library(tidymodels)
library(probably)

cuenca_macro <- read_rds('data/processed_data/cuencas_macrozona.rds')

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
  full_join(cuenca_macro,by = c('cuenca' = 'COD_SUBC')) |> 
  relocate(macrozona,.after=cuenca) |> 
  relocate(Latitude,.after=macrozona) |> 
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
  rename('Barren_land' = `Barren land`)

#random forest
vars <- c("Shrubland",   "Savanna",     "Grassland",   "Barren_land", "Cropland",    "Forest")
macrozones <- data_model |> distinct(macrozona) |> pull(macrozona)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi()
}

#probando la tendencia como categoria
#
# para tendencia en usos de suelo

df <- map(1:6,\(i){
  map(macrozones,\(macro){
    t <- which(vars == vars[[i]])
    data_m <- data_model |> filter(macrozona == {{ macro }}) |> select(-cuenca,-macrozona,-Latitude) 
    df_split <- initial_split(data_m |> select(-vars[-t]),prop =0.85)
    df_train <- training(df_split)
    df_test <- testing(df_split)
    
    rf_spec <- rand_forest(trees = 1000, mode = "regression") |>
      set_args(importance = 'impurity')
    
    rf_wflow <- workflow(as.formula(paste0(vars[i],'~ .')), rf_spec)
    rf_fit <- fit(rf_wflow, df_train)
    
    # predict(rf_fit,new_data =df_test) |>
    #   bind_cols(trend = df_test$Shrubland) |>
    #   ggplot(aes(.pred,trend)) + geom_point() + geom_abline(col='green') +
    #   ggtitle(paste0('Macrozona: ',macro,' \n Uso de suelo: ',vars[t]))
    # 
    # # 
    #  augment(rf_fit, new_data = df_test) %>%
    #    metrics(as.name(vars[i]),.pred)
    # # 
    print(macro)
    print(vars[t])
    augment(rf_fit, new_data = df_test) %>%
        metrics(vars[i],.pred) |> print()
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
      mutate(type = vars[i],macrozona = {{macro}}) |> 
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
    ggsave(paste0('output/figs/fig_errorbar_resample_random_forest_trends_',vars[i],'_',macro,'.png'),scale =1.5)
    df_var_imp <- df_var_imp |> 
      slice_max(Mean, n = 5) |> 
      mutate(type = vars[i],macrozona = {{macro}}) 
    list(df_met,df_var_imp)
  })
})

#crear tablas
#

tabla <- map_df(1:6,\(i){
  map_df(1:5,\(j){
    pluck(df,i,j,2)
  })
})

#grafico de var importance
#
#paleta colores landcover persistencia
library(broom.helpers)

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
  dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
  mutate(Name = case_when(Name == 'Barren land' ~ "Barren Land",
                          .default = Name))

colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
#attr(colors,'names') <- paleta$class
#paleta$Name[6] <- 'Barren_land'
attr(colors,'names') <- paleta$Name

tabla |>
  mutate(
    type = case_when(type == 'Barren_land' ~ 'Barren Land',
                     .default = type),
    type = factor(type,levels=paleta$Name),
         macrozona = factor(macrozona,levels=c("norte grande", "norte chico",
                                               'zona central','zona sur','zona austral'),
                            labels = c('Norte Grande','Norte Chico','Centro','Sur','Austral')),
         Variable = .clean_backticks(Variable),
         Mean = case_when(macrozona == "Norte Grande" & (type == "Forest" | type == "Cropland" | type == "Savanna") ~ NA,
                          macrozona == "Norte Chico" & type == "Forest" ~ NA,
                          macrozona == "Sur" & type == "Shrubland" ~ NA,
                          macrozona == "Austral" & type == "Cropland" ~ NA,
                          .default = Mean)
  ) |> 
  mutate(Variable = 
           factor(Variable,
             levels = rev(c(
                 paste0('SPI-',c(12,24,36)),
                 paste0('EDDI-',c(1,3,6,12,24,36)),
                 paste0('SPEI-',c(3,24,36)),
                 paste0('SSI-',c(1,3,36))
                 )))) |>
  drop_na() |> 
  group_by(type,macrozona) |> 
  mutate(rel_imp = Mean*10e2) |> 
  slice_max(rel_imp, n= 3) |> 
  ggplot(aes(Variable,rel_imp)) +
  geom_col(aes(fill = type),position = 'dodge') +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = 'Landcover class',
                    values=colors) +
  labs(y = 'Relative variable importance') +
  coord_flip() +
  guides(fill = guide_legend(nrow = 1)) +
  facet_grid(.~macrozona) +
  theme_bw() +
  theme(strip.background = element_rect('white'),
        legend.background = element_rect('transparent'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'bottom')
ggsave('output/figs/bars_relative_importance_RF.png',height = 3,width=12,scale = 1)


tabla2 <- map_df(1:6,\(i){
  map_df(1:5,\(j){
    pluck(df,i,j,1)
  })
})

r2s <- tabla2 |> 
  filter(.metric == 'rsq') |> 
  select(-std_err,-n) |>
  mutate(
    mean = case_when(
      macrozona == "norte grande" & (type == "Forest" | type == "Cropland" | type == "Savanna") ~ NA,
      macrozona == "norte chico" & type == "Forest" ~ NA,
      macrozona == "zona sur" & type == "Shrubland" ~ NA,
      macrozona == "zona austral" & type == "Cropland" ~ NA,
      .default = mean)
    
  ) |> 
  # group_by(type) |> 
  # summarize(mean = mean(mean)) |> 
  pivot_wider(names_from = type,values_from = mean) |> 
  _[c(2,1,4,5,3),] |> 
  select(Forest,Cropland,Grassland, Savanna,Shrubland,Barren_land ) 

head <- c('Macrozone',paste0(names(r2s),'\n (R<sup>2=',round(r2s[1,],2),')'))

library(gt)
tabla_gt <- tabla |> 
  mutate(macrozona = factor(macrozona),
         macrozona = fct_relevel(macrozona,c('norte grande','norte chico','zona central','zona sur','zona austral')),
         macrozona =factor(macrozona,labels = c('Norte Grande','Norte Chico','Centro','Sur','Austral')))  |> 
  group_by(type,macrozona) |> 
  slice_max(Mean,n =1) |> 
  select(-Mean,-Variance) |> 
  mutate(Variable = .clean_backticks(Variable)) |>
  pivot_wider(names_from = type,values_from = Variable) |> 
  select(Forest,Cropland,Grassland, Savanna,Shrubland,Barren_land ) |> 
  bind_cols(set_names(r2s,paste0(names(r2s),'r2')))

tabla_gt$Forest[1:2] <- ''
tabla_gt$Cropland[c(1,5)] <- ''
tabla_gt$Savanna[1] <- ''
tabla_gt$Shrubland[4] <- ''

tabla_gt |> 
  rename(Macrozone = macrozona,
         `Barren Land` = Barren_land) |> 
  ungroup() |> 
  gt() %>% 
  #opt_stylize(style = 6, color = 'gray')
  data_color(
    columns = 8:13,
    target_columns =2:7,
    palette = rev(viridis::inferno(20)),
    na_color = 'white',
    domain = c(0.12,.51),
    alpha = .8
  ) |> 
  cols_hide(columns = 8:13) |> 
  cols_align(align = 'center') |> 
  tab_footnote(
    footnote = html(local_image('output/figs/leyenda_tabla_r2_random_forest.png',height = 150))) |> 
  gt::gtsave('output/figs/table_r2_most_important_variables.png')  

#crear legenda para incluir en la tabla
plot <- r2s |> mutate(id=1:5) |> 
  pivot_longer(-id) |> 
  ggplot(aes(name,value,color=value)) + 
  geom_point() + 
  scale_color_viridis_c(option = 'inferno',
                        name = 'r-squared',
                        direction = -1,
                        alpha = .8) + theme(legend.position = 'bottom')

ggpubr::get_legend(plot) |> 
  ggpubr::as_ggplot() |> 
  ggsave(filename = 'output/figs/leyenda_tabla_r2_random_forest.png')
