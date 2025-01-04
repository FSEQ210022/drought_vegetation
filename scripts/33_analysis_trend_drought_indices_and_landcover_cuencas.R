library(tidyverse)
library(tidymodels)
library(probably)

# a que ecoregion pertenece cada cuenca
cuenca_eco <- read_rds('data/processed_data/cuencas_ecoregiones.rds')

data_trend_lc <- read_rds('data/processed_data/trends_landcover_cuencas_2001-2023.rds') |> 
  pivot_longer(-cuenca) |> 
  rename(landcover = value,class = name) |> 
  replace_na(list(landcover = 0)) |> 
  pivot_wider(names_from = 'class',values_from = 'landcover')

data_trend_di <- read_rds('data/processed_data/df_trends_indices_cuencas.rds') |> 
  pivot_wider(names_from = 'index',values_from = 'trend') |> 
  rename(cuenca = COD_SUBC )

data_ana <- data_trend_di |> 
  full_join(data_trend_lc) |> 
  full_join(cuenca_eco,by = c('cuenca' = 'COD_SUBC')) |> 
  relocate(ECO_NAME,.after=cuenca) |> 
  relocate(Latitude,.after=ECO_NAME) |> 
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
  set_names(str_remove(names(data_model),'trend_mann_kendall_mod_')) |> 
  rename('Barren_land' = `Barren land`)
  

#random forest
vars <- c("Shrubland",   "Savanna",     "Grassland",   "Barren_land", "Cropland",    "Forest")
ecoregiones <- data_model |> distinct(ECO_NAME) |> pull(ECO_NAME)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi()
}

#probando la tendencia como categoria
#
# para tendencia en usos de suelo

scales <- c(1, 3, 6, 12, 24, 36)

scales |> 
  map(\(scale){

  df <- map(1:6,\(i){
    map(ecoregiones,\(eco){
      t <- which(vars == vars[[i]])
      data_m <- data_model |> 
        filter(ECO_NAME == {{ eco }}) |> 
        select(-cuenca,-ECO_NAME,-Latitude) |> 
        select(-vars[-t]) 
      
      
      #seleccionar los indices para una escala de tiempo
      data_m |> names() |> str_extract("(\\d)+") -> a
      ind <- which(a %in% c(scale))
      
      data_m <- data_m |> 
        select(ind,-`zcNDVI-6`,-`zcNDVI-1`,-`zcNDVI-3`,-`zcNDVI-12`,trend_area_quemada,trend_luces_nocturnas,densidad_vial,last_col())
      
      df_split <- initial_split(data_m,prop =0.75)
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
      print(eco)
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
        mutate(type = vars[i],ecoregion = {{eco}}) |> 
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
      #ggsave(paste0('output/figs/fig_errorbar_resample_random_forest_trends_',vars[i],'_',eco,'.png'),scale =1.5)
      df_var_imp <- df_var_imp |> 
        slice_max(Mean, n = 5) |> 
        mutate(type = vars[i],ecoregion = {{eco}}) 
      list(df_met,df_var_imp)
    })
  })
  
  #crear tablas
  #
  
  tabla <- map_df(1:6,\(i){
    map_df(1:8,\(j){
      pluck(df,i,j,2)
    })
  })
  
  #grafico de var importance
  #
  #paleta colores landcover persistencia
  
  paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv') |> 
    dplyr::filter(Name %in% c('Forest','Shrubland','Savanna','Grassland','Cropland','Barren land')) |> 
    mutate(Name = case_when(Name == 'Barren land' ~ "Barren Land",
                            .default = Name))
  
  colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
  #attr(colors,'names') <- paleta$class
  #paleta$Name[6] <- 'Barren_land'
  attr(colors,'names') <- paleta$Name
  
  library(broom.helpers)
  tabla |>
    filter(ecoregion != "Rock and Ice") |> 
    mutate(
      std.err = sqrt(Variance),
      Variable = .clean_backticks(Variable),
      Variable = case_when(Variable == 'trend_area_quemada' ~ 'Burned area',
                           Variable == 'trend_luces_nocturnas' ~ 'Nigh Lights',
                           Variable == 'densidad_vial' ~ 'Road density',
                           Variable == 'zcNDVI-6' ~ 'zcNDVI',
                           Variable == "zcET-6" ~ "SETI-6",
                           Variable == "zcET-12" ~ "SETI-12",
                           Variable == "zcET-24" ~ "SETI-24",
                           Variable == "zcET-36" ~ "SETI-36",
                           .default = Variable),
      type = case_when(type == 'Barren_land' ~ 'Barren Land',
                       .default = type),
      type = factor(type,levels=paleta$Name),
      ecoregion = fct(ecoregion,
                      levels = 
                        c("Atacama desert","Central Andean dry puna",
                          "Southern Andean steppe","Chilean Matorral",
                          "Valdivian temperate forests","Magellanic subpolar forests",
                          "Patagonian steppe"))
           # Variable = .clean_backticks(Variable),
           # Mean = case_when(macrozona == "Norte Grande" & (type == "Forest" | type == "Cropland" | type == "Savanna") ~ NA,
           #                  macrozona == "Norte Chico" & type == "Forest" ~ NA,
           #                  macrozona == "Sur" & type == "Shrubland" ~ NA,
           #                  macrozona == "Austral" & type == "Cropland" ~ NA,
           #                  .default = Mean)
    ) |> 
    group_by(type,ecoregion) |> 
    mutate(rel_imp = Mean*10e2) -> table_vip
  
  write_rds(table_vip,glue('data/processed_data/tabla_vip_scale_{scale}.rds'))
  
  table_vip |> 
    slice_max(rel_imp, n= 2) |> 
    ggplot(aes(Variable,rel_imp)) +
    geom_col(aes(fill = type),position = position_dodge2(preserve = "single")) +
    #geom_errorbar(aes(ymin = rel_imp-std.err,ymax = rel_imp+std.err),position = position_dodge2(preserve = "single")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(name = 'Landcover class',
                      values=colors) +
    labs(y = 'Relative variable importance') +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1)) +
    facet_grid(.~ecoregion) +
    theme_bw() +
    theme(strip.background = element_rect('white'),
          legend.background = element_rect('transparent'),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size=9),
          legend.text = element_text(size=8),
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'bottom')
  ggsave(glue::glue('output/figs/bars_relative_importance_RF_scale_{scale}.png'),height = 3,width=12,scale = 1)
  
  
  tabla2 <- map_df(1:6,\(i){
    map_df(1:8,\(j){
      pluck(df,i,j,1)
    })
  })
  
  #Gráfico de los r-squared
  
  tabla2 |>  
    filter(.metric == 'rsq' & ecoregion != "Rock and Ice") |> 
    mutate(type = case_when(type == 'Barren_land' ~ 'Barren Land',
                            .default = type)) |> 
    ggplot(aes(ecoregion,mean,color=type)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin=mean - std_err,ymax= mean+std_err),position = position_dodge(width = .5)) +
    scale_color_manual(name = 'Landcover class', values = colors) +
    labs(y='r-squared') +
    #facet_grid(.~type) +
    theme_bw() +
    theme(axis.text.x  =element_text(angle=0,hjust=.5),
          axis.title.x = element_blank(),
          strip.background = element_rect(fill = 'white'),
          legend.position = 'bottom')
  ggsave(glue::glue('output/figs/error_bar_random_forest_10_fold_{scale}.png'),scale=1.5)
  
  r2s <- tabla2 |> 
    filter(.metric == 'rsq') |> 
    select(-std_err,-n) |>
    mutate(scale = {{scale}})  
  write_rds(r2s,glue::glue('data/processed_data/r-squared_zcNDVI6_vs_drought_indices_{scale}_months.rds'))
  
  })

# Hay que modificar desde acá hacia abajo ----


head <- c('Ecoregion',paste0(names(r2s),'\n (R<sup>2=',round(r2s[1,-1],2),')'))

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
