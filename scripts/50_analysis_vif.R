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

library(car)

data_model <- data_ana |> 
  set_names(str_remove(names(data_ana),'trend_mann_kendall_mod_')) |> 
  rename('Barren_land' = `Barren land`)

#random forest
vars <- c("Shrubland",   "Savanna",     "Grassland",   "Barren_land", "Cropland",    "Forest")
ecoregiones <- data_model |> distinct(ECO_NAME) |> pull(ECO_NAME)

#probando la tendencia como categoria
#
# para tendencia en usos de suelo

scales <- c(1, 3, 6, 12, 24, 36)

map(scales,\(scale){
  df <- map_df(1:6,\(i){
    map(ecoregiones,\(eco){
      t <- which(vars == vars[[i]])
      data_m <- data_model |> 
        filter(ECO_NAME == {{ eco }}) |> 
        select(-cuenca,-ECO_NAME,-Latitude) |> 
        select(-vars[-t]) |> 
        select(-starts_with('SPI'))
      
      data_m |> names() |> str_extract("(\\d)+") -> a
      ind <- which(a %in% scale)
      
      data_m <- data_m |> 
        select(ind,`zcNDVI-6`,-`zcNDVI-1`,-`zcNDVI-3`,-`zcNDVI-12`,trend_area_quemada,trend_luces_nocturnas,densidad_vial,last_col()) |> 
        rename_with(\(x) str_replace(x,'zcET','SETI'),contains('zcET'))
      
      
      model <- lm(as.formula(paste0(vars[[i]],'~.')),data_m)
      
      vif_results <- car::vif(model)
      
      # Convert VIF results to a data frame for plotting
      vif_df <- data.frame(Variable = names(vif_results), VIF = vif_results,type = vars[[i]])
      vif_df
      # Set a threshold to indicate high VIF
      #high_vif_threshold <- 5
      # 
      # # Create a ggplot bar plot to visualize VIF values
      # ggplot(vif_df, aes(x = Variable, y = VIF)) +
      #   geom_bar(stat = "identity", fill = "steelblue") +
      #   geom_hline(yintercept = high_vif_threshold, linetype = "dashed", color = "red") +
      #   scale_y_continuous(limits = c(0, max(vif_df$VIF) + 1)) +
      #   labs(title = "Variance Inflation Factor (VIF) for Regression Model",
      #        y = "VIF",
      #        x = "Variable") +
      #   theme_minimal() +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # 
    })
  })
  
  library(broom.helpers)
  
  high_vif_threshold <- 5
  df |> 
    #filter(VIF < 10) |> 
    mutate(Variable = broom.helpers::.clean_backticks(Variable),
           Variable = case_when(Variable == 'trend_area_quemada' ~ 'Bourned area',
                                Variable == 'densidad_vial' ~ 'Road density',
                                Variable == 'trend_luces_nocturnas' ~ 'Night lights',
                                Variable == 'zcNDVI-6'~'zcNDVI',
                                .default = Variable)) |> 
    ggplot(aes(x = Variable, y = VIF)) +
      #geom_col( aes(fill = VIF),position = 'stack') +
      geom_point() +
      geom_line() +
      geom_hline(yintercept = high_vif_threshold, linetype = "dashed", color = "red") +
      scale_y_continuous(limits = c(0, max(df$VIF) + 1)) +
      labs(title = "Variance Inflation Factor (VIF) for Regression Model",
           y = "VIF",
           x = "Variable") +
    facet_wrap(.~type) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = 'white'),
          axis.title.x = element_blank())
  ggsave(glue::glue('output/figs/vif_analysis_scale_{scale}.png'),scale=1,width=10,height = 5)
     
})    