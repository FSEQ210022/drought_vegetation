library(gt)
library(gtExtras)
library(tidyverse)
library(kableExtra)

dataLCV_ts <- readRDS('data/processed_data/timeseries_landcover_ecoregion_LC_class_2001-2023.rds')  |> 
  filter(eco != "Rock and Ice") |> 
  mutate(eco =fct(as.character(eco),levels = c("Atacama desert","Chilean Matorral","Valdivian temperate forests","Magellanic subpolar forests","Patagonian steppe")))

dataLCV_trend <- readRDS('data/processed_data/trends_landcover_2001-2023.rds') |> 
  filter(eco != "Rock and Ice") |> 
  mutate(eco =fct(as.character(eco),levels = c("Atacama desert","Chilean Matorral","Valdivian temperate forests","Magellanic subpolar forests","Patagonian steppe")))

dataSpark <- c('Shrubland', 'Savanna', 'Grassland', 'Barren land','Forest','Cropland') %>% 
  map(function(class){
    a <- dataLCV_ts |> 
      complete(LC_type) |> 
      ungroup() |> 
      filter(LC_type == class) |>   
      group_by(eco,LC_type) |> 
      select(eco,LC_type,sup_km2) |> 
      group_split() |> 
      setNames(c('a','b','c','d','e')) |> 
      map(function(x) pull(x,sup_km2))
    map(a,\(x) ifelse(is.na(x),0,x))
  })


dataLCV_trend |> 
  select(-eco) |> 
  pivot_longer(-ECO_NAME,names_to = 'cropland_class') |> 
  write_rds('data/processed_data/trend_landcover_change_classXzone_2001_2023.rds')

# dataLCV_trend  |> 
#   kbl(booktabs = TRUE,digits = 1,align ='r','latex',position='!ht',
#       caption='Value of linear change trend next to time-series plot of surface, per landcover class (IGBP MCD12Q1.006) for 2001-2019 through Central Chile. Red dots on the plots indicate the maximum and minimum surface.') %>%
#   kable_styling(latex_options="scale_down") %>% 
#   kable_paper(full_width = FALSE) %>%
#   column_spec(2, image = spec_plot(dataSpark[[1]],same_lim = FALSE)
#   ) %>%
#   column_spec(3, image = spec_plot(dataSpark[[2]],same_lim = FALSE)
#   ) %>%
#   column_spec(4, image = spec_plot(dataSpark[[3]],same_lim = FALSE)
#   ) %>%
#   column_spec(5, image = spec_plot(dataSpark[[4]],same_lim = FALSE)
#   ) %>%
#   column_spec(6, image = spec_plot(dataSpark[[5]],same_lim = FALSE)
#   ) %>%
#   column_spec(7, image = spec_plot(dataSpark[[6]],same_lim = FALSE)
#   ) %>% 
#   add_header_above(c(" ",'Trend of change [$km^2 year^{-1}$]' = 6),escape =FALSE) |> 
#   save_kable(file = "table_2.png",
#              zoom = 1.5)

#con gt

dataLCV_trend_tran <- dataLCV_trend |> 
  select(-ECO_NAME) |> 
  pivot_longer(-eco) |> 
  pivot_wider(names_from = 'eco',values_from = 'value')

data_gt <- dataLCV_ts |> 
  ungroup() |> 
  #mutate(zone = fct_relevel(zone,c('norte grande','norte chico','centro', 'sur','austral'))) |> 
  dplyr::group_by(eco,LC_type)  |> 
  # must end up with list of data for each row in the input dataframe
  dplyr::summarize(lc_data = list(prop), .groups = "drop") |> 
  pivot_wider(names_from = eco, values_from = lc_data) |> 
  full_join(dataLCV_trend_tran,by = c('LC_type' = 'name')) |> 
  select(LC_type,"Atacama desert.x","Atacama desert.y",
         "Chilean Matorral.x","Chilean Matorral.y",
         "Valdivian temperate forests.x","Valdivian temperate forests.y",
         "Magellanic subpolar forests.x","Magellanic subpolar forests.y",
         "Patagonian steppe.x","Patagonian steppe.y")
         
         
# data_gt$Savanna.x[1] <- NA
# data_gt$Savanna.y[1] <- NA
# data_gt$Forest.x[c(1,2)] <- NA
# data_gt$Forest.y[c(1,2)] <- NA
# data_gt$Cropland.x[c(1,5)] <- NA
# data_gt$Cropland.y[c(1,5)] <- NA
# data_gt$Shrubland.x[4] <- NA
# data_gt$Shrubland.y[4] <- NA

data_gt |> 
  mutate(LC_type = factor(LC_type,levels = c('Forest','Cropland','Grassland','Savanna','Shrubland','Barren land'))) |> 
  rename(`Land cover type` = "LC_type") |> 
  arrange(`Land cover type`) |> 
  gt() |> 
  fmt_number(decimals = 0) |> 
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) |> 
  gt_plt_sparkline(`Atacama desert.x`,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(`Chilean Matorral.x`,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(`Valdivian temperate forests.x`,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(`Magellanic subpolar forests.x`,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(`Patagonian steppe.x`,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  cols_align(
    align = "left",
    columns = `Land cover type`
  ) |> 
  tab_spanner_delim(
    delim="." ) |> 
  tab_header(
    title = md("Trend of change [km<sup>2</sup> year<sup>-1</sup>]"),
  ) |> 
  gtsave('output/figs/table_var_landcover_macro.png',vwidth=2000)
  
