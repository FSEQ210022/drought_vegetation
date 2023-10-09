library(gt)
library(gtExtras)
library(tidyverse)
library(kableExtra)

dataLCV_ts <- readRDS('data/processed_data/timeseries_landcover_zone_LCclass_2001-2021.rds')  
dataLCV_trend <- readRDS('data/processed_data/trends_landcover_2001-2021.rds') |> 
  mutate(zone = fct_recode(zone,'norte grande' = 'big north' , 'norte chico' = 'little north','zona central' = 'central', 'zona sur' = 'south' ,'zona austral' = 'austral'))

dataSpark <- c('Shrubland', 'Savanna', 'Grassland', 'Barren land','Forest','Cropland') %>% 
  map(function(class){
    a <- dataLCV_ts |> 
      complete(LC_type) |> 
      ungroup() |> 
      filter(LC_type == class) |>   
      group_by(zone,LC_type) |> 
      select(zone,LC_type,sup_km2) |> 
      group_split() |> 
      setNames(c('a','b','c','d','e')) |> 
      map(function(x) pull(x,sup_km2))
    map(a,\(x) ifelse(is.na(x),0,x))
  })


dataLCV_trend  |> 
  kbl(booktabs = TRUE,digits = 1,align ='r','latex',position='!ht',
      caption='Value of linear change trend next to time-series plot of surface, per landcover class (IGBP MCD12Q1.006) for 2001-2019 through Central Chile. Red dots on the plots indicate the maximum and minimum surface.') %>%
  kable_styling(latex_options="scale_down") %>% 
  kable_paper(full_width = FALSE) %>%
  column_spec(2, image = spec_plot(dataSpark[[1]],same_lim = FALSE)
  ) %>%
  column_spec(3, image = spec_plot(dataSpark[[2]],same_lim = FALSE)
  ) %>%
  column_spec(4, image = spec_plot(dataSpark[[3]],same_lim = FALSE)
  ) %>%
  column_spec(5, image = spec_plot(dataSpark[[4]],same_lim = FALSE)
  ) %>%
  column_spec(6, image = spec_plot(dataSpark[[5]],same_lim = FALSE)
  ) %>%
  column_spec(7, image = spec_plot(dataSpark[[6]],same_lim = FALSE)
  ) %>% 
  add_header_above(c(" ",'Trend of change [$km^2 year^{-1}$]' = 6),escape =FALSE) |> 
  save_kable(file = "table_2.png",
             zoom = 1.5)

#con gt

data_gt <- dataLCV_ts |> 
  ungroup() |> 
  mutate(zone = fct_recode(zone,'norte grande' = 'big north' , 'norte chico' = 'little north','zona central' = 'central', 'zona sur' = 'south' ,'zona austral' = 'austral')) |> 
  dplyr::group_by(zone,LC_type)  |> 
  # must end up with list of data for each row in the input dataframe
  dplyr::summarize(lc_data = list(prop), .groups = "drop") |> 
  pivot_wider(names_from = LC_type, values_from = lc_data) |> 
  full_join(dataLCV_trend,by = 'zone') |> 
  select(1,12,6,13,7,10,4,9,3,8,2,11,5) 

data_gt$Savanna.x[1] <- NA
data_gt$Savanna.y[1] <- NA
data_gt$Forest.x[2] <- NA
data_gt$Forest.y[2] <- NA
data_gt$Cropland.x[c(2,5)] <- NA
data_gt$Cropland.y[c(2,5)] <- NA
data_gt$Shrubland.x[5] <- NA
data_gt$Shrubland.y[5] <- NA


data_gt |> 
  gt() |> 
  fmt_number(decimals = 1) |> 
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) |> 
  gt_plt_sparkline(Shrubland.x,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(Savanna.x,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(Grassland.x,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline('Barren land.x',same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(Forest.x,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  gt_plt_sparkline(Cropland.x,same_limit = FALSE,label = FALSE,palette = c('grey','grey','red','blue','grey')) |> 
  tab_spanner_delim(
    delim="." ) |> 
  tab_header(
    title = md("Trend of change [km<sup>2</sup> year<sup>-1</sup>]"),
  ) |> 
  gtsave('output/figs/table_var_landcover_macro.png',vwidth=2000)
  
