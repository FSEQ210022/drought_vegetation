#Trend zcNDVi-6 por amcrozona y landcover 

#datos para landcover
paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$Name

dir <- '/mnt/md0/raster_procesada/analysis/trends'
files <- dir_ls(dir,regexp = 'mann_kendall.*zcNDVI-6.*tif$')

trends <- rast(files)[[2]]
names(trends) <- 'zcNDVI-6'



macro <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg') |> 
  mutate(macrozona = c('Norte Chico','Norte Grande','Austral','Centro','Sur')) |> 
  st_transform(32719)

# ahora por tipo de landcover

lc_pers <- rast('/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/IGBP80_reclassified.tif') 
#nieve permanente
lc_pers <- resample(lc_pers,trends,method = 'near')
lc_pers[lc_pers %in% c(5,7:10)] <- NA

data <-  map_df(1:5,\(i){
  macro_n <- macro[i,]
  trends_n <- mask(trends,macro_n)
  lc_pers_n <- mask(lc_pers,macro_n)
  trends_df <- zonal(trends_n,lc_pers_n,'mean',na.rm = TRUE)
  trends_df |> 
    mutate(macro = factor(macro_n$macrozona,levels = c('Norte Grande','Norte Chico','Centro','Sur','Austral')),
           clase = factor(lyr.1,levels = paleta$class, labels = paleta$Name)) |> 
    select(-lyr.1)
})

data_final <- data |> 
  pivot_wider(names_from = clase,values_from = `zcNDVI-6`) |> 
  arrange(macro) |> 
  select(c(1,6,5,4,3,2)) 
