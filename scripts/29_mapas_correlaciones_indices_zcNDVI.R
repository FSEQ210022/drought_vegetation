library(terra)
library(sf)
library(tidyverse)
library(fs)
library(tmap)
library(basemaps)
library(rnaturalearth)
library(rmapshaper)

#cargar ecoregiones
ecoregions <- read_sf('data/processed_data/spatial/ecoregiones_2017.gpkg') |> 
  st_transform(32719) |> 
  ms_simplify(keep=0.02)

#persistencia de landcover
lc <- rast('/home/rstudio/discoB/processed/MODIS/IGBP.pers.MCD12Q1.061/IGBP80_reclassified.tif')
lc[lc %in% c(5,7:10)] <- NA

dir <- '/home/rstudio/discoB/processed/analysis/correlations/'
files <- dir_ls(dir,type = 'file',regexp = 'tif$')

#reordenar indices
files <- files[c(3,1,2,5,4)]
chl <- ne_countries(country = 'chile',scale='medium',returnclass = 'sf') |> st_transform(32719)

chl <- chl |> st_geometry() |> st_cast('POLYGON') |> _[-c(1,2)] 

cors <- files |> 
  lapply(\(file) {
    c(resample(rast(file)[[1]],lc,method = 'near'),
      resample(rast(file)[[2]],lc))
  })

cors <- rast(cors)
cors <- mask(cors,lc)
cors_i <- subset(cors,seq(1,10,2))
cors_r <- subset(cors,seq(2,10,2))

set_defaults(map_token = "pk.eyJ1IjoiZnJ6YW1icmEiLCJhIjoiY2tqdmw5Z3QxMDZyZjJydG54M2RobWMyeSJ9.rl8_KzhiKaV0wgsLL2Y1WQ")
bm <- basemap_raster(chl,map_service = 'carto',map_type="light_no_labels")

names(cors_i) <- 1:5

# map_i <- tm_shape(bm) + 
#   tm_rgb() + 
map_i <- tm_shape(bm) + 
  tm_rgb() +
  tm_shape(cors_i) + 
  tm_raster(style = 'pretty',
            labels = as.character(c(3,6,12,24,36)),
            palette = '-magma',
            alpha = .6,
            title =  'Time-scale (months)',
            legend.is.portrait = FALSE) +
  # tm_shape(chl) +
  # tm_borders() +
  tm_shape(ecoregions) + 
  tm_borders(col = 'black') +
  tm_facets(nrow = 1) + 
  tm_layout(
    panel.labels = c('SPI','EDDI','SPEI','SETI','SSI'),
    panel.label.bg.color = 'white',
    legend.outside.position = 'bottom',
    legend.height = -.2
            )
names(cors_r) <- 1:5

# map_r <- tm_shape(bm) + 
#   tm_rgb() +
map_r <- tm_shape(bm) + 
  tm_rgb() +
  tm_shape(cors_r) +
  tm_raster(style='equal',palette = 'RdBu',
            midpoint = 0,
            title = 'r',
            legend.hist = TRUE,
            legend.reverse = TRUE) +
  tm_shape(ecoregions) +
  tm_borders(col = 'black') +
  tm_facets(free.scales	= FALSE) + 
  tm_layout(
    panel.labels = c('SPI','EDDI','SPEI','SETI','SSI'),
    panel.label.bg.color = 'white',
    legend.hist.bg.color = 'lightgrey',
    legend.outside = FALSE,
    legend.hist.width = .5,
    legend.hist.height = .15,
    legend.format = list(digits = 1)
  )

#mapU <- tmap_arrange(map_i,map_r,nrow = 2)
tmap_save(map_i,'output/figs/mapa_cor_selec_indices_zcNDVI6.png',asp = .25)
tmap_save(map_r,'output/figs/mapa_cor_r_indices_zcNDVI6.png',asp = .25)
