# Figures for landcover
# may 2023
# by frzambra

dir <- '/mnt/md0/raster_procesada/MODIS_derived/IGBP.MCD12Q1.061/'

library(terra)
library(tmap)
library(sf)
library(basemaps)
library(rnaturalearth)

chl <- ne_countries(country = 'chile',scale = 'medium',returnclass = 'sf') |> st_transform(32719)
ext <- st_bbox(chl)
ext[1] <- -8389.057
chl <- st_crop(chl,ext)

set_defaults(map_token = "pk.eyJ1IjoiZnJ6YW1icmEiLCJhIjoiY2tqdmw5Z3QxMDZyZjJydG54M2RobWMyeSJ9.rl8_KzhiKaV0wgsLL2Y1WQ")

#macrozonas
zones <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg','marcozonas_chile')

#centroides de las macrozonas
zones <- zones |> st_centroid() |> st_coordinates() |> cbind(zones)

divisiones <- read_sf('data/processed_data/spatial/macrozonas_chile.gpkg','divisiones_macrozona')

bm <- basemap_raster(st_as_sf(chl),map_service = 'carto',map_type="light")
bm_topo <- basemap_raster(st_as_sf(chl),'osm','topographic')

# landcover persitencia >80%
igbp80 <- rast(paste0(dir,'IGBP','80_reclassified.tif')) 
igbp80 <- mask(igbp80,chl)
#igbp80[igbp80 == 10] <- NA

#landcover año 2021
igbp2022 <- rast(paste0(dir,'IGBP_2022_reclassified.tif'))
igbp2022 <- mask(igbp2022,chl)

paleta <- read.csv('data/processed_data/paleta_colores_landcover.csv')
colors <-  rgb(paleta$R,paleta$G,paleta$B,maxColorValue = 255)
attr(colors,'names') <- paleta$class

m2 <- tm_shape(bm) +
  tm_rgb() +
  tm_credits('(c)',position = c('left','bottom'),fontface = 'bold') +
  # tm_shape(divisiones) + 
  # tm_lines(lwd=1,col='black',lty='solid',alpha=.6)+
  tm_shape(igbp2022) +
  tm_raster(style = 'cat',
            palette = colors,title = 'Landcover classes',
            labels = paleta$Name) +
  # tm_shape(zones,col=NA) + 
  # tm_text('macrozona',just='top',xmod=c(-2,-2,2,-2,-2),size=.5) +
  tm_layout(legend.show = FALSE) 

m3 <- tm_shape(bm) +
  tm_rgb() +
  tm_credits('(d)',position = c('left','bottom'),fontface = 'bold') +
  # tm_shape(divisiones) + 
  # tm_lines(lwd=1,col='black',lty='solid',alpha=.6)+
  tm_shape(igbp80) +
  tm_raster(style = 'cat',
            palette = colors,title = 'Landcover class',
            labels = paleta$Name) +
  # tm_shape(zones) +
  # tm_text('macrozona',just='top',xmod=c(-2,-2,2,-2,-2),size=.5) +
  tm_layout(legend.outside = FALSE) 

m1 <- tm_shape(bm_topo) +
  tm_rgb() +
  # tm_shape(zones) +
  # tm_borders(lwd=1,col='white',lty='solid',alpha=0.8) +
  #tm_text('macrozona',just='top',xmod=c(-2,-2,2,-2,-2),size=.5) +
  tm_credits('(b)',position = c('left','bottom'),fontface = 'bold')

mapArr <- tmap_arrange(m1,m2,m3,asp=0.23)
tmap_save(mapArr,'output/figs/landcover_pers.png',dpi =300,width=7,height=10,scale=2)

#mapa chile con planeta tierra

crs = "+proj=laea +lat_0=-20 +lon_0=-72 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
world <- ne_countries(scale = "medium", returnclass = "sf")
bm_world <- basemap_raster(st_as_sf(world),'carto','light')
bm_world <- terra::project(rast(bm_world), crs)
bm2 <- basemap_raster(st_as_sf(chl),'carto','light')
# bm3 <- basemap_raster(lim,'mapbox','hybrid')

map_globe <- tm_shape(bm_world) +
  tm_rgb() +
  tm_shape(chl) +
  tm_polygons(col = 'grey',alpha=.7) +
  tm_text('admin',size=.4,xmod = -.5,col='black') +
  tm_layout(frame = TRUE,bg.color = "white")

# Mapa Chile con Koppen-Geiger 1991-2020

kp_chile <- rast('data/processed_data/spatial/koppen_geiger_1991_2020.tif')
rgb <- coltab(kp_chile)[[1]][2:31,]
paleta_kg <- readr::read_csv2('data/processed_data/legend_koppen-geiger.csv')
labs <- substr(paleta_kg$type,1,3)
names(labs) <- 1:30
colores <- rgb(rgb[,2],rgb[,3],rgb[,4],rgb[,5],maxColor = 255)
names(colores) <- 1:30

kp_chile <- raster::raster(kp_chile)
vals <- values(kp_chile)
vals[vals %in% c(6,26,10,30,27,18)] <- NA
values(kp_chile) <- vals

zones$macrozona <- c('Norte Chico','Norte Grande',
                     'Austral','Centro','Sur')
  
map_chl <- tm_shape(bm2) +
  tm_rgb() +
  tm_shape(kp_chile) +
  tm_raster(palette = colores,
            style = 'cat',
            labels = labs,
            title = 'Climate (1990-2020)') +
  tm_shape(zones) +
  tm_borders(lwd=2,col='black',lty='solid',alpha=0.8) +
  tm_text('macrozona',just='top',xmod=c(5,-7,3,-4,-4),ymod=c(0,-3.5,1,0,0),size=1.5) +
  tm_credits('(a)',position = c('left','bottom'),fontface = 'bold',size=1.7) +
  tm_layout(legend.outside = FALSE,
            legend.text.size = 1.2,
            legend.position = c(0.1,0.7))

#tmap_save(map_chl,'output/figs/mapa_koppen_geiger.png')

xy <- st_bbox(world)
asp2 <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)

xy <- st_bbox(chl)
asp <- (xy$ymax - xy$ymin)/(xy$xmax - xy$xmin)

library(grid)
#asp <- asp2 <- 0.23
w <- 0.5
h <- asp2 * w
vp <- viewport(x=.53, y=1.43, width = w, height=h, just=c("right", "top"))

tmap_save(map_chl,filename="output/figs/map_study_area_koppen-geiger.png",
          dpi=300, insets_tm=map_globe, insets_vp=vp,scale =1,
          height=asp*91, width=125, units="mm",bg="transparent")
