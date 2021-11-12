library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RStoolbox)
library(RColorBrewer)

# 

GDALinfo('data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif')

DSM_HARV <- raster('data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif')

class(DSM_HARV)

crs(DSM_HARV)
nlayers(DSM_HARV)
minValue(DSM_HARV)
maxValue(DSM_HARV)

summary(DSM_HARV)
summary(DSM_HARV, maxsamp=ncell(DSM_HARV))

DSM_HARV_df <- as.data.frame(DSM_HARV,xy = TRUE)

View(DSM_HARV_df)
names(DSM_HARV_df)

ggplot()+
  geom_raster(data = DSM_HARV_df, aes(x=x, y =y, fill =HARV_dsmCrop))+
  scale_fill_gradientn(colors = terrain.colors(20), name = 'Digital')+
  coord_quickmap()+
  theme(axis.title = element_text())

DSM_HARV_df <- DSM_HARV_df %>% mutate(fc_elevation = cut(HARV_dsmCrop, breaks = 10))


ggplot()+
  geom_raster(data = DSM_HARV_df, aes(x=x, y =y, fill =fc_elevation))+
  coord_quickmap()+
  theme(axis.title = element_text())

max(DSM_HARV_df$HARV_dsmCrop)

min(DSM_HARV_df$HARV_dsmCrop)

DSM_HARV_df <- DSM_HARV_df %>% mutate(fc_elevation1 = cut(HARV_dsmCrop, c(300, 350, 400, 420)))


ggplot()+
  geom_raster(data = DSM_HARV_df, aes(x=x, y =y, fill =fc_elevation1))+
  coord_quickmap()+
  theme(axis.title = element_text())


ggplot()+
  geom_bar(data = DSM_HARV_df, aes(fc_elevation))

ggplot()+
  geom_bar(data = DSM_HARV_df, aes(fc_elevation1))

# overlay hillshade to DSM_HARV

DSM_hill_HARV <- raster('data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif')


DSM_hill_HARV_df <- as.data.frame(DSM_hill_HARV, xy=T) %>% drop_na()

head(DSM_hill_HARV_df)

min(DSM_hill_HARV_df$HARV_DSMhill)
max(DSM_hill_HARV_df$HARV_DSMhill)

DSM_hill_HARV_df$HARV_DSMhill

sum(is.na(DSM_hill_HARV_df$HARV_DSMhill))
dim(DSM_hill_HARV_df)

ggplot()+
  geom_raster(data = DSM_hill_HARV_df, aes(x = x, y= y, alpha = HARV_DSMhill))+
  scale_alpha(range = c(0.15,0.65), guide = guide_legend(reverse = T), name = 'Hillshade')+
  coord_quickmap()

ggplot()+
  geom_raster(data = DSM_HARV_df, aes(x=x, y=y, fill = HARV_dsmCrop))+
  scale_fill_gradientn(colors = terrain.colors(20), name = 'Elevation')+
  geom_raster(data = DSM_hill_HARV_df, aes(x= x, y= y, alpha = HARV_DSMhill), show.legend = F)+
  scale_alpha(range = c(0.15,0.65))+
  ggtitle('Elevation of Harvard Forest', subtitle = 'Digital Surface Model')+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))+
  coord_quickmap()


# theme
# axis, plot, legend, panel, strip 

# reproject 
# projectRaster

DTM_HARV <- raster('data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif')

DTM_hill_HARV <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_DTMhill_WGS84.tif")

crs(DTM_HARV)
crs(DTM_hill_HARV)

DTM_HARV_df <- as.data.frame(DTM_HARV,xy=T)
DTM_hill_HARV_df<- as.data.frame(DTM_hill_HARV,xy = T)

names(DTM_HARV_df)
names(DTM_hill_HARV_df)

ggplot()+
  geom_raster(data = DTM_HARV_df,aes(x=x,y=y,fill =HARV_dtmCrop))+
  geom_raster(data = DTM_hill_HARV_df,aes(x=x,y=y,alpha = HARV_DTMhill_WGS84))+
  coord_quickmap()

extent(DTM_HARV)
extent(DTM_hill_HARV)

# projectRaster

DTM_hill_HARV_sp <- projectRaster(DTM_hill_HARV,crs = crs(DTM_HARV),res = res(DTM_HARV))

crs(DTM_hill_HARV_sp)

DTM_hill_HARV_sp_df <- as.data.frame(DTM_hill_HARV_sp,xy = T)

ggplot()+
  geom_raster(data = DTM_HARV_df,aes(x=x,y=y, fill = HARV_dtmCrop))+
  scale_fill_viridis_c(name = 'Terrain Elevation')+
  geom_raster(data = DTM_hill_HARV_sp_df,aes(x=x,y=y, alpha =HARV_DTMhill_WGS84 ))+
  scale_alpha(range = c(0.15,0.65),guide = 'none')+
  coord_quickmap()

# raster calculation 
# * - 
# overlay()

CHM_HARV <- DSM_HARV - DTM_HARV

CHM_HARV_1 <- overlay(DSM_HARV,DTM_HARV, fun = function(r1,r2){return(r1-r2)})

CHM_HARV_df <- as.data.frame(CHM_HARV,xy = T)
CHM_HARV_1_df <- as.data.frame(CHM_HARV_1,xy = T)

summary(CHM_HARV_df)
summary(CHM_HARV_1_df)

names(CHM_HARV_df)

ggplot()+
  geom_raster(data = CHM_HARV_df,aes(x=x,y=y,fill = layer ))+
  scale_fill_gradientn(colors = terrain.colors(20))+
  coord_quickmap()


ggplot()+
  geom_histogram(data = CHM_HARV_df,aes(layer),bins = 60)

sum(is.na(CHM_HARV_df$layer))
which(is.na(CHM_HARV_df$layer))


# export the raster
# writeRaster()


writeRaster(CHM_HARV,'data/CHM_HARV_raster.grd',
            format = 'raster',
            NAflag = 9999, overwrite = T)


CHM_HARV_write <- raster('data/CHM_HARV_raster.tif')

CHM_HARV_write_df <- as.data.frame(CHM_HARV_write,xy = T)

head(CHM_HARV_write_df)

ggplot()+
  geom_raster(data = CHM_HARV_write_df,aes(x=x,y=y,fill=CHM_HARV_raster))+
  guides(guide_legend(reverse = T))+
  coord_quickmap()

aoi_boundary_HARV <- st_read('data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp')

ggplot()+
  geom_raster(data = CHM_HARV_df,aes(x=x,y=y,fill = layer ))+
  geom_sf(data = aoi_boundary_HARV,color = 'green',size = 2, alpha = 0.2)+
  scale_fill_gradientn(colors = terrain.colors(20))+
  coord_sf()


CHM_HARV_crop <- crop(CHM_HARV,aoi_boundary_HARV)

CHM_HARV_crop_df <- as.data.frame(CHM_HARV_crop,xy = T)

names(CHM_HARV_crop_df)

ggplot()+
  geom_sf(data = aoi_boundary_HARV,color = 'green',size =2)+
  geom_raster(data = CHM_HARV_crop_df,aes(x=x,y=y,fill = layer ))+
  coord_sf()

extent(aoi_boundary_HARV)

new_extent <- extent(aoi_boundary_HARV)

CHM_HARV_crop <- crop(CHM_HARV, new_extent)

# extract(rasterlayer, extractlayer, fun = mean)

CHM_HARV_extract_mean <- raster::extract(CHM_HARV,aoi_boundary_HARV,fun=mean, df = T)

CHM_HARV_extract_mean

point_HARV <- st_read('data/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp')

CHM_HARV_extract_point <- raster::extract(CHM_HARV,point_HARV, buffer = 800, fun = mean, df = T)

CHM_HARV_extract_point 

View(point_HARV)

# multibands raster data 
raster()

RGB_band_HARV <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

nlayers(RGB_band_HARV)


# raster
# cut
# overlay
# prjectRaster
# crop
# extract
# stack
# brick

RGB_stack_HARV <- stack("data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

nlayers(RGB_stack_HARV)

RGB_stack_HARV
RGB_band_HARV

RGB_brick <- brick("data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

RGB_brick
object.size(RGB_stack_HARV)
object.size(RGB_brick)

ggRGB(RGB_brick,r = 1, g = 2, b = 3,
      scale = 800,
      stretch = 'lin')+
  theme_void()

ggRGB(RGB_stack_HARV,r = 1, g = 2, b = 3,
      scale = 800,
      stretch = 'lin')+
  theme_void()


ggRGB(RGB_stack_HARV,r = 1, g = 2, b = 3)

ggRGB(RGB_brick,r = 1, g = 2, b = 3)

# time series data 
# NDVI 
# list.files(path,full.names = T, pattern = 'tif$')

list.file(path, full.names = T, pattern = 'tif$')

NDVI_HARV_path <- 'data/NEON-DS-Landsat-NDVI/HARV/2011/NDVI'

NDVI_all <- list.files(NDVI_HARV_path,full.names = T,pattern='tif$')

all_NDVI_stack <- stack(NDVI_all)

all_NDVI_stack 

all_NDVI_stack_df <- as.data.frame(all_NDVI_stack, xy=T)
names(all_NDVI_stack_df)

View(all_NDVI_stack_df)

all_NDVI_stack_df_gather <- all_NDVI_stack_df %>% gather(Day,Value, -x,-y)

all_NDVI_stack_df_gather 

typeof(all_NDVI_stack_df_gather$Day[1])

# gsub(A, B, data) replace A with B from data
all_NDVI_stack_df_gather$Day_recode <- gsub('_HARV_ndvi_crop','',all_NDVI_stack_df_gather$Day)

all_NDVI_stack_df_gather$Day_recode <- gsub('X','Day ',all_NDVI_stack_df_gather$Day_recode)


ggplot()+
  geom_raster(data = all_NDVI_stack_df_gather, aes(x=x, y=y, fill = Value))+
  scale_fill_gradientn(colors = color_palette(15),name = 'NDVI')+
  facet_wrap(~Day_recode)+
  ggtitle('Canopy Height - Harvard Forest')+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.text.x =  element_text(angle = 10))


color_palette <- brewer.pal(9, 'YlGn') %>% colorRampPalette()

ggplot()+
  geom_raster(data = all_NDVI_stack_df_gather, aes(x=x, y=y, fill = Value))+
  scale_fill_gradientn(colors = color_palette(15),name = 'NDVI')+
  facet_wrap(~Day_recode)+
  theme_void()+
  ggtitle('Canopy Height - Harvard Forest')+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold',vjust = 4), strip.text = element_text(vjust = 2))





















































































































































































































































