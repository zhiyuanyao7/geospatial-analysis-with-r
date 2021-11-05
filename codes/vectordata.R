# st_read()
# st_crs()
# st_bbox()
# st_geometry_type()
# st_crop()
# st_intersection()
# st_buffer()
# st_transform()
# st_as_sf()
# st_write()

library(sf)
library(dplyr)
library(ggplot2)
aoi_boundary_HARV <- st_read('data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp')
getwd()


st_bbox(aoi_boundary_HARV)
st_crs(aoi_boundary_HARV)
class(aoi_boundary_HARV)
st_geometry_type(aoi_boundary_HARV)
aoi_boundary_HARV
summary(aoi_boundary_HARV)

View(aoi_boundary_HARV)

ggplot()+
  geom_sf(data = aoi_boundary_HARV,size = 2,color = 'black',fill='blue')+
  coord_sf(crs=st_crs(32618))

st_crs(aoi_boundary_HARV)



line_HARV <- st_read('data/NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp')
point_HARV <- st_read('data/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp')

ncol(line_HARV)
nrow(line_HARV)

View(line_HARV)
names(line_HARV)

head(line_HARV)

names(point_HARV)

line_HARV$TYPE
levels(line_HARV$TYPE)


sum(is.na(line_HARV$TYPE))

ggplot()+
  geom_sf(data = line_HARV,aes(color = TYPE),size = 2)+
  labs(color = 'Road Types',
       title = 'Road types in Harvard Forest')+
  theme(plot.title = element_text(hjust = 0.5))


footpath_HARV <- line_HARV %>% filter(TYPE =='footpath')

View(footpath_HARV)
ggplot()+
  geom_sf(data = footpath_HARV,aes(color = factor(OBJECTID)),size = 2)+
  labs(color = 'Footpath ID')+
  ggtitle('NEON Harvard Forest Field Site',subtitle = 'Footpaths')+
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5))+
  coord_sf()

# st_buffer()
# st_crop()
# st_intersection()

ggplot()+
  geom_sf(data= line_HARV)+
  geom_sf(data = point_HARV)+
  geom_sf(data= buffer_point,alpha = 0.5)+
  geom_sf(data = line_intersection,color = 'blue')+
  geom_sf(data = line_insect,color = 'red')


buffer_point <- st_buffer(point_HARV, 1000)
buffer_point

line_crop <- st_crop(line_HARV,buffer_point)
line_crop
View(line_crop)  


line_intersection <- st_intersection(line_HARV,buffer_point)
View(line_intersection)

line_insect <- st_intersects(line_HARV,buffer_point)
class(line_insect)
View(line_insect)


# customize 


ggplot()+
  geom_sf(data=line_intersection, aes(color = TYPE, size = TYPE),show.legend = 'line')+
  scale_color_manual(values=c('blue','green','navy','purple'))+
  scale_size_manual(values = c(1,2,3,4))+
  labs(color ='Road Types',
       size = 'Road width')+
  ggtitle('NEON Harvart Forest Field Site',subtitle = 'Roads & Trails - line with varies')+
  coord_sf()
  
line_intersection
  
st_geometry_type(line_intersection)

levels(line_intersection$TYPE)

# add plot a legend
ggplot()+
  geom_sf(data = line_intersection,aes(color = TYPE),size = 1.5,show.legend = 'line')+
  scale_color_manual(values = c('blue','green','navy','purple'))+
  labs(color ='Road Types')+
  ggtitle('NEON Harvart Forest Field Site',subtitle = 'Roads & Trails - line with varies')+
  theme(legend.box.background = element_rect(size = 1,color = 'red'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 15))+
  coord_sf()


# bike and horse allowed

bike_hores_allowed <- line_HARV %>% filter(BicyclesHo == 'Bicycles and Horses Allowed')

ggplot()+
  geom_sf(data = line_HARV,size = 1)+
  geom_sf(data = bike_hores_allowed, aes(color = BicyclesHo),size = 3)+
  scale_color_manual(values = 'orange')+
  labs(color = 'Bikes and Horses path')+
  coord_sf()+
  ggtitle('NEON Harvard Forest Field',subtitle = 'Bike and Horses are allowed')


state_boundary_US <- 
  st_read("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp")

View(state_boundary_US)

ggplot()+
  geom_sf(data = state_boundary_US,aes(color = region,fill = region))+
  scale_fill_manual(values = rainbow(6))+
  coord_sf()

library(RColorBrewer)
brewer.pal(9,'YlGn')

green_color <- brewer.pal(9,'YlGn') %>% 
  colorRampPalette()

# plot multiple shapefiles

road_color <- c('blue','green','navy','purple')

ggplot()+
  geom_sf(data = aoi_boundary_HARV,size = 1,color = 'grey',fill = 'grey')+
  geom_sf(data = line_HARV,aes(color = TYPE),size = 1.5)+
  scale_color_manual(values = c('blue','green','navy','purple'),name='Road type')+
  geom_sf(data = point_HARV,aes(fill=Sub_Type),shape = 8)+
  scale_fill_manual(values = 'black',name = 'Towerlocation')+
  coord_sf()

?pch

plot_locations <- st_read("data/NEON-DS-Site-Layout-Files/HARV/PlotLocations_HARV.shp")

st_crs(plot_locations)

levels(plot_locations$soilTypeOr)

blue_orange <- c('orange','red')


ggplot()+
  geom_sf(data = line_HARV,aes(color = TYPE),size=1.5,show.legend = 'line')+
  scale_color_manual(values = road_color,name ='Road Type',guide = guide_legend(override.aes = list(linetype = 'solid',shape = NA)))+
  geom_sf(data=plot_locations,aes(fill = soilTypeOr),shape = 16,show.legend = 'point')+
  scale_fill_manual(values = soil_color, name = 'Soil Type',
                    guide=guide_legend(override.aes=list(linetype = 'blank',shape =16,color = soil_color)))+
  coord_sf()



ggplot()+
  geom_sf(data = line_HARV,aes(color = TYPE),size=1.5)+
  scale_color_manual(values = road_color,name ='Road Type')+
  geom_sf(data=plot_locations,aes(fill = soilTypeOr))+
  coord_sf()

View(plot_locations)


ggplot()+
  geom_sf(data = plot_locations,aes(color= soilTypeOr))+
  scale_fill_manual(values = soil_color)+
  coord_sf()




ggplot() + 
  geom_sf(data = line_HARV, aes(color = TYPE)) + 
  geom_sf(data = plot_locations, aes(fill = soilTypeOr),shape = 23) 
?pch

# handling coordinate system 

# st_crs
# st_st_crs()

country_boundary_US <- st_read("data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-Boundary-Dissolved-States.shp")

ggplot()+
  geom_sf(data = country_boundary_US,color='black',size = 2)+
  geom_sf(data = state_boundary_US,color = 'grey')+
  coord_sf()

st_crs(country_boundary_US)
st_crs(state_boundary_US)

country_boundary_sp <- st_transform(country_boundary_US,st_crs(point_HARV))

st_crs(point_HARV)

st_crs(country_boundary_sp)



ggplot()+
  geom_sf(data = plot_locations)

plot_locations_HARV <- read.csv('data/NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv')

View(plot_locations_HARV)

names(plot_locations_HARV)

st_crs(point_HARV)

utm18n <- st_crs(point_HARV)

plot_locations_sp_HARV <- st_as_sf(plot_locations_HARV,coords = c('easting','northing'),crs=utm18n)

class(plot_locations_sp_HARV)

ggplot()+
  geom_sf(data = plot_locations_sp_HARV)


# export a shapefile
st_write(plot_locations_sp_HARV,'data/plot_locations_sp_HARV.shp',driver='ESRI Shapefile')



download.file('https://ndownloader.figshare.com/articles/2009586/versions/10','../data1.zip', mode='wb')  












































































  
  
  
  
  
  






































