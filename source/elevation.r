######################################################

##### Packages 

######################################################

library(sf)
library(rnaturalearth)
library(elevatr)
library(raster)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(paletteer)

# Notes : 
  # This R script looks like almost 100% as the one posted on Moodle 
  # On this part specifically, we do not have much "creativity" of variables, like as ecosystem, climatic, or the species 
  # So this is the shortest scripts out of the others



######################################################

##### Switzerland boundaries 

######################################################

CH = ne_countries(
    scale = "large",
    returnclass = "sf",
    country = "Switzerland"
) # as explained in ./source/ecosystem.r





######################################################

##### Download elevation data 

######################################################

elevation_CH = get_elev_raster(
    CH,
    z = 8
)





######################################################

##### Prepare sampling points 

######################################################

spatial_points <- SpatialPoints(
  coords = matrix_full_eco[, c("longitude", "latitude")], # getting the coordinates out of the matrix of each observations
  proj4string = CRS("+proj=longlat +datum=WGS84")         # setting the projection CRS to WGS84 (switzerland) for the observations points 
)  





######################################################

##### Extract the elevation values 

######################################################

elevation <- raster::extract(elevation_CH, spatial_points) # extracting the value for each observations

# Add the elevation data to the ecosystem dataset 

matrix_full_eco_elev <- data.frame(
  matrix_full_eco,
  elevation = elevation
)

head(matrix_full_eco_elev)
# fix(matrix_full_eco_elev)





######################################################

##### Visualisation 

######################################################

ggplot(
  matrix_full_eco_elev,
  aes(
    x = elevation,
    fill = Landcover
  )) + 
  geom_density(
    alpha = 0.6,
    color = "white"
  ) + 
  labs( 
    x = "Altitude [m]",
    y = "Density",
    title = "Landcover repartition on altitudinal gradient"
  ) +
  scale_fill_paletteer_d(
    "severance::Hell"
  ) +
  theme_minimal()



### or another way to represent elevation and landcover 

x11()
ggplot(
  data = CH
  ) +
  geom_sf(
    fill = "aliceblue",
    color = "grey"
  ) + 
  geom_point(
    data = matrix_full_eco_elev,
    aes(
      x = longitude,
      y = latitude,
      size = elevation,
      color = Landcover
    )
  ) + 
  scale_color_paletteer_d(
    "PNWColors::Cascades"
  ) + 
  theme_minimal()



### Also for the species distribution 

ggplot(
  matrix_full_eco_elev,
  aes(
    x = species,
    y = elevation,
    fill = species
  )) + 
  geom_boxplot(
    outliers = FALSE
  ) +
  geom_jitter(
    data = matrix_full_eco_elev,
    size = 0.4,
    width = 0.3,
    shape = 5,
    alpha = 0.5
  ) +
  scale_fill_paletteer_d(
    "MetBrewer::Demuth"
  ) + 
  labs(
    x = "Species",
    y = "Altitude [m]",
    title = "Species distribution on altitudinal gradient"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none"  # getting rid of the legend because it is useless in this particular plot (the x axis is already giving enough information, no need of a legend box)
  )
