######################################################

##### Packages 

######################################################

tidyverse_style()
library(luna)
library(MODIStsp)
library(appeears)  
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(extrafont)
library(paletteer)
# font_import() # if already done no need to do it again, takes a few minutes



######################################################

##### Creating a file .json of switzerland to extract informations from AppEEARS

######################################################

# important note :
    # I didnt choose anything other than NDVI because after checking for a while on AppEEARS I didn't find anything interesting 
    # there was GEDI data that was interesting (height of canopy with lidar data) but I was unable to find them on appeears



CH = ne_countries(
    scale = 10, 
    country = "switzerland",
    returnclass = "sf"
) # as explained in ./source/ecosystem.r

# st_write(
#   CH,
#   "./data/switzerland.geojson",
#   delete_dsn = TRUE
# )





######################################################

##### Checking the manually downloaded appeears tif files 

######################################################

manual_path = "./data/appeears_dl"

manual_tif = list.files(
    manual_path,
    pattern = '\\.tif$',
    full.names = T,
    recursive = T
)   # listing files from appeears
manual_tif



### reading the first raster 

ndvi_raster = rast(manual_tif[1])
ndvi_raster



### getting all the raster a new name 

for(i in 1:24){

    value = manual_tif[i]

    result = rast(value)

    assign(paste0("ndvi_raster_", i), result)

} # all the raster have been rasterized by raster()



### plotting the raster 

# x11()
# plot(
#     ndvi_raster,
#     main = "first raster"
# )


### no need to crop the raster to the exact border of switzerland since it's already have been done on appeears (the file switzerland.geojson, that comes from "CH", is the one that was used on appeears). 





######################################################

##### Convert the sampling table to spatial points

######################################################

points_vect = vect(
    matrix_full_eco_elev_clim,
    geom = c("longitude","latitude"),
    crs = "EPSG:4326"
)



### reprojecting the points to the raster CRS

points_vect <- project(points_vect, crs(ndvi_raster))





######################################################

##### extracting the raster values 

######################################################

ndvi_values = terra::extract(ndvi_raster, points_vect)

head(ndvi_values)
names(ndvi_values)
nrow(ndvi_values) 
nrow(matrix_full_eco_elev_clim) # same number as nrow(ndvi_values), so each observation has a value 




### extracting values of every raster to make a mean out of them in the end 

for(i in 1:24){ 

    obs = get(paste0("ndvi_raster_", i))

    result = terra::extract(obs, points_vect)

    assign(paste0("ndvi_values_", i), result)

} # values of each raster are extracted 

head(ndvi_values_1)
sum(is.na(ndvi_values_1$MOD13Q1.061__250m_16_days_NDVI_doy2020353000000_aid0001)) # 86 NA values 
sum(is.na(ndvi_values_2$MOD13Q1.061__250m_16_days_NDVI_doy2021001000000_aid0001)) # 195 NA values... it will be a lot in the end 





######################################################

##### mean maker of the ndvi values

######################################################

### taking the column "ID" out to keep only the second column with ndvi value 

for(i in 1:24){

    obs = get(paste0("ndvi_values_", i))

    result = obs[, -1]

    matrix_full_eco_elev_clim[[paste0("ndvi_", i)]] = result
   
}

names(matrix_full_eco_elev_clim)



### making the ndvi mean out of all the values

matrix_full_eco_elev_clim$ndvi_mean = rowMeans(matrix_full_eco_elev_clim[c(
    "ndvi_1", "ndvi_2", "ndvi_3", "ndvi_4", "ndvi_5", "ndvi_6", 
    "ndvi_7", "ndvi_8", "ndvi_9", "ndvi_10", "ndvi_11", "ndvi_12", 
    "ndvi_13", "ndvi_14", "ndvi_15", "ndvi_16",  "ndvi_17", "ndvi_18", 
    "ndvi_19", "ndvi_20", "ndvi_21", "ndvi_22",  "ndvi_23", "ndvi_24" 
)])

head(matrix_full_eco_elev_clim)

matrix_full_eco_elev_clim = matrix_full_eco_elev_clim %>%
    select(-all_of(c(
    "ndvi_1", "ndvi_2", "ndvi_3", "ndvi_4", "ndvi_5", "ndvi_6", 
    "ndvi_7", "ndvi_8", "ndvi_9", "ndvi_10", "ndvi_11", "ndvi_12", 
    "ndvi_13", "ndvi_14", "ndvi_15", "ndvi_16",  "ndvi_17", "ndvi_18", 
    "ndvi_19", "ndvi_20", "ndvi_21", "ndvi_22",  "ndvi_23", "ndvi_24" 
    )))   # this specific line of code was done with chatGPT, really didnt want to type all of that again

head(matrix_full_eco_elev_clim) # HELL YEAH, ndvi mean heeeere 
sum(is.na(matrix_full_eco_elev_clim$ndvi_mean)) # Ouch, 454 values down 
nrow(matrix_full_eco_elev_clim) - sum(is.na(matrix_full_eco_elev_clim$ndvi_mean)) # still operating with 2963 values, that's not that bad actually 



### getting rid of the NA values for the final matrix 

matrix_final = na.omit(matrix_full_eco_elev_clim)

head(matrix_final)



### Saving the dataframe (matrix_final) as a csv file

# write.csv(
#     matrix_final,
#     "./data/matrix_final.csv"
# )




######################################################

##### plotting the new ndvi data 

######################################################

### NDVI mapped on switzerland for each observation 

CH_shadow = st_geometry(CH) + c(0.07 , -0.07) 
    # with a little help of chatgpt. just create a new sf object that is moved right and down to create a "shadow"
st_crs(CH_shadow) = st_crs(CH) # setting the crs of "CH_shadow" to the same as "CH"

x11()
ggplot() +
    geom_sf(
        data = CH_shadow,
        fill = "grey40",
        color = NA
    ) + # just making a shadow behind the actual map
    geom_sf(
        data = CH,
        fill = "aliceblue",
        color = "grey"
    ) +
    geom_point(
        data = matrix_final,
        aes(
            x = longitude,
            y = latitude, 
            size = elevation,
            color = ndvi_mean
        )
    ) +
    scale_color_viridis_c() + 
    labs(
        x = "Longitude",
        y = "Latitude",
        title = "NDVI and elevation",
        subtitle = "for each observation site",
        size = "Altitude [m]",
        color = "NDVI indice [-1:1]"
    ) +
    theme_fivethirtyeight() +  # from the package ggthemes
    theme(
        axis.title = element_text(size = 15),   # adding title (because deleted from the theme_538) and adjusting the size
        # text = element_text(family = "Trebuchet MS"), # changing the font for all text on the graph
        panel.background = element_blank(), # removing the grey part behind the map 
        plot.background = element_blank(),  # also removing the grey part, but the one behind the panel 
        legend.background = element_blank(), # removing the background of the legend 
        plot.title = element_text(size = 40)   # bigger title 
    )



### NDVI for each landcover 

x11()
ggplot(
    data = matrix_final,
    aes(
        x = Landcover,
        y = ndvi_mean,
        fill = species
    )
    ) +
    geom_boxplot(
        outlier.size = 0.7,
        outlier.color = "grey50"
    ) + 
    scale_fill_paletteer_d(
        "MetBrewer::Archambault"
    ) + 
    labs(
        x = "Landcover",
        y = "NDVI value [-1:1]",
        title = "NDVI values",
        subtitle = "For the different landcover types, and for each species"
    ) + 
    theme_fivethirtyeight() + 
    theme(
        axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        text = element_text(family =  ".SF Compact Rounded"),
        panel.background = element_blank(),
        plot.title = element_text(size = 60)
    )


