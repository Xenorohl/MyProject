######################################################

##### Packages 

######################################################

library(styler)
tidyverse_style()
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(XML)
library(terra)
library(gdalUtilities)
library(paletteer)
library(ggthemes)
library(extrafont)

## install of packages 
# install.packages(
#     "XML",
#      "gdalUtilities",
#      "paletteer")







######################################################

##### Load the ecosystem raster

######################################################

ecosystem_raster = raster("./data/WorldEcosystem.tif") # "rasterising" the .tiff file from ./data toi work with it as a raster (with the package "raster") 
print(ecosystem_raster) # basic information for the downloaded raster





######################################################

##### Boundaries of switzerland 

######################################################

CH = ne_countries(
    scale = "large",
    returnclass = "sf",
    country = "Switzerland"
)   # it isn't necessary to it every time (it is in every source R script), but for repeatability I guess it is better, while it doesn't really matter since it has been loaded from the beginning 





######################################################

##### Crop and mask the raster to the Swiss boundaries

######################################################

square = crop(          # keeps only the raster around the rectangle of switzerland
    ecosystem_raster,
    extent(CH)
)

ecosystem_CH = mask(    # keeps only the pixel inside switzerland
    square,
    CH
)





######################################################

##### Convert species coordonates into spatial points 

######################################################

spatial_points = SpatialPoints(
    coords = matrix_species[, c("longitude", "latitude")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
)

# x11()   # Important to open via another graphic device, otherwise the points are not alligned with the raster 
# plot(
#     ecosystem_CH, 
#     main = "Species Occurrences on Ecosystem Map"
#     )
# plot(
#     spatial_points,
#     add = TRUE, 
#     pch = 16,
#     cex = 0.6
#     )





######################################################

##### Extract ecosystem values at each occurence point 

######################################################

eco_values = raster::extract(
    ecosystem_CH,
    spatial_points
)
head(eco_values)
length(eco_values) # ~7000 values
nrow(matrix_species) # ~7000 values (can change because of iNat), but same number of values as "eco_values"
# i write "~7000" and not "7015" because it changes from time to time because of iNat


# new dataframe with extracted values 

matrix_full_eco = data.frame(
    matrix_species,
    eco_values
)

head(matrix_full_eco)





######################################################

##### Getting soil data information, via SoilGrid, all is detailed on https://docs.isric.org/globaldata/soilgrids/wcs_from_R.html

######################################################

# I tried to do it via R but it seems it didnt worked quite well, so I downloaded the .tiff directly on soilgrid



# variables of interest, here for pH water 

# voi = "phh20"         # variable of interest 
# depth = "5-15cm"      # depth of soil 
# quantile = "mean"     # what type of variation do we want (either 0.1, mean or 0.9), didnt fully understood that part

# voi_layer = paste(        # just to have everything in one line, separated by "_" -> goes for the url later 
#     voi,
#     depth,
#     quantile,
#     sep = "_"
# ) # layer of interest 

# wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to WCS
# wcs_service = "SERVICE=WCS"
# wcs_version = "VERSION=2.0.1"

# # Making the request 

# wcs_request = "request=GetCoverage"
# format = "format=GEOTIFF_INT16"
# roi = "subset=X(-1026935.24156938,-1015591.34251064)&subset=Y(2005364.48088881,2014307.50831563)"
# layer = paste0(
#     "coverageid=",
#     voi_layer
# )
# url_request = paste(
#     wcs_path,
#     wcs_service,
#     wcs_version,
#     wcs_request,
#     format,
#     roi,
#     sep = "&"
# )
# small_tiff = rast(url_request) # didnt work, told me this file didnt exist... got it online and worked fine, maybe an error in coverageid= or in the version of gald





######################################################

##### Getting the soil information via raster downloaded on soilgrid

######################################################

### Soil carbon, between 0-5cm of soil, raster downloaded on soilgrid, extent of switzerland, in dg/kg

# options used on soilgrid : 
    # layer : organic carbon density 
    # depth : 0-5 cm
    # value : mean
    # longitude :
        # min : 5.5000 ; max : 10.5000
    # latitude : 
        # min 45.0000 ; max 48.0000

# the options are the same for the following values (nitrogen and ph_water), the only difference is the layer

soil_carbon_raster = raster("./data/carbon_soilgrid.tif") # same as the .tif file before 

square_2 = crop(          # keeps only the raster around the rectangle of switzerland
    soil_carbon_raster,
    extent(CH)
)
carbon_CH = mask(    # keeps only the pixel inside switzerland
    square_2,
    CH
)

carbon_values = raster::extract(
    carbon_CH,
    spatial_points
)

length(carbon_values) # ~7000
length(carbon_values) - sum(is.na(carbon_values)) # ~6950 datapoint available for carbon, not much loss 



### soil nitrogen in cg/kg

nitrogen_raster = raster("./data/nitrogen_soilgrid.tif") 
square_3 = crop(
    nitrogen_raster,
    extent(CH)
)

nitrogen_CH = mask(
    square_3,
    CH
)

nitrogen_values = raster::extract(
    nitrogen_CH,
    spatial_points
)

length(nitrogen_values) # ~ 7000
length(nitrogen_values) - sum(is.na(nitrogen_values)) # ~6900 datapoint available for carbon. exact same result as carbon.
    # seems like some area on soilgrid are not mapped, maybe because of the resolution of 250x250m that is not high.
    # some points could could be in "lake cells" of the raster, therefore not having any information 



### soil pH, in pH*10

ph_raster = raster("./data/phwater_soilgrid.tif")

square_4 = crop(
    ph_raster,
    extent(CH)
)
ph_CH = mask(
    square_4,
    CH
)

ph_values = raster::extract(
    ph_CH,
    spatial_points
)

length(ph_values) - sum(is.na(nitrogen_values)) # same 'ol, ~6900 datapoint





######################################################

##### merging the new data obtained with the other ecosystem values 

######################################################

matrix_full_eco = data.frame(
    matrix_full_eco,
    carbon_values,
    nitrogen_values,
    ph_values
)





######################################################

##### Load the ecosystem metadata table 

######################################################

metadata_eco = read.delim("./data/WorldEcosystem.metadata.tsv")

head(metadata_eco)





######################################################

##### Merge the extracted values with the metadata 

######################################################

matrix_full_eco = merge(
    matrix_full_eco,
    metadata_eco,
    by.x = "eco_values",
    by.y = "Value"
)

head(matrix_full_eco)
names(matrix_full_eco) # getting the different columns names to see which is relevant to keep

matrix_full_eco$Landcover = as.factor(matrix_full_eco$Landcover)
levels(matrix_full_eco$Landcover)
table(matrix_full_eco$Landcover) # way more observation in forest (keeping in mind that I have more observation of Amanita muscaria than trees)

matrix_full_eco$Landforms = as.factor(matrix_full_eco$Landforms)
levels(matrix_full_eco$Landforms) # not really usefull since we will have elevation later on

matrix_full_eco = select(matrix_full_eco, !c("Red", "Green", "Blue", "color", "eco_values", "W_Ecosystm", "Temperatur", "Moisture", "Landforms")) # removing the unneccesary columns 
    # RGB values ("Red", "Green", "Blue") can go to trash, no need to know the color of the point 
    # color goes as well to trash 
    # eco_values isn't usefull anymore since we linked them to metadata 
    # W_Ecosystem is temperature, moisture and climate region all pasted in one column -> instant yeet 
    # Temperature and moisture goes to trash because we get them later from Chelsa
    # Landcover is interesting, at least the results from table(matrix_full_eco$Landcover) are cool to see

names(matrix_full_eco) # this is much better 
head(matrix_full_eco)



### a tiny bit of curation, cuz I saw that some values of the soil were equal to 0, which is not possible considering the different values (like pH, it doesnt make any sense)

nrow(matrix_full_eco[matrix_full_eco$ph_values == 0, ])         # 655 rows were pH = 0
nrow(matrix_full_eco[matrix_full_eco$carbon_values ==0, ])      # also 655, but for carbon
nrow(matrix_full_eco[matrix_full_eco$nitrogen_values == 0, ])   # also 655 again, but for nitrogen 
    # I guess that some points were maybe in the lakes, where the value is automatically 0. 
    # For the values before that were NA, it is possible that those were points out of bounds, without any value attributed to them (therefore being NA) 

matrix_full_eco = matrix_full_eco %>%
    filter(ph_values != 0)
nrow(matrix_full_eco) # ~6150, a great loss of ~650 data point

# just a quick test to see if it worked (should have for every columns since it should be the same point that causes problem)
nrow(matrix_full_eco[matrix_full_eco$ph_values == 0, ])         # 0 rows
nrow(matrix_full_eco[matrix_full_eco$carbon_values ==0, ])      # 0 rows too
nrow(matrix_full_eco[matrix_full_eco$nitrogen_values == 0, ])   # 0 rows too, just to be sure





######################################################

##### Visualize the number of observations per climate category and species 

######################################################

ggplot(
    matrix_full_eco,
    aes(
        x = Climate_Re,
        fill = species,
    )) +
    geom_bar(
        position = "dodge",
        colour = "black") +
    labs(
        title = "Count of observation per climate category",
        x = "Climate category",
        y = "Number of observations"
    ) +
    scale_fill_paletteer_d(
        "MetBrewer::Archambault"
    ) +
    theme_minimal() 



ggplot(
    matrix_full_eco,
    aes(
        x = Landcover,
        fill = species
    )) +
    geom_bar(
        position = "dodge",
        color = "black") + 
    labs(
        title = "Count of observation per landcover category",
        x = "Landcover category",
        y = "Number of observations"
    ) + 
    scale_fill_paletteer_d(
        "MetBrewer::Archambault"
    ) +
    theme_minimal()



ggplot(
    matrix_full_eco,
    aes(
        x = ph_values,
        fill = species
    )) + 
    geom_bar() + 
    scale_fill_paletteer_d(
        "MoMAColors::Ernst"
    ) + 
    labs(
        title = "Distribution of species depending on the pH",
        x = "pH [pH*10]",
        y = "Count of individuals"
    ) +
    theme_minimal()

    # they tend to all prefere a more acidic soil (majority between 5.5 and 6.5)
    # cool normal distribution tho 
    # not necessary to do a map for each variable (nitrogen and carbon for example) 
