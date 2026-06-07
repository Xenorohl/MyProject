######################################################

##### Important note 

######################################################

    # this grid was made for the random forest analysis 
    # this analysis comes after the "tree preference" one
    # so this grid will be used to predict the distribution of A. muscaria and the two "best trees" in France 
    # I chose France because it has a somewhat close climate compared to switzerland and found that the prediction could fit well 

    # the final matrix is directly available in data/grid_final.csv if anything went wrong 
    



######################################################

##### spatial points creation 

######################################################

latitude = seq(42, 52, length.out = 250)
longitude = seq(-5, 8.5, length.out = 200)

grid = expand.grid(
    latitude = latitude,
    longitude = longitude
)
grid = as.data.frame(grid)




######################################################

##### getting the ecosystem values 

######################################################

ecosystem_raster = raster("./data/WorldEcosystem.tif")



### Boundaries of France 

france = ne_countries(
    scale = "large",
    country = "France",
    returnclass = "sf"
) 

# ggplot(france) +
#     geom_sf()
    # little problem because it took also the Guadeloupe, Martinique, etc.. when I was looking only for France Island

fr_parts = st_cast(france, "POLYGON") # cast geometry to polygon, because it was a multipolygon before 

fr_parts$area = st_area(fr_parts) # calculate the area of each polygon

FR = fr_parts %>%
    filter(area == max(area)) # keeping only the polygon with max area (a.k.a France Island)

# ggplot(FR) + 
#     geom_sf()
     # worked fine 



### Crop and masking the raster to France 

square = crop(
    ecosystem_raster,
    extent(FR)
)

ecosystem_FR = mask(
    square,
    FR
)



### Convert coordinates to spatial points

spatial_points = SpatialPoints(
    coords = grid[, c("longitude", "latitude")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
)

# x11() 
# plot(
#     ecosystem_FR
# )
# plot(
#     spatial_points,
#     add = TRUE,
#     pch = 16,
#     cex = 0.5
# )
    # the points are located on the raster, I can continue 



### Extracting the ecosystem values for each point

eco_values = raster::extract(
    ecosystem_FR,
    spatial_points
)
head(eco_values)
is.na(eco_values)
    # most are NA (logical beacause outside of France without raster) 

grid_eco = data.frame(
    grid,
    eco_values
)

grid_eco = grid_eco %>%
    filter(!is.na(eco_values)) # removing NA values 
nrow(grid_eco)



### quick checking that the points are within france 

# ggplot(FR) + 
#     geom_sf() +
#     geom_point(
#         data = grid_eco,
#         aes(x = longitude, y = latitude),
#         color = "red",
#         size = 0.5
#     )
    # seems pretty good 



### metadata extraction 

metadata_grid = read.delim("./data/WorldEcosystem.metadata.tsv")

grid_eco_values = merge(
    grid_eco,
    metadata_grid,
    by.x = "eco_values",
    by.y = "Value"
)

head(grid_eco_values)



###  making the new spatial points only within France 

spatial_points_FR = SpatialPoints(
    coords = grid_eco[, c("longitude", "latitude")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
)



### soil informations 
    # it was the same process as the intermediate project 
    # only the extent changed 
    # latitude : 
        # min : 42
        # max : 52
    # longitude : 
        # min : -5
        # max : 8.5

# for carbon (in cg/kg) :   
carbon = raster("./data/carbon_soilgrid_fr.tif")
square_C = crop(
    carbon,
    extent(FR)
)
carbon_fr = mask(
    square_C,
    FR
)
carbon_fr_values = raster::extract(
    carbon_fr,
    spatial_points_FR
)
sum(is.na(carbon_fr_values)) # 3 NA values


# for nitrogen (in cg/kg) :
nitrogen = raster("./data/nitrogen_soilgrid_fr.tif")
square_N = crop(
    nitrogen,
    extent(FR)
)
nitrogen_fr = mask(
    square_N,
    FR
)
nitrogen_fr_values = raster::extract(
    nitrogen_fr,
    spatial_points_FR
)
sum(is.na(nitrogen_fr_values)) # 3 NA values


# for pH (in pH*10) :
phwater = raster("./data/phwater_soilgrid_fr.tif")
square_ph = crop(
    phwater,
    extent(FR)
)
phwater_fr = mask(
    square_ph,
    FR
)
phwater_fr_values = raster::extract(
    phwater_fr,
    spatial_points_FR
)
sum(is.na(phwater_fr_values)) # 3 NA values



### merging the soil data with the grid_eco_values

grid_full_eco = data.frame(
    grid_eco_values,
    carbon_values = carbon_fr_values,
    nitrogen_values = nitrogen_fr_values,
    ph_values = phwater_fr_values
)

head(grid_full_eco)



### keeping only the columns of interest

grid_full_eco = grid_full_eco %>%
    select(
        latitude, 
        longitude, 
        carbon_values, 
        nitrogen_values, 
        ph_values, 
        Landcover, 
        Climate_Re
    )



### removing NA values

grid_full_eco = grid_full_eco %>%
    filter(!is.na(carbon_values) & !is.na(nitrogen_values) & !is.na(ph_values))





######################################################

##### elevation 

######################################################

elevation_fr = get_elev_raster(
    FR,
    z = 8
)



### extract elevation values 

# elevation = raster::extract(
#     elevation_fr,
#     spatial_points_FR
# )
    # made a problem for later because three values were NA, the merge later didnt work so I will create again, a new spatial points object

spatial_points_FR_new = SpatialPoints(
    coords = grid_full_eco[, c("longitude", "latitude")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
)

elevation = raster::extract(
    elevation_fr,
    spatial_points_FR_new
)


### adding elevation to the grid 

grid_full_eco_elev = data.frame(
    grid_full_eco,
    elevation = elevation
)

str(grid_full_eco_elev)





######################################################

##### climatic variables (only the current, not the future ones)

######################################################

### creating spatial object 

pts_v_grid = terra::vect(
    grid_full_eco_elev,
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326" 
)

# extracting simple coordinates as a standard data frame 
coords_df_grid = as.data.frame(terra::geom(pts_v_grid)[, c("x", "y")]) %>%
    rename(
        longitude = x,
        latitude = y
    )

head(coords_df_grid)



### annual precipitation (2021)

annual_prec_grid = getChelsa(
    var = "pr",
    coords = coords_df_grid %>% select(longitude, latitude),
    startdate = as.Date("2021-01-01"),
    enddate = as.Date("2021-12-31"),
    dataset = "chelsa-monthly"
)

# removing time column and converting to matrix 
mat_prec_grid = annual_prec_grid %>%
    select(-time) %>%
    as.matrix()


# annual mean of precipitation and converting to data frame 
prec_mean_grid = colMeans(mat_prec_grid, na.rm = TRUE)

prec_df_grid = data.frame(
    prec_mean = as.numeric(prec_mean_grid)
)



### extracting the temperature values (2021)

annual_temp_grid = getChelsa(
    var = "tasmax",
    coords = coords_df_grid %>% select(longitude, latitude),
    startdate = as.Date("2021-01-01"),
    enddate = as.Date("2021-12-31"),
    dataset = "chelsa-monthly"
)

# removing time column and converting to matrix
mat_temp_grid = annual_temp_grid %>%
    select(-time) %>%
    as.matrix()


# annual mean of temperature and converting to data frame
temp_mean_grid = colMeans(mat_temp_grid, na.rm = TRUE)

temp_df_grid = data.frame(
    temp_mean = as.numeric(temp_mean_grid)
)

temp_df_grid$temp_mean = temp_df_grid$temp_mean - 273.15 # converting from Kelvin to Celsius



### extracting cloud cover values (2021)

clouds_cover_grid = getChelsa(
    var = "clt",
    coords = coords_df_grid %>% select(longitude, latitude),
    startdate = as.Date("2021-01-01"),
    enddate = as.Date("2021-12-31"),
    dataset = "chelsa-monthly"
)

# removing time column and converting to matrix
mat_clouds_grid = clouds_cover_grid %>%
    select(-time) %>%
    as.matrix()

# annual mean of cloud cover and converting to data frame
clouds_mean_grid = colMeans(mat_clouds_grid, na.rm = TRUE)

clouds_df_grid = data.frame(
    clouds_mean = as.numeric(clouds_mean_grid)
)



### merging climatic data with the grid 

grid_full_eco_elev_clim = data.frame(
    grid_full_eco_elev,
    prec_df_grid, 
    temp_df_grid,
    clouds_df_grid
)

head(grid_full_eco_elev_clim)
str(grid_full_eco_elev_clim)





######################################################

##### satellite data (NDVI)

######################################################

# st_write(
#     FR,
#     "./data/france.geojson",
#     delete_dsn = TRUE
# )
    # will be used to extract the NDVI values from appeears 

manual_path_fr = "./data/appeears_dl_france"

manual_tif = list.files(
    manual_path_fr,
    pattern = '\\.tif$',
    full.names = TRUE,
    recursive = TRUE
)

# reading first raster 
ndvi_raster_fr = rast(manual_tif[1])
ndvi_raster_fr



### getting all the raster a new name

for(i in 1:24){

    value = manual_tif[i]

    result = rast(value)

    assign(paste0("ndvi_raster_fr_", i), result)

}



### convert the sampling table to spatial points 

points_vect_grid = vect(
    grid_full_eco_elev_clim,
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326"
)

# reprojecting the points to match the raster CRS
points_vect_grid_reproj = project(
    points_vect_grid,
    crs(ndvi_raster_fr)
)



### extracting the raster values 

ndvi_values_grid = terra::extract(
    ndvi_raster_fr,
    points_vect_grid_reproj
)
head(ndvi_values_grid) # NA values that will need to get removed 
nrow(ndvi_values_grid)
nrow(grid_full_eco_elev_clim) # same number of rows


# for every raster to make a mean 

for(i in 1:24){

    obs = get(paste0("ndvi_raster_fr_", i))

    result = terra::extract(
        obs,
        points_vect_grid_reproj
    )

    assign(
        paste0("ndvi_values_grid_", i),
        result
    )

}

head(ndvi_values_grid_1)
sum(is.na(ndvi_values_grid_1$MOD13Q1.061__250m_16_days_NDVI_doy2020353000000_aid0001)) # 23 NA
sum(is.na(ndvi_values_grid_2$MOD13Q1.061__250m_16_days_NDVI_doy2021001000000_aid0001)) # 123 NA
    # all the NA will be removed at the very end, not yet



### making the mean of NDVI values 

for(i in 1:24){

    obs = get(paste0("ndvi_values_grid_", i))

    result = obs %>%
        select(-ID) # removing the column ID (better than in intermediate project)

    grid_full_eco_elev_clim[[paste0("ndvi_", i)]] = as.double(result[[1]]) # I put "1" because there is only one column here, so it is reproducible
 
}

head(grid_full_eco_elev_clim)


# making the mean 

grid_full_eco_elev_clim$ndvi_mean = rowMeans(grid_full_eco_elev_clim[c(
    "ndvi_1", "ndvi_2", "ndvi_3", "ndvi_4", "ndvi_5", "ndvi_6", 
    "ndvi_7", "ndvi_8", "ndvi_9", "ndvi_10", "ndvi_11", "ndvi_12", 
    "ndvi_13", "ndvi_14", "ndvi_15", "ndvi_16",  "ndvi_17", "ndvi_18", 
    "ndvi_19", "ndvi_20", "ndvi_21", "ndvi_22",  "ndvi_23", "ndvi_24" 
)])

head(grid_full_eco_elev_clim) # new column that is mean of ndvi

# grid_full_eco_elev_clim %>%
#     select(-all_of(c(
#         "ndvi_1", "ndvi_2", "ndvi_3", "ndvi_4", "ndvi_5", "ndvi_6", 
#         "ndvi_7", "ndvi_8", "ndvi_9", "ndvi_10", "ndvi_11", "ndvi_12", 
#         "ndvi_13", "ndvi_14", "ndvi_15", "ndvi_16",  "ndvi_17", "ndvi_18", 
#         "ndvi_19", "ndvi_20", "ndvi_21", "ndvi_22",  "ndvi_23", "ndvi_24" 
#     )))

# str(grid_full_eco_elev_clim)
# grid_full_eco_elev_clim %>%
#     dplyr::select(-(ndvi_1:ndvi_24))

# str(grid_full_eco_elev_clim)
# head(grid_full_eco_elev_clim)

    # I just don't understand why it doesn't work while it perfectly worked on the intermediate project 
    # I will do the exact contrary then 

grid_nearly_final = grid_full_eco_elev_clim %>%
    select(latitude:clouds_mean)

ndvi_mean_val = grid_full_eco_elev_clim %>%
    select(ndvi_mean)

grid_final = data.frame(
    grid_nearly_final,
    ndvi_mean_val
)


str(grid_final)
sum(is.na(grid_final$ndvi_mean)) # 291 


grid_final = na.omit(grid_final)
anyNA(grid_final) # False, splendid


# write.csv(
#     grid_final,
#     "./data/grid_final.csv"
# )