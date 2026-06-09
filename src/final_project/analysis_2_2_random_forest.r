######################################################

##### Packages 

######################################################

    # all packages available in inst/packages.r 





######################################################

##### import the datasets

######################################################

### importing the saved matrix 

matrix_final = read.csv("./data/matrix_final.csv") 



### importing the prediction grid 

grid = read.csv("./data/grid_final.csv") 



### france as sf 

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





######################################################

##### first map of occurence point 

######################################################

# x11()
# ggplot(
#     matrix_final,
#     aes(
#         x = longitude,
#         y = latitude, 
#         color = species
#     )
#     ) + 
#     geom_point() +
#     coord_equal() +
#     theme_minimal() +
#     labs(
#         title = "Occurence points of A. muscaria and 7 tree species",
#         x = "Longitude", 
#         y = "Latitude", 
#         color = "Species"
#     )
    # not surprising, same as the very first plot in src/intermediate_project/matrix_species.r

# x11()
# ggplot(
#     grid,
#     aes(
#         x = longitude,
#         y = latitude,
#         color = Landcover
#     )
#     ) +
#     geom_point() +
#     coord_equal() +
#     theme_minimal() +
#     labs(
#         title = "Points for the random forest prediction",
#         y = "Longitude",
#         x = "Latitude"
#     )
    # just to see the different points for the prediction grid, with Landcover as color





######################################################

##### Prepare occurence data for machine learning 

######################################################

# Creation of a clean data table for the random forest model 

# response variable is : 
# - species (A. muscaria ; A. alba ; P. abies)

# predictor variables are : 
# - carbon_values 
# - nitrogen_values 
# - ph_values 
# - Landcover 
# - Climate_Re
# - elevation
# - temp_mean
# - prec_mean
# - clouds_mean
# - ndvi_mean

# futur temperature is not included for the prediction 

ml_matrix_muscaria = matrix_final %>%
    filter(
        species == "Amanita muscaria" | # | means "or", so it adds the others
        species == "Picea abies" |
        species == "Abies alba")

ml_matrix = ml_matrix_muscaria %>%
    select(
        species,
        carbon_values, 
        nitrogen_values, 
        ph_values, 
        Landcover, 
        Climate_Re, 
        elevation, 
        temp_mean, 
        prec_mean, 
        clouds_mean, 
        ndvi_mean
    )

# missing values 

anyNA(ml_matrix)
    # no NA values, we can proceed 

ml_matrix$species = as.factor(ml_matrix$species)
ml_matrix$Climate_Re = as.factor(ml_matrix$Climate_Re)
ml_matrix$Landcover = as.factor(ml_matrix$Landcover)

table(ml_matrix$species)





######################################################

##### Train / test split 

######################################################

# as showed in the example, we split the data into two part 
    # 70 % training data 
    # 30 % testing data 

index = createDataPartition(
    y = ml_matrix$species,
    p = 0.7,
    list = FALSE
)

df_train = ml_matrix[index, ]
df_test = ml_matrix[-index, ]

# check the partition
table(df_train$species)
table(df_test$species)
    # nice





######################################################

##### Train the random forest model 

######################################################

# The formula species ~ . means:
# predict species using all other columns as predictors.
#
# ntree = 500 means that the forest contains 500 trees.
#
# importance = TRUE allows us to calculate variable importance.

species_rf = randomForest(
    species ~ .,                # predict species using all other columns as predictors
    data = df_train,            # training dataset
    ntree = 500,                # means that the forest contains 500 trees 
    importance = TRUE           # allows to calculate variable importance 
)

species_rf





######################################################

##### Prediction on test data 

######################################################

species_pred = predict(
    species_rf,
    newdata = df_test
)

head(species_pred)





######################################################

##### Model evaluation 

######################################################

# making a confusion matrix to compare : 
    # predicted values 
    # observed values 

    # as said in the example, it gives an estimation of the model performance 

confusionMatrix(
    data = species_pred, 
    reference = df_test$species
)
    # accuracy of 58.4 %, not that much tbh 
    # p-value of 0.0017 (is it significant ? But what is significant ?)

importance(species_rf)
    # For Abies alba, the three most important variables are : 
        # temp_mean 
        # clouds_mean (for real ?)
        # ph_values 
    # For Amanita muscaria, the three most important variables are : 
        # elevation
        # temp_mean 
        # clouds_mean (ok ?!)
    # Picea abies : 
        # carbon_values 
        # prec_mean 
        # clouds_mean 

    # Very interesting results, I would have never thought that the cloud cover would explain that much
    # Climate_Re doesn't explain anything for anything (maybe way too broad as a variable)

# basic plot to show the importance 

# x11()
# varImpPlot(species_rf)
    # pretty clear that Climate_Re doesn't explain anything 
    # temp_mean, elevation, clouds_mean and ndvi_mean are really important however 
    # soil values aren't that much important, kinda sad to see but it may be because of the accuracy of the soil data (250m) and the accuracy of gbif data 



### Cleaner version with ggplot 

df_importance = importance(species_rf) %>%
    as.data.frame()

df_importance$feature = rownames(df_importance)

# x11()
# ggplot(
#     data = df_importance,
#     aes(
#         x = MeanDecreaseGini,
#         y = reorder(feature, MeanDecreaseGini)
#     )
#     ) + 
#     geom_col() + 
#     theme_minimal() +
#     labs(
#         x = "Variables", 
#         y = "Mean decrease in Gini",
#         title = "Most important variables to discriminate the species"
#     )
    # same as before, but a bit prettier (will not be a plot on the final panel so I wont really work on this one)





######################################################

##### Prepare the prediction grid 

######################################################

# will contains all the predictory variables without the response variable (not there anyway)

grid_ml = grid %>%
    select(
        longitude,
        latitude,
        carbon_values, 
        nitrogen_values, 
        ph_values, 
        Landcover, 
        Climate_Re, 
        elevation, 
        temp_mean, 
        prec_mean, 
        clouds_mean, 
        ndvi_mean
    )



### converting categorical variables to factor 

grid_ml$Landcover = as.factor(grid_ml$Landcover)
grid_ml$Climate_Re = as.factor(grid_ml$Climate_Re)



### check for NA values (already did before but never too sure)

anyNA(grid_ml)
    # FALSE, nice 





######################################################

##### Predict species probability on the grid 

######################################################

# grid_prob = predict(
#     species_rf,
#     newdata = grid_ml,
#     type = "prob"
# )
#     # error, more levels to factor in newdata than in reference dataframe 

# levels(ml_matrix$Climate_Re)
#     # Boreal Moist 
#     # Cool Temperate Moist
#     # Polar Moist
#     # Warm Temperate Moist 
# levels(ml_matrix$Landcover)
#     # Cropland 
#     # Forest 
#     # Grassland
#     # Settlement 
#     # Shrubland 
#     # Sparsely or Non vegetated 

# levels(grid_ml$Climate_Re)
#     # Warm Temperate Dry is added 
# levels(grid_ml$Landcover)
#     # Snow and Ice is added 



### Problem --> solution 

# The idea is now to delete those factor levels 
# It is not a problem, because according to the importance, those two are the least important variables 
# So we can just get rid of them 



### Yeeting out Landcover and Climate_Re of the random forest analysis 

df_train_new = df_train %>%
    select(-Landcover & -Climate_Re)

grid_ml_new = grid_ml %>%
    select(-Landcover & -Climate_Re)


str(df_train_new)

species_rf_new = randomForest(
    species ~ .,
    data = df_train_new,
    ntree = 500, 
    importance = TRUE
)



### quick check of the new random forest 

importance(species_rf_new)
    # The model is much more stronger now (at least for mean decrease in Gini)
    # seems way better than the older model, great new 

df_importance_new = importance(species_rf_new) %>%
    as.data.frame()
df_importance_new$feature = rownames(df_importance_new)

# x11()
# ggplot(
#     data = df_importance_new,
#     aes(
#         x = MeanDecreaseGini,
#         y = reorder(feature, MeanDecreaseGini)
#     )
#     ) + 
#     geom_col() + 
#     theme_minimal() +
#     labs(
#         x = "Variables", 
#         y = "Mean decrease in Gini",
#         title = "Most important variables to discriminate the species"
#     )
#     # visually it is way better than before 
#     # all variables are above 50 
#     # The top 4 stayed still with the modification, but came out stronger than before 



### Now with the new dataframes 

grid_prob = predict(
    species_rf_new, 
    newdata = grid_ml_new,
    type = "prob"
)
    # this time it worked ! 

head(grid_prob)
    # nice 

# combining coordinates, predictors and probabilities in one table 

grid_map = cbind(grid_ml_new, grid_prob)

head(grid_map)
str(grid_map)
    # this is just beautiful tbh, really happy now 





######################################################

##### Map probability for one selected species 

######################################################

# all species available in the model : 

species_name = levels(df_train_new$species)
species_name
    # 3 different species : 
        # Abies alba 
        # Picea abies 
        # And most importantly, Amanita muscaria ! 

amanita = "Amanita muscaria"
abies = "Abies alba"
picea = "Picea abies"
    # this is made to play with the three different species 
    # just gotta replace the name in geom_tile(aes(fill = ./data[species_name]))


FR_shadow = st_geometry(FR) + c(0.2 , -0.3) # same as previously done in /src/intermediate_project/satellite_data.r
st_crs(FR_shadow) = st_crs(FR)

x11()      
plot_c = ggplot() + 
    geom_sf(
        data = FR_shadow,   # projected shadow of france 
        fill = "grey30",
        color = NA
    ) + 
    geom_sf(
        data = FR,
        fill = "aliceblue",
        color = "grey"
    ) + 
    geom_tile(
        data = grid_map,
        aes(
            x = longitude,
            y = latitude,
            fill = .data[[amanita]]
        ) 
    ) +
    scale_fill_viridis_c(
        limits = c(0,1)
    ) + 
    coord_sf() +
    theme_minimal() + 
    labs(
        x = "Longitude",
        y = "Latitude",
        title = "Predicted Amanita muscaria",
        subtitle = "France, according to Random Forest model, in probability",
        fill = "Probability of presence"
    ) + 
    theme(
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 15),
        text = element_text(family = ".SF Compact Rounded", color = "grey20"),
        legend.title = element_text(
            size = 13,
            margin = ggplot2::margin(r = 20)
        ),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.title.position = "left",
        legend.direction = "horizontal",
    )
plot_c 

# quartz.save(
#     "./plots/france_rf.png",
#     type = "png",
#     dpi = 300,
#     width = 13,
#     height = 13 
# )

# ?theme



### mapping the importance (choices made after doing a final panel once and wanting to change few things)

    # setting the names as in the environment radar chart 

df_importance_new$feature = c(
    "Carbon", 
    "Nitrogen", 
    "pH", 
    "Elevation", 
    "T˚", 
    "Rain", 
    "Clouds",
    "NDVI"
)

plot_e = ggplot(
    data = df_importance_new,
    aes(
        x = MeanDecreaseGini,
        y = reorder(feature, MeanDecreaseGini),
        fill = reorder(feature, -MeanDecreaseGini)
    )
    ) + 
    geom_col() + 
    theme_fivethirtyeight() +
    scale_fill_paletteer_d(
        "MoMAColors::Flash"
    ) +
    labs( 
        x = "Mean decrease in Gini",
        title = "Variables importance",
        subtitle = "According to the random forest algorithm"
    ) + 
    theme(
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size = 15),
        legend.position = "none", 
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15, hjust = 1, vjust = 0)
    )
plot_e
    # visually it is way better than before 
    # all variables are above 50 
    # the variables that explains the most is NDVI and then Temperature, elevation and precipitation
    # a bit sad to see that soil variables are the three least important 
    # I would have thought otherwise, but it's possible that this comes from the precision of the soilgrid which is 250m 
    # But even with the least importance, Nitrogen and Carbon still are at ~80 of Mean decrease in Gini

# x11()
# plot_e 
# quartz.save(
#     "./plots/importance.png",
#     type = "png",
#     dpi = 300, 
#     width = 8, 
#     height = 11
# )


  