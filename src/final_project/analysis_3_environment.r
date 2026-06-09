######################################################

##### Packages 

######################################################

    # all packages are available at inst/packages.r 





######################################################

##### Radar chart 

######################################################

matrix = read.csv("./data/matrix_final.csv")
str(matrix)



### computing means for each species 

profils = matrix %>%
    group_by(species) %>%
    summarise(
        Elevation = mean(elevation, na.rm = TRUE),
        Rain = mean(prec_mean, na.rm = TRUE), 
        Temperature = mean(temp_mean, na.rm = TRUE), 
        NDVI = mean(ndvi_mean, na.rm = TRUE), 
        Carbon = mean(carbon_values, na.rm = TRUE), 
        pH = mean(ph_values, na.rm = TRUE),
        Nitrogen = mean(nitrogen_values, na.rm = TRUE), 
        Temperature_future = mean(fut_temp, na.rm = TRUE), 
        Clouds = mean(clouds_mean, na.rm = TRUE),
        .groups = "drop"
    )

# getting rid of row 1 
profils_mat = as.data.frame(profils[, -1])
rownames(profils_mat) = profils$species



### 0-1 normalization because the variables have different units with different ranges. Without normalization, we cannot compare 

profils_norm = as.data.frame(
    lapply(
        profils_mat, 
        function(x) (x - min(x)) / (max(x) - min(x))
    )
)

rownames(profils_norm) = rownames(profils_mat)
profils_norm




### fmsb format require that row 1 = max value and row 2 = min value, so : 

radar_data = rbind(
    rep(1, ncol(profils_norm)),
    rep(0, ncol(profils_norm)),
    profils_norm
)





### new order and colors  

# new order (according to the tree preference calculated before, with A. muscaria at the top)
new_order = c(
    "1", 
    "2",
    "Amanita muscaria", 
    "Picea abies", 
    "Abies alba", 
    "Pinus sylvestris", 
    "Pinus mugo", 
    "Betula pendula", 
    "Pinus cembra", 
    "Betula pubescens"
)
radar_data_order = radar_data[new_order, ]

# colors 
cols = c("#BD3106FF", "#5B7314FF", "#454B87FF", "#D9700EFF", "#E9A00EFF", "#EEBE04FF",  "#C3D6CEFF", "#89A6BBFF")

radar_data
radar_data_order

# names for the legend to match 

names = rownames(radar_data_order)
names = setdiff(names, c("1", "2")) # just removing the min and max rows that must not show in the legend 
names



### drawing the radar chart 

x11()

par(
    family = ".SF Compact Rounded",
    bg = "white"
)
radarchart(
    radar_data_order,
    pcol = cols,
    plwd = c(9, 9, 9, 4, 4, 4, 4, 4), 
    cglty = 6,
    cglwd = 2.5,
    cglcol = "grey50",
    vlcex = 1.2
)
title(
    "Environmental profile for each species",
    cex.main = 3,
    sub = "for each species",
    adj = 0
)
legend(
    "bottomright",
    legend = names,
    col = cols, 
    lwd = c(6, 6, 6, 2, 2, 2, 2, 2), 
    cex = 1.5,
    bty = "n"
)
    # one cool thing to see is how close the shape of A. muscaria and P. abies are 
    # this is an additionnal argument to the tree preference calculated before 
    # this is somewhat logical to find that it ressembles A. muscaria a lot because it is the most often nearest tree, so it is in the same climate
    # but as seen with the distances, P. abies are not that much closer than the other trees are (in term of distance)
        # so it means that P. abies is close, but also have a same environment as A. muscaria 
    # If we look at Betula pubescens, the shape is really different from A. muscaria 
        # furthemore, it was the least often nearest tree of A. muscaria
        # This indicates that the tree distance could be (even with all the shortfalls described in /src/final_project/analysis_1_tree_pref.r) a good proxy of relatedness of environment


# quartz.save(
#     "./plots/radar_chart.png", 
#     type = "png",
#     dpi = 300, 
#     width = 20, 
#     height = 13
# )
