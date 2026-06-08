######################################################

##### Packages 

######################################################

    # all packages are available in inst/packages.r 





######################################################

##### Import dataset 

######################################################

matrix = read.csv("./data/matrix_final.csv")





######################################################

##### Distance calculation between A. muscaria and nearest tree 

######################################################

# Important note : 
    # the tree calculation was made with the help of Claude AI 
    # the "form" of the code was rebuild by me to be consistent with the other parts
    # everything was re-written so I would try to understand this code the best I could 


### separation between fungi and trees 

amanita = matrix %>%
    filter(species == "Amanita muscaria")

trees = matrix %>%
    filter(species != "Amanita muscaria") 



### Function : nearest tree 
    # a function is created in order to apply it to every row of the matrix

find_nearest_tree_species = function(amanita_row, trees_df) {

    coords_fungi = c(
        amanita_row$longitude,
        amanita_row$latitude
    )   # coordinates of fungi as vector 
    
    coords_trees = cbind(
        trees_df$longitude,
        trees_df$latitude
    )   # coordinates of trees as matrix 

    dists = distHaversine(
        coords_fungi,
        coords_trees
    )   # distHaversine from the package geosphere
        # gives the distance between two coordinates in meter [m]

    dist_min = which.min(dists) # have the closest distance 

    data.frame( # creating a dataframe to store the values 
        nearest_distance_m = round(dists[dist_min], 2), # rounding the nearest distance with 2 digits after comma   
        nearest_tree_sp = trees_df$species[dist_min] # which tree species has that distance 
    )

}



### Applying the function to every rows 

nearest_info = do.call( # do.call allow to call a function with the elements of a list as arguments 
    rbind, # rbind to glue all the different rows calculated under two variables 
    lapply( # lapply run a function for each number of the sequence, where i is the iteration. lapply return a list 
        seq_len(nrow(amanita)), # create a sequence from 1 to the total number of row of amanita (just like 1:nrow(amanita))
        function(i) {
            find_nearest_tree_species(
                amanita[i ,],   # for each i value (1:nrow(amanita)), we run the function created before 
                trees
            )   # the function then return the dataframe with the two columns (nearest_distance_m and nearest_tree_sp)
        }
    )
)



### Addind the new columns 

amanita_distance = cbind(
    amanita,
    nearest_info
)



### Quick overview of the results 

table(amanita_distance$nearest_tree_sp)
    # More Picea abies and Abies alba than other tree species 
    # could indicate a preference for those tree species 





######################################################

##### Plotting the data 

######################################################

amanita_distance$nearest_tree_sp = factor(
    amanita_distance$nearest_tree_sp,
    levels = c(
        "Betula pubescens",
        "Pinus cembra",
        "Betula pendula", 
        "Pinus mugo", 
        "Pinus sylvestris", 
        "Abies alba", 
        "Picea abies"
    )
)


plot_a = ggplot(
    data = amanita_distance,
    aes(
        x = nearest_tree_sp,
        fill = nearest_tree_sp
    )
    ) + 
    geom_bar(
        color = "white" 
    ) + 
    scale_fill_paletteer_d(
        "MetBrewer::Archambault"
    ) + 
    labs(
        x = "Tree species",
        y = "Count of trees",
        title = "Closest tree species", 
        subtitle = "Count of nearest tree to A. muscaria"
    ) +
    theme_fivethirtyeight() + 
    theme(
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 13), 
        plot.subtitle = element_text(size = 15),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),   
        panel.background = element_rect(fill = "white"), 
        plot.title = element_text(size = 30),
        text = element_text(family = ".SF Compact Rounded", color = "grey20")
    )
plot_a
    # as we can see Picea abies and Abies alba are the trees that are more often the closest to A. muscaria 
    # we could say from these results that A. muscaria prefere an association with those trees 
    # what is interesting is that those are both fir trees 
    # the two that comes after are Pinus sylvestris and Pinus mugo, also fir trees 
    # it would mean that A. muscaria prefere fir trees rather than deciduous trees 
    # before coming to any conclusion, it would be interesting to see how close those trees actually are 

plot_b = ggplot(
    data = amanita_distance,
    aes(
        x = nearest_tree_sp,
        y = nearest_distance_m,
        fill = nearest_tree_sp
    )
    ) + 
    geom_boxplot(
        color = "#414141"
    ) + 
    geom_jitter(
        pch = 20,
        width = 0.25,
        alpha = 0.6
    ) + 
    labs(
        title = "Distances from A. muscaria",
        subtitle = "For each tree species",
        x = "",
        y = "Distance [m]"
    ) +
    theme_fivethirtyeight() + 
    scale_fill_paletteer_d(
        "MetBrewer::Archambault"
    ) +
    theme(
        legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 15),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"), 
        text = element_text(family = ".SF Compact Rounded", color = "grey20")
    )
plot_b
    # the distinction isn't as clear as in plot_a 
    # it seems like the distances are relatively always the same for the 4 fir trees 
    # could mean that the fungi are simply taken in different environment with different tree species 
    # however, one thing that is clearly weird is the distance itself 
        # for 3 A. muscaria, the closest tree recorded was at 10km (the nearest one!)
        # since A. muscaria is a obligatory mycorhizal fungi, this result is simply impossible 
        # either I took not enough trees for each species, or not took a various enough number of species 
        # could be the number of individuals for each species since it was pretty low compared to A. muscaria 
        # furthermore, during the data curation, there were a lot of data point lost (NA values) or duplicates 
        # those data lost influences a lot the result 
    # One thing to note additionnally : 
        # most of the data were taken on gbif 
        # data from gbif are altered (not precise)
        # means that a lot of those data are duplicate of location 
        # some datapoint of a tree could have had the exact same spot as a tree, meaning the loss of one of the two 
        # so, with this dataset, it is difficult to interpret which tree A. muscaria prefer out of these 7 species 
        # a we can see, there are more points at even distances (like 5000m especially) which isn't realistic biologically speaking 
            # this could be due to the altered data of gbif
        # However, the results are still cool looking visually 




### saving the plots with quartz.save (since MacOS works with x11() and XQuartz)

# x11()
# plot_a
# quartz.save(
#     "./plots/nearest_tree_score.png",
#     type = "png",
#     dpi = 300,
#     width = 11,
#     height = 11
#     )

# x11()
# plot_b
# quartz.save(
#     "./plots/nearest_tree_distance.png",
#     type = "png",
#     dpi = 300,
#     width = 11,
#     height = 11
# )




