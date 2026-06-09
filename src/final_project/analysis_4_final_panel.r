######################################################

##### Packages 

######################################################

    # all packages available in inst/packages.r 





######################################################

##### Loading the different plots 

######################################################

### tree preferences 

source("./src/final_project/analysis_1_tree_pref.r")

plot_a 
# plot_b
    # was here for the pre_final_panel before the adjustment



### random forest prediction on france 

source("./src/final_project/analysis_2_2_random_forest.r")

plot_c # map
plot_e # importance 



### environment type among the different species (radar chart)

source("./src/final_project/analysis_3_environment.r")

# all the other plots are made with ggplot, this one is made with base R
# need to convert it with grob so it compatible with cowplot


radar_data_order_col = radar_data_order %>%
    select(
        Temperature, 
        Elevation, 
        Rain, 
        NDVI, 
        Carbon, 
        Temperature_future, 
        Nitrogen, 
        pH, 
        Clouds
    )   # I also change the order of the columns 
        # radarchart will display the columns anticlockwise on the chart
        # the idea was to put the long names on the top and bottom and the shorter names on the sides (so the figure could come out bigger on the final pannel)

# changing the colors of the lines (also made after the pre_final_panel that contains the colors from src/final_project/anaylsis_3_environment.r)

cols = c("#D9AF6BFF", "#855C75FF", "#736F4CFF", "#AF6458FF", "#526A83FF", "#625377FF", "#68855CFF", "#9C9C5EFF", "#A06177FF")


tmp = "./plots/radar_chart_def.png"        # creating a png file to store this plot (caused a lot of trouble in the final panel and this solution worked)
png(tmp, width = 2*600, height = 2*1000, bg = "white") # opening the editor of png file (the width and height had to be found to have a good ratio for the final panel --> see plots/final_panel.png if things do not work)

par(                                                 # basically all that follows here is creating a png file in grDevices
    family = ".SF Compact Rounded",
    bg = "white",
    mar = c(0, 3, 0, 1),      # modify the margin of the plots (also needed to tweak this parameter for a while to find the best match possible)
    oma = c(0, 0, 0, 0),
    col = "grey20"
)
radarchart(
    radar_data_order_col,
    pcol = cols,
    plwd = c(13, 13, 13, 4, 4, 4, 4, 4), # to highlight A. muscaria and the two nearest species of tree (otherwise, the graph would look too messy and barely readable)
    plty = c(1, 1, 1, 5, 5, 5, 5, 5),
    cglty = 6,
    cglwd = 3,
    cglcol = "grey50",
    vlcex = 1.2*2
)
title(
    "Environmental profiles",
    cex.main = 2.8*2,
    adj = 0,
    col.main = "grey20",
    line = -29.5  # having the title closer to the figure, but will cause a giant white gab above the figure that will be corrected in the final gg_draw()
)
legend(
    x = -0.6,
    y = -1.3,
    legend = names,
    col = cols, 
    lwd = c(13, 13, 13, 3, 3, 3, 3, 3),
    lty = c(1, 1, 1, 5, 5, 5, 5, 5),
    cex = 3.5,
    bty = "n"
)
dev.off()   # telling the system to stop writing through grDevices in the png file 



  # storing the modifications 
img = png::readPNG(tmp)
plot_d_grob = rasterGrob(img, interpolate = TRUE)   # rasterGrob allows a bettet fit for cowplot later, just like as_grob() done in the example but for a png



######################################################

##### Making the final panel 

######################################################

final_panel = ggdraw() + 

    draw_plot(plot_c,       x = 0.50, y = 0.00, width = 0.50, height = 0.99) +
    draw_plot(plot_e,       x = 0.00, y = 0.00, width = 0.25, height = 0.66) + 
    draw_plot(plot_d_grob,  x = 0.25, y = 0.00, width = 0.25, height = 0.83) +  # really important to put that one first, so plot_a can go above this one (see plots/radar_chart_def.png). The height is at 0.83 because there was some problem with the distance between the title and the figure
    draw_plot(plot_a,       x = 0.00, y = 0.66, width = 0.50, height = 0.33) + 

    draw_label("A",         x = 0.015, y = 0.98, fontface = "bold", size = 14) + 
    draw_label("B",         x = 0.02, y = 0.65, fontface = "bold", size = 14) + 
    draw_label("C",         x = 0.25, y = 0.65, fontface = "bold", size = 14) + 
    draw_label("D",         x = 0.51, y = 0.98, fontface = "bold", size = 14)   # the parameters seems a bit artificial, but again, it is for the final figure on my computer to fit as best as possible 
                                                                                # to see how it looked on my computer at that time, just see plots/final_panel.png


# x11()
final_panel
    # for the panel to display correctly, we must enlarge the window created by x11() (or windows() on windows)
    # the figure C doesn't allign well in this window since it was made with something else than ggplot 
    # it isn't the end of the world but, for the sake of it, it is well alligned in plots/true_final_panel.png (all the micro-adjustement was because of this specific graphic)



# quartz.save(
#     "./plots/true_final_panel.png",
#     type = "png",
#     dpi = 400, 
#     width = 30,           # width and height had to be tweaked for a while to find the best match as possible, the result is however not completly satisfying (blank between plot C and D that I couldnt fix unfortunately)
#     height = 16.55,
#     bg = "white"
# )


##### Analysis / interpretation (copy-pasted from the other scripts) : 

## Figure A 
    # as we can see Picea abies and Abies alba are the trees that are more often the closest to A. muscaria 
    # we could say from these results that A. muscaria prefere an association with those trees 
    # what is interesting is that those are both fir trees 
    # the two that comes after are Pinus sylvestris and Pinus mugo, also fir trees 
    # it would mean that A. muscaria prefere fir trees rather than deciduous trees 
    # before coming to any conclusion, it would be interesting to see how close those trees actually are 

    # something to add up when I checked the distances (see plot_b in the dedicated file)
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



## Figure B

    # visually it is way better than before 
    # all variables are above 50 
    # the variables that explains the most is NDVI and then Temperature, elevation and precipitation
    # a bit sad to see that soil variables are the three least important 
    # I would have thought otherwise, but it's possible that this comes from the precision of the soilgrid which is 250m 
    # But even with the least importance, Nitrogen and Carbon still are at ~80 of Mean decrease in Gini
    # So it seems that to predict the presence of A. muscaria at best we should use variables that are not linked to soil 
    # as explained, the data isn't really accurate (problem with gbif, soil grid of 250m, duplicates, ...)
    # But according to this graph, the soil values are less important that the others 



## Figure C 

    # one cool thing to see is how close the shape of A. muscaria and P. abies are 
    # this is an additionnal argument to the tree preference calculated before 
    # this is somewhat logical to find that it ressembles A. muscaria a lot because it is the most often nearest tree, so it is in the same climate
    # but as seen with the distances, P. abies are not that much closer than the other trees are (in term of distance)
        # so it means that P. abies is close, but also have a same environment as A. muscaria 
    # If we look at Betula pubescens, the shape is really different from A. muscaria 
        # furthemore, it was the least often nearest tree of A. muscaria
        # This indicates that the tree distance could be (even with all the shortfalls described in /src/final_project/analysis_1_tree_pref.r) a good proxy of relatedness of environment



## Figure D 

    # we can see that the presence is predicted more in the south that in the north 
    # it is important to take into account that the model isn't completely accurate (~58%)
    # normally we should find A. muscaria in more northern type of place 
    # I have done a test to compare visually the model with the observed data points 
        # it is really not complicate to modify the file analysis_2_1_grid_pred.r 
            # all it needs is : 
                # change the grid in the begining (lon(5:11), lat(45:48))
                # change the ne_countrie with : 
                    # # FR = ne_countries(          important to keep "FR" so the script can run without changing everything 
                    #             scale = "large",
                    #             country = "switzerland", 
                    #             returnclass = "sf")
                # change the 3 different soils pathway to have the swiss ones 
            # since the changes aren't really big, I didnt add another script 
        # the test was not really conclusive, the precision is indeed around 60% when it comes to compare with observed datas
    # It is therefore important not to take this graph too seriously
    # I decided to keep it because it is cool looking, and I was happy to have done a random forest 
