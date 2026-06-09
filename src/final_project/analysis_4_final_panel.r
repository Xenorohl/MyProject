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
    plwd = c(11, 11, 11, 4, 4, 4, 4, 4), 
    cglty = 6,
    cglwd = 2.5,
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
    lwd = c(11, 11, 11, 4, 4, 4, 4, 4),
    lty = c(1, 2, 3, 4, 5, 6, 1, 2),
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


x11()
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






