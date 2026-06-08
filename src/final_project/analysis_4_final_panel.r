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
plot_b



### random forest prediction on france 

source("./src/final_project/analysis_2_2_random_forest.r")

plot_c



### environment type among the different species 

source("./src/final_project/analysis_3_environment.r")

# all the other plots are made with ggplot, this one is made with base R
# need to convert it with grob so it compatible with cowplot


tmp = "./data/temporary.png"        # creating a png file to store this plot (caused a lot of trouble in the final panel and this solution worked)
png(tmp, width = 1400, height = 505, bg = "white") # opening the editor of png file (the width and height had to be found to have a good ratio for the final panel --> see plots/final_panel.png if things do not work)

par(                                                 # basically all that follows here is creating a png file in grDevices
    family = ".SF Compact Rounded",
    bg = "white",
    mar = c(0, 6.5, 5, 0),      # modify the margin of the plots (also needed to tweak this parameter for a while to find the best match possible)
    oma = c(0, 0, 0, 0),
    col = "grey20"
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
    cex.main = 3.5,
    adj = 0,
    col.main = "grey20"
)
legend(
    "left",
    legend = names,
    col = cols, 
    lwd = c(6, 6, 6, 2, 2, 2, 2, 2), 
    cex = 3,
    bty = "n"
)
dev.off()   # telling the system to stop writing through grDevices in the png file 


img = readPNG(tmp)  # storing the modifications 
plot_d_grob = rasterGrob(img, interpolate = TRUE)   # rasterGrob allows a bettet fit for cowplot later, just like as_grob() done in the example but for a png



######################################################

##### Making the final panel 

######################################################

final_panel = ggdraw() + 

    draw_plot(plot_c,       x = 0.50, y = 0.00, width = 0.50, height = 0.99) +
    draw_plot(plot_a,       x = 0.00, y = 0.66, width = 0.50, height = 0.33) + 
    draw_plot(plot_b,       x = 0.00, y = 0.33, width = 0.50, height = 0.33) + 
    draw_plot(plot_d_grob,  x = 0.00, y = 0.00, width = 0.50, height = 0.33) + 

    draw_label("A",         x = 0.015, y = 0.98, fontface = "bold", size = 14) + 
    draw_label("B",         x = 0.02, y = 0.65, fontface = "bold", size = 14) + 
    draw_label("C",         x = 0.02, y = 0.32, fontface = "bold", size = 14) + 
    draw_label("D",         x = 0.51, y = 0.98, fontface = "bold", size = 14)   # the parameters seems a bit artificial, but again, it is for the final figure on my computer to fit as best as possible 
                                                                                # to see how it looked on my computer at that time, just see plots/final_panel.png


x11()
final_panel

quartz.save(
    "./plots/final_panel.png",
    type = "png",
    dpi = 300, 
    width = 30,           # width and height had to be tweaked for a while to find the best match as possible, the result is however not completly satisfying (blank between plot C and D that I couldnt fix unfortunately)
    height = 16.55,
    bg = "white"
)











