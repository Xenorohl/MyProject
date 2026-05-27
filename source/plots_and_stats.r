###############################################

##### Packages #####

###############################################

# library(styler)
# tidyverse_style()
# library(ggplot2)
# library(paletteer)
# library(ggthemes)
# library(extrafont)
# #  font_import() if already done no need to do it again, takes a few minutes, done from with MacOS
# library(sf)
# library(rnaturalearth)
# library(cowplot)



### import the dataset

matrix = read.csv("./data/matrix_final.csv") # importing the saved matrix 

CH = ne_countries(
    scale = "large",
    country = "switzerland",
    returnclass = "sf"
)





###############################################

##### Plotting the ecosystem data 

###############################################

