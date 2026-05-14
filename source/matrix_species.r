######################################################

##### Packages

######################################################

library(styler)
tidyverse_style() # for a coding-style closer to RStudio interface
library(rgbif)         
library(rnaturalearth) 
library(ggplot2)      
library(rinat)         
library(raster)        
library(dplyr)        
library(sf) 
library(plotly)
library(rgl)
library(htmlwidgets)
library(ggthemes)
library(extrafont)
library(paletteer)


### install 

# remotes::install_github("ropensci/rnaturalearthhires")





######################################################

##### User parameters 

######################################################

### species of interest 

fungi = "Amanita muscaria"
tree1 = "Pinus sylvestris"
tree2 = "Pinus mugo"
tree3 = "Pinus cembra"
tree4 = "Picea abies"
tree5 = "Abies alba"
tree6 = "Betula pendula"
tree7 = "Betula pubescens"



### max of gbif download

limit_fungi = 3000
limit_tree = 750    # limit a 750 for each because there are 7 tree species



### time filtering period 

date_start = as.Date("2020-01-01")
date_end = as.Date("2026-03-18")  # 6 years worth of observation 





######################################################

##### Switzerland base map 

######################################################

CH = ne_countries(
    scale = "large",
    returnclass = "sf", 
    country = "Switzerland"
)





######################################################

##### Get GBIF data 

######################################################

### downloading the occurences of A. muscaria with coordinates

gbif_raw = occ_data(
    scientificName = fungi, # takes Amanita muscaria, since fungi = "Amanita muscaria"
    hasCoordinate = TRUE,   # gives latitude and longitude
    limit = limit_fungi,    # 3500 obs max, as set before
    country = "CH"          # select only the switzerland occurences 
)



### downloading occurences for the different species of tree (with coordinates also)

for(i in 1:7){

    obs = get(paste0("tree", i )) # get() return the value of a named object. So here there are values for the 7 (because i in 1:7) raw 

    result = occ_data(
        scientificName = obs,
        hasCoordinate = TRUE,
        limit = limit_tree,
        country = "CH",
    ) # basic code that I want to repeat 7 times 

    assign(paste0("gbif_raw_tree", i ), result) # assign() assigns a value to a named object. Here df_result in the value, assigned to treei_inat

}
    # this loop "for" was made with the help of ChatGPT 
    # nevertheless, it doesn't mean i dont understand it, i just had to have a basic architecture because it's the first time im writing a loop
    # this loop "for" avoid to write the same code 7 times in a row 
    # PS : I did it before the lecture on loops, so I didnt know that it made the code run slower (in my head it was faster than repeating each step)

    # what it does is easy to understand (at this point) 
        # first line (obs = get(paste0)...) get 7 different values (from tree1 to tree7, because i in 1:7) and put them in the object "obs" for the further part of the code
        # the core of the code is how to retrieve data from gbif 
        # here with the loop, obs is 7 different values (explained earlier), so this loop retrieves 7 different species information 
        # final line (assign()...) assign a value (result from the core code) to a name (gbif_raw_treei, were i is 1:7)



# This is what the loop avoided (I will delete the "what it avoided" part for the next loops): 

# gbif_raw_tree1 = occ_data(
#     scientificName = tree1,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 

# gbif_raw_tree2 = occ_data(
#     scientificName = tree2,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 

# gbif_raw_tree3 = occ_data(
#     scientificName = tree3,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 

# gbif_raw_tree4 = occ_data(
#     scientificName = tree4,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 

# gbif_raw_tree5 = occ_data(
#     scientificName = tree5,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 

# gbif_raw_tree6 = occ_data(
#     scientificName = tree6,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 

# gbif_raw_tree7 = occ_data(
#     scientificName = tree7,
#     hasCoordinate = TRUE,
#     limit = limit_tree,
#     country = "CH",
# ) 


### extracting main data table 

gbif_occ_fungi = gbif_raw$data
gbif_occ_tree1 = gbif_raw_tree1$data
gbif_occ_tree2 = gbif_raw_tree2$data
gbif_occ_tree3 = gbif_raw_tree3$data
gbif_occ_tree4 = gbif_raw_tree4$data
gbif_occ_tree5 = gbif_raw_tree5$data
gbif_occ_tree6 = gbif_raw_tree6$data
gbif_occ_tree7 = gbif_raw_tree7$data



### quick inspection of the data 

head(gbif_occ_fungi)
names(gbif_occ_fungi)

nrow(gbif_occ_fungi) # number of records 
nrow(gbif_occ_tree2) # 750
nrow(gbif_occ_tree3) # 750 also, no need to further check

# fix(gbif_occ_fungi)



### quick base plot 

# x11()
# plot(
#     gbif_occ_fungi$decimalLongitude, 
#     gbif_occ_fungi$decimalLatitude,
#     pch = 16, 
#     col = "darkgreen", 
#     xlab = "Longitude",
#     ylab = "Latitude", 
#     main = "GBIF occurences in CH"
# )





######################################################

##### formating GBIF data 

######################################################

fungi_gbif = data.frame(
    species = gbif_occ_fungi$species,               # creating a column named "species" with the species names from gbif, here "Amanita muscaria"
    latitude = gbif_occ_fungi$decimalLatitude,      # renaming column as "latitude" with latitude data point of gbif
    longitude = gbif_occ_fungi$decimalLongitude,    # ... (same every step, renaming)
    date_obs = as.Date(gbif_occ_fungi$eventDate),
    source = "gbif"
)

head(fungi_gbif)
str(fungi_gbif)
nrow(fungi_gbif)

for(i in 1:7){

    source = get(paste0("gbif_occ_tree",i))

    result = data.frame(
        species = source$species,
        latitude = source$decimalLatitude,
        longitude = source$decimalLongitude,
        date_obs = as.Date(source$eventDate),
        source = "gbif"
    )

    assign(paste0("tree",i,"_gbif"), result)

}   # exactly the same as fungi but for trees. The loop explanation is already done before (line 100-125)





######################################################

##### get the iNaturalist data 

######################################################

inat_raw = get_inat_obs(
    query = fungi,              # getting the name "Amanita muscaria", not 100% reliable (explanation below, line 292-293)
    place_id = "switzerland",   # only in switzerland 
    maxresults = 2500           # limit of 2500, and not fungi_limit because there will be lower than that anyway (already tested)
)

for(i in 1:7){ 

    source = get(paste0("tree", i ))

    result = get_inat_obs(
        query = source,
        place_id = "switzerland",
        maxresults = 750
    )

    assign(paste0("tree", i ,"_inat_raw"), result)

} # same explanation for the loop as before

head(inat_raw)
nrow(inat_raw)
names(inat_raw)

inat_new = inat_raw %>%
    filter(scientific_name == "Amanita muscaria")   # to keep only the "Amanita muscaria", become some other names popped up

inat_new$scientific_name = as.factor(inat_new$scientific_name)  # converting to factor to check if there are only "Amanita muscaria" and not other names 
levels(inat_new$scientific_name)    # it worked, only "Amanita muscaria"
# fix(inat_new)



### quick check for inaturalist data concerning the trees (same as amanita muscaria that had other obsevation like amanita pantherina for some reason)

tree1_inat_raw$scientific_name = as.factor(tree1_inat_raw$scientific_name)
levels(tree1_inat_raw$scientific_name) # seems like the problem persist everytime, like WHY other species than pinus sylvestris are taken ????
    # Anyway, I will just correct every dataframe 



### data curation, to keep only the desired species 

for(i in 1:7){

    x = get(paste0("tree", i ))
    y = get(paste0("tree", i ,"_inat_raw"))

    result = y %>%
        filter(scientific_name == x)

    assign(paste0("tree", i ,"_inat_cured"), result)

} # omg im so happy to understand loops 



# no need to make a loop for the following, it doesnt take much space 

nrow(tree1_inat_raw) - nrow(tree1_inat_cured) # 55 wrong species 
nrow(tree2_inat_raw) - nrow(tree2_inat_cured) # 101 wrong species 
nrow(tree3_inat_raw) - nrow(tree3_inat_cured) # 9 wrong species 
nrow(tree4_inat_raw) - nrow(tree4_inat_cured) # 276 wrong species !!
nrow(tree5_inat_raw) - nrow(tree5_inat_cured) # 62 wrong species 
nrow(tree6_inat_raw) - nrow(tree6_inat_cured) # 12 wrong species 
nrow(tree7_inat_raw) - nrow(tree7_inat_cured) # 1 wrong species 



### plot for iNat occurences only 

# x11()
# ggplot(data = CH) +
#     geom_sf(
#         fill = "grey95",
#         color = "black") +
#     geom_point(
#         data = inat_new, 
#         aes(
#             x = longitude,
#             y = latitude
#         ),
#         size = 3, 
#         shape = 21,
#         fill = "darkred",
#         color = "black"
#     ) + 
#     theme_classic()





######################################################

##### formating the iNat data 

######################################################

fungi_inat = data.frame(
    species = inat_new$scientific_name,         # getting the same column name, so we can merge the several dataframe later 
    latitude = inat_new$latitude,               # same ol
    longitude = inat_new$latitude,              # ...
    date_obs = as.Date(inat_new$observed_on),
    source = "inat"
)

for(i in 1:7){

  source <- get(paste0("tree", i, "_inat_cured"))

  result <- data.frame(
    species = source$scientific_name,
    latitude = source$latitude,
    longitude = source$longitude,
    date_obs = as.Date(source$observed_on),
    source = "inat"
  ) 

  assign(paste0("tree", i, "_inat"), result) 

} # same explanation for the loop as before 





######################################################

##### merging the two matrix into one #####

######################################################

all_merged = bind_rows(
    fungi_gbif,
    fungi_inat,
    tree1_gbif,
    tree2_gbif,
    tree3_gbif,
    tree4_gbif,
    tree5_gbif,
    tree6_gbif,
    tree7_gbif,
    tree1_inat,
    tree2_inat,
    tree3_inat,
    tree4_inat,
    tree5_inat,
    tree6_inat,
    tree7_inat
)   # merging the dataframe by the rows, works because every dataframe has the same column names 

str(all_merged)

all_merged$species = as.factor(all_merged$species)
all_merged$source = as.factor(all_merged$source)
levels(all_merged$species)
table(all_merged$species) # more observation for fungi (~3800) than tree species (~1000 each)

levels(all_merged$source)
table(all_merged$source) # 8250 observation from gbif, 3365 from inat



### keeping results only between the two selected dates 

all_dated = all_merged %>%
    filter(!is.na(date_obs)) %>%    # remove the NA values
    filter(date_obs >= date_start & date_obs <= date_end) # keeping only the observations of the desired date 

matrix_species = distinct(all_dated) # distinct allows to remove duplicate 
nrow(matrix_species)





######################################################

##### mapping the data 

######################################################

# a = ggplot(data = CH) + 
#     geom_sf(
#         fill = "aliceblue",
#         color = "black"
#     ) +
#     geom_point(
#         data = matrix_species,
#         aes(
#             x = longitude,
#             y = latitude, 
#             fill = species
#             ),
#         color = "black",
#         size = 1,
#         shape = 21
#     ) + 
#     labs(
#         title = "Observations of Amanita muscaria and 7 species of tree in CH"
#     ) + 
#     theme_minimal()

# ggplotly(a)
    # there seems to be a problem
    # some observation have a longitude higher than 7 (most of them have a latitude of 45) 
    # it's almost like the longitute is a copy of the latitude. Don't know why it happened
    # gotta remove those since they are absurd 



### removing the too high longitude points 

matrix_species = matrix_species %>%
    filter(!(longitude > 20)) # 20 is arbitrary, but all the absurd observation were at around 47 for longitude, and I must keep observations below ~10, so I chose 20

nrow(matrix_species) # around 700 observations lost. Still 7000 available tho 



### mapping with clean geographic data 

x11()
ggplot(data = CH) + 
    geom_sf(
        fill = "aliceblue",
        color = "grey40"
    ) + 
    geom_point(
        data = matrix_species,
        aes(
            x = longitude,
            y = latitude,
            color = source
        ),
        size = 1.5,
        alpha = 0.7
    ) + 
    labs(
        title = "Different sources of observations",
        x = "Longitude",
        y = "Latitude"     
    ) +
    theme_minimal()



x11()
plot_species = ggplot(
    data = CH
    ) +
    geom_sf(
        fill = "aliceblue",
        color = "grey"
    ) + 
    geom_point(
        data = matrix_species,
        aes(
            x = longitude,
            y = latitude, 
            color = species
        )
    ) + 
    labs(
        x = "Longitude",
        y = "Latitude",
        title = "Observation of 8 species on CH",
        subtitle = "Amanita muscaria and 7 associated trees"
    ) + 
    scale_color_paletteer_d(
        "MoMAColors::Ernst"
    ) + # found on google, very cool palettes on there, installation is very easy (description on https://r-graph-gallery.com/color-palette-finder)
    theme_minimal() +
    theme(
        plot.title = element_text(size = 20)    # bigger title
    )

ggplotly(plot_species) # allow interactive plot, because it is not really readable with all those points, works better with by being interactive
