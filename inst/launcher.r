##### Packages 

source("./inst/packages.r")




##### Intermediate project 

# script to get the species information on iNat and gbif 
source("./src/intermediate_project/matrix_species.r")

# getting the ecosystem values for each point using different raster (including soil information)
source("./src/intermediate_project/ecosystem.r")

# getting the elevation information using another raster 
source("./src/intermediate_project/elevation.r")

# getting climatic information (precipitaion, temperature, cloud cover) from the Chelsa dataset 
source("./src/intermediate_project/climatic.r")

# getting satellite information from appeears (ndvi)
source("./src/intermediate_project/satellite_data.r") 




##### Final project 

# just stating the different ecological questions 
source("./src/final_project/analysis_0_ecological_question.r")

# analysing which is the preferred tree of A. muscaria based on the distance 
source("./src/final_project/analysis_1_tree_pref.r")

# making a grid to predict the presence of A. muscaria in France, where no datapoints where initially observed for this projet
source("./src/final_project/analysis_2_1_grid_pred.r")

# random forest on the grid to predict the presence of A. muscaria in France & importance of each variable 
source("./src/final_project/analysis_2_2_random_forest.r")

# environment analysis using a radar chart 
source("./src/final_project/analysis_3_environment.r")
    
# putting all the plots together in one final panel 
source("./src/final_project/analysis_4_final_panel.r")
