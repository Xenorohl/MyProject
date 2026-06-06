###############################################

##### import the dataset

###############################################



matrix = read.csv("./data/matrix_final.csv") # importing the saved matrix 

CH = ne_countries(
    scale = "large",
    country = "switzerland",
    returnclass = "sf"
)





###############################################
