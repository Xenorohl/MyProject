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

##### Plotting the ecosystem data 

###############################################



# ============================================================
# Calcul de la distance entre Amanita muscaria et l'arbre
# le plus proche (une seule occurrence par champignon)
# ============================================================

library(dplyr)
library(geosphere)  # distHaversine() pour distances géographiques

# --- 1. Chargement des données ---
df <- read.csv("./data/matrix_final.csv", stringsAsFactors = FALSE)

# --- 2. Séparation champignons / arbres ---
amanita <- df %>% filter(species == "Amanita muscaria")
trees   <- df %>% filter(species != "Amanita muscaria")

cat("Nombre d'observations Amanita muscaria :", nrow(amanita), "\n")
cat("Nombre d'observations arbres           :", nrow(trees),   "\n")
cat("Espèces d'arbres présentes :", paste(unique(trees$species), collapse = ", "), "\n\n")

# --- 3. Fonction : arbre le plus proche pour une ligne de champignon ---
# Retourne un data.frame avec distance_m et nearest_tree_species

find_nearest_tree <- function(amanita_row, trees_df) {
  # Coordonnées du champignon (lon, lat — ordre requis par geosphere)
  coords_fungi <- c(amanita_row$longitude, amanita_row$latitude)
  
  # Matrice de coordonnées de tous les arbres
  coords_trees <- cbind(trees_df$longitude, trees_df$latitude)
  
  # Distances Haversine en mètres
  dists <- distHaversine(coords_fungi, coords_trees)
  
  # Index du plus proche
  idx_min <- which.min(dists)
  
  data.frame(
    distance_nearest_tree_m = round(dists[idx_min], 2),
    nearest_tree_species     = trees_df$species[idx_min]
  )
}

# --- 4. Application à toutes les lignes d'Amanita muscaria ---
# (lapply sur les lignes, puis rbind)
nearest_info <- do.call(rbind, lapply(seq_len(nrow(amanita)), function(i) {
  find_nearest_tree(amanita[i, ], trees)
}))

# --- 5. Ajout des nouvelles colonnes ---
amanita_enriched <- cbind(amanita, nearest_info)

# --- 6. Aperçu des résultats ---
cat("Aperçu des 6 premières lignes enrichies :\n")
print(head(amanita_enriched[, c("species", "latitude", "longitude",
                                 "distance_nearest_tree_m",
                                 "nearest_tree_species")]))

cat("\nDistribution des espèces d'arbres les plus proches :\n")
print(table(amanita_enriched$nearest_tree_species))

cat("\nStatistiques sur les distances (mètres) :\n")
print(summary(amanita_enriched$distance_nearest_tree_m))

amanita_enriched$distance_nearest_tree_m 
