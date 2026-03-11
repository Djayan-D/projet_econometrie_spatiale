    #----- 1. IMPORTER LES PACKAGES -----

library(readxl)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(mapview)
library(cartography)
library(dplyr)
library(spdep)
    

    #----- 2. IMPORTER LES DONNÉES -----

# « Dans quelle mesure les disparités socio-démographiques se reflètent-elles 
# dans la distribution territoriale de l’abstention au second tour des 
# législatives de 2024 ? »

# Fichier Excel :
#       
# Code_INSEE : numéro du département
# Libellé : libellé du département
# Abstentions_pct : taux d'abstention au second tour des élections législatives anticipées de 2024 (en %)
# Part_25_59_ans : part des personnes âgées de 25 à 59 ans dans la population totale (en %)
# Taux_pauvrete_2021 : taux de pauvreté en 2021 (en %)
# Part_Femme : part des femmes dans la population totale en 2024 (en %)
# 
# 
# Fichier shp :
# 
# ID       : identifiant long
# NOM_M    : libellé en majuscule des départements
# NOM      : libellé des départements
# INSEE_DEP: numéro de département
# INSEE_REG: numéro de région

    #----- 2.1. Importer les données relatives aux élections et aux caractéristiques socio-démographiques -----
  
# Importer

data_raw <- read_excel("data/data_raw.xlsx")



# Visualiser les caractéristiques

str(data_raw)



    #----- 2.2. Importer les données spatiales -----

# Importer

data_space <- st_read("carte/DEPARTEMENT.shp")


# Visualiser les caractéristiques

str(data_space)


#  Vérifier le référentiel

st_crs(data_space)
## RGF93 Lambert 93 



    #----- 2.3. Joindre les deux bases -----

# Joindre

shp_join <- merge(data_space, data_raw, by.x = "INSEE_DEP", by.y = "Code_INSEE", all.x = TRUE)


# Visualiser les caractéristiques

str(shp_join)


# Chercher les différences entre shp_join$Libellé et shp_join$NOM

shp_join$Libellé[shp_join$Libellé != shp_join$NOM]
shp_join$NOM[shp_join$Libellé != shp_join$NOM]

## Différence : Dans NOM, "Côtes d'Armor" n'a pas de tirets et "Indre-et-Loire "
## a un espace à la fin. On ne conserve que Libellé pour la suite de l'analyse. 


# Retirer les variables redondantes

## À l'image de NOM, NOM_M n'est que la version en majuscules de NOM. On peut 
## donc retirer cette variable.

shp_vote <- shp_join |>
  select(-NOM, -NOM_M)



    #----- 2.4. Retirer la Corse -----

## L'étude ne portant que sur la France continentale, il est nécessaire de
## retirer les départements de la Corse (2A et 2B).

shp_vote <- shp_vote |>
  filter(!INSEE_DEP %in% c("2A", "2B"))





    #----- 3. ANALYSER LES DONNÉES NON SPATIALES ("ÉTUDE UNIVARIÉE") ----

# Réaliser le résumé statistique des variables

summary(shp_vote$Abstentions_pct)
summary(shp_vote$Part_25_59_ans)
summary(shp_vote$Taux_pauvrete_2021)
summary(shp_vote$Part_Femme)


# Visualiser les variables avec des boxplots

ggplot(shp_vote, aes(y = Abstentions_pct)) +
  geom_boxplot() +
  labs(title = "Boxplot du taux d'abstention au second tour des élections législatives anticipées de 2024",
       y = "Taux d'abstention (%)") +
  theme_minimal()

ggplot(shp_vote, aes(y = Part_25_59_ans)) +
  geom_boxplot() +
  labs(title = "Boxplot de la part des personnes âgées de 25 à 59 ans dans la population totale",
       y = "Part des 25-59 ans (%)") +
  theme_minimal()

ggplot(shp_vote, aes(y = Taux_pauvrete_2021)) +
  geom_boxplot() +
  labs(title = "Boxplot du taux de pauvreté en 2021", 
       y = "Taux de pauvreté (%)") +
  theme_minimal()

ggplot(shp_vote, aes(y = Part_Femme)) +
  geom_boxplot() +
  labs(title = "Boxplot de la part des femmes dans la population totale en 2024", 
       y = "Part des femmes (%)") +
  theme_minimal()


# Réaliser une première cartographie

## Définir une fonction pour réaliser les cartes choroplèthes

plot_choro <- function(data, var, title, pal, nclass = 6) {
  
  methods <- c("quantile", "equal", "sd")
  
  values <- data[[var]]
  
  layout(matrix(c(1,2,
                  3,0), 
                nrow = 2, byrow = TRUE))
  
  par(mar = c(1,1,2,1))
  
  for (m in methods) {
    
    if (m == "sd"){
      brks <- getBreaks(values, method = "sd", nclass = nclass)
      ncols <- length(brks) - 1
      method_use <- "fixed"
    } else {
      ncols <- nclass
      method_use <- m
      brks <- NULL
    }
    
    choroLayer(
      spdf = data,
      dfid = "INSEE_DEP",
      var = var,
      method = method_use,
      breaks = brks,
      nclass = nclass,
      col = carto.pal(pal1 = pal, n1 = ncols),
      legend.pos = "bottomleftextra",
      legend.title.txt = "Legende"
    )
    
    layoutLayer(
      title = paste0(title, "\nMéthode : ", m),
      author = "INSEE",
      scale = NULL,
      north = FALSE,
      frame = TRUE,
      tabtitle = TRUE
    )
  }
}



## Réaliser les cartes choroplèthes pour les différentes variables

plot_choro(
  data = shp_vote,
  var = "Abstentions_pct",
  title = "Taux d'abstention au second tour des élections législatives anticipées de 2024",
  pal = "red.pal"
)

plot_choro(
  data = shp_vote,
  var = "Part_25_59_ans",
  title = "Part des personnes âgées de 25 à 59 ans dans la population totale (en %)",
  pal = "blue.pal"
)

plot_choro(
  data = shp_vote,
  var = "Taux_pauvrete_2021",
  title = "Taux de pauvreté en 2021 (en %)",
  pal = "grey.pal"
)

plot_choro(
  data = shp_vote,
  var = "Part_Femme",
  title = "Part des femmes dans la population totale en 2024 (en %)",
  pal = "pink.pal"
)


plot_choro <- function(data, var, title, pal, nclass = 6) {
  
  methods <- c("quantile", "equal", "sd")
  
  # extraire les valeurs de la variable
  values <- data[[var]]
  
  layout(matrix(c(1,2,
                  3,0), 
                nrow = 2, byrow = TRUE))
  
  par(mar = c(1,1,2,1))
  
  for (m in methods) {
    
    if (m == "sd"){
      brks <- getBreaks(values, method = "sd", nclass = nclass)
      ncols <- length(brks) - 1
      method_use <- "fixed"
    } else {
      ncols <- nclass
      method_use <- m
      brks <- NULL
    }
    
    choroLayer(
      spdf = data,
      dfid = "INSEE_DEP",
      var = var,
      method = method_use,
      breaks = brks,
      nclass = nclass,
      col = carto.pal(pal1 = pal, n1 = ncols),
      legend.pos = "bottomleftextra",
      legend.title.txt = "Legende"
    )
    
    layoutLayer(
      title = paste0(title, "\nMéthode : ", m),
      author = "INSEE",
      scale = NULL,
      north = FALSE,
      frame = TRUE,
      tabtitle = TRUE
    )
  }
  
  par(mfrow = c(1,1))
  
}



    #----- 4. ANALYSER LES DONNÉES SPATIALES ("ÉTUDE UNIVARIÉE SUITE") ----

    #----- 4.1. Définir les voisins selon le critère de contiguïté d'ordre 1 -----

# Représenter avec le type "Tour"

nb_tour <- poly2nb(shp_vote, row.names=shp_vote$Libellé, queen = FALSE)
summary(nb_tour)

nb_lines_tour <- nb2lines(nb_tour, coords = st_centroid(st_geometry(shp_vote)))
nb_sf_tour <- st_as_sf(nb_lines_tour)
ggplot() +
  geom_sf(data = shp_vote, fill = "lightblue", color = "black") +
  geom_sf(data = nb_sf_tour, color = "red") +
  theme_minimal() +
  ggtitle("Carte des relations de voisinage de type 'Tour'")


# Représenter avec le type "Reine"

nb_reine <- poly2nb(shp_vote, row.names=shp_vote$Libellé, queen = TRUE)
summary(nb_reine)

nb_lines_reine <- nb2lines(nb_reine, coords = st_centroid(st_geometry(shp_vote)))
nb_sf_reine <- st_as_sf(nb_lines_reine)
ggplot() +
  geom_sf(data = shp_vote, fill = "lightblue", color = "black") +
  geom_sf(data = nb_sf_reine, color = "red") +
  theme_minimal() +
  ggtitle("Carte des relations de voisinage de type 'Reine'")

## Pas de différence avec le type "Tour" car dans aucune situation il n'y a 
## qu'un angle partagé, il y a toujours un côté partagé.



    #----- 4.2. Créer la matrice de contiguïté d'ordre 1 standardisée de type reine -----

# Créer la matrice

W_reine <- nb2listw(nb_reine,style="W",zero.policy=TRUE)


# Afficher la 28e ligne

shp_vote$Libellé[28]
W_reine$weights[28]

## Retour logique : Le Finistère ayant 2 voisins (le Morbihan et les 
## Côtes-d'Armor) les poids doivent être 1/2 = 0.5 pour chacun de ces voisins.



    #----- 4.3. Créer la matrice de poids standardisée et représenter les liens

# Créer les coordonnées des centroids

centroids <- st_centroid(st_geometry(shp_vote))
coords <- st_coordinates(centroids)
crs <- st_crs(shp_vote)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")



    #----- 4.3.1. Réaliser l'opération avec k=1 (le plus proche voisin) -----

# Créer la liste de voisinage

k <- 1
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
summary(neighbors)


# Représenter les liens

nb_lines <- nb2lines(neighbors, coords = coords_sp)
nb_sf <- st_as_sf(nb_lines)
ggplot() +
  geom_sf(data = shp_vote, fill = "lightblue", color = "black")+
  geom_sf(data = nb_sf, color = "red") +
  theme_minimal() +
  ggtitle(paste("Carte des relations de voisinage avec le voisin le plus proche"))


# Créer la matrice de poids

PPV1 <- nb2listw(neighbors,style="W", zero.policy=TRUE)



    #----- 4.3.2. Réaliser l'opération avec k=3 -----

# Créer la liste de voisinage

k <- 3
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
summary(neighbors)


# Représenter les liens

nb_lines <- nb2lines(neighbors, coords = coords_sp)
nb_sf <- st_as_sf(nb_lines)
ggplot() +
  geom_sf(data = shp_vote, fill = "lightblue", color = "black")+
  geom_sf(data = nb_sf, color = "red") +
  theme_minimal() +
  ggtitle(paste("Carte des relations de voisinage avec les", k, "plus proches voisins"))


# Créer la matrice de poids

PPV3 <- nb2listw(neighbors,style="W", zero.policy=TRUE)





    #----- 5. ANALYSER L'AUTOCORRÉLATION SPATIALE -----

    #----- 5.1. Réaliser le test de Moran pour le taux d'abstention -----

## H0 : Il n'y a pas d'autocorrélation spatiale (p-value > 0.05)
## H1 : Il y a une autocorrélation spatiale (p-value < 0.05)

## Rappel : une autocorrélation spatiale positive signifie que les départements 
## ayant des taux d'abstention similaires sont regroupés spatialement, tandis 
## qu'une autocorrélation spatiale négative indiquerait que les départements 
## avec des taux d'abstention différents sont regroupés spatialement.


    #----- 5.1.1. Faire le test avec la matrice de poids de type Reine ----

# Utiliser la méthode analytique

moran.test(shp_vote$Abstentions_pct, W_reine, zero.policy=TRUE,randomisation=TRUE)

## Résultat : p-value < 2.2e-16 < 0.05 et Moran's I = 0.582064416
## => On rejette H0 et on conclut qu'il y a une autocorrélation spatiale 
## positive du taux d'abstention au second tour des élections législatives 
## anticipées de 2024.


# Utiliser la méthode de permutation (Monte Carlo)

set.seed(1234)
moran.mc(shp_vote$Abstentions_pct, W_reine, nsim = 999, zero.policy = TRUE)

## Résultat : p-value = 0.001 < 0.05
## => On rejette H0 et on conclut qu'il y a une autocorrélation spatiale.


# Conclure sur la base des 2 méthodes

## Les deux méthodes donnent des résultats allant dans le même sens : il existe
## une autocorrélation spatiale significative au seuil de 5% du taux
## d'abstention au second tour des élections législatives anticipées de 2024.
## De plus, Moran's I étant positif, on conclut que cette autocorélation est
## positive.


    #----- 5.1.2. Faire le test avec la matrice de poids de type k=1 ----

# Utiliser la méthode analytique

moran.test(shp_vote$Abstentions_pct, PPV1, zero.policy=TRUE,randomisation=TRUE)

## Résultat : p-value < 4.21e-06 < 0.05 et Moran's I = 0.55362996
## => On rejette H0 et on conclut qu'il y a une autocorrélation spatiale 
## positive du taux d'abstention au second tour des élections législatives 
## anticipées de 2024.


# Utiliser la méthode de permutation (Monte Carlo)

set.seed(1234)
moran.mc(shp_vote$Abstentions_pct, PPV1, nsim = 999, zero.policy = TRUE)

## Résultat : p-value = 0.001 < 0.05
## => On rejette H0 et on conclut qu'il y a une autocorrélation spatiale.


# Conclure sur la base des 2 méthodes

## Les deux méthodes donnent des résultats allant dans le même sens : il existe
## une autocorrélation spatiale significative au seuil de 5% du taux
## d'abstention au second tour des élections législatives anticipées de 2024.
## De plus, Moran's I étant positif, on conclut que cette autocorélation est
## positive.



    #----- 5.1.3. Faire le test avec la matrice de poids de type k=3 ----

# Utiliser la méthode analytique

moran.test(shp_vote$Abstentions_pct, PPV3, zero.policy=TRUE,randomisation=TRUE)

## Résultat : p-value < 2.524e-16 < 0.05 et Moran's I = 0.609453860
## => On rejette H0 et on conclut qu'il y a une autocorrélation spatiale 
## positive du taux d'abstention au second tour des élections législatives 
## anticipées de 2024.


# Utiliser la méthode de permutation (Monte Carlo)

set.seed(1234)
moran.mc(shp_vote$Abstentions_pct, PPV3, nsim = 999, zero.policy = TRUE)

## Résultat : p-value = 0.001 < 0.05
## => On rejette H0 et on conclut qu'il y a une autocorrélation spatiale.


# Conclure sur la base des 2 méthodes

## Les deux méthodes donnent des résultats allant dans le même sens : il existe
## une autocorrélation spatiale significative au seuil de 5% du taux
## d'abstention au second tour des élections législatives anticipées de 2024.
## De plus, Moran's I étant positif, on conclut que cette autocorélation est
## positive.



    #----- 5.1.4. Conclure sur la base des 3 matrice -----

## Les trois matrices de poids donnent des résultats allant dans le même sens :
## il existe une autocorrélation spatiale significative au seuil de 5% du taux
## d'abstention au second tour des élections législatives anticipées de 2024.
## De plus, Moran's I étant positif dans les trois cas, on conclut que cette
## autocorrélation est positive. Les coefficients sont très proches, les
## différences s'expliquant par les différences dans la définition des voisins.





    #----- 5.2. Réaliser le diagramme de Moran pour le taux d'abstention -----

## Rappel : Interprétation du diagramme de Moran
## Axe X : valeur centrée de la variable (x_i - moyenne)
## Axe Y : moyenne des voisins centrée (W x_i - moyenne)
## Quadrants :
## Haut droite  : High-High → cluster positif (valeurs élevées regroupées)
## Haut gauche : Low-High  → outlier spatial (valeur faible entourée de fortes)
## Bas gauche : Low-Low   → cluster positif (valeurs faibles regroupées)
## Bas droite  : High-Low  → outlier spatial (valeur élevée entourée de faibles)
## Pente de la droite de régression ≈ Moran's I : 
##   >0 : autocorrélation positive, <0 : autocorrélation négative


# Standardiser la variable d'intérêt

shp_vote$Abstentions_pct_std <- scale(shp_vote$Abstentions_pct)


    #----- 5.2.1. Faire le diagramme avec la matrice de poids de type Reine ----

moran.plot(as.vector(shp_vote$Abstentions_pct_std),
           W_reine,
           xlab="Taux d'abstention", 
           ylab="Lag Taux d'abstention",
           main="Matrice type Reine",
           labels=as.character(shp_vote$Libellé))



    #----- 5.2.2. Faire le diagramme avec la matrice de poids de type k=1 ----

moran.plot(as.vector(shp_vote$Abstentions_pct_std),
           PPV1,
           xlab="Taux d'abstention", 
           ylab="Lag Taux d'abstention",
           main="Matrice type PPV1",
           labels=as.character(shp_vote$Libellé))



    #----- 5.2.3. Faire le diagramme avec la matrice de poids de type k=3 ----

moran.plot(as.vector(shp_vote$Abstentions_pct_std),
           PPV3,
           xlab="Taux d'abstention", 
           ylab="Lag Taux d'abstention",
           main="Matrice type PPV3",
           labels=as.character(shp_vote$Libellé))