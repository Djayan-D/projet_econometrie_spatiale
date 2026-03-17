    #----- 1. IMPORTER LES PACKAGES -----

library(readxl)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(mapview)
library(cartography)
library(dplyr)
library(spdep)
library(ggplot2)
library(car)
library(spatialreg)
    

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





    #----- 3. ANALYSER LES DONNÉES NON SPATIALES ----

    #----- 3.1. Réaliser le résumé statistique des variables -----

summary(shp_vote$Abstentions_pct)
summary(shp_vote$Part_25_59_ans)
summary(shp_vote$Taux_pauvrete_2021)
summary(shp_vote$Part_Femme)



    #----- 3.2. Visualiser les variables avec des boxplots -----

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



    #----- 3.4. Visualiser les variables avec des histogrammes -----

ggplot(shp_vote, aes(x = Abstentions_pct)) +
  geom_histogram(binwidth = 2, fill = "red", color = "black") +
  labs(title = "Histogramme du taux d'abstention au second tour des élections législatives anticipées de 2024",
       x = "Taux d'abstention (%)", 
       y = "Fréquence") +
  theme_minimal()

ggplot(shp_vote, aes(x = Part_25_59_ans)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Histogramme de la part des personnes âgées de 25 à 59 ans dans la population totale",
       x = "Part de 25-59 ans (%)", 
       y = "Fréquence") +
  theme_minimal()

ggplot(shp_vote, aes(x = Taux_pauvrete_2021)) +
  geom_histogram(binwidth = 2, fill = "grey", color = "black") +
  labs(title = "Histogramme du taux de pauvreté en 2021",
       x = "Taux de pauvreté (%)", 
       y = "Fréquence") +
  theme_minimal()

ggplot(shp_vote, aes(x = Part_Femme)) +
  geom_histogram(binwidth = 2, fill = "pink", color = "black") +
  labs(title = "Histogramme de la part des femmes dans la population totale en 2024",
       x = "Part de femmes (%)", 
       y = "Fréquence") +
  theme_minimal()



    #----- 3.4. Analyser les corrélations entre les variables -----

cor_matrix <- shp_vote |> 
  st_drop_geometry() |> 
  select(Abstentions_pct, Part_25_59_ans, Taux_pauvrete_2021, Part_Femme) |> 
  cor(method = "spearman", use = "complete.obs")

cor_matrix

## Abstentions_pct est modérément corrélé positivement avec :
## - Part_25_59_ans (r ≈ 0.38) → les zones avec plus de 25-59 ans tendent
##   à avoir un peu plus d'abstention.
## - Taux_pauvrete_2021 (r ≈ 0.31) → l'abstention est légèrement plus
##   élevée dans les zones plus pauvres.

## Part_25_59_ans est faiblement corrélé négativement avec :
## - Taux_pauvrete_2021 (r ≈ -0.19) → les zones avec plus d'actifs
##   ont tendance à être un peu moins pauvres.

## Part_Femme présente des corrélations faibles avec les autres variables :
## - corrélation légère avec le taux de pauvreté (r ≈ 0.30)
## - quasi aucune relation avec l'abstention ou la part des 25-59 ans.

## Conclusion : les relations entre variables existent mais restent
## globalement faibles à modérées (aucune corrélation forte > 0.7).



    #----- 3.5. Réaliser une première cartographie -----

# Définir une fonction pour réaliser les cartes choroplèthes

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



# Réaliser les cartes choroplèthes pour les différentes variables

plot_choro(
  data = shp_vote,
  var = "Abstentions_pct",
  title = "Taux d'abstention au second tour des élections législatives anticipées de 2024 (en %)",
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





    #----- 4. ANALYSER LES DONNÉES SPATIALES ----

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





    #----- 6. ESTIMER ET ANALYSER UN MODÈLE MCO -----

    #----- 6.1. Estimer un modèle de régression linéaire ordinaire (MCO) -----

fit1 <- lm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme, data=shp_vote)

summary(fit1)

## Residuals:
##   Min      1Q    Median    3Q     Max 
## -6.8635 -1.8785 -0.1656  2.0337  3.8325 
## 
## Coefficients:
##                    Estimate  Std. Error t value Pr(>|t|)    
## (Intercept)        13.76261   23.16296   0.594    0.554    
## Part_25_59_ans      0.52778    0.09796   5.388 5.67e-07 ***
## Taux_pauvrete_2021  0.45528    0.08063   5.646 1.89e-07 ***
## Part_Femme         -0.23066    0.45791  -0.504    0.616    
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 2.29 on 90 degrees of freedom
## Multiple R-squared:  0.3843,	Adjusted R-squared:  0.3638 
## F-statistic: 18.73 on 3 and 90 DF,  p-value: 1.595e-09


AIC(fit1)

## 428.4802



    #----- 6.2. Vérifier la multicolinéarité -----

vif(fit1)

## Part_25_59_ans  Taux_pauvrete_2021   Part_Femme 
## 1.022558           1.050571           1.053103

## Pas de problème de multicolinéarité (VIF < 5)



    #----- 6.3. Calculer l’indice de Moran sur les résidus de l’estimation -----

    #----- 6.3.1. Utiliser la matrice de poids de type Reine -----

moran.lm1 <- lm.morantest(fit1, W_reine, alternative="two.sided")
print(moran.lm1)

## Résultat : p-value < 2.2e-16 < 0.05 et Moran's I = 0.512257485
## => On rejette H0 et on conclut qu'il existe une autocorrélation spatiale 
## positive significative dans les résidus du modèle expliquant le taux 
## d'abstention au second tour des élections législatives anticipées de 2024.
## => Cela indique que le modèle MCO présente une dépendance spatiale non 
## prise en compte, ce qui suggère qu'un modèle économétrique spatial serait 
## plus approprié.



    #----- 6.3.2. Utiliser la matrice de poids de type k=1 -----

moran.lm2 <- lm.morantest(fit1, PPV1, alternative="two.sided")
print(moran.lm2)

## Résultat : p-value < 3.521e-05 < 0.05 et Moran's I = 0.50448548
## => On rejette H0 et on conclut qu'il existe une autocorrélation spatiale 
## positive significative dans les résidus du modèle expliquant le taux 
## d'abstention au second tour des élections législatives anticipées de 2024.
## => Cela indique que le modèle MCO présente une dépendance spatiale non 
## prise en compte, ce qui suggère qu'un modèle économétrique spatial serait 
## plus approprié.



    #----- 6.3.3. Utiliser la matrice de poids de type k=3 -----

moran.lm3 <- lm.morantest(fit1, PPV3, alternative="two.sided")
print(moran.lm3)

## Résultat : p-value < 7.276e-11 < 0.05 et Moran's I = 0.467424666
## => On rejette H0 et on conclut qu'il existe une autocorrélation spatiale 
## positive significative dans les résidus du modèle expliquant le taux 
## d'abstention au second tour des élections législatives anticipées de 2024.
## => Cela indique que le modèle MCO présente une dépendance spatiale non 
## prise en compte, ce qui suggère qu'un modèle économétrique spatial serait 
## plus approprié.





    #----- 7. TROUVER LE MODÈLE SPATIAL LE PLUS ADAPTÉ EN FONCTION DE LA MATRICE DE POIDS -----

    #----- 7.1. Réaliser le test de Lagrange -----

## Note : lm.RStests remplace lm.LMtests mais garde les mêmes inputs et outputs.

    #----- 7.1.1. Réaliser le test pour la matrice de type Reine -----

LM_reine <- lm.RStests(fit1, W_reine, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM_reine)

## Les tests de Lagrange Multiplier robustes montrent que la dépendance spatiale
## des résidus du modèle est significative (adjRSerr p = 0.0044), tandis que la 
## dépendance spatiale du lag n'est pas significative (adjRSlag p = 0.394). Les
## tests non robustes sont tout deux significatifs (LMerr p = 3.297e-14 et LMlag 
## p = 1.405e-12).


    #----- 7.1.2. Réaliser le test pour la matrice de type k=1 -----

LM_ppv1 <- lm.RStests(fit1, PPV1, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM_ppv1)

## Les tests de Lagrange Multiplier classiques (RSerr et RSlag) sont
## significatifs, ce qui indique la présence d’une dépendance spatiale
## potentielle dans le modèle. Cependant, les versions robustes des tests
## (adjRSerr et adjRSlag) ne sont pas significatives (p = 0.8459 et p = 0.1425).



    #----- 7.1.3. Réaliser le test pour la matrice de type k=3 -----

LM_ppv3 <- lm.RStests(fit1, PPV3, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM_ppv3)

## Les tests LM classiques (RSerr et RSlag) sont significatifs, indiquant
## la présence d’une dépendance spatiale dans le modèle MCO. Les versions
## robustes des tests montrent que seul le test du lag spatial est
## significatif (adjRSlag p = 0.0388), tandis que le test de l’erreur
## spatiale ne l’est pas (adjRSerr p = 0.0616).



    #----- 7.2. Utiliser la méthode ascendante (Le Gallo) -----

    #----- 7.2.1. Trouver le modèle pour la matrice de poids de type Reine -----

## LMerr -> significatif
## LMlag -> significatif
## RLMerr > significatif
## RLMlag -> non significatif

## D'après la méthode ascendante de Le Gallo, le modèle à privilégier lorsque 
## les 2 tests de Lagrange Multiplier classiques (LMerr, LMlag) sont 
## significatifs et que seul le test RLMerr est significatif, est le modèle SEM.

## Conclusion : le modèle SEM est le plus adapté pour la matrice de poids de 
## type Reine.



    #----- 7.2.2. Trouver le modèle pour la matrice de poids de type k=1 -----

## LMerr -> significatif
## LMlag -> significatif
## RLMerr > non significatif
## RLMlag -> non significatif

## D'après la méthode ascendante de Le Gallo, aucun modèle n'apparaît à 
## privilégier lorsque les 2 tests de Lagrange Multiplier classiques sont
## signficatifs mais qu'aucun des tests robustes ne l'est car cela implique 
## qu'il n'y a pas de preuve solide en faveur d'un processus de retard spatial 
## ou d'erreur spatiale exclusivement. Dans ce cas, il est recommandé de rester 
## sur le modèle MCO (ou en quittant la méthode le Gallo : changer la 
## spécification du modèle ou envisager d'autres types de modèles (SDM, SLX)).

## Conclusion : le modèle MCO est le plus adapté pour la matrice de poids de 
## type k=1.



    #----- 7.2.3. Trouver le modèle pour la matrice de poids de type k=3 -----

## LMerr -> significatif
## LMlag -> significatif
## RLMerr > non significatif (à 5%)
## RLMlag -> significatif

## D'après la méthode ascendante de Le Gallo, le modèle à privilégier lorsque 
## les 2 tests de Lagrange Multiplier classiques (LMerr, LMlag) sont 
## significatifs et que seul le test RLMlag est significatif, est le modèle SAR.

## Conclusion : le modèle SAR est le plus adapté pour la matrice de poids de 
## type k=3.



    #----- 7.3. Utiliser la méthode Elhorst -----

    #----- 7.3.1. Trouver le modèle pour la matrice de poids de type Reine -----

## Étant donné qu'au moins un des tests Lagrange Multiplier (LM) robuste est,
## signficiatif, il faut, d'après la méthode Elhorst, estimer un modèle SDM.

    #----- 7.3.1.1. Estimer un modèle SDM -----

sdm_reine <- lagsarlm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme, 
                      data=shp_vote,
                      W_reine,
                      type="mixed")
summary(sdm_reine)

## Le modèle SDM est valide. Le paramètre spatial ρ est positif et significatif 
## (p < 0.001), indiquant une dépendance spatiale. Le test LM des résidus n’est 
## pas significatif (p = 0.53), ce qui indique l'absence d’autocorrélation  
## spatiale résiduelle. L’AIC est plus faible que celui du modèle MCO (382.33 
## contre 428.48), ce qui confirme une meilleure spécification.
## => Prochaine étape : estimer un modèle SAR et le comparer au SDM.



    #----- 7.3.1.2. Estimer un modèle SAR et le comparer au SDM -----

# Estimer le modèle SAR

sar_reine <- lagsarlm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme, 
                      data=shp_vote,
                      W_reine)
summary(sar_reine)

## Le modèle SAR est valide. Le paramètre spatial ρ est positif et significatif 
## (p < 0.001), indiquant une dépendance spatiale. Le test LM des résidus n’est 
## pas significatif (p = 0.14), ce qui indique l’absence d’autocorrélation 
## spatiale résiduelle. L’AIC est plus faible que celui du modèle MCO (387.61 
## contre 428.48), ce qui confirme une meilleure spécification.


# Comparer SDM et SAR

## Rappel LR.Sarlm(mod1, mod2) :
## H0 : Le modèle mod2 est suffisant, les effets spatiaux des variables 
## explicatives ne sont pas nécessaires (p-value > 0.05)
## H1 : Le modèle mod1 est préférable, l’inclusion des effets spatiaux des 
## variables explicatives améliore significativement le modèle (p-value < 0.05)

TestSDM_SAR_reine <- LR.Sarlm(sdm_reine, sar_reine)
print(TestSDM_SAR_reine)

## Likelihood ratio for spatial linear models
##
## data:  
## Likelihood ratio = 11.28, df = 3, p-value = 0.0103
## sample estimates:
## Log likelihood of sdm_reine Log likelihood of sar_reine 
##                   -182.1644                   -187.8045

## Conclusion : le résultat du test est significatif (p = 0.0103 < 0.05) et
## indique que le modèle SDM est significativement meilleur que le modèle SAR.
## => Prochaine étape : estimer un modèle SEM et le comparer au SDM.



    #----- 7.3.1.3. Estimer un modèle SEM et le comparer au SDM -----

# Estimer le modèle SEM

sem_reine <- errorsarlm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme,
                        data=shp_vote,
                        W_reine)

summary(sem_reine)

## Le modèle SEM est valide. Le paramètre spatial λ est positif et significatif 
## (p < 0.001), indiquant une autocorrélation spatiale dans les résidus. Comme 
## il s’agit d’un modèle d’erreur spatiale, le test LM des résidus n’est pas 
## nécessaire pour la validité ici. L’AIC est plus faible que celui du modèle 
## MCO (380.58 contre 428.48), ce qui confirme une meilleure spécification et 
## que le modèle capture correctement la dépendance spatiale.


# Comparer SDM et SEM

TestSDM_SEM_reine <- LR.Sarlm(sdm_reine, sem_reine)
print(TestSDM_SEM_reine)

## Likelihood ratio for spatial linear models
##
## data:  
## Likelihood ratio = 4.2548, df = 3, p-value = 0.2352
## sample estimates:
## Log likelihood of sdm_reine Log likelihood of sem_reine
##                   -182.1644                   -184.2918

## Conclusion : le résultat du test est non significatif (p = 0.2352 > 0.05) et
## indique que le modèle SDM n'est pas significativement meilleur que le modèle
## SEM. Par conséquent, le modèle SEM est le modèle conservé dans le cas de la
## matrice de type Reine.



    #----- 7.3.2. Utiliser la matrice de poids de type k=1 -----

## Étant donné qu'aucun des tests Lagrange Multiplier (LM) robuste n'est
## signficiatif, il faut, d'après la méthode Elhorst, estimer un modèle SLX.

    #----- 7.3.2.1. Estimer un modèle SLX -----

slx_ppv1 <- lmSLX(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme, 
                  data=shp_vote,
                  PPV1)
summary(slx_ppv1)

AIC(slx_ppv1)

## Le modèle SLX montre que les variables retardées spatialement ne sont pas 
## significatives (p > 0.05 pour tous les lags), ce qui indique que θ = 0. 
## Autrement dit, il n’existe pas d’effet spatial détectable pour ces variables 
## et, dans ce cas, le modèle reste proche d’un MCO classique. L’AIC du SLX 
## (431.00) est légèrement supérieur à celui du MCO (428.48), confirmant que 
## l’inclusion des effets spatiaux n’apporte pas d’amélioration notable.

## Conclusion : étant donné que θ = 0, il n’est pas nécessaire d’estimer un SDM, 
## et le modèle MCO peut suffire pour cette spécification. Par conséquent, 
## le modèle MCO est le modèle conservé dans le cas de la matrice de type k=1.



    #----- 7.3.3. Utiliser la matrice de poids de type k=1 -----

## Étant donné qu'au moins un des tests Lagrange Multiplier (LM) robuste est,
## signficiatif, il faut, d'après la méthode Elhorst, estimer un modèle SDM.

    #----- 7.3.3.1. Estimer un modèle SDM -----

sdm_ppv3 <- lagsarlm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme, 
                     data=shp_vote,
                     PPV3,
                     type="mixed")
summary(sdm_ppv3)

## Le modèle SDM est valide. Le paramètre spatial ρ est positif et significatif 
## (p < 0.001), indiquant une dépendance spatiale. Le test LM des résidus n’est 
## pas significatif (p = 0.46), ce qui indique l'absence d’autocorrélation  
## spatiale résiduelle. L’AIC est plus faible que celui du modèle MCO (386.04 
## contre 428.48), ce qui confirme une meilleure spécification.
## => Prochaine étape : estimer un modèle SAR et le comparer au SDM.



    #----- 7.3.3.2. Estimer un modèle SAR et le comparer au SDM -----

# Estimer le modèle SAR

sar_ppv3 <- lagsarlm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme, 
                     data=shp_vote,
                     PPV3)
summary(sar_ppv3)

## Le modèle SAR est valide. Le paramètre spatial ρ est positif et significatif 
## (p < 0.001), indiquant une dépendance spatiale. Le test LM des résidus n’est 
## pas significatif (p = 0.51), ce qui indique l’absence d’autocorrélation 
## spatiale résiduelle. L’AIC est plus faible que celui du modèle MCO (390.84 
## contre 428.48), ce qui confirme une meilleure spécification.


# Comparer SDM et SAR

TestSDM_SAR_ppv3 <- LR.Sarlm(sdm_ppv3, sar_ppv3)
print(TestSDM_SAR_ppv3)

## Likelihood ratio for spatial linear models
##
## data:  
## Likelihood ratio = 10.807, df = 3, p-value = 0.01282
## sample estimates:
## Log likelihood of sdm_ppv3 Log likelihood of sar_ppv3 
##                  -184.0189                  -189.4223

## Conclusion : le résultat du test est significatif (p = 0.01282 < 0.05) et
## indique que le modèle SDM est significativement meilleur que le modèle SAR.
## => Prochaine étape : estimer un modèle SEM et le comparer au SDM.



    #----- 7.3.3.3. Estimer un modèle SEM et le comparer au SDM -----

# Estimer le modèle SEM

sem_ppv3 <- errorsarlm(Abstentions_pct ~ Part_25_59_ans+Taux_pauvrete_2021+Part_Femme,
                       data=shp_vote,
                       PPV3)

summary(sem_ppv3)

## Le modèle SEM est valide. Le paramètre spatial λ est positif et significatif 
## (p < 0.001), indiquant une autocorrélation spatiale dans les résidus. Comme 
## il s’agit d’un modèle d’erreur spatiale, le test LM des résidus n’est pas 
## nécessaire pour la validité ici. L’AIC est plus faible que celui du modèle 
## MCO (390.53 contre 428.48), ce qui confirme une meilleure spécification et 
## que le modèle capture correctement la dépendance spatiale.


# Comparer SDM et SEM

TestSDM_SEM_ppv3 <- LR.Sarlm(sdm_ppv3, sem_ppv3)
print(TestSDM_SEM_ppv3)

## Likelihood ratio for spatial linear models
##
## data:  
## Likelihood ratio = 10.49, df = 3, p-value = 0.01483
## sample estimates:
## Log likelihood of sdm_ppv3 Log likelihood of sem_ppv3
##                  -184.0189                  -189.2638

## Conclusion : le résultat du test est significatif (p = 0.01483 < 0.05) et
## indique que le modèle SDM est pas significativement meilleur que le modèle
## SEM. Par conséquent, le modèle SDM est le modèle conservé dans le cas de la
## matrice de type k=3.



    #----- 7.4 Comparer les modèles retenus pour les 3 matrices par les 2 méthodes -----

##            ┌-------┬------------┬------------┐
##            | Reine | k=1 (PPV1) | k=3 (PPV3) |
## ┌----------|-------┼------------┼------------┤
## | Le Gallo |  SEM  |   MCO      |    SAR     |
## ├----------┼-------┼------------┼------------┤
## | Elhorst  |  SEM  |   MCO      |    SDM     |
## └----------┴-------┴------------┴------------┘

## Au final, les modèles choisis sont très similaires entre les 2 méthodes. Il
## faut tout de fois noter que, par exemple, un modèle SDM n'aurait pas pu être
## sélectionné dans la méthode Le Gallo car, au même titre que le modèle SLX, il
## ne fait pas partie des modèles proposés.