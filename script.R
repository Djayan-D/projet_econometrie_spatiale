    #----- 1. IMPORTER LES PACKAGES -----

library(readxl)
library(sf)
    

    #----- 2. IMPORTER LES DONNÉES -----

# « Dans quelle mesure les disparités socio-démographiques se reflètent-elles 
# dans la distribution territoriale de l’abstention au second tour des 
# législatives de 2024 ? »

# Fichier Excel :
#       
# Code_INSEE : Numéro du département
# Libellé : libellé du département
# Abstentions_pct : Taux d'abstention au second tour des élections législatives anticipées de 2024 (en %)
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
    
data_raw <- read_excel("data/data_raw.xlsx")
data_space <- st_read("carte/DEPARTEMENT.shp") 
shp_join <- merge(data_space, data_raw, by.x = "INSEE_DEP", by.y = "Code_INSEE", all.x = TRUE)