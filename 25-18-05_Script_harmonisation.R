#SCRIPT CHEQUES
#Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")

#Chargement de l'envrionnement de travail -----------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") {
  setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))
}


#Nov 22
Carnet_nov_22<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_nov_22.xlsx", sep="")))
FFQ_nov_22<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_22.xlsx", sep="")))
FFQ_nov_22 <- FFQ_nov_22[!duplicated(FFQ_nov_22$Identifiant), ]

#Mars23
Carnet_mars_23<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_mars_23.xlsx", sep="")))
FFQ_mars_23<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_mars_23.xlsx", sep="")))
FFQ_mars_23 <- FFQ_mars_23[!duplicated(FFQ_mars_23$Identifiant), ]

#Nov23
Carnet_nov_23<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_nov_23.xlsx", sep="")))
FFQ_nov_23<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_23.xlsx", sep="")))
FFQ_nov_23 <- FFQ_nov_23[!duplicated(FFQ_nov_23$Identifiant), ]

#Mars24
Carnet_mars_24<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_mars_24.xlsx", sep="")))
FFQ_mars_24<- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_mars_24.xlsx", sep="")))
FFQ_mars_24 <- FFQ_mars_24[!duplicated(FFQ_mars_24$Identifiant), ]

#Annexe
#Revenu_lime_survey <- read.xlsx((paste("Données analysées - Article N°1 chèques/Tableaux_annexes/Revenus.xlsx", sep="")))


#Fusion et Harmonisation des FFQ---------------------------------------------------
  #Fusion des tableaux de novembre
FFQ_NOV <- rbind(FFQ_nov_22, FFQ_nov_23)

##Imputation du diplome au tableau de mars ------------------------------
temp  <- FFQ_NOV[, c("Identifiant", "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.")]
FFQ_mars_24  <- left_join(FFQ_mars_24, temp, by="Identifiant")

names(FFQ_mars_24)[ncol(FFQ_mars_24)] <- "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu"
FFQ_mars_24 <- FFQ_mars_24[, -c(which(names(FFQ_mars_24) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu..x"))] #colonne en doublon vide et .n, aussi
FFQ_mars_23 <- FFQ_mars_23[, -c(which(names(FFQ_mars_23) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."))] #colonne en doublon vide et .n, aussi

##Fusion des tableaux de mars ----------------------------------
FFQ_MARS <- rbind(FFQ_mars_23, FFQ_mars_24)


#Fusion et harmonisation des carnets 

## on remplace d'abord les .x par _POIDS
#names(Carnet_nov_22) <- gsub("\\.x$", "_POIDS", names(Carnet_nov_22))
## puis les .y par _KCAL
#names(Carnet_nov_22) <- gsub("\\.y$", "_KCAL", names(Carnet_nov_22))
#
## on remplace d'abord les .x par _POIDS
#names(Carnet_nov_23) <- gsub("\\.x$", "_POIDS", names(Carnet_nov_23))
## puis les .y par _KCAL
#names(Carnet_nov_23) <- gsub("\\.y$", "_KCAL", names(Carnet_nov_23))
#
## on remplace d'abord les .x par _POIDS
#names(Carnet_mars_23) <- gsub("\\.x$", "_POIDS", names(Carnet_mars_23))
## puis les .y par _KCAL
#names(Carnet_mars_23) <- gsub("\\.y$", "_KCAL", names(Carnet_mars_23))
#
## on remplace d'abord les .x par _POIDS
#names(Carnet_mars_24) <- gsub("\\.x$", "_POIDS", names(Carnet_mars_24))
## puis les .y par _KCAL
#names(Carnet_mars_24) <- gsub("\\.y$", "_KCAL", names(Carnet_mars_24))

Carnet_NOV <- bind_rows(Carnet_nov_22, Carnet_nov_23)

Carnet_MARS <- bind_rows(Carnet_mars_23, Carnet_mars_24)


##Retire de MARS toutes les lignes qui n'apparaissent pas en NOV
    ###supprimer les id de carnet mars qui ne se retouvent pas dans les carnets de novembre------------
Carnet_MARS <- Carnet_MARS %>%
  semi_join(Carnet_NOV, by = "Identifiant")

#print(unique(Carnet_MARS$Identifiant))

    ###supprimer les id de carnet novembre qui ne se retouvent pas dans les carnets de mars-----------------
Carnet_NOV <- Carnet_NOV %>%
  semi_join(Carnet_MARS, by = "Identifiant")

FFQ_NOV$KCAL_TOTAL_FFQ_Kcal
# Filtres pour exclure les FFQ abérrants----------------------------
initial_ids <- FFQ_NOV$Identifiant
FFQ_NOV<- FFQ_NOV %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ_NOV <- FFQ_NOV %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)

initial_ids <- FFQ_MARS$Identifiant
FFQ_MARS<- FFQ_MARS %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ_MARS <- FFQ_MARS %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)

#supprimer les id de FFQ mars qui ne se retouvent pas dans les FFQ de novembre------------
  initial_ids <- FFQ_MARS$Identifiant
  
  FFQ_MARS <- FFQ_MARS %>%
  semi_join(FFQ_NOV, by = "Identifiant")
  removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)

###supprimer les id de Carnet novembre qui ne se retouvent pas dans les Carnet de mars-----------------
FFQ_NOV <- FFQ_NOV %>%
  semi_join(FFQ_MARS, by = "Identifiant")

print(unique(Carnet_NOV$Identifiant))

FFQ_NOV$borne_inf  <- NULL
FFQ_MARS$borne_inf <- NULL
FFQ_NOV$borne_sup  <- NULL
FFQ_MARS$borne_sup <- NULL


sgsdata_Carnets <- bind_rows(Carnet_NOV, Carnet_MARS)


# 1. Repérer les colonnes partagées
cols_communes <- intersect(names(FFQ_NOV), names(FFQ_MARS))

# 2. Identifier celles dont la classe diffère
diff_cols <- cols_communes[sapply(cols_communes, function(col) {
  !identical(class(FFQ_NOV[[col]]), class(FFQ_MARS[[col]]))
})]

# 3. Forcer uniquement ces colonnes en numeric (attention aux valeurs non numériques → NA)
FFQ_NOV <- FFQ_NOV %>%
  mutate(across(all_of(diff_cols), ~ as.character(.)))

FFQ_MARS <- FFQ_MARS %>%
  mutate(across(all_of(diff_cols), ~ as.character(.)))

# 4. On peut maintenant binder sans erreur
sgsdata_FFQ <- bind_rows(FFQ_NOV, FFQ_MARS)


#CORRECTION DES INTITULES DE COLONNE
#names(sgsdata_Carnets) <- sub("_POIDS$",  "_Poids",  names(sgsdata_Carnets))
#names(sgsdata_Carnets) <- sub("_KCAL$",  "_Kcal",  names(sgsdata_Carnets))
#names(sgsdata_FFQ) <- sub("_FFQ$",  "_FFQ_Poids",  names(sgsdata_FFQ))
#sgsdata_FFQ<- sgsdata_FFQ %>%
#  rename_with(
#    .fn   = ~ paste0(.x, "_FFQ_Kcal"),
#    .cols = 74:106
#  )
#



#sgsdata <- sgsdata %>%
#  # on se met en ordre par Identifiant si besoin pour remplir “down”/“up”
#  arrange(Identifiant) %>%
#  group_by(Identifiant) %>%
#  # on remplit les NA en descendant d’abord, puis remontant
#  fill(groupe, .direction = "downup") %>%
#  ungroup()

#Téléchargement des tableaux finaux -----------------------------------------
# Créer un nouvel objet workbook
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata_IT.xlsx")))



####HARMONISATION NUDGES ---------------------------------------------------------------------
#
##Importation des packages -------------------
#rm(list = ls())
#
##Chargement de l'envrionnement de travail -----------------
#researcher<-"adenieul" #"vbellassen" edumont
#if (researcher == "adenieul") {
#  setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
#} else {
#  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))
#}
#
#
##Nov 21
#Carnet_NOV <- read.xlsx((paste("Données analysées - Article N°4- Nudge/Fichiers_nettoyés/Fichiers_prétaités/Carnets_Tableaux_nov_21.xlsx", sep="")))
#FFQ_NOV  <- read.xlsx((paste("Données analysées - Article N°4- Nudge/Fichiers_nettoyés/Fichiers_prétaités/FFQ_Tableaux_nov_21.xlsx", sep="")))
#FFQ_NOV <- FFQ_NOV[!duplicated(FFQ_NOV$Identifiant), ]
#
##mars22
#Carnet_MARS <- read.xlsx((paste("Données analysées - Article N°4- Nudge/Fichiers_nettoyés/Fichiers_prétaités/Carnets_Tableaux_mars_22.xlsx", sep="")))
#FFQ_MARS <- read.xlsx((paste("Données analysées - Article N°4- Nudge/Fichiers_nettoyés/Fichiers_prétaités/FFQ_Tableaux_mars_22.xlsx", sep="")))
#FFQ_MARS <- FFQ_MARS[!duplicated(FFQ_MARS$Identifiant), ]
#
#
#
## on remplace d'abord les .x par _POIDS
#names(Carnet_NOV ) <- gsub("\\.x$", "_POIDS", names(Carnet_NOV))
## puis les .y par _KCAL
#names(Carnet_NOV ) <- gsub("\\.y$", "_KCAL", names(Carnet_NOV))
#
## on remplace d'abord les .x par _POIDS
#names(Carnet_MARS) <- gsub("\\.x$", "_POIDS", names(Carnet_MARS))
## puis les .y par _KCAL
#names(Carnet_MARS) <- gsub("\\.y$", "_KCAL", names(Carnet_MARS))
#
#
#
#
#
###Retire de MARS toutes les lignes qui n'apparaissent pas en NOV
####supprimer les id de carnet mars qui ne se retouvent pas dans les carnets de novembre------------
#initial_ids <- Carnet_MARS$Identifiant
#Carnet_MARS <- Carnet_MARS %>%
#  semi_join(Carnet_NOV, by = "Identifiant")
#removed_ids <- setdiff(initial_ids, Carnet_MARS$Identifiant)
#
##print(unique(Carnet_MARS$Identifiant))
#
####supprimer les id de carnet novembre qui ne se retouvent pas dans les carnets de mars-----------------
#initial_ids <- Carnet_NOV$Identifiant
#Carnet_NOV <- Carnet_NOV %>%
#  semi_join(Carnet_MARS, by = "Identifiant")
#removed_ids <- setdiff(initial_ids, Carnet_NOV$Identifiant)
#
#
## Filtres pour exclure les FFQ abérrants----------------------------
#initial_ids <- FFQ_NOV$Identifiant
#FFQ_NOV<- FFQ_NOV %>%
#  mutate(borne_inf = 500,
#         borne_sup =  4500)
#FFQ_NOV <- FFQ_NOV %>%
#  filter(SOMME_FFQ_KCAL >= borne_inf & SOMME_FFQ_KCAL <= borne_sup)
#removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)
#
#initial_ids <- FFQ_MARS$Identifiant
#FFQ_MARS<- FFQ_MARS %>%
#  mutate(borne_inf = 500,
#         borne_sup =  4500)
#FFQ_MARS <- FFQ_MARS %>%
#  filter(SOMME_FFQ_KCAL >= borne_inf & SOMME_FFQ_KCAL <= borne_sup)
#removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)
#
##supprimer les id de FFQ mars qui ne se retouvent pas dans les FFQ de novembre------------
#initial_ids <- FFQ_MARS$Identifiant
#
#FFQ_MARS <- FFQ_MARS %>%
#  semi_join(FFQ_NOV, by = "Identifiant")
#removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)
#
####supprimer les id de Carnet novembre qui ne se retouvent pas dans les Carnet de mars-----------------
#FFQ_NOV <- FFQ_NOV %>%
#  semi_join(FFQ_MARS, by = "Identifiant")
#
#print(unique(Carnet_NOV$Identifiant))
#
#FFQ_NOV$borne_inf  <- NULL
#FFQ_MARS$borne_inf <- NULL
#FFQ_NOV$borne_sup  <- NULL
#FFQ_MARS$borne_sup <- NULL
#
#
#sgsdata_Carnets <- bind_rows(Carnet_NOV, Carnet_MARS)
#
#
## 1. Repérer les colonnes partagées
#cols_communes <- intersect(names(FFQ_NOV), names(FFQ_MARS))
#
## 2. Identifier celles dont la classe diffère
#diff_cols <- cols_communes[sapply(cols_communes, function(col) {
#  !identical(class(FFQ_NOV[[col]]), class(FFQ_MARS[[col]]))
#})]
#
## 3. Forcer uniquement ces colonnes en numeric (attention aux valeurs non numériques → NA)
#FFQ_NOV <- FFQ_NOV %>%
#  mutate(across(all_of(diff_cols), ~ as.character(.)))
#
#FFQ_MARS <- FFQ_MARS %>%
#  mutate(across(all_of(diff_cols), ~ as.character(.)))
#
## 4. On peut maintenant binder sans erreur
#sgsdata_FFQ <- bind_rows(FFQ_NOV, FFQ_MARS)
#
#
##CORRECTION DES INTITULES DE COLONNE
#names(sgsdata_Carnets) <- sub("_POIDS$",  "_Poids",  names(sgsdata_Carnets))
#names(sgsdata_Carnets) <- sub("_KCAL$",  "_Kcal",  names(sgsdata_Carnets))
#names(sgsdata_FFQ) <- sub("_FFQ$",  "_FFQ_Poids",  names(sgsdata_FFQ))
#
#
#
#
#
#sgsdata <- bind_rows(sgsdata_Carnets, sgsdata_FFQ)
#
#sgsdata <- sgsdata %>%
#  # on se met en ordre par Identifiant si besoin pour remplir “down”/“up”
#  arrange(Identifiant) %>%
#  group_by(Identifiant) %>%
#  # on remplit les NA en descendant d’abord, puis remontant
#  fill(groupe, .direction = "downup") %>%
#  ungroup()
#
##Téléchargement des tableaux finaux -----------------------------------------
## Créer un nouvel objet workbook
#wb <- createWorkbook()
#
#addWorksheet(wb, "sgsdata")
#writeData(wb, sheet = "sgsdata", sgsdata  )
#
#saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata_nudges.xlsx")))
#
#
#
#
#

###HARMONISATION CSGA ------------------------------------------------------
#Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")

#Chargement de l'envrionnement de travail -----------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") {
  setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))
}

Carnets <- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_CSGA.xlsx", sep="")))
FFQ  <- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_CSGA.xlsx", sep="")))

Carnets <- Carnets %>%
  semi_join(FFQ, by = "Identifiant") %>%  # garde ceux qui sont aussi dans FFQ
  arrange(Identifiant)

FFQ <- FFQ %>%
  semi_join(Carnets, by = "Identifiant") %>%  # garde ceux qui sont aussi dans Carnets
  arrange(Identifiant)



initial_ids <- FFQ$Identifiant
FFQ<- FFQ %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ <- FFQ %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)


#names(Carnets) <- sub("_CARNET.x$",  "_CARNET_Poids",  names(Carnets))
#names(Carnets) <- sub("_CARNET.y$",  "_CARNET_Kcal",  names(Carnets))
#names(FFQ) <- sub("_FFQ$",  "_FFQ_Poids",  names(FFQ))
#FFQ <- FFQ %>%
#  rename_with(
#    .fn   = ~ paste0(.x, "_FFQ_Kcal"),
#    .cols = 35:66
#  )
#



sgsdata <- bind_rows(Carnets, FFQ)


#Téléchargement des tableaux finaux -----------------------------------------
# Créer un nouvel objet workbook
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata.xlsx")))

