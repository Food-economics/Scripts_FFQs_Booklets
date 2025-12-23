# CHARGEMENT DE L'ENVIRONNEMENT DE TRAVAIL  --------------
  ## Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(readxl);library(dplyr);library(broom);library(scales)
library(modelsummary);library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");
library("dplyr");library("tidyr");library("ggplot2");library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library(questionr)

  ## Importation des données ---------------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") {
  setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))
}


    ### Importation des données  ------------------
#donnees brutes
CSGA_base <- read_excel(
  path  = "Données analyses - Article N°2 FFQvsCarnets/Fichiers bruts/Données_CSGA.xlsx",
  sheet = "ffq_donnees-brutes"
)

CSGA_base <- CSGA_base %>% mutate_all(~gsub("\"","",.))
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\\(", "",.))
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\\)", "",.))

#CSGA TRANSFO
CSGA_FREQ <- read_excel(
  path  = "Données analyses - Article N°2 FFQvsCarnets/Fichiers bruts/Données_CSGA.xlsx",
  sheet = "ffq_donnees-transform-suvimax"
)

CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\"","",.))
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\\(", "",.))
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\\)", "",.))

#POIDS
quant_cols <- grep("Quantite", names(CSGA_FREQ), value = TRUE)

CSGA_POIDS <- setNames(
  data.frame(
    Code = CSGA_FREQ$Code,
    lapply(
      CSGA_FREQ[grep("Quantite", names(CSGA_FREQ))],
      function(x) as.numeric(as.character(x))
    )
  ),
  c("Code", grep("Quantite", names(CSGA_FREQ), value = TRUE))
)

CSGA_FREQ <- NULL

### Importation des données des tableaux annexes ----------------

CALNUT<- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/Alim_CALNUT_CODAPPRO_FFQ.xlsx")
Encodage <- read_xlsx(paste("Données analysées - Article N°1 chèques/Tableaux_annexes/Freq_FFQ.xlsx"))
Taille_Portion <- read_xlsx(paste("Données analysées - Article N°1 chèques/Tableaux_annexes/Taille portion.xlsx"))


#HARMONISER LES NOMS DE PORTIONS
#Traducation des fréquences de portions en équivalent TI
# Vectorisez les noms des colonnes à recoder
cols_to_recode <- c(
  "QTLEGUMES_1_3", "QTCRUDITES_1_3", "QTFRITES_1_3", "QTSTEAK_1_3",
  "QTVIANDE_1_3", "QTPOULET_1_3", "QTCHARCUT_1_3", "QTPOISSON_1_3",
  "QTPIZZA_1_3", "QTPATES_1_3", "QTFROM_1_3", "QTFROMRAP_1_3",
  "QTTARTE_1_3", "QTBAGUET_1_3", "QTPAINTR_1_3"
)

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ dplyr::recode(as.character(.),
                    `0` = "B", #Comme on va multiplier / des fréquences on peut imputer direct la valeur moyenne
                    `1` = "Plus petit que A",
                    `2` = "A",
                    `3` = "B",
                    `4` = "C",
                    `5` = "Plus grand que C",
                    .default = NA_character_)
  ))



#Traduction des fréquences de consommation des boissons
# Vectorisez les noms des colonnes à recoder
cols_to_recode <- c( "boi_1_3" , "boi_2_3",  "boi_3_3" , "boi_4_3", "boi_5_3" , "boi_6_3",
                     "alc_1_3" , "alc_2_3", "alc_3_3" , "alc_4_3")

library(dplyr)

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 0.066,
      . == 3 ~ 0.142,
      . == 4 ~ 0.498,
      . == 5 ~ 1,
      . == 6 ~ 3,
      . == 7 ~ 6,
      . == 8 ~ 9,
      TRUE    ~ NA_real_
    )
  ))




#Traduction des fréquences de consommation des boissons
# Vectorisez les noms des colonnes à recoder
cols_to_recode <- c( 
                     "lai_1_3" , "lait_2_3", "lai_3_3" , "lai_4_3","lai_5_3" , "lai_6_3")

library(dplyr)

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 0.066,
      . == 3 ~ 0.142,
      . == 4 ~ 0.498,
      . == 5 ~ 1,
      . == 6 ~ 3,
      . == 7 ~ 6,
      . == 8 ~ 9,
      TRUE    ~ NA_real_
    )
  ))



#Traduction des fréquences de consommation des boissons
# Vectorisez les noms des colonnes à recoder
cols_to_recode <- c( 
  "pai_1_3",  "pai_2_3",  "pai_3_3",  "pai_4_3", "cru_1_3",  "cru_2_3", "cru_3_3",  "cru_4_3",
  "leg_1_3",  "leg_2_3","leg_3_3",  "leg_4_3","leg_5_3",  "leg_6_3","leg_7_3",  "leg_8_3","leg_9_3",  "leg_10_3",
  "leg_11_3", "leg_12_3", "leg_13_3", "fec_1_3", "fec_2_3",  "fec_3_3", "fec_4_3",  "fec_5_3", "fec_6_3",  "fec_7_3",
  "fec_8_3",  "fec_9_3","fec_10_3","via_1_3",  "via_2_3","via_3_3",  "via_4_3","via_5_3",  "via_6_3",
  "aba_1_3",  "aba_2_3","aba_3_3",  "aba_4_3",  "aba_5_3",  "aba_6_3","aba_7_3",  "aba_8_3","aba_9_3",  "poi_2_3","poi_3_3",  "poi_4_3",
  "poi_5_3",  "poi_6_3","poi_7_3",  "poi_8_3","poi_9_3",  "pla_1_3","pla_2_3",  "pla_3_3","pla_4_3",  "pla_5_3",
  "pla_6_3",  "pla_8_3", "pla_9_3",  "pla_10_3", "pla_11_3",  "pla_12_3", "pla_13_3",  "fro_1_3", "fro_2_3",  "fro_3_3",
  "fro_4_3",  "fro_5_3","fro_6_3",  "fro_7_3","fro_8_3",  "fro_9_3","fro_10_3",  "fro_11_3","fru_1_3",  "fru_2_3",
  "fru_3_3",  "fru_4_3","fru_5_3",  "fru_6_3","fru_7_3",  "fru_8_3","fru_9_3",  "fru_10_3","fru_11_3",  "bis_1_3","bis_2_3",  "bis_3_3",
  "bis_4_3",  "bis_5_3","bis_6_3",  "bis_7_3","bis_8_3",  "bis_9_3","bis_10_3",  "bis_11_3","bis_12_3",  "bis_13_3","bis_14_3",  "sau_1_3",
  "sau_2_3",  "sau_3_3","sau_4_3",  "sau_5_3","sau_6_3",  "sau_7_3")

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ dplyr::recode(
      as.numeric(.),
      `1` = 0,
      `2` = 0.066,
      `3` = 0.142,
      `4` = 0.498,
      `5` = 1,
      `6` = 2,
      .default = NA_real_
    )
  ))



# CREATION D'UN TABLEAU UNIFORME POUR TOUTES LES CAMPAGNES  ------
data <- CSGA_base   #left_join(CSGA_base, CSGA_FREQ, by="Code")
data$Biscottes.consommees.hors.petit.dejeuner.portion <- data$QTPAINTR_1_3
#Je garde en mémoire les noms d'origine pour supprimer plus tard les colonnes que je n'utilise pas 
orig_names <- names(data)

# On extrait le nom de la colonne d'encodage 
column_name <- "CSGA_FREQ"  

# On récupère les vecteurs anciens vs. nouveaux noms
anciens  <- Encodage[[column_name]]
nouveaux <- Encodage$Aliment

# On boucle sur ces vecteurs pour renommer
for (i in seq_along(anciens)) {
  pattern <- anciens[i]
  new_nm  <- make.names(nouveaux[i])
  # lesquelles des colonnes actuelles contiennent ce pattern ?
  cols_to_rename <- grep(pattern, names(data), value = TRUE)
  # on remplace chacune
  if (length(cols_to_rename) > 0) {
    names(data)[ names(data) %in% cols_to_rename ] <- new_nm
  }
}

# identifie les colonnes dont le nom a changé
renamed_cols <- setdiff(names(data), orig_names)
#ne conserve que celles-ci
Frame <- data[, renamed_cols, drop = FALSE]


# Vérification
report <- tibble( ancien   = anciens, nouveau  = nouveaux) %>%
  mutate(
    # quelles colonnes initiales matchent le pattern ?
    matched = map(ancien, ~ grep(.x, orig_names, value = TRUE)),
    n_match = map_int(matched, length),
    # préparez un champ texte lisible
    matched = map_chr(matched, ~ paste(.x, collapse = ", ")),
    new_name = make.names(nouveau))


  ## Création d'un tableau qui regroupe toutes les variables des différentes campagnes ---------------------
colonne_chaligne <- Encodage[, 1]
Frame <- data.frame(t(colonne_chaligne))
new_column_names <- Frame[1,]
names(Frame) <- new_column_names
Frame <- Frame[-1, ]


  ## Imputation des valeurs de data au nouveau tableau Frame ------------
if (nrow(Frame) != nrow(data)) { Frame <- Frame[1:nrow(data), ]}
colonnes_communes <- intersect(names(data), names(Frame))

Frame[colonnes_communes] <- data[colonnes_communes]
Frame$Biscottes.consommees.hors.petit.dejeuner.portion


cols <- c(
  "THE.portion","CAFE.portion","EAU_BOUTEILLE.portion","EAU_ROB.portion",
  "JUS.portion","SIROP.portion","NON_LIGHT.portion","LIGHT.portion",
  "LAIT_ENT.portion","LAIT_DEMI.portion","LAIT_ECR.portion",
  "CACAO.portion","LAIT_VEGE.portion"
)

# Chargement + recodage
Frame <- Frame %>%
  mutate(across(
    all_of(cols),
    ~ dplyr::recode(as.character(.x),
                    `0` = NA_character_, 
                    `1` = "Type A",
                    `2` = "Type B",
                    `3` = "Type C",
                    `4` = "Type D",
                    `5` = "Type E",
                    `6` = "Type F",
                    .default = NA_character_)
  ))


#Suppresion des individus qui vivent chez leur parents
Frame <- Frame[rowSums(is.na(Frame)) < ncol(Frame), ]
#Suppresion des personnes qui vivent chez les parents ou en colocation
Frame <- subset(Frame, !lieuVie %in% c(1,2,4,5,9,10))
Frame <- subset(Frame, !Numero.d.identifiant %in% c("accakf","ajtblk","azajdj","aibbga"))


# CALCUL DES COEFF DE CORRECTION POUR CORRIGER LES FREQUENCES -----------------------------------------




  ## Fonction pour remplacer les NA par zéro ---------
replace_na_with_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
  ## Création d'une fonction permettant d'extraire les données numériques dans le tableau---------------
FREQ_intake <- function(data, x) {
  result <- as.numeric(data[[x]])
  result[is.na(result)] <- 0
  return(result)}

# ATTRIBUTION DES TAILLES DE PORTION -------------------------------
## Definition de la fonction pour remplacer les codes par les poids avec des messages de débogage -------------
#La fonction remplacer_poids prend deux arguments, taille et aliment, et effectue les étapes suivantes :
#Affichage d'un message : Affiche un message indiquant le traitement de l'aliment et de la taille fournis en arguments.
#Filtrage et extraction du poids :Utilise le data_frame Taille_Portion_long  et applique les étapes suivantes :
#Filtre les lignes où la colonne Aliment correspond à l'aliment donné et la colonne Taille correspond à la taille donnée.
#Extrait les valeurs de la colonne Poids des lignes filtrées.
#Gestion des cas sans correspondance :
#Vérifie si la longueur du vecteur poids est zéro (ce qui signifie qu'aucune correspondance n'a été trouvée).
#Si aucune correspondance n'est trouvée, affiche un message indiquant qu'il n'y a pas de correspondance pour l'aliment et la taille donnés, puis retourne NA.
#Si une correspondance est trouvée, la fonction retourne le poids extrait.
remplacer_poids <- function(taille, aliment) {message("Traitement de l'aliment: ", aliment, " et de la taille: ", taille)
  poids <- Taille_Portion_long %>%
    filter(Aliment == aliment, Taille == taille) %>%
    pull(Poids)
  if (length(poids) == 0) {message("Pas de correspondance trouvée pour ", aliment, " avec la taille ", taille)
    return(NA)}
  return(poids)}


##Constitution du tableau de poids des portions---------------
### Filtrage des colonnes de portions -------------------------------------
#Certains grouoes d'aliments disposent d'une taille de portion (légumes, crudités, poissons,
#steak, tartes sucrées, tartes salées...)
#On commence par isoler ces colonnes et on dimensionne un nouveau dataframe "data_duplicated"
#qui prend en compte uniquement ces valeurs
colonnes_portion <- grep("portion$", names(Frame), value = TRUE)
Frame <- Frame %>%
  rename(Identifiant = Numero.d.identifiant)

Frame_duplicated <- subset(Frame, select = c("Identifiant", colonnes_portion))

###Duplication des colonnes de portion autant de fois qu'un aliment appartient à une catégorie générale : légume, crudités... ---------
#La boucle for examine chaque catégorie unique dans la colonne Catégorie du data frame Taille_Portion. 
#Voici ce que fait cette boucle en détail :
#Itération sur les catégories : Pour chaque catégorie unique dans Taille_Portion$Catégorie :
#Vérification des valeurs NA : Si la catégorie est NA, elle passe à l'itération suivante sans exécuter le reste du code :
#Sélection des colonnes : Sélectionne les colonnes de Frame dont les noms commencent par le nom de la catégorie 
#Calcul du nombre de répétitions : Calcule combien de fois la catégorie apparaît dans Taille_Portion$Catégorie 
#Duplication des colonnes : Pour chaque occurrence de la catégorie, ajoute les colonnes renommées à Frame_duplicated :
for (categorie in unique(Taille_Portion$Catégorie)) {
  if (is.na(categorie)) next 
  # Trouver les colonnes dont le nom commence par la valeur de categorie
  column_names <- grep(paste0("^", categorie), names(Frame), value = TRUE)
  
  # Sélectionner les colonnes correspondantes
  columns <- Frame[, column_names, drop = FALSE]

  nb_repeats <- sum(Taille_Portion$Catégorie == categorie, na.rm = TRUE)
  for (i in 1:nb_repeats) {
    Frame_duplicated <- cbind(Frame_duplicated, columns)
  }
}


#Cette étape vise à harmoniser la constitution du dataframe 
#Chaque ligne correspond à un identifiant et l'objectif est que les catégories aient été 
#copiées autant de fois qu'il y a d'aliment spécifique.
# Initialisation de Poids avec les données de Frame_duplicated
# Afficher le DataFrame avec les colonnes dupliquées
Poids <- print(Frame_duplicated)

# Séparer les deux premières colonnes et les autres colonnes
debut <- Poids[, 1]
fin <- Poids[, -c(1)]

# Trier les colonnes restantes par ordre alphabétique
fin_trie <- fin[, order(names(fin))]

# Fusionner les deux parties
Poids <- cbind(debut, fin_trie)

# Identifier les colonnes se terminant par un chiffre
colonnes_a_garder <- grep("\\d$", names(Poids), value = TRUE)

# Sélectionner uniquement les colonnes se terminant par un chiffre
Poids <- Poids[, colonnes_a_garder]

# Identifier les noms de colonnes
colnames_data <- names(Poids)

# Compter les occurrences de chaque nom de colonne
occurrences <- table(colnames_data)


# Identifier les colonnes dupliquées sans se terminer par un chiffre
colonnes_a_supprimer <- names(occurrences[occurrences > 1])
colonnes_a_supprimer <- colonnes_a_supprimer[!grepl("\\d$", colonnes_a_supprimer)]
# Supprimer les colonnes dupliquées sans se terminer par un chiffre
data_filtre <- Poids[, !names(Poids) %in% colonnes_a_supprimer]
# Afficher le DataFrame filtré
print("DataFrame filtré:")
Poids <- print(data_filtre)
    ### Renommer les colonnes copiées avec le nom de l'aliment spécifique ----------
#Cette étape permet de faire renommer les portions copiées du tableau poids avec les aliments spécifiques de taille portion
new_column_names <- Taille_Portion$Aliment[match(names(Poids), Taille_Portion$Catégorie2)]
names(Poids) <- new_column_names

Taille_Portion$`Plus petit que A`<- as.character(Taille_Portion$`Plus petit que A`)
Taille_Portion$A <- as.character(Taille_Portion$`Plus petit que A`)
Taille_Portion$B <- as.character(Taille_Portion$B)
Taille_Portion$C <- as.character(Taille_Portion$C)
Taille_Portion$`Plus grand que C` <- as.character(Taille_Portion$`Plus grand que C`)
Taille_Portion$`Type A`<- as.character(Taille_Portion$`Type A`)
Taille_Portion$`Type B`<- as.character(Taille_Portion$`Type B`)
Taille_Portion$`Type C`<- as.character(Taille_Portion$`Type C`)
Taille_Portion$`Type D`<- as.character(Taille_Portion$`Type D`)
Taille_Portion$`Type E`<- as.character(Taille_Portion$`Type E`)
Taille_Portion$`Type F`<- as.character(Taille_Portion$`Type F`)
Taille_Portion$Poids_unitaire <- as.character(Taille_Portion$Poids_unitaire) 
### Application de la fonction pour remplacer les tailles de portions par les
# Transformation de la table Taille_Portion en format long
#La fonction pivot_longer convertit les colonnes spécifiées (toutes les colonnes sauf Aliment) en un format long. Cela signifie que les colonnes seront "empilées" dans deux nouvelles colonnes nommées Taille (pour les noms des anciennes colonnes) et Poids (pour les valeurs des anciennes colonnes).
#La fonction na.omit supprime les lignes qui contiennent des valeurs manquantes (NA).
#La fonction filter exclut les lignes où la colonne Taille est égale à "Catégorie" ou "Catégorie2".
Taille_Portion_long <- Taille_Portion %>%
  pivot_longer(cols = -Aliment, names_to = "Taille", values_to = "Poids") %>%
  na.omit() %>%
  filter(Taille != "Catégorie" & Taille != "Catégorie2")

    ### Remplacement des tailles par les poids en appliquant la fonction remplacer_poids---------------------
Poids_modifie <- Poids
for (col in names(Poids_modifie)) {
  Poids_modifie[[col]] <- sapply(Poids_modifie[[col]], function(taille) remplacer_poids(taille, col))
}

colnames(Poids_modifie)
    ###Cette étape vise à ajouter le poids unitaire des aliments dont la portion ne varie pas---------------
#Filtrage des données : subset(Taille_Portion_long, Taille == "Poids_unitaire") : sélectionne uniquement les lignes de Taille_Portion_long où la colonne Taille est égale à "Poids_unitaire". Le résultat est stocké dans filtered_df1.
#Boucle for pour mettre à jour Poids_modifie : Pour chaque ligne de filtered_df1, extrait les valeurs des colonnes Aliment et Poids.
#Crée un élément dans la liste Poids_modifie où le nom de l'aliment est la clé et la valeur du poids (valeur) est la valeur associée.

filtered_df1 <- subset(Taille_Portion_long, Taille == "Poids_unitaire")
for (i in 1:nrow(filtered_df1)) {
  aliment <- filtered_df1$Aliment[i]
  valeur <- filtered_df1$Poids[i]
  Poids_modifie[[aliment]] <- valeur
}

# Finalisation du tableau
Poids_modifie <- cbind(Frame$Identifiant, Poids_modifie)
names(Poids_modifie)[1] <- "Identifiant"
Poids_modifie[,-1] <- lapply(Poids_modifie[,-1], as.numeric)

#SUPPRESSION DES COLONNES DE BOISSONS
cols_to_remove <- c(
  "du.cacao.ou.chocolat.en.poudre",
  "de.cafe.y.compris.decafeine",
  "d.eau.du.robinet.verre",
  "d.eau.en.bouteille.ou.bonbonne.verre",
  "de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre",
  "de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre",
  "de.sirop.verre",
  "de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre",
  "de.lait.demi.ecreme",
  "de.lait.ecreme",
  "de.lait.entier.",
  "de.lait.vegetal.sojarizavoine.",
  "de.the"
)

Poids_modifie <- Poids_modifie %>%
  dplyr::select(-dplyr::any_of(cols_to_remove))

Poids_modifie[is.na(Poids_modifie)] <- 0

Poids_modifie <- Poids_modifie[Poids_modifie$Identifiant != 0, ]
Frame <- Frame[!is.na(Frame$Identifiant), ]

# CALCUL DU POIDS DES ALIMENTS CONSOMMES    -------------------------------
#Dans cette étape on calcule le poids des aliments en multipliant les fréquences par les tailles de portion pour tous les aliments
FFQ_POIDS <-data.frame(Frame$Identifiant)
names(FFQ_POIDS)[1] = "Identifiant"

FFQ_POIDS_Int <-data.frame(Frame$Identifiant)
names(FFQ_POIDS_Int )[1] = "Identifiant"

  ##ALCOOL_FFQ ---------------
FFQ_POIDS$ALCOOL_FFQ <- rep(0, nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")

# Initialiser un vecteur pour stocker les colonnes reconnues
recognized_columns <- list()

# Boucle sur les catégories pour vérifier quelles colonnes sont reconnues
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
  }
}

terms <- numeric(nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$ALCOOL_FFQ <- terms


CSGA_POIDS$ALCOOL_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cidrebiere + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.vin.blancroserouge +
  CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.alcools.forts + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.aperitifs)/1000

#  ##CAFE_THE_FFQ--------------
#FFQ_POIDS$CAFE_THE_FFQ <- rep(0,nrow(Frame))
#ncol1 <- grep("de.cafe.y.compris.decafeine", colnames(Frame))
#ncol2 <- grep("de.the",colnames(Frame))
#term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$CAFE.portion))  
#term2 <- replace_na_with_zero(FREQ_intake(Frame, ncol2)*replace_na_with_zero(Poids_modifie$THE.portion))
#FFQ_POIDS_Int$de.cafe.y.compris.decafeine <-term1 
#FFQ_POIDS_Int$de.the <- term2 
#FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2

#CSGA_POIDS$CAFE_THE_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.the + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cafe)/1000


  ##CEREALES_PD_FFQ---------------
FFQ_POIDS$CEREALES_PD_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli",colnames(Frame))
FFQ_POIDS$CEREALES_PD_FFQ  <- FREQ_intake(Frame,ncol1)*Poids_modifie$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli
FFQ_POIDS_Int$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli <- FFQ_POIDS$CEREALES_PD_FFQ

CSGA_POIDS$CEREALES_PD_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cereales.petit.dejeuner/1000


#CHARCUTERIE_HORS_JBc_FFQ
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- rep(0,nrow(Frame))
categories <- c("du.saucisson.sec.ou.salamiy.compris.a.l.aperitif", "du.cervelas.ou.de.la.mortadelle",
                "du.pate.ou.des.rillettes", "du.jambon.crubacon","du.jambon.blanc" , "des.saucisses.fraiches.ou.fumees.y.compris.merguez")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
  }
}

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- terms

CSGA_POIDS$CHARCUTERIE_HORS_JB_FFQ  <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.saucisson.sec +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cervelasmortadelle + 
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.paterillettes + 
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.saucisses.fraichesfumees +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.jambon)/1000
  


  ##DESSERTS_LACTES_FFQ----------------
FFQ_POIDS$DESSERTS_LACTES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.glace", "des.entremets.cremes.desserts.de.type.Danetteliegeoismoussesflans.",
"des.entremets.au.soja.ou.yaourts.au.soja.ou.autres.yaourts.aux.laits.vegetaux")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$DESSERTS_LACTES_FFQ <- terms

CSGA_POIDS$DESSERTS_LACTES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.glace +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.entremets +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.entremets.au.sojayaourts.soja )/1000
  


  ##EAU_FFQ--------------
FFQ_POIDS$EAU_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("d.eau.en.bouteille.ou.bonbonne.verre", colnames(Frame))
#ncol2 <- grep("d.eau.du.robinet.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$EAU_BOUTEILLE.portion) 
#term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$EAU_ROB.portion)
FFQ_POIDS$EAU_FFQ <- term1 # + term2
FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1 
#FFQ_POIDS_Int$d.eau.du.robinet.verre <- term2 

#CSGA_POIDS$EAU_FFQ <- ( CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.eau.en.bouteillebonbonne)/1000
  
##FEC_NON_RAF_FFQ----------------------
FFQ_POIDS$FEC_NON_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("du.painspeciaux.hors.petit.dejeuner.","du.pain.complet.et.autres.pains.speciaux.au.petit.dejeuner",
                "du.mais.ou.de.la.polenta","des.pommes.de.terre.a.l.eau.ou.au.four","des.pommes.de.terre.rissolees.ou.sautees",
                "de.la.puree.de.pomme.de.terre","d.autres.feculents.quinoamaniocbanane.plantainigname.",
                "des.pates.completes.ou.semi.completes","du.riz.complet.ou.semi.complet","du.mais1")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}

cols_entierement_na <- names(which(sapply(unlist(lapply(categories, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))


FFQ_POIDS$FEC_NON_RAF_FFQ <- terms
CSGA_POIDS$FEC_NON_RAF_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pain.completpains.speciaux +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mais + 
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommes.de.terre.a.l.eau +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommes.de.terre.rissolees + 
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.puree.de.pommes.de.terre +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.feculents)/1000


  ##FEC_RAF_FFQ---------------------
FFQ_POIDS$FEC_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.semouledu.ble.tabouleen.accompagnement.autre.que.dans.un.couscousEbly",
                "du.riz.blanc", "des.pates.macaronisspaghettiscoquillettes", "du.pain.blancde.mie.hors.petit.dejeuner.",
                "des.biscottesdes.craquottesdes.pains.grilles.au.petit.dejeuner","des.biscottesdes.craquottesdes.pains.grilles.type.suedois.hors.petit.dejeuner",
                "du.pain.blanc.au.petit.dejeuner")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FEC_RAF_FFQ <- terms


CSGA_POIDS$FEC_RAF_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.semouleble +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.riz + 
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pates +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pain +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.biscottescracottepains.grilles )/1000

  ##FROMAGES_FFQ----------------------------
FFQ_POIDS$FROMAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.en.morceaux","du.Roquefortdu.Bleu.quelle.qu.en.soit.l.origine",
                "du.fromage.de.chevre","autres.types.de.fromages.camembertbrie.","de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.rape.sur.les.plats.patesriz.",
                "du.fromage.a.pate.molle.camembertcoulommiersbrie.","du.fromage.a.tartiner.cancoillotteSaint.MoretVache.qui.rit.","de.la.mozzarella")
terms <- numeric(nrow(Frame))
non_reconnues <- c()

for (cat in categories) {
  ncol <- grep(cat, colnames(Frame))
  
  if (length(ncol) == 0) {
    # Ajouter à la liste des catégories non reconnues
    non_reconnues <- c(non_reconnues, cat)
  } else {
    # Appliquer la formule si la colonne est trouvée
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[cat]])
    terms <- terms + term
    FFQ_POIDS_Int[[cat]] <- term
  }
}

FFQ_POIDS$FROMAGES_FFQ <- terms
  
CSGA_POIDS$FROMAGES_FFQ <-  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.emmentalgruyerecomtebeaufort.en.morceaux +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.emmentalgruyerecomtebeaufort.rape +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.roquefortbleu +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fromage.de.chevre +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.types.de.fromages)/1000


  ##FRUITS_FFQ -----------------------
FFQ_POIDS$FRUITS_FFQ <- rep(0,nrow(Frame))
categories <- c("des.compotes","des.fruits.en.sirop","des.abricotspechesprunescerises","des.fraisesframboises",
                "du.raisin","du.melonde.la.pasteque","des.bananes","des.kiwis","des.agrumes.orangesmandarinespamplemoussescitrons.",
                "des.pommesdes.poires","des.fruits.exotiques.ananasmangueslitcheesgoyaves.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FRUITS_FFQ <- terms
cols_entierement_na <- names(which(sapply(unlist(lapply(categories, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))
cols_entierement_na <- names(which(sapply(unlist(lapply(Poids_modifie, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))

CSGA_POIDS$FRUITS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.abricotpecheprunecerise +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fraiseframboise + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.raisin +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.melonpasteque + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.banane +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.kiwi + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.agrumes +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommepoire + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.exotiques )/1000

  ##FRUITS_JUS_FFQ---------------
FFQ_POIDS$FRUITS_JUS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre", colnames(Frame))
FFQ_POIDS$FRUITS_JUS_FFQ<-  replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$JUS.portion)) 
FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ

CSGA_POIDS$FRUITS_JUS_FFQ <- CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.jus.d.orangepamplemousseananaspommeraisin / 1000

  ##FRUITS_SECS_FFQ ------------------
FFQ_POIDS$FRUITS_SECS_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.seches.abricotsdattesfiguespruneaux.",colnames(Frame))
FFQ_POIDS$FRUITS_SECS_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.seches.abricotsdattesfiguespruneaux.)
FFQ_POIDS_Int$des.fruits.seches.abricotsdattesfiguespruneaux. <- FFQ_POIDS$FRUITS_SECS_FFQ
  

CSGA_POIDS$FRUITS_SECS_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.seches / 1000

Frame$de.lait.demi.ecreme
  ##LAIT_FFQ---------------------
FFQ_POIDS$LAIT_FFQ <- rep(0,nrow(Frame))
  ncol1 <- grep("de.lait.entier.", colnames(Frame))
  ncol2 <- grep("de.lait.demi.ecreme",colnames(Frame))
  ncol3 <- grep("de.lait.ecreme",colnames(Frame))
  #ncol4 <- grep("du.cacao.ou.chocolat.en.poudre",colnames(Frame))
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) *  Poids_modifie$LAIT_ENT.portion )
  term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$LAIT_DEMI.portion  )
  term3 <-replace_na_with_zero(FREQ_intake(Frame,ncol3)*Poids_modifie$LAIT_ECR.portion ) 
  #term4 <-replace_na_with_zero(FREQ_intake(Frame,ncol4)*Poids_modifie$CACAO.portion)
  FFQ_POIDS$LAIT_FFQ <- term1 + term2 + term3 # + term4 
   FFQ_POIDS_Int$de.lait.entier. <- term1
   FFQ_POIDS_Int$de.lait.demi.ecreme <- term2
   FFQ_POIDS_Int$de.lait.ecreme <- term3
   #FFQ_POIDS_Int$du.cacao.ou.chocolat.en.poudre <- term4

CSGA_POIDS$LAIT_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.demi.ecreme + 
  CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.ecreme +
  CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.entier ) / 1000

  ##LAITAGES_FFQ------------------
FFQ_POIDS$LAITAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("du.fromage.blanc.ou.des.yaourts.a.0.de.matieres.grasses.natureaux.fruits.","du.fromage.blancdes.petits.suisses.ou.des.yaourts.a.2030.ou.40.de.matieres.grasses",
                "du.fromage.blanc.a.0.de.matieres.grasses.natureaux.fruits.","du.fromage.blanc.a.2030.ou.40.de.matieres.grasses.natureaux.fruits.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LAITAGES_FFQ <- terms

CSGA_POIDS$LAITAGES_FFQ <- (CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.fromage.blancpetits.suissesyaourts.20%30%40%MG`+ CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.fromage.blancyaourt.0%MG`)/1000
  ##LEG_SECS_FFQ-------------------
FFQ_POIDS$LEG_SECS_FFQ <- rep(0,nrow(Frame))
categories <- c("des.legumes.secs.lentillesharicots.secspois.chichesfeves.","Lentilles",
                "des.tartinables.a.base.de.legumes.secs.houmous","des.falafels","du.tofudes.steaks.vegetaux.et.autres.similis.carnes")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LEG_SECS_FFQ <- terms
CSGA_POIDS$LEG_SECS_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.legumes.secs/1000


  ##LEGUMES_FFQ-------------------
FFQ_POIDS$LEGUMES_FFQ <- rep(0,nrow(Frame))
categories <- c("des.haricots.verts",
                "des.endivesdes.epinardsdu.cresson",
                "des.poireaux",
                "du.chou.vertchou.fleurBruxellesbrocolis",
                "des.carottes.cuites",
                "des.courgettesdes.auberginesdes.poivronsdes.tomates.cuites.ratatouille.",
                "des.petits.pois",
                "des.artichautsdu.fenouildes.aspergesdu.celeri",
                "des.champignons",
                "du.potirondes.patates.douces",
                "de.la.soupe.de.legumes",
               "de.la.salade.vertede.la.machede.la.roquettedes.epinardsdu.cresson",
                "des.carottes.rapees",
               "de.l.avocat.au.moins.un.demi.avocat",
                "d.autres.crudites")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LEGUMES_FFQ <- terms
Poids_modifie$d.autres.crudites.tomatesbetteraveschouconcombreradis.


CSGA_POIDS$LEGUMES_FFQ  <- 
  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.endivesepinardcresson +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.haricots.verts+
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poireaux +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chou +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.carottes.cuites +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.courgetteauberginespoivronstomates.cuites +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.petits.pois + 
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.artichautfenouilaspergeceleri +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.champignon +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.potironpatate.douce +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.salade.vertemacheroquetteepinardcresson +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.carottes.rapees +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.avocat +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.crudites +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.soupe.de.legumes )/1000

##MGA_FFQ -----------------------
FFQ_POIDS$MGA_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates.",colnames(Frame))
ncol2 <- grep("de.la.creme.fraiche",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. )
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.la.creme.fraiche)
FFQ_POIDS$MGA_FFQ <- term1 + term2 
FFQ_POIDS_Int$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. <-term1 
FFQ_POIDS_Int$de.la.creme.fraiche <-term2 


CSGA_POIDS$MGA_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.creme.fraiche + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.beurre)/1000

  ##MGV_FFQ------------------
FFQ_POIDS$MGV_FFQ <- rep(0,nrow(Frame))
CSGA_POIDS$MGV_FFQ <- rep(0,nrow(CSGA_POIDS))


  ##NOIX_FFQ----------------
FFQ_POIDS$NOIX_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.a.coque.noixnoisettesamandes.",colnames(Frame))
FFQ_POIDS$NOIX_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)

CSGA_POIDS$NOIX_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.a.coque)/1000
FFQ_POIDS_Int$des.fruits.a.coque.noixnoisettesamandes. <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.) 

  ##OEUFS_FFQ-----------------
FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.oeufspochesdurs.ou.a.la.coque.2",colnames(Frame))
ncol2 <- grep("des.oeufssur.le.plat.en.omelette1",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*0.08333 ) # Poids_modifie$des.oeufspochesdurs.ou.a.la.coque.1)
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$des.oeufssur.le.plat.en.omelette1)
FFQ_POIDS$OEUFS_FFQ <- term1 + term2 

FFQ_POIDS_Int$des.oeufspochesdurs.ou.a.la.coque.2 <-term1 
FFQ_POIDS_Int$des.oeufssur.le.plat.en.omelette1 <-term2 


CSGA_POIDS$OEUFS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.oeufs.pochesdursa.la.coque +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.oeufs.sur.le.platomelette)/1000

##PDTS_SUCRES_FFQ ---------------------
FFQ_POIDS$PDTS_SUCRES_FFQ <- rep(0,nrow(Frame))
  categories <- c("de.la.tarte.aux.fruitsau.flan.","de.la.briochedu.cakedu.quatre.quarts",
    "des.biscuitspur.beurresecsa.la.confiturefourresau.chocolat.","Des.gateaux.patissiers.tout.faits.browniecrepepain.d.epice.","des.gateux.patissiers.au.chocolata.la.creme.",
    "de.la.pate.a.tartiner.au.chocolat.type.Nutella","des.barres.chocolatees.MarsBounty.","des.barres.de.cereales.Granny.","des.bonbons","des.viennoiseries.croissantspains.au.chocolat.",
    "du.chocolat.noirau.laitaux.noisettes.","du.mielde.la.confitureou.marmelade","du.sorbet")
  terms <- numeric(nrow(Frame))
  for (i in seq_along(categories)) {
    ncol <- grep(categories[i], colnames(Frame))
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
    terms <- terms + term
    FFQ_POIDS_Int[[categories[i]]] <- term
  }
  FFQ_POIDS$PDTS_SUCRES_FFQ <- terms

CSGA_POIDS$PDTS_SUCRES_FFQ <-  
  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.tarte.sucree + 
  CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.briochecakequatre-quart`+ 
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.biscuits +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.gateaux.patissiers +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pate.a.tartiner.au.chocolat +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.barres.chocolatees +
  CSGA_POIDS$ Quantite.g.journaliere.de.consommation.de.barres.de.cereales +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.bonbons +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viennoiseries +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chocolat +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mielconfituremarmelade +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sorbet)/1000
  
  ##PLAT_PREP_FFQ------------------
FFQ_POIDS$PLATS_PREP_CARNES_FFQ  <- rep(0,nrow(Frame))
categories <- c("des.raviolislasagnespates.fourrees","du.cassoulet","du.couscous","des.salades.composees.toutes.faites.avec.feculents.et.viande",
                "de.la.paella","de.la.choucroute.avec.de.la.charcuterie", "du.chili.con.carne", "des.plats.cuisines.alleges", "des.plats.cuisines.a.base.de.poisson")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PLATS_PREP_CARNES_FFQ <- terms

CSGA_POIDS$PLATS_PREP_CARNES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.raviolislasagnespates.fourrees +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cassoulet +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.couscous +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.paella  +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.choucroute +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chili.con.carne + 
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.plats.cuisines.alleges+
    + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.plats.cuisines.a.base.de.poisson)/1000

##POISSONS_FFQ--------------------
FFQ_POIDS$POISSONS_FFQ  <- rep(0,nrow(Frame))
categories <- c("du.poisson.cabillaudlieumerlansoletruite.frais.ou.congele.sauf.poisson.pane","du.poisson.a.l.huile.thonsardines.",
                "du.poisson.fume.saumontruite","du.poisson.sale.ou.en.saumure.morueharenganchoissprats","du.poisson.pane.cabillaudcolin",
                 "des.coquillages.mouleshuitrescoquilles.st.Jacques","des.crustaces.crevettescrabe")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$POISSONS_FFQ <- terms

CSGA_POIDS$POISSONS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.frais.ou.congele +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.a.l.huile + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.pane +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.saleen.saumure +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.coquillages)/1000

  ##PORC_FFQ-----------------
FFQ_POIDS$PORC_FFQ<- rep(0,nrow(Frame))
ncol1 <- grep("de.la.viande.de.porc.sauf.charcuterie",colnames(Frame))
FFQ_POIDS$PORC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.viande.de.porc.sauf.charcuterie)
FFQ_POIDS_Int$de.la.viande.de.porc.sauf.charcuterie <- FFQ_POIDS$PORC_FFQ

CSGA_POIDS$PORC_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.porc.sf.charcuterie/1000

##POULET_FFQ------------------
FFQ_POIDS$POULET_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.la.volaille.pouletdinde.du.lapin",colnames(Frame))
FFQ_POIDS$POULET_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.volaille.pouletdinde.du.lapin)
FFQ_POIDS_Int$de.la.volaille.pouletdinde.du.lapin <- FFQ_POIDS$POULET_FFQ

CSGA_POIDS$POULET_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.volaillelapin/1000

##QUICHES_PIZZAS_TARTES_SALLEES-----------------
FFQ_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ  <- rep(0,nrow(Frame))
categories <- c("de.la.pizza1","de.la.pizza.sans.viande","des.tartes.salees.quiche.1","des.tartes.salees.quichesans.viande")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ <- terms  


CSGA_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pizza + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.tartes.salees)/1000

##SAUCES_FFQ-----------------
FFQ_POIDS$SAUCES_FFQ  <- rep(0,nrow(Frame))
categories <- c("de.la.mayonnaise","de.la.sauce.vinaigrette.avec.crudites.","de.la.sauce.soja","de.la.sauce.de.type.ketchuptomatebarbecue.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$SAUCES_FFQ <- terms
CSGA_POIDS$SAUCES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mayonnaise +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sauce.vinaigrette +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sauce.soja +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.ketchup)/1000

##SNACKS_AUTRES_FFQ---------------
FFQ_POIDS$SNACKS_AUTRES_FFQ  <- rep(0,nrow(Frame))
categories <- c("des.cacahuetes","des.gateaux.aperitifs.sales","des.olives","des.chips.au.repasa.l.aperitif.",
                "des.friands.ou.croque.monsieur1","des.friands.ou.croque.monsieursans.viande","des.sandwichs1",
                "des.sandwichs.sans.viande.y.compris.tacospanini","des.hamburgers1","des.hamburgers.sans.viande","des.frites")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$SNACKS_AUTRES_FFQ <- terms
CSGA_POIDS$SNACKS_AUTRES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cacahuetes +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.gateaux.aperitifs.sales +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chips +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.friands +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sandwichs +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.hamburgers)/1000

  ##SODAS_LIGHT_FFQ-------------------
FFQ_POIDS$SODAS_LIGHT_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.lait.vegetal.sojarizavoine.", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$LAIT_VEGE.portion) 
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$LIGHT.portion)
FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2

CSGA_POIDS$SODAS_LIGHT_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.boisson.au.soja + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.colalimonadesoda.light)/1000

  ##SODAS_SUCRES_FFQ------------------
FFQ_POIDS$SODAS_SUCRES_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.sirop.verre", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre", colnames(Frame))
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$SIROP.portion) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$NON_LIGHT.portion)
  FFQ_POIDS_Int$de.sirop.verre <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2

FFQ_POIDS$SODAS_SUCRES_FFQ <- term1 + term2
CSGA_POIDS$SODAS_SUCRES_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.siropeau.aromatisee + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.colalimonadesoda.non.light)/1000

  ##VIANDE_ROUGE_FFQ--------------
FFQ_POIDS$VIANDE_ROUGE_FFQ  <- rep(0,nrow(Frame))
categories <- c("de.la.viande.de.boeuf.sauf.steak.hache","des.steaks.haches","de.la.viande.de.veau",
                "de.la.viande.d.agneaude.mouton","des.andouillettesdu.boudin.et.autres.abats",
                "de.la.langue.de.boeufdes.tripesdu.boudindes.andouillettesdes.ris.de.veaudes.rognons",
                "du.foie.de.genisse.volaille","du.foie.genissevolaillesautres.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$VIANDE_ROUGE_FFQ <- terms

CSGA_POIDS$VIANDE_ROUGE_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.boeuf +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.steaks.haches + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.veau +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.d.agneaumouton + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.languetripesboudinandouilletteris.de.veaurognons +
  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.foie)/1000


#jE VIRE epices / CAFE_THE/ MGV
CSGA_POIDS$EPICES_CONDIMENTS_FFQ <- NULL
CSGA_POIDS$CAFE_THE_FFQ <- NULL
CSGA_POIDS$MGV_FFQ <- NULL
CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cafe<- NULL
CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.the<- NULL
CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.margarine <- NULL
FFQ_POIDS$CAFE_THE_FFQ <- NULL


#Somme - TRAITEMENT TI
FFQ_POIDS$SOMME_FFQ_POIDS<- rowSums(FFQ_POIDS[2:31])
FFQ_POIDS$SOMME_FFQ_HORS_BOISSON <- replace_na_with_zero(FFQ_POIDS$SOMME_FFQ_POIDS- 
                                                           FFQ_POIDS$ALCOOL_FFQ - 
                                                           FFQ_POIDS$FRUITS_JUS_FFQ - 
                                                           FFQ_POIDS$LAIT_FFQ -
                                                           FFQ_POIDS$EAU_FFQ -
                                                           FFQ_POIDS$SODAS_LIGHT_FFQ -
                                                           FFQ_POIDS$SODAS_SUCRES_FFQ)

#Somme - TRAITEMENT CSGA
#CSGA_POIDS$SOMME_FFQ_POIDS<- rowSums(CSGA_POIDS[124:151])
#
#CSGA_POIDS$SOMME_FFQ_HORS_BOISSON <- replace_na_with_zero(CSGA_POIDS$SOMME_FFQ_POIDS- 
#                                                            CSGA_POIDS$ALCOOL_FFQ - 
#                                                            CSGA_POIDS$FRUITS_JUS_FFQ - 
#                                                            CSGA_POIDS$LAIT_FFQ -
#                                                            CSGA_POIDS$EAU_FFQ -
#                                                            CSGA_POIDS$SODAS_LIGHT_FFQ -
#                                                            CSGA_POIDS$SODAS_SUCRES_FFQ)


#VF_CSGA_POIDS
#CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.eau.du.robinet <- NULL
#CSGA_POIDS_VF <- CSGA_POIDS[, -c(2:122)]
#CSGA_POIDS_VF <- CSGA_POIDS_VF %>%
#  rename(Identifiant = Code)

#CALCUL DE  KCAL -----------------------------------
#CSGA_POIDS_int <- CSGA_POIDS[, -c(123:ncol)]
#CSGA_POIDS_int  <- CSGA_POIDS_int  %>%
#  rename(Identifiant = Code)

#CSGA_POIDS_int <- CSGA_POIDS_int %>%
#  mutate(across(where(is.numeric), ~ .x / 1000))


#df_long <- CSGA_POIDS_int %>%
#  pivot_longer(cols = -Identifiant, names_to = "FFQ_CSGA", values_to = "Poids") 

df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

#df_long <- inner_join(df_long, CALNUT, by= "FFQ_CSGA", relationship = "many-to-many")
df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")

#CALCULER LES KILOCALORIes PAR ALIMENT TI 
df_long$nrj_kcal_alim <- df_long$nrj_kcal*df_long$Poids*10
FFQ_KCAL <- aggregate(nrj_kcal_alim ~  Identifiant + groupe_TI_TdC   , df_long, FUN = sum)
FFQ_KCAL<- pivot_wider(
  FFQ_KCAL,
  id_cols = Identifiant,
  names_from = groupe_TI_TdC,
  values_from = nrj_kcal_alim
)


# Calculer la somme des colonnes  pour chaque ligne
FFQ_KCAL$SOMME_FFQ_KCAL <- rowSums(FFQ_KCAL[, 2:ncol(FFQ_KCAL)], na.rm = TRUE)


# Calculer la somme des colonnes hors boisson
FFQ_KCAL$SOMME_CARNET_HORS_BOISSON <- with(FFQ_KCAL, SOMME_FFQ_KCAL - 
                                                ALCOOL - 
                                                FRUITS_JUS - 
                                                
                                                LAIT - 
                                                EAU - 
                                                SODAS_LIGHT - 
                                                SODAS_SUCRES)

FFQ_KCAL$KCAL_SANS_ALCOOL <-  with(FFQ_KCAL, SOMME_FFQ_KCAL - ALCOOL)
FFQ_KCAL$KCAL_SANS_BOISSON <-  with(FFQ_KCAL, SOMME_FFQ_KCAL - ALCOOL- LAIT - SODAS_LIGHT - SODAS_SUCRES - EAU- FRUITS_JUS )



#CALCUL DE MAR /MER--------------------------------------

## Calcul de MAR et MER

###Calcul de la vitamine A --------------------------
###Calcul de la vitamine A --------------------------
df_long$vit_a_mcg <- (df_long$retinol_mcg + (df_long$beta_carotene_mcg/6)) 
#Ajout des dernières colonnes modifiées
df_long$proteines_g_alim <- df_long$Poids* df_long$proteines_g *10 
df_long$proteines_kcal_alim <- ((df_long$proteines_g*4) * df_long$Poids *10 )
df_long$ag_18_2_lino_g_alim  <- (df_long$Poids * df_long$ag_18_2_lino_g*10 )
df_long$ag_18_2_lino_kcal_alim   <- (df_long$Poids *df_long$ag_18_2_lino_g*9*10 )
df_long$ag_18_3_a_lino_g_alim<- (df_long$Poids *  df_long$ag_18_3_a_lino_g*10 )
df_long$ag_18_3_a_lino_kcal_alim <- (df_long$Poids*df_long$ag_18_3_a_lino_g*9*10 )
df_long$ags_g_alim  <- (df_long$ags_g* df_long$Poids * 10)
df_long$ags_kcal_alim <- (df_long$ags_g *9* df_long$Poids  * 10)


### Calcul des quantités de nutriments par aliment -----------------
# Sélection des colonnes à transformer
colonnes_a_transformer <- c("fibres_g","ag_20_6_dha_g", "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
                            "selenium_mcg", "iode_mcg","vit_a_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                            "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                            "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g")

# Vérifier si toutes les colonnes sont présentes
colonnes_manquantes <- setdiff(colonnes_a_transformer, names(df_long))
if (length(colonnes_manquantes) > 0) {
  stop("Les colonnes suivantes ne sont pas reconnues : ", paste(colonnes_manquantes, collapse = ", "))
}

# Si tout est correct, appliquer la transformation
df_long <- df_long %>%
  mutate(across(all_of(colonnes_a_transformer),
                ~ . * Poids * 10,
                .names = "{.col}_alim"))

### Somme par ID des nutriments d'interet --------------------------
colonnes_a_sommer <- names(df_long)[grep("_alim$", names(df_long))]
print(colonnes_a_sommer)  # Debugging check

somme_par_identifiant <- df_long %>%
  group_by(Identifiant) %>%
  summarise(across(colonnes_a_sommer, ~ sum(.x, na.rm = TRUE)))

print(df_long$ags_kcal_alim)

#Somme des sucres 
somme_par_identifiant$sucre_aj_g_appro_alim <- somme_par_identifiant$fructose_g_alim+ somme_par_identifiant$glucose_g_alim + somme_par_identifiant$maltose_g_alim + somme_par_identifiant$saccharose_g_alim

#Calcul des nutriments sans alcool
cols_to_extract <- c("Identifiant", "KCAL_SANS_ALCOOL" , "SOMME_FFQ_KCAL") 
extracted_df <- FFQ_KCAL[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")
cols_to_extract <- c("Identifiant", "Sexe") 
extracted_df <- Frame[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")

#Calcul dernières colonnes 
somme_par_identifiant$proteines_kcal_2000 <- (somme_par_identifiant$proteines_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$fibres_g_2000 <- (somme_par_identifiant$fibres_g_alim*2000)/  somme_par_identifiant$SOMME_FFQ_KCAL
somme_par_identifiant$ag_18_3_a_lino_g_2000 <- (somme_par_identifiant$ag_18_3_a_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_18_2_lino_g_2000 <- (somme_par_identifiant$ag_18_2_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_20_6_dha_g_2000 <- (somme_par_identifiant$ag_20_6_dha_g_alim*2000)/(somme_par_identifiant$SOMME_FFQ_KCAL)

somme_par_identifiant$ags_kcal_2000 <- (somme_par_identifiant$ags_kcal_alim *100) /(somme_par_identifiant$KCAL_SANS_ALCOOL)

### Rajustement / 2000 KCAL---------------------------------------
exclude_cols <-  c("proteines_kcal_alim", "ags_kcal_alim", "ag_18_2_lino_g_alim", "ag_18_3_a_lino_g_alim","ag_18_3_a_lino_kcal_alim",
                   "ags_g_alim","proteines_g_alim" ,"fructose_g_alim"  ,"maltose_g_alim"       ,   "glucose_g_alim"    , "saccharose_g_alim", "alcool_g_alim",
                   "ag_18_2_lino_kcal_alim", "fibres_g_alim", "ag_20_6_dha_g_alim")
alim_cols <- grep("_alim$", names(somme_par_identifiant), value = TRUE)
alim_cols <- setdiff(alim_cols, exclude_cols)
for (col in alim_cols) {
  somme_par_identifiant[[col]] <- (somme_par_identifiant[[col]] * 2000) / somme_par_identifiant$SOMME_FFQ_KCAL
  new_col_name <- sub("_alim$", "_2000", col)
  names(somme_par_identifiant)[names(somme_par_identifiant) == col] <- new_col_name
}

### Calcul des ratios du MAR------------------------
# Les recommandations communes, peu importe le genre
somme_par_identifiant$ratio_prot <- ifelse(somme_par_identifiant$proteines_kcal_2000 / 10 > 1, 1, somme_par_identifiant$proteines_kcal_2000/ 10)
somme_par_identifiant$ratio_fibre <- ifelse(somme_par_identifiant$fibres_g_2000 / 30 > 1, 1, somme_par_identifiant$fibres_g_2000 / 30)
somme_par_identifiant$ratio_lino <- ifelse(somme_par_identifiant$ag_18_2_lino_g_2000/ 4 > 1, 1, somme_par_identifiant$ag_18_2_lino_g_2000 / 4)
somme_par_identifiant$ratio_alphalino <- ifelse(somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1 > 1, 1, somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1)
somme_par_identifiant$ratio_dha <- ifelse(somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25 > 1, 1, somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25)
somme_par_identifiant$ratio_potassium <- ifelse(somme_par_identifiant$potassium_mg_2000 / 3500 > 1, 1, somme_par_identifiant$potassium_mg_2000 / 3500)
somme_par_identifiant$ratio_calcium <- ifelse(somme_par_identifiant$calcium_mg_2000 / 950 > 1, 1, somme_par_identifiant$calcium_mg_2000 / 950)
somme_par_identifiant$ratio_selenium <- ifelse(somme_par_identifiant$selenium_mcg_2000 / 70 > 1, 1, somme_par_identifiant$selenium_mcg_2000 / 70)
somme_par_identifiant$ratio_iode <- ifelse(somme_par_identifiant$iode_mcg_2000 / 150 > 1, 1, somme_par_identifiant$iode_mcg_2000 / 150)
somme_par_identifiant$ratio_vit_d <- ifelse(somme_par_identifiant$vitamine_d_mcg_2000 / 15 > 1, 1, somme_par_identifiant$vitamine_d_mcg_2000 / 15)
somme_par_identifiant$ratio_vit_c <- ifelse(somme_par_identifiant$vitamine_c_mg_2000 / 110 > 1, 1, somme_par_identifiant$vitamine_c_mg_2000 / 110)
somme_par_identifiant$ratio_vit_b2 <- ifelse(somme_par_identifiant$vitamine_b2_mg_2000 / 1.6 > 1, 1, somme_par_identifiant$vitamine_b2_mg_2000 / 1.6)
somme_par_identifiant$ratio_vit_b12 <- ifelse(somme_par_identifiant$vitamine_b12_mcg_2000 / 4 > 1, 1, somme_par_identifiant$vitamine_b12_mcg_2000 / 4)
somme_par_identifiant$ratio_vit_b9 <- ifelse(somme_par_identifiant$vitamine_b9_mcg_2000 / 330 > 1, 1, somme_par_identifiant$vitamine_b9_mcg_2000 / 330)

somme_par_identifiant$Sexe
# Définir une fonction pour calculer le ratio
calculate_ratio <- function(sexe, valeur, seuil_femme, seuil_homme) {
  if (sexe == 1) {
    return(ifelse(valeur / seuil_femme > 1, 1, valeur / seuil_femme))
  } else if (sexe == 2) {
    return(ifelse(valeur / seuil_homme > 1, 1, valeur / seuil_homme))
  } else {
    return(NA)
  }
}

## Appliquer la fonction pour chaque nutriment
somme_par_identifiant$ratio_magnesium <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$magnesium_mg_2000, 300, 380)
somme_par_identifiant$ratio_fer <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$fer_mg_2000, 13.5, 11)
somme_par_identifiant$ratio_cuivre <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$cuivre_mg_2000, 1.5, 1.9) 
somme_par_identifiant$ratio_zinc <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$zinc_mg_2000, 9.3, 11.7)
somme_par_identifiant$ratio_vit_a <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vit_a_mcg_2000, 650, 750)
somme_par_identifiant$ratio_vit_e <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_e_mg_2000, 9, 10)
somme_par_identifiant$ratio_vit_b1 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b1_mg_2000,0.965, 1.2)
somme_par_identifiant$ratio_vit_b3 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b3_mg_2000, 14.9, 18.5) 
somme_par_identifiant$ratio_vit_b6 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b6_mg_2000, 1.6, 1.7)

somme_par_identifiant <- na.omit(somme_par_identifiant)

### Calcul du MAR -----------------------------------

somme_par_identifiant$MAR <- ((somme_par_identifiant$ratio_prot + somme_par_identifiant$ratio_fibre + somme_par_identifiant$ratio_lino + somme_par_identifiant$ratio_alphalino + somme_par_identifiant$ratio_dha + 
                                 somme_par_identifiant$ratio_magnesium + somme_par_identifiant$ratio_potassium + somme_par_identifiant$ratio_calcium + somme_par_identifiant$ratio_fer + somme_par_identifiant$ratio_cuivre +
                                 somme_par_identifiant$ratio_zinc + somme_par_identifiant$ratio_selenium + somme_par_identifiant$ratio_iode + somme_par_identifiant$ratio_vit_a + somme_par_identifiant$ratio_vit_d + 
                                 somme_par_identifiant$ratio_vit_e + somme_par_identifiant$ratio_vit_c + somme_par_identifiant$ratio_vit_b1 + somme_par_identifiant$ratio_vit_b2 + somme_par_identifiant$ratio_vit_b3 + 
                                 somme_par_identifiant$ratio_vit_b6 + somme_par_identifiant$ratio_vit_b9 + somme_par_identifiant$ratio_vit_b12)/23)*100;
mean(somme_par_identifiant$MAR, na.rm=TRUE)

### Ratio pour le MER ----------------------------------------
# Définir une fonction pour calculer le ratio
somme_par_identifiant$ratio_ags <- ifelse((somme_par_identifiant$ags_kcal_2000 / 12 < 1),( 1), (somme_par_identifiant$ags_kcal_2000/ 12 ))
somme_par_identifiant$ratio_sodium  <- ifelse(somme_par_identifiant$sodium_mg_2000/ 2300 < 1, 1, somme_par_identifiant$sodium_mg_2000/ 2300 )
somme_par_identifiant$ratio_sucre_aj<- ifelse(somme_par_identifiant$sucre_aj_g_appro_2000/100 < 1, 1, somme_par_identifiant$sucre_aj_g_appro_2000/100)

### Calcul du MER -----------------------------------
somme_par_identifiant$MER <- (((somme_par_identifiant$ratio_ags + somme_par_identifiant$ratio_sodium + somme_par_identifiant$ratio_sucre_aj)*100)/3)-100
mean(somme_par_identifiant$MER)
mean(somme_par_identifiant$MAR)



#CALCUL des indicateurs environnementaux --------------------------------------
df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")

  ## Sélection des colonnes à transformer----------------
colonnes_a_transformer <- c("climat", "couche_ozone","ions","ozone",	"partic",	"acid",	"eutro_terr", "eutro_eau","eutro_mer",	"sol",	"toxi_eau",	"ress_eau",	"ress_ener",	"ress_min")

#  On multiplie par le poids de l'aliment et par 1000 pour convertir au kg pour chaque indicateur env 
df_long <- df_long %>%
  mutate(across(all_of(colonnes_a_transformer),~ . * Poids , .names = "{.col}_env" ))

df_long$climat_env <- df_long$climat_env*1000
  ## Somme par ID des résultats de chaque aliment  --------------------------

colonnes_a_sommer_env <- grep("_env$", names(df_long), value = TRUE)
df_selected <- df_long[, colonnes_a_sommer_env, drop = FALSE]

somme_par_identifiant_env <- df_long %>%
  group_by(Identifiant) %>%
  summarise(across(all_of(colonnes_a_sommer_env), ~ sum(.x, na.rm = TRUE)))

#Constitution du tableau final ------------------------------------------------
FFQ_id <- FFQ_POIDS
FFQ_id<- inner_join(FFQ_id, FFQ_KCAL, by="Identifiant")
FFQ_id$Mesure <- "FFQ"
new_df <- somme_par_identifiant[, c("Identifiant", "MAR", "MER")]
FFQ_id <- inner_join(FFQ_id, new_df, by='Identifiant')
FFQ_id <- inner_join(FFQ_id, somme_par_identifiant_env, by='Identifiant')
FFQ_id <- subset(FFQ_id, SOMME_FFQ_KCAL > 0)
FFQ_id <- subset(FFQ_id,
                 SOMME_FFQ_KCAL >= 500 & 
                   SOMME_FFQ_KCAL <= 4500)


# TELECHARGEMENT ----------------------------

# Créer un nouvel objet workbook
wb <- createWorkbook()

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", FFQ_id)

addWorksheet(wb, "Frequences_corrigées")
writeData(wb, sheet = "Frequences_corrigées", Frame)

addWorksheet(wb, "Poids_corrigée_TI")
writeData(wb, sheet = "Poids_corrigée_TI", FFQ_POIDS)

addWorksheet(wb, "Poids_corrigée_CSGA")
writeData(wb, sheet = "Poids_corrigée_CSGA", FFQ_POIDS)

saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_CSGA.xlsx")))




