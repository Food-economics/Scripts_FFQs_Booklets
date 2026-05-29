# CHARGEMENT DE L'ENVIRONNEMENT DE TRAVAIL  --------------
## Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx); library(readxl);library(dplyr);
library(broom);library(scales);library(modelsummary);library(ggplot2);library(effsize);library(lfe);
library(ggpubr);library(vtable);library;library("openxlsx");
library("dplyr"); library("tidyr");library("ggplot2");library("gridExtra");library(lubridate);
library("RColorBrewer");library(reshape2);library(Metrics);library(questionr);library(zoo)
## Importation des données ---------------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") { setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))}

### Entrer la date de la campagne ---------------------
campaign<-"23-02" 
Nj <- 28 #"Nombre de jour de saisie 

### Importation des données CSGA -------------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") {
  setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))
}
resultats_codachats <- read_excel(
  path  = "C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers bruts/Données_CSGA.xlsx",
  sheet = "Table_appli"
)

#DONNEES CORRIGEES FFQ
CSGA_FFQ <- read_excel(
  path  = "C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_CSGA.xlsx",
  sheet = "Frequences_corrigées"
)

CSGA_FFQ_id <- read_excel(
  path  = "C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_CSGA.xlsx",
)

describe(is.na(resultats_codachats$Prix_detail_VF))
describe(is.na(resultats_codachats$Prix_global_VF))
### Importation des données des tableaux annexes ----------------
CALNUT<- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/Alim_CALNUT_CODAPPRO_CARNET.xlsx")
magasins <- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/Reclassement_magasins.xlsx")

resultats_pondérés <- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/moyennes_pondérées.xlsx")
Poids_unitaires_manquants <- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/poids_unitaire_manquants.xlsx")
resultats_pondérés <- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/resultats_pondérés.xlsx")

# NETTOYAGE DU FICHIER COD_ACHATS : LIEUX / DATES / LIBELLE_CUSTOM / LIBELLE_CIQUAL-----------------
resultats_codachats$Date <- as.Date(
  resultats_codachats$Date,
  format = "%Y-%m-%d"
)



#Ajout du genre et du revenu mensuel
Ajout <- CSGA_FFQ[, c("Identifiant", "Sexe", "Budget.mensuel.alimentation.")]
names(resultats_codachats)[1] <- "Identifiant"
resultats_codachats <- left_join(Ajout, resultats_codachats,  by = "Identifiant")
#SUPPRIMER LES COLONNES EN TROP
resultats_codachats <- resultats_codachats[, -c(15:25)]
resultats_codachats <- resultats_codachats[, -c(22:30)]


### Jointure des lieux d'approvisionnement et de la classidication par type d'enseigne "Lieu 1" / "Lieu2"----------

names(resultats_codachats)[4] <- "Lieu_vf"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Leclerc"] <- "LECLERC"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Leclerc drive"] <- "LECLERC"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Leader Price"] <- "LEADER PRICE"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Restaurant/bar/cafe (sur place)"] <- "Restaurant/bar/café (sur place)"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Casino"] <- "CASINO"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Autre dons"] <- "Autres dons"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Colruyt"] <- "COLRUYT"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "carrefour"] <- "Carrefour"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Carrefour drive"] <- "Carrefour"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Super U"] <- "SUPER U"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Intermarche"] <- "INTERMARCHE"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Netto"] <- "NETTO"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "casino"] <- "CASINO"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Crous"] <- "CROUS"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Fins de marches ou poubelles"] <- "Fins de marché ou poubelles"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Vente a emporter"] <- "Vente à emporter"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Autre supermarche bio"] <- "Autre supermarché bio"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Livraison a domicile"] <- "Livraison à domicile"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Autre supermarche"] <- "Autre supermarché"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Marche"] <- "Marché"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Marche du bonheur"] <- "Marché du bonheur"
resultats_codachats$Lieu_vf[resultats_codachats$Lieu_vf == "Restos du coeur"] <- "Restos du cœur"
resultats_codachats <- left_join(resultats_codachats, magasins, by=c("Lieu_vf")) 



## Association Cod-AChats et Calnut ---------------------------
#FUSION du réferentiel CALNUT et du fichier de saisie "resultats_Codachats" 
#On renomme sur le fichier de saisie : "CodeCIQUAL" en "CODACHATS_alim_code" et "Categorie1" en "groupe_TI_TdC".
# resultats_codachats et CALNUT sont associés par CODACHATS_alim_code
colnames(resultats_codachats)[colnames(resultats_codachats) == 'CodeCIQUAL'] <- 'CODACHATS_alim_code'
resultats_codachats <- left_join(resultats_codachats, CALNUT, by=c("CODACHATS_alim_code"))
#Rename "Category1" with groupe_TI_TdC
colnames(resultats_codachats)[colnames(resultats_codachats) == 'groupe_TI_TdC'] <- 'groupe_TI_TdC1'


# NETTOYAGE DU FICHIER COD_ACHATS :  POIDS / PRIX / UNITES ----------------------------
#-	Si le poids égal 0 grammes, appliquer une conversion en unités 
names(resultats_codachats)[9] <- "Unite"
names(resultats_codachats)[26] <- "LibelleCIQUAL"



resultats_codachats$groupe_TI_TdC1[resultats_codachats$groupe_TI_TdC1 == "JAMBON_BLANC"] <- "CHARCUTERIE_HORS_JB"


resultats_codachats <- resultats_codachats %>%
  group_by(Identifiant) %>% 
  mutate(
    date_starting = min(Date, na.rm = TRUE)
  ) %>%
  ungroup()

#Supprimer les tickets avec plus de 28 jours de saisie 
resultats_codachats$jour_num <- 
  as.numeric(resultats_codachats$Date - resultats_codachats$date_starting) + 1
table(resultats_codachats$jour_num)
resultats_codachats <- resultats_codachats[resultats_codachats$jour_num <= 28, ]




# NETTOYAGE DU FICHIER COD_ACHATS :  POIDS / PRIX / UNITES ----------------------------

#-	Si la quantité est supérieure à 20 unités et qu’il ne s’agit pas d’œuf ou de Café_thé, attribuer des grammes. 
resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = ifelse(
      # votre condition d’origine…
      Nb > 30 &
        Unite == "unités" &
        # …et qu’il ne s’agit PAS de compote
        LibelleCIQUAL != "Compote de fruits, allégée en sucres" &
        LibelleCIQUAL != "Compote de pomme" &
        LibelleCIQUAL != "Compote de fruits" &
        LibelleCIQUAL != "Compote de fruits, allégée en sucres" & 
        LibelleCIQUAL != "Sushi ou maki aux produits de la mer" &
        groupe_TI_TdC1!="OEUFS" & 
        groupe_TI_TdC1!="CAFE_THE", 
      "grammes",
      Unite
    )
  )



#Si la quantité, les prix, le PrixMenu ou l’appréciation est inférieur ou égal à 0 (CODAPPRO attribue la valeur « - 1 »pour « ne sait pas ») appliquer une valeur manquante 
resultats_codachats$Prix[resultats_codachats$Prix < 0 ] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu < 0 ] <- NA
resultats_codachats$Nb[resultats_codachats$Nb <= 0 ] <- NA
resultats_codachats$Appreciation[resultats_codachats$Appreciation < 0 ] <- NA
#	Si le prix ou le le PrixMenu sont égals à 0 et que le lieu n’est pas renseigné comme un « don», attribuer une valeur manquante
resultats_codachats$Prix[resultats_codachats$Prix == 0 & resultats_codachats$Lieu1 != "dons" ] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu == 0 & resultats_codachats$Lieu1 != "dons" ] <- NA
#	Si la quantité est égale 0 attribuer une valeur manquante
resultats_codachats$Nb[resultats_codachats$Nb == 0 ] <- NA

## Correction des principales erreurs de conversion--------------
#Si la quantité est inférieure à 10g et qu'il ne s'agit pas d'épices multiplier par 1000 (poids rentré en Kilos)
#Si le prix est supérieur à 100€ et que l'aliment n'est pas l'alcool, indiquer une valeur manquante
resultats_codachats$Nb <- ifelse((resultats_codachats$Unite == "grammes" & resultats_codachats$Nb < 10 & resultats_codachats$groupe_TI_TdC1 != "EPICES_CONDIMENTS" & resultats_codachats$Lieu2 != "RHD"), (resultats_codachats$Nb*1000 ), (resultats_codachats$Nb))
resultats_codachats$Prix <- ifelse((resultats_codachats$Prix > 100 & resultats_codachats$groupe_TI_TdC1 != "ALCOOL"), (NA), (resultats_codachats$Prix))

#Si le prix est supérieur à 50 kilos indiquer des grammes
#Si la quantité est supérieure à 10 g et inférieure à 20,5 grames indiquer qu'il s'agit d'unités
#SI la quantité est inférieure à 1 cl indiquer qu'il s'agit de litres 
#Si la quantité est supérieure à 1000 Kilos indiquer qu'il s'agit de grammes
resultats_codachats$Unite <- with(resultats_codachats,
                                  ifelse( Nb > 50 & Unite == "kilos", "grammes",
                                          ifelse(Nb > 10 & Nb < 20.5 & Unite == "grammes", "unités",
                                                 ifelse( Nb < 1 & Unite == "centilitres", "litres",
                                                         ifelse(Nb > 1000 & Unite == "kilos", "grammes", Unite)))))


## Réajustement du nombre si le prix est bas et la quantité élevée------------
resultats_codachats$Nb <- with(resultats_codachats,ifelse(Prix < 3 & Nb > 17 & Unite == "litres", Nb /10,Nb))
##Corrections au cas par cas  ---------------

#Ajout d'unn prix au Kg
resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb

###Nouvelle condition pour multiplier Nb par 10 pour les aliments spécifiques si Nb <= 100 g --------------
aliments_specifiques <- c(
  "Bonbon/ bouchée chocolat fourrage gaufrettes/ biscuit", "Bonbons, tout type", "Champignon, tout type, cru",
  "Champignons à la grecque, appertisés", "Barre chocolatée biscuitée", "Fromage à pâte molle et croûte fleurie double crème environ 30% MG",
  "Miel", "Sucre blanc", "Crème fraîche, 15 à 20% MG, UHT", "Rillettes de poulet", "Champignon noir, séché",
  "Jambon sec, découenné, dégraissé", "Cacahuète ou Arachide", "Croissant, sans précision", "Viande rouge, cuite (aliment moyen)",
  "Confiserie au chocolat dragéifiée", "Chocolat blanc aux fruits secs (noisettes, amandes, raisins, praliné), tablette",
  "Yaourt, lait fermenté ou spécialité laitière, aux fruits, sucré", "Fruit cru (aliment moyen)", "Rillettes de thon", "Crevette, crue",
  "Crêpe, nature, préemballée, rayon température ambiante", "Brioche fourrée au chocolat", "Dessert (aliment moyen)",
  "Chocolat au lait fourré au praliné, tablette", "Biscuit sec chocolaté, préemballé", "Barre céréalière aux amandes ou noisettes",
  "Nougat ou touron", "Aubergine, crue", "Crème fraîche, 30% MG, UHT", "Banane, pulpe, crue", "Poulet, filet, sans peau, cru",
  "Pâté (aliment moyen)", "Nem ou Pâté impérial", "Sauce kebab", "Crevette, cuite", "Gaufrette ou éventail sans fourrage",
  "Bonbon gélifié", "Pâte d'amande", "Cabillaud, cru", "Chocolat au lait, tablette", "Bonbon / bouchée au chocolat fourrage gaufrettes / biscuit",
  "Barres ou confiserie chocolatées au lait", "Kiwi, pulpe et graines, cru", "Chocolat au lait fourré", "Sucre vanillé",
  "Pain d'épices fourré ou nonette", "Oignon, cru", "Chocolat, en tablette (aliment moyen)")


resultats_codachats <- resultats_codachats %>%
  mutate(Nb = ifelse(LibelleCIQUAL %in% aliments_specifiques & Nb <= 20 & Unite == "grammes" & Prix_kg > 0.05, Nb * 10, Nb))

### Nouvelle condition pour diviser Prix par 10 si Nb >= 100 g et Prix >= 10€ pour les aliments spécifiques ------------
aliments_specifiques <- c("Saumon fumé", "Barres chocolatées", "Pâte à tartiner chocolat et noisette", "Rosette ou Fuseau", "Pâtisserie (aliment moyen)",
                          "Pomme de terre de conservation, crue", "Chocolat, en tablette (aliment moyen)", "Mélange apéritif graine non salée fruit séché",
                          "Mozzarella au lait de vache", "Sandwich baguette, jambon emmental")
resultats_codachats$Prix <- ifelse(
  resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & 
    resultats_codachats$Nb >= 100 & 
    resultats_codachats$Prix >= 10 & resultats_codachats$Prix_kg > 0.05 & resultats_codachats$Unite == "grammes", 
  resultats_codachats$Prix / 10,
  resultats_codachats$Prix)

### Nouvelle condition pour convertir Nb en centilitres si Nb < 100 g pour des aliments spécifiques ---------
aliments_specifiques <- c( "Bière \"de spécialités\" ou d'abbaye, régionales ou d'une brasserie (degré d'alcool variable)",
                           "Boisson gazeuse, sans jus de fruit, sucrée","Jus de fruits (aliment moyen)", "Boisson préparée à partir de sirop à diluer type menthe, fraise, etc., sucré, dilué dans l'eau",
                           "Huile de pépins de raisin")
resultats_codachats <- within(resultats_codachats, {
  Unite <- ifelse(
    LibelleCIQUAL %in% aliments_specifiques & Nb < 100 & Unite == "grammes" & Prix_kg > 0.05 , 
    "centilitres", 
    Unite
  )})

### Corrections au cas par cas sur les quantités-----------
print(unique(resultats_codachats$Identifiant))

#Attribution d'un poids unitaire pour les oeufs de 63 grammes
resultats_codachats <- resultats_codachats %>%
  mutate(
    # conversion du nombre
    Nb = if_else(
      groupe_TI_TdC1 == "OEUFS" & Unite == "unités",
      Nb * 0.063,
      Nb
    ),
    # mise à jour de l'unité
    Unite = if_else(
      groupe_TI_TdC1 == "OEUFS" & Unite == "unités" & !is.na(Nb),
      "kilos",
      Unite
    )
  )
oeufs <- resultats_codachats[resultats_codachats$groupe_TI_TdC1 == "OEUFS", ]

resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Aubergine, crue" & 
    resultats_codachats$Nb == 75 & 
    resultats_codachats$Prix == 3.98,
  750,
  resultats_codachats$Nb
)




#1 pot de compote = 100g
resultats_codachats <- resultats_codachats %>%
  mutate(
    # conversion du nombre
    Nb = if_else(
      (LibelleCIQUAL == "Compote de fruits allégée en sucres rayon frais"  |
         LibelleCIQUAL == "Compote de pomme" |
         LibelleCIQUAL == "Compote de fruits" |
         LibelleCIQUAL == "Compote de fruits, allégée en sucres" |
         LibelleCIQUAL == "Compote (aliment moyen)" 
       
      )
      & Unite == "unités",
      Nb * 0.1,
      Nb
    ),
    # mise à jour de l'unité
    Unite = if_else(
      (LibelleCIQUAL == "Compote de fruits allégée en sucres rayon frais"  |
         LibelleCIQUAL == "Compote de pomme" |
         LibelleCIQUAL == "Compote de fruits" |
         LibelleCIQUAL == "Compote de fruits, allégée en sucres" |
         LibelleCIQUAL == "Compote (aliment moyen)" 
      ) & Unite == "unités" & !is.na(Nb),
      "kilos",
      Unite
    )
  )




resultats_codachats$Unite <- ifelse(
  (resultats_codachats$LibelleCIQUAL == "Amande, avec peau" & 
     resultats_codachats$Nb == 0.2 & 
     resultats_codachats$Unite == "centilitres"),
  "kilos",
  resultats_codachats$Unite
)

df_libelles <- unique(resultats_codachats$LibelleCIQUAL)

# Ne garder que ceux qui commencent par "S"
libelles_S <- df_libelles[grepl("^Su", df_libelles)]



#resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Champignon, tout type, cru" & resultats_codachats$Nb == 25 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix == 2.32), (250), (resultats_codachats$Nb))
# Poids unitaire d’un sushi = 0,04 kg (soit 40 g)
resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
    resultats_codachats$Unite      == "unités",
  resultats_codachats$Nb * 0.04,  # on multiplie par 0.04 (kg)
  resultats_codachats$Nb
)

# Passage de l’unité de "unités" à "kilos"
resultats_codachats$Unite <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
    resultats_codachats$Unite      == "unités",
  "kilos",
  resultats_codachats$Unite
)

# Sélectionner les lignes où LibelleCIQUAL correspond exactement à ce texte
subset_sushi <- resultats_codachats %>%
  filter(LibelleCIQUAL == "Sushi ou maki aux produits de la mer")



resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Oeuf, cru" & resultats_codachats$Nb==30 ), (300), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Nem ou Pâté impérial" & resultats_codachats$Unite=="unités"), (resultats_codachats$Nb*0.1), (resultats_codachats$Nb))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Nem ou Pâté impérial" & resultats_codachats$Unite=="unités"), ("kilos"), (resultats_codachats$Unite))




resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Champignon, morille, crue" & resultats_codachats$Nb == 30 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (300), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Champignon, lentin ou shiitaké, séché" & resultats_codachats$Nb == 20 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (200), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Yaourt à la grecque, nature" & resultats_codachats$Nb == 12 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05 ), (120), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Yaourt aromatisé, avec édulcorants, 0% MG" & resultats_codachats$Nb == 12 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05 ), (120), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Yaourt aux fruits, sucré" & resultats_codachats$Nb == 16 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (160), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Purée de tomate" & resultats_codachats$Nb == 15000 ), (15), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Fruits de mer (aliment moyen), cru" & resultats_codachats$Nb == 12 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (120), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Pizza 4 fromages" & resultats_codachats$Nb == 16 & resultats_codachats$Unite=="unités"), ( 1), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Terrine de canard" & resultats_codachats$Nb == 130 & resultats_codachats$Prix==7.6  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Sandwich baguette, jambon emmental" & resultats_codachats$Prix<0.45  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (NA), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Toasts ou Canapés salés, garnitures diverses, préemballés" & resultats_codachats$Prix==8.05  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05),(resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Rillettes de poulet" & resultats_codachats$Nb==180 & resultats_codachats$Prix==8.8  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05),(resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Bœuf, fauxfilet, cru" &
    resultats_codachats$Nb == 240 &
    resultats_codachats$Prix == 12.48 &
    resultats_codachats$Unite == "grammes" &
    resultats_codachats$Prix_kg > 0.05,
  resultats_codachats$Nb * 10,
  resultats_codachats$Nb
)
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL == "Bœuf, fauxfilet, cru" & resultats_codachats$Nb==110 & resultats_codachats$Prix==7.5  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05),(resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Moutarde" & resultats_codachats$Nb < 200 & resultats_codachats$Unite=="grammes" & resultats_codachats$Nb < 0.05  & resultats_codachats$Prix_kg > 0.05 ), (resultats_codachats$Nb*10 ), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="banane, crue" & resultats_codachats$Nb > 1070 ), (1.07 ), (resultats_codachats$Nb))

### Corrections au cas par cas sur les unités-----------
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Pomme, crue" & resultats_codachats$Nb == 1 & resultats_codachats$Unite=="centilitres"), ("kilos" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Tomate, bouillie/cuite à l'eau" & resultats_codachats$Nb == 1600 ), ("unités"), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Vin rouge" & resultats_codachats$Nb == 1 & resultats_codachats$Unite=="centilitres"), ("litres" ), (resultats_codachats$Unite))
aliments_specifiques <- c("Sauce pour nems à base de nuocmam dilué, préemballée", "Sauce soja, préemballée",
                          "Bière \"spéciale\" (56° alcool)", "Bière \"spéciale\" (5-6° alcool)")
resultats_codachats$Unite <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Nb <= 3 & resultats_codachats$Unite == "centilitres", "litres", resultats_codachats$Unite)
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Sauce soja, préemballée" & resultats_codachats$Nb == 2.7 & resultats_codachats$Unite=="centilitres"), ("litres" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Fromage (aliment moyen)" & resultats_codachats$Nb == 1600 & resultats_codachats$Unite=="kilos"), ("grammes" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Vinaigre" & resultats_codachats$Nb == 1 & resultats_codachats$Unite=="grammes"), ("litres" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Sel blanc, non iodé, non fluoré" & resultats_codachats$Nb == 0.75 & resultats_codachats$Unite=="grammes"), ("kilos" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Moutarde" & resultats_codachats$Nb < 200 & resultats_codachats$Unite=="centilitres"), ("grammes" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Cornichon, au vinaigre" & resultats_codachats$Unite=="litres"), ("kilos" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Cornichon, au vinaigre" & resultats_codachats$Unite=="centilitres"), ("grammes" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Nb == 1 & resultats_codachats$Unite == "grammes", "kilos", resultats_codachats$Unite)
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Oeuf, cru" & resultats_codachats$Unite=="grammes"& resultats_codachats$Prix==100), ("unités" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Croissant, sans précision" & resultats_codachats$Identifiant=="LE116"& resultats_codachats$Nb=="2"), ("unités" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Crème de lait, 15 à 20% MG, légère, épaisse, rayon frais" & resultats_codachats$Identifiant=="LE126" & resultats_codachats$Nb == 8), ("unités"), (resultats_codachats$Unite))

print(unique(resultats_codachats$Identifiant)) 

### Corrections au cas par cas sur les prix-----------
#Si les prix sont égals à 100, attribuer une valeur manquante
resultats_codachats$Prix <- ifelse((resultats_codachats$LibelleCIQUAL == "Oeuf, à la coque" & resultats_codachats$Prix==100),(NA), (resultats_codachats$Prix))
aliments_specifiques <- c("Pâtes sèches standard, cuites, non salées","Pâtes sèches standard, crues")
resultats_codachats$Prix <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Prix==100, 1, resultats_codachats$Prix)
resultats_codachats$Prix <- ifelse((resultats_codachats$LibelleCIQUAL == "Ravioli chinois vapeur à la crevette" & resultats_codachats$Nb==105 & resultats_codachats$Prix==7.5),(NA), (resultats_codachats$Prix))
aliments_specifiques <- c("Abat, cru (aliment moyen)", "Abat, cuit (aliment moyen)",
                          "Thon, cru ", "Accra de poisson")
resultats_codachats$Prix <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Prix == 100, 10, resultats_codachats$Prix)




# IMPUTATION DES POIDS/PRIX MANQUANTS ------------------------------
## Correction des dernières données données de Prix----------------
#Lors de la campagne de novembre 2023, des personnes ont indiqué comme "prix menu" des courses réalisées en magasin
#Ces acahts ne doivent pas apparaitre comme lorsqu'on indique Oui à la variable RHD_DON
#On attribue NA aux prix indiqués pour ces courses car on possède uniquement le prix de toutes les courses et on appliquera la procédure d'imputation des poids prix à ces mêmes aliments 
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="commerce" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="Epicerie" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="commerce" | resultats_codachats$Lieu2 =="Epicerie"), (NA), (resultats_codachats$Menu))
# Si le produit provient de la RHD: Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(NA),(resultats_codachats$Prix))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="RHD" ), ("Oui"), (resultats_codachats$Menu))
resultats_codachats$Nb <-ifelse(((resultats_codachats$Lieu2 =="RHD") & resultats_codachats$Nb <9 & resultats_codachats$Unite =="grammes"    ),(NA),(resultats_codachats$Nb ))
resultats_codachats$Unite <-ifelse(((resultats_codachats$Lieu2 =="RHD") & is.na(resultats_codachats$Nb) & resultats_codachats$Unite =="grammes" ),(NA),(resultats_codachats$Unite ))

# Si le produit est un don: Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="dons")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="dons")),(NA),(resultats_codachats$Prix))
# Si ce n'est pas un Menu entrer NA dans la variable prixMenu 
resultats_codachats$PrixMenu <-ifelse((is.na(resultats_codachats$Menu)),(NA),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse((is.na(resultats_codachats$PrixMenu)),(resultats_codachats$Prix),(NA))

#On divise le prix des menus par le nbre total d'aliments achetés ensembles
# Grouper les achats par date et lieu d'approvisionnement
resultats_codachats <- resultats_codachats %>% arrange(Date) 
grouped_data <- resultats_codachats %>% group_by(Date, Lieu_vf)
# Copier le prix menu dans toutes les lignes du même ticket
resultats_codachats <- grouped_data %>% mutate(PrixMenu = ifelse(row_number() == 1, PrixMenu / n(), PrixMenu))
# Calculer le prix individuel pour chaque aliment
resultats_codachats <- grouped_data %>% mutate(PrixMenu = ifelse(PrixMenu, PrixMenu[1] / n(), PrixMenu))
#Si à ce stade, les prix des aliments achetés en RHD sot nuls on attribu une valeur de NA 
resultats_codachats$Menu[resultats_codachats$PrixMenu == 0 ] <- NA
resultats_codachats$Menu[resultats_codachats$Menu == 0 ] <- NA



## Creation des variables Prix_all (€) et poids (kg)-------------------
#deux variables homogènes qui réunissent toutes les données de poids et de prix sous la même unité 
resultats_codachats$Poids <- resultats_codachats$Unite    
resultats_codachats$Prix_all <- resultats_codachats$Prix
resultats_codachats$Prix_all <-ifelse((is.na(resultats_codachats$Prix_all)),(resultats_codachats$PrixMenu),(resultats_codachats$Prix_all))
resultats_codachats <- resultats_codachats %>%
  mutate(Poids = case_when(
    Unite == "kilos" ~ Nb,
    Unite == "kilo" ~ Nb,
    Unite == "litres" ~ Nb,
    Unite == "centilitres" ~ Nb /100,
    Unite == "grammes" ~ Nb / 1000,
    Unite == "unités" ~  Nb*poids_unitaire/1000,
    TRUE ~ NA_real_
  )) %>%
  mutate(Poids = ifelse(Poids == 0, NA_real_, Poids)) %>%
  mutate(Poids = as.numeric(Poids))
resultats_codachats %>%
  filter(LibelleCIQUAL == "Compote de fruits, allégée en sucres") %>%
  select(Nb, Unite)


resultats_codachats$Poids <-ifelse((resultats_codachats$Unite == "unités" & is.na(resultats_codachats$Poids) ),(NA),(resultats_codachats$Poids))
resultats_codachats$Nb <- ifelse((resultats_codachats$Nb== 0 ), (NA), (resultats_codachats$Nb))
resultats_codachats$Poids <- ifelse((resultats_codachats$Poids == 0 ), (NA), (resultats_codachats$Poids))
resultats_codachats$Prix_all <- ifelse((resultats_codachats$Prix == 0 & resultats_codachats$Lieu1!= "dons" ), (NA), (resultats_codachats$Prix_all ))

describe(is.na(resultats_codachats$Poids))
describe(is.na(resultats_codachats$Prix_all))

## Calcul du Prix / Kg pour tous les aliments -------------
resultats_codachats$Prix_kg <- resultats_codachats$Prix_all / resultats_codachats$Poids
print(unique(resultats_codachats$Prix_kg))



## Calcul du prix moyen par Kg et du poids moyen pour l'Imputation CODE_CIQUAL X Lieu1 -----------
# Calculer le prix moyen par unité de poids pour chaque aliment
prix_moyen_par_poids1 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # pas de NA
    str_trim(LibelleCIQUAL) != "",   # pas de chaîne vide
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  summarise(
    prix_moyen_ciqual1     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual1 = n(),
    .groups = "drop"
  )




#Joindre les Données de Prix Moyen à la Table Principale
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids1, by = c("LibelleCIQUAL", "Lieu1")) 

#prix_calculé_ciqual1 : Calcule un prix pondéré en utilisant Poids et prix_moyen_ciqual1.
#poids_calculé_ciqual1 : Calcule un poids dérivé en utilisant Prix_all et prix_moyen_ciqual1.
resultats_codachats$prix_calculé_ciqual1  <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual1
resultats_codachats$poids_calculé_ciqual1 <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual1

#Compter le Nombre d'Observations par Groupe
resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  mutate( nombre_donnees_ciqual1 = n())

## Calcul du prix moyen par Kg et du poids moyen pourImputation CODE_CIQUAL X Lieu2 -------------
# Calculer le prix moyen par unité de poids pour chaque aliment
prix_moyen_par_poids2 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # pas de NA
    str_trim(LibelleCIQUAL) != "",   # pas de chaîne vide
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  summarise(
    prix_moyen_ciqual2     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual2 = n(),
    .groups = "drop"
  )



# Imputer les valeurs manquantes en utilisant la fonction na.aggregate
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids2,
            by = c("LibelleCIQUAL", "Lieu2"))

resultats_codachats$prix_calculé_ciqual2 <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual2
resultats_codachats$poids_calculé_ciqual2  <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual2

resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  mutate(nombre_donnees_ciqual2 = n())

## Calcul du prix moyen par Kg et du poids moyen pourImputation GroupeTI X Lieu2 -------------
# Calculer le prix moyen par unité de poids pour chaque aliment sauf pour "Epices_condiments"
prix_moyen_par_poids3 <- resultats_codachats %>%
  filter(!is.na(Prix_all), !is.na(Poids),
         groupe_TI_TdC1 != "Epices_condiments") %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  summarise(
    prix_moyen_groupe_TI_TdC1     = mean(Prix_kg),
    nombre_donnees_groupe_TI_TdC1 = n(),
    .groups = "drop"
  )

# Joindre les prix moyens au dataset original
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids3,
            by = c("groupe_TI_TdC1", "Lieu2"))

# Calculer le prix et le poids imputés pour les groupes sauf "Epices_condiments"
resultats_codachats <- resultats_codachats %>%
  mutate(prix_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Poids * prix_moyen_groupe_TI_TdC1, NA_real_),
         poids_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Prix_all / prix_moyen_groupe_TI_TdC1, NA_real_))





# Calculer le nombre de données par groupe et lieu
resultats_codachats <- resultats_codachats %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  mutate(nombre_donnees_groupe_TI_TdC1 = n()) %>%
  ungroup()


bis <- resultats_codachats %>%
  filter(Identifiant == "39-Epimut") %>%
  select(Date, LibelleCIQUAL, groupe_TI_TdC1 , Poids, Prix_all, prix_moyen_ciqual1, prix_moyen_ciqual2,prix_moyen_groupe_TI_TdC1
  ) %>%
  print(n = Inf)

## Imputation finale ---------------
# Initialisation des colonnes finals à partir des colonnes d'origine
resultats_codachats$Poids_vf <- resultats_codachats$Poids
resultats_codachats$Prix_vf  <- resultats_codachats$Prix_all

# Fonction corrigée : prend en compte l'état courant de Poids_vf / Prix_vf
calculate_vf <- function(data, prix_calc_col, poids_calc_col, nombre_col) {
  # On part des valeurs déjà imputées
  base_poids <- data$Poids_vf
  base_prix  <- data$Prix_vf
  
  data$Poids_vf <- ifelse(
    is.na(base_poids) & data[[nombre_col]] >= 10,
    data[[poids_calc_col]],
    base_poids
  )
  
  data$Prix_vf <- ifelse(
    is.na(base_prix) & data[[nombre_col]] >= 10,
    data[[prix_calc_col]],
    base_prix
  )
  
  return(data)
}

# Application en trois passes, sans réécraser les imputations précédentes
resultats_codachats <- calculate_vf(
  resultats_codachats,
  "prix_calculé_ciqual1",   "poids_calculé_ciqual1",   "nombre_donnees_ciqual1"
)




resultats_codachats <- calculate_vf(
  resultats_codachats,
  "prix_calculé_ciqual2",   "poids_calculé_ciqual2",   "nombre_donnees_ciqual2"
)

resultats_codachats <- calculate_vf(
  resultats_codachats,
  "prix_calculé_groupe_TI_TdC1", "poids_calculé_groupe_TI_TdC1", "nombre_donnees_groupe_TI_TdC1"
)


data_filtré <- resultats_codachats %>%
  filter(LibelleCIQUAL == "Piment, cru")


#Imputation des dons
#La seule modif apportée est faite sur le prix. Si NA, on impute une valeur de zéro
resultats_codachats$Prix_vf <- ifelse((resultats_codachats$Lieu1 =="dons" & is.na(resultats_codachats$Prix_vf)), (0), (resultats_codachats$Prix_vf))

### Imputation des poids des  aliments RHD----------
####RHD_COL / RHD_COM
resultats_codachats$Poids_vf <- ifelse(
  is.na(resultats_codachats$Poids_vf),
  resultats_codachats$PoidsRHD,
  resultats_codachats$Poids_vf
)

## Verification du prix au kg des données imputées = ---------------------------
#On calcule le prix au kg. 
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # si le prix est zéro → 0
      Poids_vf == 0          ~ NA_real_,    # si le poids est zéro → NA
      TRUE                    ~ Prix_vf / Poids_vf
    )
  )

describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))
describe(is.na(resultats_codachats$Prix_global_VF))
describe(is.na(resultats_codachats$Poids_VF))

lignes_vide <- resultats_codachats[is.na(resultats_codachats$Prix_Kg_post_imput), ]

## Suppression des identifiants qui enregistrent une dépense inférieure à 25% du budget alimentaire déclaré---------
# Calculer la somme des prix_vf par identifiant
somme_prix_vf <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(somme_prix_vf = sum(Prix_vf, na.rm = TRUE),)

# Calculer le quart du budget alimentaire
valeurs_uniques_budget <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(budget_unique = unique(Budget.mensuel.alimentation.))



Comparaison <- inner_join(somme_prix_vf , valeurs_uniques_budget, by="Identifiant")
Comparaison$quart_budget <- as.numeric(Comparaison$budget_unique)/4
Comparaison$dix_budget <- as.numeric(Comparaison$budget_unique)*10

print(unique(resultats_codachats$Identifiant))
# Affichage du message d'alerte pour les identifiants où somme prix vf < budget_unique
Comparaison <- Comparaison %>%
  mutate(
    message_alerte = ifelse( quart_budget > somme_prix_vf,
                             paste("Alerte: La somme des prix_vf (", somme_prix_vf, ") est inférieure au quart du budget alimentaire (", budget_unique, ")"),
                             "Aucune alerte"))
identifiants_alerte <- Comparaison %>%
  filter(somme_prix_vf < quart_budget )%>%
  pull(Identifiant)

# Supprimer les identifiants qui ressortent avec un message d'alerte dans resultats_codachats
resultats_codachats<- resultats_codachats%>%
  filter(!Identifiant %in% identifiants_alerte )



#Supprimer les catégories incomparables
resultats_codachats <- subset(
  resultats_codachats,
  !groupe_TI_TdC1 %in% c( "EPICES_CONDIMENTS", "MGV",  "PLATS_PREP_VEGETARIENS")
) 


# IMPUTATION  DES DONNEES NUTRITIONNELLE ET ENVIRONNEMENTALES -----------------
  ##Agrégation des variables d'intérêt par groupe_TI_TdC à partir du dataframe CALNUT----------
resultats_codachats$Unite <- ifelse(is.na(resultats_codachats$Unite),(resultats_codachats$Unite=="unités"), (resultats_codachats$Unite))
#Les données renseignées uniquement par une catégorie TI ne possèdent pas d'imputation pour les différents nutriments / les données env et les pct conso et yield_factor
#Par conséquent on impute ces valeurs moyenne sur la base du fichier CALNUT par catégorie TI
##Filtrage des données : filter(!is.na(Poids_vf) & Poids_vf != 0) filtre les lignes où Poids_vf n'est pas NA et n'est pas égal à zéro (!= 0). Cela exclura toutes les lignes où le poids est manquant ou nul.
##Agrégation des données : Ensuite, les données filtrées sont regroupées par groupe_TI_TdC1 à l'aide de group_by.
##Calcul de la moyenne pondérée : Enfin, summarise calcule la moyenne pondérée de yield_factor pour chaque groupe défini par groupe_TI_TdC1, en utilisant les poids valides spécifiés par Poids_vf et en ignorant les valeurs NA.
colonnes_a_transformer<- c("yield_factor","pct_conso", "retinol_mcg", "nrj_kcal", "proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
              "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
              "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
              "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
              "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "DQR", "EF", "climat", "couche_ozone", "ions",
                            "ozone", "partic", "acid", "eutro_terr", "eutro_eau", 
                            "eutro_mer", "sol", "toxi_eau", "ress_eau", "ress_ener", "ress_min")

#L'idée c'est d'affecter les valeurs de nutriments moyennes en fonction des aliments que l'on retrouve le plus / cat 
  ## Associer le tableau TI aux resultats_codachats ---------
resultats_codachats <-left_join(resultats_codachats,resultats_pondérés,by="groupe_TI_TdC1")
# Remplacer les valeurs manquantes par la moyenne correspondante
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer), 
                ~ ifelse(is.na(.x), get(paste0("mean_", cur_column())), .x)))

## Supprimer les colonnes mean_ après le remplacement si nécessaire
resultats_codachats <- resultats_codachats[, setdiff(names(resultats_codachats), grep("^mean_", names(resultats_codachats), value = TRUE))]

resultats_codachats$UC_TI <- 1

# CALCUL DES INDICATEURS ------------------
  ## Poids (Kg/personne/jour) ----------------------------------
#Pour lier les quantités COD-Appro et FFQ, il faut systématiquement multiplier 
#le poids des fournitures par yield_factor*pct_conso : 
#On obtient ainsi le poids consommé pour les carnets de fournitures. 
#Pour lier les quantités COD-Appro et FFQ, il faut systématiquement multiplier 
#le poids des fournitures par yield_factor*pct_conso : 
#On obtient ainsi le poids consommé pour les carnets de fournitures. 
resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD"),(resultats_codachats$Poids_vf*resultats_codachats$yield_factor*resultats_codachats$pct_conso),(resultats_codachats$Poids_vf))
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf


#Conversion 
#https://www.femobook.com/blogs/coffee-knowledge/a-guide-to-the-golden-cup-standard?
#55 g de café pour 1 L d’eau
#The ISO 3103 (2g de the pour 1L d'eau)
#resultats_codachats$nrj_kcal <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_vf))


resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_consomme_vf))
resultats_codachats_CAFE_THE <- resultats_codachats %>%
  filter(groupe_TI_TdC1 == "CAFE_THE")

resultats_codachats$kcal_aliment_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$kcal_aliment_vf/ resultats_codachats$Sec_Vol),(resultats_codachats$kcal_aliment_vf))

###POIDS GENERAL ------------------------
Nourriture_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$Poids_consomme_vf  )
names(Nourriture_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","Poids_vf_consommee")

#print(unique(Nourriture_consommee$groupe_TI_TdC))
# Liste des catégories de nourriture
categories <- unique(Nourriture_consommee$groupe_TI_TdC)
# Boucle pour créer les colonnes correspondantes dans Nourriture_consommee
for (categorie in categories) {
  Nourriture_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Nourriture_consommee$groupe_TI_TdC == categorie, Nourriture_consommee$Poids_vf_consommee, 0)}
# Supprimer les colonnes "groupe_TI_TdC" et "Poids_vf_consomme" si besoin
Nourriture_consommee <- subset(Nourriture_consommee, select = -c(groupe_TI_TdC, Poids_vf_consommee, NA_CARNET))
# Agrégation pour le dataframe Carnet_POIDS
Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# Liste des noms de colonnes à agréger
colonnes <- names(Nourriture_consommee)[-1] # Exclure la colonne "Identifiant"
# Boucle pour agréger les données par colonne
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Nourriture_consommee, FUN = sum)
  Carnet_POIDS <- left_join(Carnet_POIDS, Temp, by = "Identifiant")
}

Carnet_POIDS$AUTRE_CARNET <- NULL

# Diviser les colonnes par la colonne UC multipliée par le nombre de jour de saisie
Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$UC_TI*Nj)
# Calculer la somme des colonnes pour chaque ligne
Carnet_POIDS$POIDS_TOTAL_CARNET <- rowSums(Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE)
# Calculer la somme des colonnes hors boisson
Carnet_POIDS$POIDS_HORS_BOISSON_CARNET <- with(Carnet_POIDS, POIDS_TOTAL_CARNET - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET -
                                                 CAFE_THE_CARNET)


#Ajout suffixe _Poids
Carnet_POIDS <- Carnet_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_CARNET$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )

  ## Kcal (Kcal/personne/jour)------------------------------------------
#Pour chaque aliment, nous imputons sa valeur nutritionnelle en kcal / kg en fonction du poids consommé de chaque aliment.
#resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf
#Pour chaque aliment consommé, nous imputons sa valeur nutritionnelle en kj / kg sur la base du poids consommé.
Kcal_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$kcal_aliment_vf  )
names(Kcal_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","kcal_aliment_vf")
# Liste des catégories de nourriture
categories <- unique(Kcal_consommee$groupe_TI_TdC)
# Boucle pour créer les colonnes correspondantes dans Kcal_consommee
for (categorie in categories) {
  Kcal_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Kcal_consommee$groupe_TI_TdC == categorie,
                                                                 Kcal_consommee$kcal_aliment_vf, 0)}
# Supprimer les colonnes "groupe_TI_TdC" et "Poids_vf_consomme" si besoin
Kcal_consommee <- subset(Kcal_consommee, select = -c(groupe_TI_TdC, kcal_aliment_vf, NA_CARNET))

# Agrégation pour le dataframe Carnet_KCAL

#Carnet_KCAL <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
Carnet_KCAL <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# Liste des noms de colonnes à agréger
colonnes <- names(Kcal_consommee)[-1] # Exclure la colonne "Identifiant"
# Boucle pour agréger les données par colonne
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Kcal_consommee, FUN = sum)
  Carnet_KCAL <- left_join(Carnet_KCAL, Temp, by = "Identifiant")}
# Diviser les colonnes par la colonne UC multipliée par Nj
Carnet_KCAL$AUTRE_CARNET <- NULL
Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$UC_TI* Nj)
# Calculer la somme des colonnes  pour chaque ligne
Carnet_KCAL$KCAL_TOTAL_CARNET <- rowSums(Carnet_KCAL[, 3:ncol(Carnet_KCAL)], na.rm = TRUE)
# Calculer la somme des colonnes hors boisson
Carnet_KCAL$KCAL_HORS_BOISSON_CARNET <- with(Carnet_KCAL, KCAL_TOTAL_CARNET - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET - 
                                                CAFE_THE_CARNET)


#Ajout suffixe _KCAL
Carnet_KCAL <- Carnet_KCAL %>%
  rename_with(
    ~ ifelse(
      grepl("_CARNET$", .x),
      paste0(.x, "_Kcal"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )



Carnet_KCAL$UC_TI <- NULL
#Constitution des tableaux finaux ------------------------------------------------
Carnet_id <- Carnet_POIDS  
Carnet_id$Mesure <- "Carnet"
Carnet_id <- left_join(Carnet_id, Carnet_KCAL, by='Identifiant')





#Constitution de data fin l  ------------------------------------------------
fichier_nettoyé <- 
  data.frame(
  Identifiant = resultats_codachats$Identifiant,
  Date = resultats_codachats$Date,
  Lieu = resultats_codachats$Lieu_vf,
  Lieu1 = resultats_codachats$Lieu1,
  Lieu2 = resultats_codachats$Lieu2,
  LibelleCIQUAL = resultats_codachats$LibelleCIQUAL,
  groupe_TI_TdC = resultats_codachats$groupe_TI_TdC1,
  Poids_CSGA = resultats_codachats$Poids_VF,
  Poids_achat = resultats_codachats$Poids_vf,
  Poids_consomme = resultats_codachats$Poids_consomme_vf,
  Prix_CSGA = resultats_codachats$Prix_detail_VF,
  Prix = resultats_codachats$Prix_vf, 
  Prix_Kg = resultats_codachats$Prix_Kg_post_imput,
  Prix_Kg_CSGA = resultats_codachats$Prix_Kg_post_imput,
  Labels = resultats_codachats$Labels,
  Appeciation = resultats_codachats$Appreciation
)


# TELECHARGEMENT ----------------------------

# Créer un nouvel objet workbook
wb <- createWorkbook()

addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", Carnet_id  )

addWorksheet(wb, "Données_brutes_nettoyées")
writeData(wb, sheet = "Données_brutes_nettoyées", fichier_nettoyé )






saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_CSGA.xlsx")))
