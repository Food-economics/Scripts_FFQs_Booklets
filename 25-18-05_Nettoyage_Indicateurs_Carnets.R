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
campaign<-"24-03" #"22-11" #23-02 #"23-11" #"24-03" 
Nj <- 28 #"Nombre de jour de saisie : 28 si 23-11/24-03 #29 sinon 

### Importation des données de Nov_2022 ------------------
Carnet_nov_22 <- read.xlsx("Données analysées - Article N°1 chèques/Fichiers_bruts/22-11_Carnets.xlsx")
Metadata_nov_22 <- read_excel("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_22.xlsx", sheet = "Metadata")

### Importation des données de Mars_2023 ------------------
Carnet_mars_23 <- read_excel("Données analysées - Article N°1 chèques/Fichiers_bruts/23-02_Carnets.xlsx")
Metadata_mars_23 <- read_excel("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_mars_23.xlsx", sheet = "Metadata")
Metadata_mars_23 <- Metadata_mars_23 %>%
  mutate(Identifiant = gsub("-CCAS \\(inclus Pôle emploi et SPF\\)", "-CCAS", Identifiant))

### Importation des données de Nov_2023 -------------------
Carnet_nov_23 <- read_excel("Données analysées - Article N°1 chèques/Fichiers_bruts/23-11_Carnets.xlsx")
Metadata_nov_23 <- read_excel("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_23.xlsx", sheet = "Metadata")

### Importation des données de Mars_2024 --------------------
Saisie_a <- read_excel("Données analysées - Article N°1 chèques/Fichiers_bruts/24-03_Carnets_a.xlsx")
Saisie_b <- read_excel("Données analysées - Article N°1 chèques/Fichiers_bruts/24-03_Carnets_b.xlsx")
Carnet_mars_24 <- full_join(Saisie_a,Saisie_b,by=c("Code","TicketCode","Lieu","Date","CodeCIQUAL","LibelleCIQUAL","Categorie1","Categorie2",
                                                   "Nb","Unite","Prix","Appreciation","Labels","Menu","PrixMenu","LibelleCustom","MontantChequeAlimentaire",
                                                   "DateSaisie","DateMAJ","Photo"))
Metadata_mars_24 <- read_excel("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_mars_24.xlsx", sheet = "Metadata")

### Importation des données des tableaux annexes ----------------
CALNUT<- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/Alim_CALNUT_CODAPPRO_CARNET.xlsx")
magasins <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/Reclassement_magasins.xlsx")
RHD_COL <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/RHD_COL.xlsx")
RHD_COM <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/RHD_COM.xlsx") 
Correction_lieu_nov_22 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/22_11_correction_lieu.xlsx")
Correction_date_nov_22 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/22-11_Correction_date.xlsx")
Correction_lieu_mars_23 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/23_02_correction_lieu.xlsx")
Correction_date_mars_23 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/23-02_Correction_date.xlsx")
Correction_lieu_nov_23 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/23_11_correction_lieu.xlsx")
Correction_date_nov_23 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/23-11_Correction-date.xlsx")
Correction_date_mars_24 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/24-03_Correction-date.xlsx")
Correction_lieu_mars_24 <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/24_03_correction_lieu.xlsx")
Reclassement_Libelle_Custom <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/Reclassement_Libelle_Custom.xlsx")
Reclassement_Groupe_TI <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/Reclassement_groupe_TI.xlsx")
resultats_pondérés <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/moyennes_pondérées.xlsx")
Poids_unitaires_manquants <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/poids_unitaire_manquants.xlsx")
Recap_envoi_cheques <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/Recap_envoi_cheque.xlsx")
resultats_pondérés <- read_excel("Données analysées - Article N°1 chèques/Tableaux_annexes/resultats_pondérés.xlsx")

# NETTOYAGE DU FICHIER COD_ACHATS : LIEUX / DATES / LIBELLE_CUSTOM / LIBELLE_CIQUAL-----------------
#Selection des jeux de données et assignation aux variables resultats_codachats et metadonnees en fonction de la valeur de la variable campagne.
if (campaign == "22-11") { 
  resultats_codachats <- Carnet_nov_22
  metadata <- Metadata_nov_22 } else { 
    if (campaign == "23-02") { 
      resultats_codachats <- Carnet_mars_23
      metadata <- Metadata_mars_23 
    } else { 
      if (campaign == "23-11") { 
        resultats_codachats <- Carnet_nov_23
        metadata <- Metadata_nov_23 
      } else {  
        if (campaign == "24-03")
          resultats_codachats <- Carnet_mars_24
        metadata <- Metadata_mars_24 }}}


## Correction des identifiants ---------------
names(resultats_codachats)[1] = "Identifiant"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="8447-CCAS") ] = "8747-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PE19-CCAS") ] = "PE019-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="1564-Epimut") ] = "1654-Epimut"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="P E013-CCAS") ] = "PE013-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP032-CCAS") ] = "SP040-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP-052-CCAS") ] = "SP052-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP-017-CCAS") ] = "SP017-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="pe003-CCAS") ] = "PE003-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PS004") ] = "LE255"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PS197") ] = "PS161"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE148") ] = "PS284"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE195") ] = "PS285"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE088") ] = "PS286"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE207") ] = "PS287"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE093") ] = "PS288"

print(unique(resultats_codachats$Identifiant))
## Extraction de la composition du foyer et du nombre d'UC TI de metadata nov 22 vers mars 23  -----------------
#Pour la campagne 24-03, les questions relatives à la composition de la famille n'ont pas été reposées et donc le nbre d'UC n'est pas intégré à 
#metadata_mars_23: Pour chaque identifiant, l'information est extraite de la campagne de novembre 2022 et intégrée au ficier metadata 2023. 
if (campaign == "24-03") {
  Ajout <- Metadata_nov_23[, c("Identifiant", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer")]
  metadata <- metadata[, !names(metadata) %in% c("UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer" )]
  metadata <- inner_join(Ajout, metadata,  by = "Identifiant")}

#if (campaign == "23-02") {
#  Ajout <- Metadata_nov_22[, c("Identifiant", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer")]
#  metadata <- metadata[, !names(metadata) %in% c("UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer" )]
#  metadata <- inner_join(Ajout, metadata,  by = "Identifiant")}

#Joindre les montants de chèques envoyés à metadata
metadata <- left_join (metadata, Recap_envoi_cheques, by="Identifiant")

## Integration des variables UC et nombre de personnes à Resultats_codachats ------------
#L'identifiant, la date de saisie, le nombre de personnes du foyer ainsi que le nombre d'Combien.de.personnes.vivent.dans.votre.foyer sont extraits de metadata
#Une jointure est ensuite effectuée avec resultats_codachats. 
temp <- metadata[, c("Identifiant", "Date.de.saisie", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer", "Budget.mensuel.alimentation.", "Budget.hebdomadaire.alimentation.")]
temp$budget_alim <- ifelse(is.na(temp$Budget.mensuel.alimentation.), (4*as.numeric(temp$Budget.hebdomadaire.alimentation.)), (temp$Budget.mensuel.alimentation.))
lignes_vide <- temp[is.na(temp $Date.de.saisie), ]


#Ici, on ne tient pas compte des données vides pour lesquelle une réponse à été prevu mais aucune donnée enregsitrée: ex novembre 2023 / mars 2024. 
#Verif pour voir si la liaison avec les identifiants se fait correctement
#resultats_codachats <- resultats_codachats %>%
#  anti_join(temp,resultats_codachats , by = c("Identifiant"))
# Trouver les identifiants présents dans metadata mais pas dans resultats_codachats
#identifiants_seulement_metadata <- setdiff(metadata$Identifiant, resultats_codachats$Identifiant)
#print("Identifiants présents seulement dans metadata:")
#print(identifiants_seulement_metadata)
# Trouver les identifiants présents dans resultats_codachats mais pas dans metadata
#identifiants_seulement_resultats_codachats <- setdiff(resultats_codachats$Identifiant, metadata$Identifiant)
#print("Identifiants présents seulement dans resultats_codachats:")
#print(identifiants_seulement_resultats_codachats)
#print(unique(valeurs_non_jointes$Identifiant))
resultats_codachats <- left_join(resultats_codachats,temp, by = "Identifiant") 
print(unique(resultats_codachats$Identifiant))





lignes_vide <- resultats_codachats[is.na(resultats_codachats$Date.de.saisie), ]
unique(lignes_vide$Identifiant)
## Correction des lieux --------------
### Jointure de resultats_codachats et des lieux d'approvisionnements corrigés à partir des tableaux controlés par Pascal -----------------------------
# Cette étape permet de réaliser une jointure entre les tableaux de correction des lieux d'approvsionnement précis et les lieux
# renseignés dans resultats_codachats. 
#le code modifie et fusionne les données de resultats_codachats avec différentes sources de correction
#en fonction de la campagne, tout en standardisant le format de la date et en assignant des valeurs à une nouvelle colonne Lieu_vf.
if (campaign == "22-11") {
  resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
  resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")
  #Il y a deux formats de date dans le fichier de correction. Une des dates est donnée sous forme de caractères
  #Lors de la conversion en date R inverse le mois et le jour. Les manip' suivantes viennent corriger celà pour faire la fusion
  Correction_lieu_nov_22$Date1 <-  as.Date(as.numeric(Correction_lieu_nov_22$Date), origin = "1899-12-30")
  #Conversion des dates en chaînes de caractères
  dates_char <- format(Correction_lieu_nov_22$Date1, "%Y-%d-%m")
  #Division des chaînes de caractères en composants
  date_parts <- strsplit(dates_char, "-")
  #Réassemblage des composants dans le bon ordre
  corrected_dates_char <- sapply(date_parts, function(x) {
    paste(x[1], x[2], x[3], sep = "-")
  })
  #Conversion des nouvelles chaînes de caractères en objets Date
  Correction_lieu_nov_22$Date1 <- as.Date(corrected_dates_char, format = "%Y-%m-%d")
  Correction_lieu_nov_22$Date2 <- as.Date(Correction_lieu_nov_22$Date, format = "%m/%d/%Y")
  Correction_lieu_nov_22$Date <-  coalesce(Correction_lieu_nov_22$Date1, Correction_lieu_nov_22$Date2)
  Correction_lieu_nov_22 <- Correction_lieu_nov_22[, !names(Correction_lieu_nov_22) %in% c("Date1", "Date2")]
  #Supprimer les espaces superflus
  resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
  Correction_lieu_nov_22$Lieu <- trimws(Correction_lieu_nov_22$Lieu)
  #Vérifier les différences de casse
  resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
  Correction_lieu_nov_22$Lieu <- tolower(Correction_lieu_nov_22$Lieu)
  #Supprimer les caractères invisibles
  resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
  Correction_lieu_nov_22$Lieu <- iconv(Correction_lieu_nov_22$Lieu, to = "ASCII//TRANSLIT")
  #Verif que les valeurs de correction sont appliquées au tableau 
  valeurs_non_jointes <- Correction_lieu_nov_22 %>%
    anti_join(resultats_codachats, by = c("Lieu", "Date","Identifiant"))
  print(valeurs_non_jointes)
  resultats_codachats <- left_join(resultats_codachats, Correction_lieu_nov_22, by=c("Lieu", "Date","Identifiant"))
  resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
  #Verif que les valeurs de correction se retrouver bien dans lieu cor : si oui : NA 
  temp<- setdiff(Correction_lieu_nov_22$Lieu_cor, resultats_codachats$Lieu_vf)
  print(unique((resultats_codachats$Lieu_cor)))
}else{ 
  if (campaign == "23-02") {
    resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
    resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")
    Correction_lieu_mars_23$Date1 <-  as.Date(as.numeric(Correction_lieu_mars_23$Date), origin = "1899-12-30")
    dates_char <- format(Correction_lieu_mars_23$Date1, "%Y-%d-%m")
    date_parts <- strsplit(dates_char, "-")
    corrected_dates_char <- sapply(date_parts, function(x) {
      paste(x[1], x[2], x[3], sep = "-")
    })
    Correction_lieu_mars_23$Date1 <- as.Date(corrected_dates_char, format = "%Y-%m-%d")
    Correction_lieu_mars_23$Date2 <- as.Date(Correction_lieu_mars_23$Date, format = "%m/%d/%Y")
    Correction_lieu_mars_23$Date <-  coalesce(Correction_lieu_mars_23$Date1, Correction_lieu_mars_23$Date2)
    Correction_lieu_mars_23 <- Correction_lieu_mars_23[, !names(Correction_lieu_mars_23) %in% c("Date1", "Date2")]
    #Supprimer les espaces superflus
    resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
    Correction_lieu_mars_23$Lieu <- trimws(Correction_lieu_mars_23$Lieu)
    #Vérifier les différences de casse
    resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
    Correction_lieu_mars_23$Lieu <- tolower(Correction_lieu_mars_23$Lieu)
    #Supprimer les caractères invisibles
    resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
    Correction_lieu_mars_23$Lieu <- iconv(Correction_lieu_mars_23$Lieu, to = "ASCII//TRANSLIT")
    #Verif que les valeurs de correction sont appliquées au tableau 
    valeurs_non_jointes <- Correction_lieu_mars_23 %>%
      anti_join(resultats_codachats, by = c("Lieu", "Date","Identifiant"))
    print(valeurs_non_jointes)
    resultats_codachats <- left_join(resultats_codachats, Correction_lieu_mars_23, by=c("Lieu", "Date","Identifiant"))
    resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
    #Verif que les valeurs de correction se retrouver bien dans lieu cor : si oui : NA 
    temp<- setdiff(Correction_lieu_mars_23$Lieu_cor, resultats_codachats$Lieu_vf)
    print(temp)
    print(unique((resultats_codachats$Lieu_cor)))
  } else {
    if (campaign == "23-11") {
      resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
      resultats_codachats$Date <- as.Date(resultats_codachats$Date , format="%Y-%m-%d")
      Correction_lieu_nov_23$Date <- as.Date(Correction_lieu_nov_23$Date, format="%Y-%m-%d")
      #Supprimer les espaces superflus
      resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
      Correction_lieu_nov_23$Lieu <- trimws(Correction_lieu_nov_23$Lieu)
      #Vérifier les différences de casse
      resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
      Correction_lieu_nov_23$Lieu <- tolower(Correction_lieu_nov_23$Lieu)
      #Supprimer les caractères invisibles
      resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
      Correction_lieu_nov_23$Lieu <- iconv(Correction_lieu_nov_23$Lieu, to = "ASCII//TRANSLIT")
      #Verif que les valeurs de correction sont appliquées au tableau 
      valeurs_non_jointes <- Correction_lieu_nov_23 %>%
        anti_join(resultats_codachats,  Correction_lieu_nov_23,by = c("Lieu", "Date","Identifiant"))
      print(valeurs_non_jointes)
      resultats_codachats <- left_join(resultats_codachats, Correction_lieu_nov_23, by=c("Lieu", "Date","Identifiant"))
      resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
      #Verif que les valeurs de correction se retrouver bien dans lieu cor : si oui : NA 
      temp<- setdiff(Correction_lieu_nov_23$Lieu_cor, resultats_codachats$Lieu_vf)
      print(temp)
      print(unique((resultats_codachats$Lieu_cor)))
    } else {
      if (campaign == "24-03") {
        resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
        resultats_codachats$Date <- as.Date(resultats_codachats$Date , format="%Y-%m-%d")
        Correction_lieu_mars_24$Date <- as.Date(Correction_lieu_mars_24$Date, format="%Y-%m-%d")
        #Supprimer les espaces superflus
        resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
        Correction_lieu_mars_24$Lieu <- trimws(Correction_lieu_mars_24$Lieu)
        #Vérifier les différences de casse
        resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
        Correction_lieu_mars_24$Lieu <- tolower(Correction_lieu_mars_24$Lieu)
        #Supprimer les caractères invisibles
        resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
        Correction_lieu_mars_24$Lieu <- iconv(Correction_lieu_mars_24$Lieu, to = "ASCII//TRANSLIT")
        #Verif que les valeurs de correction sont appliquées au tableau 
        valeurs_non_jointes <- Correction_lieu_mars_24 %>%
          anti_join(resultats_codachats,  Correction_lieu_mars_24,by = c("Lieu", "Date","Identifiant"))
        print(valeurs_non_jointes)
        resultats_codachats <- left_join(resultats_codachats, Correction_lieu_mars_24, by=c("Lieu", "Date","Identifiant"))
        resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
        #Verif que les valeurs de correction se retrouver bien dans lieu cor : si oui : NA 
        temp<- setdiff(Correction_lieu_mars_24$Lieu_cor, resultats_codachats$Lieu_vf)
        print(temp)
        print(unique((resultats_codachats$Lieu_cor)))
      }}}}

#print(unique(resultats_codachats$Identifiant))




### Jointure des lieux d'approvisionnement et de la classidication par type d'enseigne "Lieu 1" / "Lieu2"----------
#Supprimer les espaces superflus
resultats_codachats$Lieu_vf <- trimws(resultats_codachats$Lieu_vf)
magasins$Lieu_vf <- trimws(magasins$Lieu_vf)
#Vérifier les différences de casse
resultats_codachats$Lieu_vf <- tolower(resultats_codachats$Lieu_vf)
magasins$Lieu_vf <- tolower(magasins$Lieu_vf)
#Supprimer les caractères invisibles
resultats_codachats$Lieu_vf<- iconv(resultats_codachats$Lieu_vf, to = "ASCII//TRANSLIT")
magasins$Lieu_vf<- iconv(magasins$Lieu_vf, to = "ASCII//TRANSLIT")
#valeurs_non_jointes <- resultats_codachats  %>%
#  anti_join(magasins, by = c("Lieu_vf"))

resultats_codachats <- inner_join(resultats_codachats, magasins, by=c("Lieu_vf")) 
describe(is.na(resultats_codachats$Lieu2))

#Supprimer les colonnes inutiles
if (campaign == "22-11" |campaign == "23-02"|campaign == "23-11") {
  resultats_codachats <- subset(resultats_codachats, select = -c(Photo, Nom_photo, Lieu_cor, Observation))}

## Correction des dates ---------------------------------
### Initialisation des bornes de date de début et date de fin 
#le code ajuste les dates de début et de fin des périodes dans resultats_codachats en 
#fonction de la campagne, après avoir converti la date de saisie en format date. Les périodes diffèrent légèrement selon la campagne spécifiée.
# Pour la campagne 22-23 : les personnes ont commencé la saisie le jour de reception du carnet d'appro.
# Pour la campagne 23-24 : les personnes ont commencé la saisie le lendemain  de leur réponse au FFQ.
#Pour la campagne 22-23, la saisie dure 29 jours et 28 jours en 23-24.

# Conversion des dates dans le format correct
formats <- c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y")
resultats_codachats$Date <- parse_date_time(resultats_codachats$Date, orders = formats)
resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")

#Définition des dates de début 
resultats_codachats$date_starting <- as.Date(resultats_codachats$Date.de.saisie, format = "%d/%m/%Y")
if (campaign == "22-11" |campaign == "23-02") {
  #29 jours de saisie en campagne 22-23
  resultats_codachats$Date_début <- resultats_codachats$date_starting 
  resultats_codachats$Date_fin <- resultats_codachats$date_starting +28
} else if (campaign == "23-11" | campaign == "24-03") {
  #28 jours de saisie en campagne 23-24
  resultats_codachats$Date_début <- resultats_codachats$date_starting+1
  resultats_codachats$Date_fin <- resultats_codachats$date_starting +28}
lignes_vide <- resultats_codachats[is.na(resultats_codachats$Date_début), ]


### Suppression des données saisies au mauvais lien pour novembre 22 et novembre 23---------------------
if (campaign == "22-11") {
  start_date <- as.Date('2023-09-12')
  end_date <- as.Date('2023-10-25')
  donnees_a_supprimer <- resultats_codachats %>%
    filter(DateSaisie >= start_date & DateSaisie <= end_date)
  #3580 observations à supprimer : il doit rester 24399 observations
  resultats_codachats <- resultats_codachats %>%
    filter(is.na(DateSaisie) | !(DateSaisie >= start_date & DateSaisie <= end_date))}

if (campaign == "23-11") {
  start_date <- as.Date('2024-03-01')
  end_date <- as.Date('2024-03-31')
  # Supprimer les dates en mars : il doit rester 31190 observations
  dates_mars <- resultats_codachats %>%
    filter(Date >= start_date & Date < end_date)
  resultats_codachats <- resultats_codachats %>%
    filter(is.na(Date) | Date < start_date | Date > end_date) } 

### Modification des bornes de fin de saisie pour les personnes ayant pris des vacances-----------------
resultats_codachats <- resultats_codachats %>%
  mutate(Date_fin = case_when(
    campaign == "24-03" & Identifiant %in% c("LE041", "LE043", "PS192", "PS194", "PS208", "PS229", "PS244") ~ date_starting + 36, #29 jours + Ajout de 7 jours 
    campaign == "24-03" & Identifiant == "PS267" ~ date_starting + 43, #Ajout de deux semaines : 29 jours + Ajout de 14 jours
    TRUE ~ Date_fin
  ))

#Verif sur qq id pour voir si le filtre fonctionne bien 
temp <- resultats_codachats %>% filter (resultats_codachats$Identifiant == "PS267") 

### Attribution d'une date aléatoire comprise dans l'intervalle de saisie pour chaque ticket saisi + de 56 jours avant le début de la saisie et après la fin de celle-ci-----------
#### Fonction pour générer une date aléatoire entre Date_début et Date_fin---------------
#Les arguments Date_début et Date_fin sont convertis en objets de classe Date pour assurer que les opérations suivantes sont effectuées sur des dates valides.
generate_random_date <- function(Date_début, Date_fin) { 
  Date_début <- as.Date(Date_début)
  Date_fin <- as.Date(Date_fin)
  #Si l'une des dates est NA, la fonction retourne NA, ce qui évite les erreurs de calcul sur des valeurs non définies.
  if (is.na(Date_début) || is.na(Date_fin)) { return(NA) }
  #La différence en jours entre Date_fin et Date_début est calculée en utilisant la fonction difftime, puis convertie en un nombre numérique.
  diff_days <- as.numeric(difftime(Date_fin, Date_début, units = "days"))
  #Un nombre aléatoire de jours est généré entre 0 et diff_days en utilisant la fonction sample.
  random_days <- sample(0:diff_days, 1)
  #La date aléatoire est obtenue en ajoutant le nombre de jours aléatoire à Date_début.
  Date_aléatoire <- Date_début + random_days
  return(Date_aléatoire)
}

#### Appliquer la fonction à chaque ligne du dataframe------------

resultats_codachats <- resultats_codachats %>%
  rowwise() %>%
  mutate(Date_aléatoire = generate_random_date(Date_début, Date_fin))




#### Condition pour remplacer les dates en dehors de la plage de 56 jours--------------
#En résumé, cette transformation vérifie pour chaque ligne de resultats_codachats si la colonne Date est en dehors de la plage 
#comprise entre 56 jours avant Date_début et 56 jours après Date_fin. Si c'est le cas, la valeur de Date est remplacée par Date_aléatoire. Sinon, Date conserve sa valeur d'origine.
resultats_codachats <- resultats_codachats %>%
  mutate(Date = case_when(
    !is.na(Date) & (Date < Date_début - 56 | Date > Date_fin + 56) ~ Date_aléatoire,
    TRUE ~ Date
  ))

resultats_codachats$Date <- as.Date(resultats_codachats$Date, origin = "1970-01-01")

### Verifier  s'il y a encore des tickets en dehors des bornes ----------
#A_verifier <-resultats_codachats %>% filter(Date < Date_début| Date > Date_fin)
#tickets_a_verifier <- A_verifier %>%
#  select(Identifiant, Lieu, Date) %>%
#  distinct()
# Définir le chemin du répertoire où sont stockées vos photos
#repertoire_photos <- "E:/TdC_novembre_2023"

# Lister tous les fichiers dans le répertoire
#photos <- list.files(path = repertoire_photos)
# Convertir la liste des noms de fichiers en un data frame
#df_photos <- data.frame(Noms_de_Photos = photos)
# Extraire l'identifiant et la date de chaque nom de fichier
#df_photos$Identifiant <- sub("^(.*)_.*$", "\\1", df_photos$Noms_de_Photos)
#df_photos$Date <- sub("^.*_(\\d{8}).*$", "\\1", df_photos$Noms_de_Photos)
# Transformer la date au format "AAAA-MM-JJ"
#df_photos$Date <- as.Date(df_photos$Date, format = "%Y%m%d")
# Supprimer la colonne Noms_de_Photos et les lignes en doublon
#df_photos <- df_photos %>%
#  select(-Noms_de_Photos) %>%
#  distinct()
#df_photos$Message <- "La_photo_existe" 

# Effectuer une jointure pour vérifier les correspondances
#result <- left_join(tickets_a_verifier, df_photos, by = c("Identifiant", "Date"))
#write.xlsx(result,(paste0(campaign,"_carnet_appro/verif2.xlsx")))
#write.xlsx(tickets_a_verifier,(paste0(campaign,"_carnet_appro/verif.xlsx")))

### Application des corrections manuelles : tous les tickets existants hors des bornes ont été vérifiés : la date est corrigée / supprimée ou laissée en suspens si le ticket est illisible. --------------
# Conversion des colonnes Date en format Date
# Pour toutes les valeurs en dehors des bornes et saisis jusqu'à 56 jours avant et après la date de saisie, la date a été vérifiée si la photo était disponible. 
#resultats_codachats$Date <- as.Date(resultats_codachats$Date, format="%Y-%m-%d")

# Logique basée sur la campagne
if (campaign == "22-11") {
  Correction_date <- Correction_date_nov_22
} else if (campaign == "23-02") {
  Correction_date <- Correction_date_mars_23
} else if (campaign == "23-11") {
  Correction_date <- Correction_date_nov_23
} else if (campaign == "24-03") {
  Correction_date <- Correction_date_mars_24
}

# Conversion des colonnes Date_corrigée en format Date si elles existent
Correction_date$Date_corrigée <- as.Date(Correction_date$Date_corrigée, format="%Y-%m-%d")
Correction_date$Date <- as.Date(Correction_date$Date, format="%Y-%m-%d")
Correction_date$Lieu <- trimws(Correction_date$Lieu)
#Vérifier les différences de casse
Correction_date$Lieu <- tolower(Correction_date$Lieu)
#Supprimer les caractères invisibles
Correction_date$Lieu<- iconv(Correction_date$Lieu, to = "ASCII//TRANSLIT")
# Jointure à gauche entre resultats_codachats et Correction_date
valeurs_non_jointes <- Correction_date %>%
  anti_join(resultats_codachats, by = c("Lieu", "Date", "Identifiant"))

resultats_codachats <- left_join(resultats_codachats, Correction_date, by=c("Lieu", "Date", "Identifiant"))
dup_rows <- Correction_date[duplicated(Correction_date[, c("Identifiant", "Date", "Lieu")]), ]
print(dup_rows)
print(unique(resultats_codachats$Date_corrigée))
print(unique(resultats_codachats$Identifiant))

# Utilisation de coalesce pour créer une nouvelle colonne Date_vf
resultats_codachats$Date_vf <- coalesce(resultats_codachats$Date_corrigée, resultats_codachats$Date)

# Conversion de Date_vf en format Date
resultats_codachats$Date_vf <- as.Date(resultats_codachats$Date_vf, format="%Y-%m-%d")

# Filtrage des lignes où A supprimer n'est pas NA
resultats_codachats <- resultats_codachats %>%
  filter(is.na(`A supprimer`)) 
# Sélection des colonnes à conserver dans resultats_codachats
print(unique(resultats_codachats$`A supprimer`))



### Verifier  s'il y a encore des tickets en dehors des bornes ----------
#A_verifier <-resultats_codachats %>% filter(Date_vf < Date_début| Date_vf > Date_fin)
#tickets_a_verifier <- A_verifier %>%
#  select(Identifiant, Lieu, Date) %>%
#  distinct()

### Pour les tickets manquants ou illisibles : comparer la date avec la date de saisie et appliquer les corrections suivantes -----------------------
#Si l'année du ticket est différent de l'année de saisie : appliquer l'année de saisie
#Idem pour le mois
#Si ni le mois ni l'année concorndent alors supprimer la date
#La première ligne if (campaign == "23-11" | campaign == "24-03") vérifie si la variable campaign est égale à "23-11" ou "24-03"
if (campaign == "23-11" | campaign == "24-03") {
  if (all(resultats_codachats$Date_vf < resultats_codachats$Date_début | resultats_codachats$Date_vf > resultats_codachats$Date_fin)) {
    
    resultats_codachats <- resultats_codachats %>%
      mutate(
        Date_finale = case_when(
          (Date_vf > Date_fin | Date_vf < Date_début) ~
            case_when(
              year(DateSaisie) != year(Date_vf) ~ make_date(year(DateSaisie), month(Date_vf), day(Date_vf)),
              month(DateSaisie) != month(Date_vf) ~ make_date(year(Date_vf), month(DateSaisie), day(Date_vf)),
              (year(DateSaisie) != year(Date_vf) & month(DateSaisie) != month(Date_vf)) ~ as.Date(DateSaisie),
              TRUE ~ Date_vf
            ),
          TRUE ~ Date_vf
        )
      )
    
    resultats_codachats$Date_vf <- resultats_codachats$Date_finale
  }
}

# Liste des identifiants à filtrer
#identifiants_a_filtrer <- c("1730-Epimut", "A1-Epimut", "6222-Episourire", "6273-Episourire", "SP041-CCAS")
#Frame_filtre <- resultats_codachats[resultats_codachats$Identifiant %in% identifiants_a_filtrer, ]
#print(unique(Frame_filtre$Identifiant))
#print(unique(resultats_codachats$Identifiant))
#temp <- resultats_codachats[is.na(resultats_codachats$Date_début) | as.Date(resultats_codachats$Date_vf) > as.Date(resultats_codachats$Date_début), ]
#temp <-resultats_codachats[is.na(resultats_codachats$Date_début) | as.Date(resultats_codachats$Date_vf) < as.Date(resultats_codachats$Date_fin), ]
#ids_in_resultats_not_in_temp <- setdiff(resultats_codachats$Identifiant, temp$Identifiant)
# Finding IDs present in temp but not in resultats_codachats
#ids_in_temp_not_in_resultats <- setdiff(temp$Identifiant, resultats_codachats$Identifiant)
# Output the differences
#cat("IDs in resultats_codachats but not in temp:\n")
#print(ids_in_resultats_not_in_temp)
#cat("\nIDs in temp but not in resultats_codachats:\n")
#print(ids_in_temp_not_in_resultats)
#print(unique(resultats_codachats$Identifiant))

#Corriger les noms d'enseignes 
resultats_codachats$Lieu_vf [resultats_codachats$Lieu_vf== "epi'sourire - dijon(place jeacques prevert)" ] <- "epi'sourire - dijon (place jeacques prevert)"

#Supprimer les données opticourses
# Stocker les lignes avant le filtrage
#resultats_avant_filtre <- resultats_codachats
#Supprimer les lignes dont date de saisie est nul : correspond aux personnes opticourses
#resultats_codachats <- resultats_codachats %>%
#  filter(!is.na(Date_début))
# Trouver les lignes supprimées en comparant avant et après filtrage
#lignes_supprimees <- anti_join(resultats_avant_filtre, resultats_codachats)
# Afficher les lignes supprimées
#lignes_supprimees
#unique(lignes_supprimees$Identifiant)


### Supprimer les données hors des bornes  ----------------------
# Appliquer le filtrage avec case_when
resultats_avant_filtre <- resultats_codachats
resultats_codachats <- resultats_codachats %>%
  filter(
    case_when(
      is.na(Date_vf) ~ TRUE, # Garde les lignes où Date_vf est NA
      as.Date(Date_vf) >= as.Date(Date_début) & as.Date(Date_vf) <= as.Date(Date_fin) ~ TRUE, 
      TRUE ~ FALSE # Exclut toutes les autres lignes
    )
  )

# Trouver les lignes supprimées en comparant avant et après filtrage
lignes_supprimees <- anti_join(resultats_avant_filtre, resultats_codachats)
# Afficher les lignes supprimées
lignes_supprimees


# Calcul de la semaine relative
resultats_codachats$semaine_num <- floor(as.numeric(resultats_codachats$Date - resultats_codachats$date_starting) / 7) + 1
# Plafonner à 4
resultats_codachats$semaine_num <- pmin(resultats_codachats$semaine_num, 4)
# On ne garde que les lignes où semaine_num >= 1
resultats_codachats <- resultats_codachats[
  resultats_codachats$semaine_num >= 1,
]


## Association Cod-AChats et Calnut ---------------------------
#FUSION du réferentiel CALNUT et du fichier de saisie "resultats_Codachats" 
#On renomme sur le fichier de saisie : "CodeCIQUAL" en "CODACHATS_alim_code" et "Categorie1" en "groupe_TI_TdC".
# resultats_codachats et CALNUT sont associés par CODACHATS_alim_code
colnames(resultats_codachats)[colnames(resultats_codachats) == 'CodeCIQUAL'] <- 'CODACHATS_alim_code'
resultats_codachats <- left_join(resultats_codachats, CALNUT, by=c("CODACHATS_alim_code"))

#Rename "Category1" with groupe_TI_TdC
colnames(resultats_codachats)[colnames(resultats_codachats) == 'Categorie1'] <- 'groupe_TI_TdC1'

###  Assignation des aliments en Libelle_Custom à un groupe TI-----------------
resultats_codachats<- left_join(resultats_codachats, Reclassement_Libelle_Custom, by=c("LibelleCustom"), relationship = "many-to-many")

#diff_df1_df2 <- setdiff(resultats_codachats_test$LibelleCustom, resultats_codachats$LibelleCustom)
#diff_df2_df1 <- setdiff(resultats_codachats$LibelleCustom, resultats_codachats_test$LibelleCustom)

resultats_codachats$Categorie2 <- toupper(resultats_codachats$Categorie2 )

temp <- ifelse(( is.na(resultats_codachats$groupe_TI_TdC1)),(resultats_codachats$Reclassement_TI),(resultats_codachats$groupe_TI_TdC1))
resultats_codachats$groupe_TI_TdC1 <- temp
resultats_codachats$groupe_TI_TdC1 <- ifelse((is.na(resultats_codachats$groupe_TI_TdC)),(resultats_codachats$groupe_TI_TdC1),(resultats_codachats$groupe_TI_TdC))

### Correction des erreurs de classification, groupe Ti_TDC ---------------
resultats_codachats<- left_join(resultats_codachats, Reclassement_Groupe_TI, by=c("groupe_TI_TdC1"), relationship = "many-to-many")
temp <- ifelse(( is.na(resultats_codachats$New)),(resultats_codachats$groupe_TI_TdC1),(resultats_codachats$New))
resultats_codachats$groupe_TI_TdC1 <- temp
describe(!is.na(resultats_codachats$Categorie2))
print(unique(resultats_codachats$Identifiant))


### Supprimer les lignes qui apparaissent en double : supprimer les montants de chèque en double 
temp <- resultats_codachats %>%
  # Séparer les lignes avec "Montant_cheque" non vide et appliquer distinct()
  filter(!is.na(MontantChequeAlimentaire) & MontantChequeAlimentaire != "") %>%
  distinct() %>%
  # Ajouter les lignes où "Montant_cheque" est vide ou NA
  bind_rows(resultats_codachats %>% filter(is.na(MontantChequeAlimentaire) | MontantChequeAlimentaire == ""))
resultats_codachats  <- temp 

#Suppression cafe the et condiments
resultats_codachats <- subset(
  resultats_codachats,
  !groupe_TI_TdC1 %in% c("CAFE_THE", "EPICES_CONDIMENTS")
)

#22-11 MEAN : 18  MEDIAN 13  --> Epiceries MEAN 15 / MEDIAN  12
#23-02 : MEAN : 21 / MEDIAN 16 --> Epiceries MEAN 19 / MEDIAN  15
#23-11 :  MEAN 31 /  MEDIAN : 22 --> LE : MEAN 29 / MEDIAN 18.5
#24/03 : MEAN 40 / MEDIAN 36 --> LE : MEAN : 39 / MEDIAN 36

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
grouped_data <- resultats_codachats %>% group_by(Date_vf, Lieu_vf)
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


resultats_codachats$Poids <-ifelse((resultats_codachats$Unite == "unités" & is.na(resultats_codachats$Poids) ),(NA),(resultats_codachats$Poids))
resultats_codachats$Nb <- ifelse((resultats_codachats$Nb== 0 ), (NA), (resultats_codachats$Nb))
resultats_codachats$Poids <- ifelse((resultats_codachats$Poids == 0 ), (NA), (resultats_codachats$Poids))
resultats_codachats$Prix_all <- ifelse((resultats_codachats$Prix == 0 & resultats_codachats$Lieu1!= "dons" ), (NA), (resultats_codachats$Prix_all ))

describe(is.na(resultats_codachats$Poids))
describe(is.na(resultats_codachats$Prix_all))


#resultats_codachats<-  resultats_codachats %>%
#  left_join( Poids_unitaires_manquants, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$Unite =="unités"  & is.na(resultats_codachats$Poids)  & is.na(resultats_codachats$Prix) ), (resultats_codachats$Poids_uni*resultats_codachats$Nb), (resultats_codachats$Poids))

describe(is.na(resultats_codachats$Poids))

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
print(unique(resultats_codachats$Identifiant))
describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

### Imputation des poids des  aliments RHD----------
####RHD_COL 
resultats_codachats<-  resultats_codachats %>%
  left_join(RHD_COL, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COL" & resultats_codachats$Unite =="unités"), (resultats_codachats$Poids_RHD_COL), (resultats_codachats$Poids_vf))
resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COL" ), (resultats_codachats$Poids_RHD_COL), (resultats_codachats$Poids_vf))

#RHD_COM
resultats_codachats<-  resultats_codachats %>%
  left_join(RHD_COM, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COM" & resultats_codachats$Unite =="unités"), (resultats_codachats$Poids_RHD_COM), (resultats_codachats$Poids_vf))
resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COM" ), (resultats_codachats$Poids_RHD_COM), (resultats_codachats$Poids_vf))

print(unique(resultats_codachats$Identifiant))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

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

#Nov 22 : Poids vf : 1,1% Prix vf : 1,2%
#Mars 23 : Poids :1%, Prix vf: 1,6%
#Nov 23 : Poids vf :0;9%, Prix vf: 13%
#Mars 24 : Poids vf : 0,5 et  prix vf : 10%


lignes_vide <- resultats_codachats[is.na(resultats_codachats$Prix_Kg_post_imput), ]


## Suppression des identifiants qui enregistrent une dépense inférieure à 25% du budget alimentaire déclaré---------
# Calculer la somme des prix_vf par identifiant
somme_prix_vf <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(somme_prix_vf = sum(Prix_vf, na.rm = TRUE),)

# Calculer le quart du budget alimentaire
valeurs_uniques_budget <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(budget_unique = unique(budget_alim))

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



# CALCUL DES INDICATEURS ------------------
  ## Poids (Kg/personne/jour) ----------------------------------
#Pour lier les quantités COD-Appro et FFQ, il faut systématiquement multiplier 
#le poids des fournitures par yield_factor*pct_conso : 
#On obtient ainsi le poids consommé pour les carnets de fournitures. 
resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD"),(resultats_codachats$Poids_vf*resultats_codachats$yield_factor*resultats_codachats$pct_conso),(resultats_codachats$Poids_vf))
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf

resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # si le prix est zéro → 0
      Poids_consomme_vf == 0          ~ NA_real_,    # si le poids est zéro → NA
      TRUE                    ~ Prix_vf / Poids_consomme_vf
    )
  )
###POIDS PAR SEMAINE -----------------------------------------------------------------------

# Calcul de la semaine relative
resultats_codachats$semaine_num <- floor(as.numeric(resultats_codachats$Date_vf - resultats_codachats$date_starting) / 7) + 1
# Plafonner à 4
resultats_codachats$semaine_num <- pmin(resultats_codachats$semaine_num, 4)
tab <- table(resultats_codachats$semaine_num)

pct <- prop.table(tab) * 100
round(pct, 1)

# On récupère la liste des semaines présentes
semaines <- sort(unique(resultats_codachats$semaine_num))

for (sem in semaines) {
  message(">>> Traitement de la semaine ", sem)
  # 1) On découpe le sous-jeu de données
  df_sem <- subset(resultats_codachats, semaine_num == sem)
  
  # ——————— Poids (Kg/personne/jour) ———————
  # Création de Nourriture_consommee
  Nourriture_consommee <- data.frame(
    Identifiant        = df_sem$Identifiant,
    groupe_TI_TdC      = df_sem$groupe_TI_TdC1,
    Poids_vf_consommee = df_sem$Poids_consomme_vf
  )
  # colonnes par catégorie
  categories <- unique(Nourriture_consommee$groupe_TI_TdC)
  for (categorie in categories) {
    col <- paste0(categorie, "_CARNET")
    Nourriture_consommee[[col]] <- ifelse(
      Nourriture_consommee$groupe_TI_TdC == categorie,
      Nourriture_consommee$Poids_vf_consommee,
      0
    )
  }
  # on supprime les anciennes colonnes
  Nourriture_consommee <- subset(
    Nourriture_consommee,
    select = -c(groupe_TI_TdC, Poids_vf_consommee, NA_CARNET)
  )
  
  # Agrégation Carnet_POIDS
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  
  # UC_TI moyen par Identifiant
  Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, df_sem, mean)
  # agrégation de chaque colonne « *_CARNET »
  colonnes_poids <- names(Nourriture_consommee)[-1]
  for (colonne in colonnes_poids) {
    Temp <- aggregate(
      formula(paste0(colonne, " ~ Identifiant")),
      data = Nourriture_consommee,
      FUN  = sum
    )
    Carnet_POIDS <- left_join(Carnet_POIDS, Temp, by = "Identifiant")
  }
  Carnet_POIDS$AUTRE_CARNET <- NULL
  
  # normalisation par UC_TI * 7
  Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <-
    Carnet_POIDS[, 3:ncol(Carnet_POIDS)] /
    (Carnet_POIDS$UC_TI *7 )
  
  Carnet_POIDS$SOMME_CARNET_POIDS <- rowSums(
    Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE
  )
  Carnet_POIDS$SOMME_CARNET_HORS_BOISSON <- with(
    Carnet_POIDS,
    SOMME_CARNET_POIDS -
      ALCOOL_CARNET -
      FRUITS_JUS_CARNET -
      LAIT_CARNET -
      EAU_CARNET -
      SODAS_LIGHT_CARNET -
      SODAS_SUCRES_CARNET
  )
  assign(paste0("Carnet_POIDS_sem", sem), Carnet_POIDS)
  
  # On renomme toutes les colonnes sauf Identifiant
  Carnet_POIDS <- Carnet_POIDS %>%
    rename_with(~ paste0(.x, "_sem", sem), -Identifiant)
  
  # on crée l’objet Carnet_POIDS_sem1, _sem2, …
  assign(paste0("Carnet_POIDS_sem", sem), Carnet_POIDS)

}

###Par jour -------------------
resultats_codachats$jour_num <- floor(as.numeric(resultats_codachats$Date_vf - resultats_codachats$date_starting) + 1)
tab <- table(resultats_codachats$jour_num)
#Les identifiants qui dépassent 29 sont ceux qui ont pris des vacances

pct <- prop.table(tab) * 100
round(pct, 1)

identifiants_30_plus <- resultats_codachats %>%
  filter(jour_num > 29) %>%      # on ne garde que les jours au-delà de 29
  distinct(Identifiant) %>%      # on sélectionne les identifiants uniques
  arrange(Identifiant)    


print(unique(identifiants_30_plus$Identifiant))
# 1) Prendre en compte UC_TI et sommer par ID × catégorie × jour

tab_cumul_cat_norm <- resultats_codachats %>%
  # 1) Remplir UC_TI manquants par Identifiant
  group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>%
  ungroup() %>%
  
  # 2) Somme des poids journaliers par catégorie
  group_by(Identifiant,
           categorie = groupe_TI_TdC1,
           jour_num,
           UC_TI) %>%
  summarise(poids_jour = sum(Poids_consomme_vf, na.rm = TRUE),
            .groups = "drop") %>%
  
  # 3) Ne garder que les jours 1 à 29
  filter(jour_num <= 29) %>%
  
  # 4) Compléter les jours manquants 1–29 à 0
  complete(
    Identifiant, categorie,
    jour_num = 1:29,
    fill = list(poids_jour = 0)
  ) %>%
  arrange(Identifiant, categorie, jour_num) %>%
  
  # 5) Calcul du cumul par catégorie
  group_by(Identifiant, categorie) %>%
  mutate(cumul_cat = cumsum(poids_jour)) %>%
  ungroup()

tab_cumul_cat_norm <- tab_cumul_cat_norm %>%
  filter(jour_num <= 29)

tab_cumul_cat_norm <- tab_cumul_cat_norm %>%
  group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>%
  ungroup()

tab_cumul_cat_norm$poids_jour <- tab_cumul_cat_norm$poids_jour / tab_cumul_cat_norm$UC_TI
tab_cumul_cat_norm$cumul_cat <- tab_cumul_cat_norm$cumul_cat / tab_cumul_cat_norm$UC_TI


## Verification du prix au kg des données imputées = ---------------------------
#On calcule le prix au kg. 





Nourriture_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$Poids_consomme_vf  )
names(Nourriture_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","Poids_vf_consommee")
#print(unique(Nourriture_consommee$groupe_TI_TdC))
# Liste des catégories de nourriture
categories <- unique(Nourriture_consommee$groupe_TI_TdC)
# Boucle pour créer les colonnes correspondantes dans Nourriture_consommee
for (categorie in categories) {
  Nourriture_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Nourriture_consommee$groupe_TI_TdC == categorie, Nourriture_consommee$Poids_vf_consomme, 0)}
# Supprimer les colonnes "groupe_TI_TdC" et "Poids_vf_consomme" si besoin
Nourriture_consommee <- subset(Nourriture_consommee, select = -c(groupe_TI_TdC, Poids_vf_consommee, NA_CARNET))


# Agrégation pour le dataframe Carnet_POIDS
Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
resultats_codachats$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(resultats_codachats$Combien.de.personnes.vivent.dans.votre.foyer)
#Carnet_POIDS <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
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
#Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$Combien.de.personnes.vivent.dans.votre.foyer*Nj)
# Calculer la somme des colonnes pour chaque ligne
Carnet_POIDS$SOMME_CARNET_POIDS <- rowSums(Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE)
# Calculer la somme des colonnes hors boisson
Carnet_POIDS$SOMME_CARNET_HORS_BOISSON <- with(Carnet_POIDS, SOMME_CARNET_POIDS - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET)



  ## Kcal (Kcal/personne/jour)------------------------------------------
#Pour chaque aliment, nous imputons sa valeur nutritionnelle en kcal / kg en fonction du poids consommé de chaque aliment.
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf
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
#Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$Combien.de.personnes.vivent.dans.votre.foyer * Nj)
Carnet_KCAL$AUTRE_CARNET <- NULL
Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$UC_TI* Nj)
# Calculer la somme des colonnes  pour chaque ligne
Carnet_KCAL$SOMME_CARNET_KCAL <- rowSums(Carnet_KCAL[, 3:ncol(Carnet_KCAL)], na.rm = TRUE)
# Calculer la somme des colonnes hors boisson
Carnet_KCAL$SOMME_CARNET_HORS_BOISSON <- with(Carnet_KCAL, SOMME_CARNET_KCAL - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET)

Carnet_KCAL$KCAL_SANS_ALCOOL <-  with(Carnet_KCAL, SOMME_CARNET_KCAL - ALCOOL_CARNET )
Carnet_KCAL$KCAL_SANS_BOISSON <-  with(Carnet_KCAL, SOMME_CARNET_KCAL - ALCOOL_CARNET -  SODAS_LIGHT_CARNET - SODAS_SUCRES_CARNET - EAU_CARNET - FRUITS_JUS_CARNET - LAIT_CARNET )


# Calculer la somme des calories par individu
calories_par_individu <- Carnet_KCAL %>%
  group_by(Identifiant) %>%
  summarise(somme_calories = sum(SOMME_CARNET_KCAL, na.rm = TRUE))

# Créer l'histogramme de la distribution des calories
ggplot(calories_par_individu, aes(x = somme_calories)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution des Calories par Individu",
       x = "Somme des Calories",
       y = "Nombre d'Individus") +
  theme_minimal()


  ## Calcul de MAR et MER
  ##MAR /MER--------------------------------------

    ###Calcul de la vitamine A --------------------------
###Calcul de la vitamine A --------------------------
resultats_codachats$vit_a_mcg <- (resultats_codachats$retinol_mcg + (resultats_codachats$beta_carotene_mcg/6)) 

#Ajout des dernières colonnes modifiées
resultats_codachats$proteines_g_alim <- resultats_codachats$Poids_consomme_vf * resultats_codachats$proteines_g *10 /(resultats_codachats$UC_TI*Nj)
resultats_codachats$proteines_kcal_alim <- ((resultats_codachats$proteines_g*4) * resultats_codachats$Poids_consomme_vf *10 )/(resultats_codachats$UC_TI*Nj)

resultats_codachats$ag_18_2_lino_g_alim  <- (resultats_codachats$Poids_consomme_vf * resultats_codachats$ag_18_2_lino_g*10 )/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ag_18_2_lino_kcal_alim   <- (resultats_codachats$Poids_consomme_vf *resultats_codachats$ag_18_2_lino_g*9*10 )/(resultats_codachats$UC_TI*Nj)

resultats_codachats$ag_18_3_a_lino_g_alim<- (resultats_codachats$Poids_consomme_vf *  resultats_codachats$ag_18_3_a_lino_g*10 )/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ag_18_3_a_lino_kcal_alim <- (resultats_codachats$Poids_consomme_vf *resultats_codachats$ag_18_3_a_lino_g*9*10 )/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ags_g_alim  <- (resultats_codachats$ags_g* resultats_codachats$Poids_consomme_vf  * 10)/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ags_kcal_alim <- (resultats_codachats$ags_g *9* resultats_codachats$Poids_consomme_vf  * 10)/(resultats_codachats$UC_TI*Nj)


    ### Calcul des quantités de nutriments par aliment -----------------
# Sélection des colonnes à transformer

colonnes_a_transformer<- c("retinol_mcg", "nrj_kcal", "beta_carotene_mcg","proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
                           "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg","phosphore_mg",
                           "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                           "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                           "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "vit_a_mcg")


# Vérifier si toutes les colonnes sont présentes
colonnes_manquantes <- setdiff(colonnes_a_transformer, names(resultats_codachats))
if (length(colonnes_manquantes) > 0) {
  stop("Les colonnes suivantes ne sont pas reconnues : ", paste(colonnes_manquantes, collapse = ", "))
}

# Si tout est correct, appliquer la transformation
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer),
                ~ . * Poids_consomme_vf * 10 / (UC_TI * Nj),
                .names = "{.col}_alim"))


#HENI_alim 
resultats_codachats$HENI_score <- (resultats_codachats$HENI  * 1000* resultats_codachats$Poids_consomme_vf)
  
    ### Somme par ID des nutriments d'interet --------------------------
resultats_codachats$budget_alim <- as.numeric(resultats_codachats$budget_alim)
colonnes_a_sommer <- names(resultats_codachats)[grep("_alim$|HENI_score", names(resultats_codachats))]
print(colonnes_a_sommer)  # Debugging check

somme_par_identifiant <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(across(
    all_of(colonnes_a_sommer), 
    ~ sum(.x, na.rm = TRUE)
    
  ))


#Somme des sucres 
somme_par_identifiant$sucre_aj_g_appro_alim <- somme_par_identifiant$fructose_g_alim+ somme_par_identifiant$glucose_g_alim + somme_par_identifiant$maltose_g_alim + somme_par_identifiant$saccharose_g_alim

#Calcul des nutriments sans alcool
cols_to_extract <- c("Identifiant", "UC_TI","KCAL_SANS_ALCOOL" , "SOMME_CARNET_KCAL") 
extracted_df <- Carnet_KCAL[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")
cols_to_extract <- c("Identifiant", "Sexe") 
extracted_df <- metadata[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")

#Calcul dernières colonnes 
somme_par_identifiant$proteines_kcal_2000 <- (somme_par_identifiant$proteines_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$fibres_g_2000 <- (somme_par_identifiant$fibres_g_alim*2000)/  somme_par_identifiant$SOMME_CARNET_KCAL
somme_par_identifiant$ag_18_3_a_lino_g_2000 <- (somme_par_identifiant$ag_18_3_a_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_18_2_lino_g_2000 <- (somme_par_identifiant$ag_18_2_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_20_6_dha_g_2000 <- (somme_par_identifiant$ag_20_6_dha_g_alim*2000)/(somme_par_identifiant$SOMME_CARNET_KCAL)

somme_par_identifiant$ags_kcal_2000 <- (somme_par_identifiant$ags_kcal_alim *100) /(somme_par_identifiant$KCAL_SANS_ALCOOL)

    ### Rajustement / 2000 KCAL---------------------------------------
exclude_cols <-  c("proteines_kcal_alim", "ags_kcal_alim", "ag_18_2_lino_g_alim", "ag_18_3_a_lino_g_alim","ag_18_3_a_lino_kcal_alim",
                   "ags_g_alim","proteines_g_alim" ,"fructose_g_alim"  ,"maltose_g_alim"       ,   "glucose_g_alim"    , "saccharose_g_alim", "alcool_g_alim",
                   "ag_18_2_lino_kcal_alim", "fibres_g_alim", "ag_20_6_dha_g_alim","DQR_alim", "EF_alim", "climat_alim","couche_ozone_alim" , 
                   "ions_alim","ozone_alim" , "partic_alim" ,"acid_alim","eutro_terr_alim","eutro_eau_alim"    ,     "eutro_mer_alim"    ,    
                   "sol_alim"   ,"toxi_eau_alim","ress_eau_alim", "ress_ener_alim"    , "budget_alim"   ,"ress_min_alim")
alim_cols <- grep("_alim$", names(somme_par_identifiant), value = TRUE)
alim_cols <- setdiff(alim_cols, exclude_cols)
for (col in alim_cols) {
  somme_par_identifiant[[col]] <- (somme_par_identifiant[[col]] * 2000) / somme_par_identifiant$SOMME_CARNET_KCAL
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


#somme_par_identifiant$ratio_prot <- somme_par_identifiant$proteines_kcal_2000 / 10 
#somme_par_identifiant$ratio_fibre <- somme_par_identifiant$fibres_g_2000 / 30 
#somme_par_identifiant$ratio_lino <- somme_par_identifiant$ag_18_2_lino_g_2000/ 4 
#somme_par_identifiant$ratio_alphalino <- somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1 
#somme_par_identifiant$ratio_dha <- somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25
#somme_par_identifiant$ratio_potassium <- somme_par_identifiant$potassium_mg_2000 / 3500 
#somme_par_identifiant$ratio_calcium <- somme_par_identifiant$calcium_mg_2000 / 950 
#somme_par_identifiant$ratio_selenium <- somme_par_identifiant$selenium_mcg_2000 / 70
#somme_par_identifiant$ratio_iode <- somme_par_identifiant$iode_mcg_2000 / 150 
#somme_par_identifiant$ratio_vit_d <- somme_par_identifiant$vitamine_d_mcg_2000 / 15
#somme_par_identifiant$ratio_vit_c <- somme_par_identifiant$vitamine_c_mg_2000 / 110
#somme_par_identifiant$ratio_vit_b2 <- somme_par_identifiant$vitamine_b2_mg_2000 / 1.6
#somme_par_identifiant$ratio_vit_b12 <- somme_par_identifiant$vitamine_b12_mcg_2000 / 4
#somme_par_identifiant$ratio_vit_b9 <- somme_par_identifiant$vitamine_b9_mcg_2000 / 330


# Définir une fonction pour calculer le ratio
calculate_ratio <- function(sexe, valeur, seuil_femme, seuil_homme) {
  if (sexe == "Femme") {
    return(ifelse(valeur / seuil_femme > 1, 1, valeur / seuil_femme))
  } else if (sexe == "Homme") {
    return(ifelse(valeur / seuil_homme > 1,1, valeur / seuil_homme))
  } else {
    return(NA)
  }
}

## Appliquer la fonction pour chaque nutriment
somme_par_identifiant$ratio_magnesium <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$magnesium_mg_2000, 300, 380)
somme_par_identifiant$ratio_fer <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$fer_mg_2000, 16, 11)
somme_par_identifiant$ratio_cuivre <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$cuivre_mg_2000, 1.5, 1.9) 
somme_par_identifiant$ratio_zinc <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$zinc_mg_2000, 9.3, 11.7)
somme_par_identifiant$ratio_vit_a <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vit_a_mcg_2000, 650, 750)
somme_par_identifiant$ratio_vit_e <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_e_mg_2000, 9, 10)
somme_par_identifiant$ratio_vit_b1 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b1_mg_2000,0.84, 0.84) # #0.1 EN 239 KCAL POUR CONVERTIR MJ ET EN J0. 
somme_par_identifiant$ratio_vit_b3 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b3_mg_2000, 13.4, 13.4) ##1.6*239 KCAL #14.9, 18.5
somme_par_identifiant$ratio_vit_b6 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b6_mg_2000, 1.6, 1.7)


#oN PREND LA RECO MOYENNE HOMME / FEMME
#somme_par_identifiant$ratio_magnesium <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$magnesium_mg_2000, 340, 340) 
#somme_par_identifiant$ratio_fer <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$fer_mg_2000, 12.25, 12.25) 
#somme_par_identifiant$ratio_cuivre <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$cuivre_mg_2000, 1.7, 1.7 ) 
#somme_par_identifiant$ratio_zinc <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$zinc_mg_2000, 10.5, 10.5)
#somme_par_identifiant$ratio_vit_a <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vit_a_mcg_2000, 700, 700) 
#somme_par_identifiant$ratio_vit_e <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_e_mg_2000, 9.5, 9.5) 
#somme_par_identifiant$ratio_vit_b1 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b1_mg_2000,0.84, 0.84) # #0.1 EN 239 KCAL POUR CONVERTIR MJ ET EN J0. 
#somme_par_identifiant$ratio_vit_b3 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b3_mg_2000, 13.4, 13.4) ##1.6*239 KCAL #14.9, 18.5
#somme_par_identifiant$ratio_vit_b6 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b6_mg_2000, 1.65, 1.65) 

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


    somme_par_identifiant$t_ratio_prot <- (somme_par_identifiant$proteines_kcal_2000 / 10)*100
    somme_par_identifiant$t_ratio_fibre <-(somme_par_identifiant$fibres_g_2000 / 30)*100
    somme_par_identifiant$t_ratio_lino <-(somme_par_identifiant$ag_18_2_lino_g_2000/ 4 )*100# (!!)
    somme_par_identifiant$t_ratio_alphalino <-(somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1)*100
    somme_par_identifiant$t_ratio_dha <-(somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25 )*100
    somme_par_identifiant$t_ratio_potassium <-(somme_par_identifiant$potassium_mg_2000 / 3500 )*100
    somme_par_identifiant$t_ratio_calcium <-(somme_par_identifiant$calcium_mg_2000 / 950 )*100
    somme_par_identifiant$t_ratio_selenium <-(somme_par_identifiant$selenium_mcg_2000 / 70)*100
    somme_par_identifiant$t_ratio_iode <-(somme_par_identifiant$iode_mcg_2000 / 150 )*100
    somme_par_identifiant$t_ratio_vit_d <-(somme_par_identifiant$vitamine_d_mcg_2000 / 15)*100
    somme_par_identifiant$t_ratio_vit_c <-(somme_par_identifiant$vitamine_c_mg_2000 / 110)*100
    somme_par_identifiant$t_ratio_vit_b2 <-(somme_par_identifiant$vitamine_b2_mg_2000 / 1.6)*100
    somme_par_identifiant$t_ratio_vit_b12 <-(somme_par_identifiant$vitamine_b12_mcg_2000 / 4 )*100
    somme_par_identifiant$t_ratio_vit_b9 <-(somme_par_identifiant$vitamine_b9_mcg_2000 / 330)*100
    somme_par_identifiant$t_ratio_magnesium <-  (somme_par_identifiant$magnesium_mg_2000/ 340)*100
    somme_par_identifiant$t_ratio_fer <-  (somme_par_identifiant$fer_mg_2000 / 12.25)*100
    somme_par_identifiant$t_ratio_cuivre <-  (somme_par_identifiant$cuivre_mg_2000 / 1.7 )*100
    somme_par_identifiant$t_ratio_zinc <- ( somme_par_identifiant$zinc_mg_2000/ 10.5)*100
    somme_par_identifiant$t_ratio_vit_a <- ( somme_par_identifiant$vit_a_mcg_2000/ 700 )*100
    somme_par_identifiant$t_ratio_vit_e <-  (somme_par_identifiant$vitamine_e_mg_2000/ 9.5)*100
    somme_par_identifiant$t_ratio_vit_b1 <-  (somme_par_identifiant$vitamine_b1_mg_2000/ 0.84)*100 # #0.1 EN 239 KCAL POUR CONVERTIR MJ ET EN J0. 
    somme_par_identifiant$t_ratio_vit_b3 <-  (somme_par_identifiant$vitamine_b3_mg_2000/13.4)*100##1.6*239 KCAL #14.9t_ratio 18.5
    somme_par_identifiant$t_ratio_vit_b6 <-  (somme_par_identifiant$vitamine_b6_mg_2000/ 1.65)*100
    somme_par_identifiant$t_ratio_ags <- (somme_par_identifiant$ags_kcal_2000 / 12 )*100
    somme_par_identifiant$t_ratio_sodium  <-(somme_par_identifiant$sodium_mg_2000/ 2300 )*100
    somme_par_identifiant$t_ratio_sucre_aj <-(somme_par_identifiant$sucre_aj_g_appro_2000/100)*100

    ratio_cols <- c("t_ratio_prot", "t_ratio_fibre", "t_ratio_lino", "t_ratio_alphalino", 
                    "t_ratio_dha", "t_ratio_potassium", "t_ratio_calcium", "t_ratio_selenium", 
                    "t_ratio_iode", "t_ratio_vit_d", "t_ratio_vit_c", "t_ratio_vit_b2", 
                    "t_ratio_vit_b12", "t_ratio_vit_b9", "t_ratio_magnesium", "t_ratio_fer", 
                    "t_ratio_cuivre", "t_ratio_zinc", "t_ratio_vit_a", "t_ratio_vit_e", 
                    "t_ratio_vit_b1", "t_ratio_vit_b3", "t_ratio_vit_b6", "t_ratio_ags", 
                    "t_ratio_sodium", "t_ratio_sucre_aj")
    
    moyennes_ratios <- colMeans(somme_par_identifiant[, ratio_cols], na.rm = TRUE)
    table_ratios <- data.frame(Ratio = names(moyennes_ratios), Moyenne = moyennes_ratios)
    print(table_ratios)



#Constitution des tableaux finaux ------------------------------------------------
Carnet_id <- metadata 
Carnet_id <- Carnet_id %>%
  select_if(~ !all(is.na(.)) & !all(. == ""))




if (campaign == "22-11" |campaign == "23-11") { Carnet_id$Periode <-0   }
if (campaign == "23-02" |campaign == "24-03") { Carnet_id$Periode <-1   }
Carnet_id$Mesure <- "Carnet"

#Carnet_POIDS <- Carnet_POIDS[, !grepl("^Combien.de.personnes.vivent.dans.votre.foyer", names(Carnet_POIDS))]
Carnet_POIDS <- Carnet_POIDS[, !grepl("UC_TI", names(Carnet_POIDS))]
Carnet_id <- left_join(Carnet_id, Carnet_POIDS, by='Identifiant')
Carnet_id <- left_join(Carnet_id, Carnet_POIDS_sem1, by='Identifiant')
Carnet_id <- left_join(Carnet_id, Carnet_POIDS_sem2, by='Identifiant')
Carnet_id <- left_join(Carnet_id, Carnet_POIDS_sem3, by='Identifiant')
Carnet_id <- left_join(Carnet_id, Carnet_POIDS_sem4, by='Identifiant')
Carnet_id <- left_join(Carnet_id, Carnet_KCAL, by='Identifiant')




#Constitution de data fin l  ------------------------------------------------
fichier_nettoyé <- 
  data.frame(
  Identifiant = resultats_codachats$Identifiant,
  Date = resultats_codachats$Date_vf,
  Lieu = resultats_codachats$Lieu_vf,
  Lieu1 = resultats_codachats$Lieu1,
  Lieu2 = resultats_codachats$Lieu2,
  LibelleCIQUAL = resultats_codachats$LibelleCIQUAL,
  groupe_TI_TdC = resultats_codachats$groupe_TI_TdC1,
  Poids_achat = resultats_codachats$Poids_vf,
  Poids_consomme = resultats_codachats$Poids_consomme_vf,
  Prix = resultats_codachats$Prix_vf, 
  Prix_Kg = resultats_codachats$Prix_Kg_post_imput,
  Montant_cheque = resultats_codachats$MontantChequeAlimentaire,
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

addWorksheet(wb, "tab_cumul_cat_norm")
writeData(wb, sheet = "tab_cumul_cat_norm", tab_cumul_cat_norm )




if (campaign == "22-11") {
  saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_nov_22.xlsx")))
}else{ 
  if (campaign == "23-02") {
    saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_mars_23.xlsx"))) 
  } else {
    if (campaign == "23-11") {
      saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_nov_23.xlsx"))) 
    } else { 
      saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/Carnets_Tableaux_mars_24.xlsx"))) 
    }}}



