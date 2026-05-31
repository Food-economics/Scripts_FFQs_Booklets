# CHARGEMENT DE L'ENVIRONNEMENT DE TRAVAIL  --------------
  ## Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(readxl);library(dplyr);library(broom);library(scales)
library(modelsummary);library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");
library("dplyr");library("tidyr");library("ggplot2");library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library(questionr)

  ## Importation des données ---------------------
if (researcher == "adenieul") {
  setwd("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else {
  setwd(paste0("C:/Users/", researcher, "/Owncloud/TI Dijon/donnees"))
}

    ### Entrer la date de la campagne ---------------------
campaign<- "24-03" #22-11 #23-02 #23-11 #24/03 

    ### Importation des données de Nov_2022 ------------------
questionnaire_nov_22<- read.xlsx(paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Fichiers_bruts/22-11_FFQ.xlsx",sep=""))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\"","",.))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\\(", "",.))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\\)", "",.))

    ### Importation des données de Mars_2023 ------------------
questionnaire_mars_23<- read.xlsx(paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Fichiers_bruts/23-02_FFQ.xlsx",sep=""))
questionnaire_mars_23<- questionnaire_mars_23%>% mutate_all(~gsub("\"","",.))

    ### Importation des données de Nov_2023 -------------------
questionnaire_nov_23<- read.xlsx(paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Fichiers_bruts/23-11_FFQ.xlsx",sep=""))
questionnaire_nov_23<- questionnaire_nov_23%>% mutate_all(~gsub("\"","",.))

    ### Importation des données de Mars_2024 --------------------
questionnaire_mars_24<- read.xlsx(paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Fichiers_bruts/24-03_FFQ.xlsx",sep=""))
questionnaire_mars_24<- questionnaire_mars_24%>% mutate_all(~gsub("\"","",.))

    ### Importation des données des tableaux annexes ----------------

CALNUT<- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/Alim_CALNUT_CODAPPRO_FFQ.xlsx")
Encodage <- read_xlsx(paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/Freq_FFQ.xlsx"))
Taille_Portion <- read_xlsx(paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/Taille portion.xlsx"))
Recap_envoi_cheques <- read_excel("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Tableaux_annexes/Recap_envoi_cheque.xlsx")



# CREATION D'UN TABLEAU UNIFORME POUR TOUTES LES CAMPAGNES  ------
  ## Définition des fonctions qui permettront de recoder les variables -------------
    ### Fonction pour obtenir le nom de la colonne en fonction de la campagne -----------
#La fonction get_column_name en R est conçue pour renvoyer le nom d'une colonne de questionnaire en fonction d'une campagne donnée.
get_column_name <- function(campaign) {
  switch(campaign,
         "22-11" = "questionnaire_nov_22","23-02" = "questionnaire_mars_23","23-11" = "questionnaire_nov_23","24-03" = "questionnaire_mars_24",stop("Campaign non reconnue"))}

    ### Fonction pour recoder les variables en fonction de la campagne ----------

#La fonction recoder_variables permet de renommer des colonnes dans un data frame
#en fonction d'un tableau d'encodage et d'une campagne spécifique.

#data : un data frame dont les colonnes doivent être renommées.
#Encodage : un data frame contenant le mapping entre les anciens noms de colonnes et les nouveaux noms. Ce data frame doit avoir une colonne pour chaque campagne ainsi qu'une colonne Aliment contenant les nouveaux noms.
#campaign : une chaîne de caractères spécifiant la campagne.
#La fonction commence par copier le data frame data dans tableau_recodé pour éviter de modifier l'original.
#La fonction appelle get_column_name(campaign) pour obtenir le nom de la colonne correspondant à la campagne dans le tableau Encodage.
recoder_variables <- function(data, Encodage, campaign) {
    tableau_recodé <- data
    column_name <- get_column_name(campaign)

#Pour chaque ligne du tableau Encodage :
#ancien_nom : le nom de colonne original pour cette ligne et cette campagne.
#nouveau_nom : le nouveau nom à donner à cette colonne.
#grep est utilisé pour trouver toutes les colonnes dans tableau_recodé dont le nom contient ancien_nom.    
  for (i in 1:nrow(Encodage)) {
    ancien_nom <- Encodage[[column_name]][i]
    nouveau_nom <- Encodage$Aliment[i]
    colonnes_similaires <- grep(ancien_nom, names(tableau_recodé), value = TRUE)
#Si des colonnes similaires sont trouvées, chaque colonne est renommée en utilisant make.names pour s'assurer que le nouveau nom est valide.
#La fonction retourne tableau_recodé, le data frame avec les colonnes renommées.
        if (length(colonnes_similaires) > 0) {
      for (col in colonnes_similaires) {
        names(tableau_recodé)[names(tableau_recodé) == col] <- make.names(nouveau_nom)
      }} }
  return(tableau_recodé)}

  ## Application de la fonction de recodage pour obtenir un premier tableau data dont le nom des variables est uniforme pour toutes les campagnes --------
if (campaign == "22-11") { data <- questionnaire_nov_22 }else{ 
  if (campaign == "23-02") { data <- questionnaire_mars_23  } else {
    if (campaign == "23-11") {data <- questionnaire_nov_23 } else {  data <- questionnaire_mars_24  }}}
data <- recoder_variables(data, Encodage , campaign)

  ## Création d'un tableau qui regroupe toutes les variables des différentes campagnes ---------------------
#Extraction de la Première Colonne de Encodage
colonne_chaligne <- Encodage[, 1]
#t(colonne_chaligne) transpose le vecteur colonne_chaligne, convertissant les lignes en colonnes.
#data.frame(t(colonne_chaligne)) crée un nouveau data frame Frame avec cette transposition.
Frame <- data.frame(t(colonne_chaligne))
#Frame[1, ] extrait la première ligne de Frame, qui contient les noms de colonnes souhaités.
#names(Frame) <- new_column_names définit ces valeurs comme les noms de colonnes de Frame.
new_column_names <- Frame[1,]
names(Frame) <- new_column_names
#Suppressionn de la première ligne de Frame
Frame <- Frame[-1, ]

  ## Imputation des valeurs de data au nouveau tableau Frame ------------
#Si le nombre de lignes de Frame est différent de celui de data, cette condition ajuste Frame pour qu'il ait le même nombre de lignes que data.
#intersect(names(data), names(Frame)) trouve les noms de colonnes qui sont communs entre data et Frame.
#Cette ligne copie les valeurs des colonnes communes de data vers Frame.
if (nrow(Frame) != nrow(data)) { Frame <- Frame[1:nrow(data), ]}
colonnes_communes <- intersect(names(data), names(Frame))
Frame[colonnes_communes] <- data[colonnes_communes]


  ## Traduction des fréquences de consommation en données numériques ---------
#La valeur de data sont initialement renseignées par des chaines de caractères. 
#L'étape suivante vise à traduire ces fréquences de consommation en valeurs numériques 
    ### Dictionnaire des traductions pour les aliments ------
Frame[Frame =="NA"]<- 0
Frame[Frame =="Jamais" ]<- 0
Frame[Frame =="Une fois par semaine" ]<- 1/7
Frame[Frame =="Entre 2 et 3 fois par semaine" ]<- 2.5/7
Frame[Frame ==   "Entre 2 fois et 3 fois par semaine" ]<- 2.5/7
Frame[Frame ==  "Deux à trois fois par semaine" ]<- 2.5/7
Frame[Frame ==  "Entre 4 et 5 fois par semaine" ]<- 4.5/7
Frame[Frame == "Quatre à cinq fois par semaine" ]<- 4.5/7
Frame[Frame ==  "1 fois par jour ou presque" ]<- 1
Frame[Frame ==  "Une fois par jour ou presque" ]<- 1
Frame[Frame == "Une fois ou presque par jour" ]<- 1
Frame[Frame ==  "Deux fois par jour" ]<- 2
Frame[Frame == "Plusieurs fois par jour" ]<- 2.5
Frame[Frame ==  "Trois fois par jour ou plus" ]<- 3

    ### Dictionnaire des traductions pour les boissons (verres) ------
Frame[Frame == "Aucun" ]<- 0
Frame[Frame == "Un verre par semaine" ]<- 1/7
Frame[Frame == "Entre 2 et 3 verres par semaine" ]<- 2.5/7
Frame[Frame ==  "Entre 4 et 5 verres par semaine" ]<- 4.5/7
Frame[Frame ==  "Un verre par jour ou presque" ]<-1
Frame[Frame ==  "2 à 4 verres par jour" ]<- 3
Frame[Frame ==  "4 à 8 verres par jour" ]<- 6
Frame[Frame ==  "Plus de 8 verres par jour" ]<- 8
Frame[Frame ==   "Pas d'alcool durant cette période"]<- 0
Frame[Frame ==   "Au moins un verre durant cette période"]<-1
Frame[Frame ==   "Plusieurs bols ou tasses par jour"]<-2
Frame[Frame ==   "Entre 2 et 5 bols ou tasses par semaine"]<-3.5/7
Frame[Frame ==   "Un bol ou tasse par semaine"]<-1/7
Frame[Frame ==   "Un bol ou tasse par jour ou presque"]<-1

    ### Dictionnaire des traductions pour les boissons (tasses) -------
Frame[Frame == "Aucun" ]<- 0
Frame[Frame == "Un bol (ou tasse) par semaine" ]<- 1/7
Frame[Frame == "Entre 2 et 5 bols (ou tasses) par semaine" ]<- 3.5/7
Frame[Frame == "Un bol (ou tasse) par jour ou presque" ]<- 1
Frame[Frame == "Plusieurs bols (ou tasses) par jour" ]<- 2
Frame[Frame == "Moins de 250 ml par jour" ]<- 0.25
Frame[Frame == "Entre 250 et 750 ml par jour" ]<- 0.4
Frame[Frame == "Entre 750 ml et 1,25 L par jour" ]<- 1
Frame[Frame == "Entre 1,25 L et 1,75 L par jour" ]<- 1.5
Frame[Frame == "Plus de 1,75 L par jour" ]<- 1.75

    ### Uniformisation des identifiants à Frame---------------

    ### Fonction de création des identifiants---------------------

create_identifiant <- function(Frame, num_col, store_col) { 
  Frame$Identifiant <- paste(Frame[[num_col]], Frame[[store_col]], sep = "-")
  return(Frame)}

    ### Harmonisation des identifiants --------------

if (campaign == "22-11" | campaign == "23-02" ) {
  Frame  <- create_identifiant(Frame, "Numero.d.identifiant", "Nom.de.l.epicerie")
  Frame <- Frame %>% 
    relocate("Identifiant", .after = "N.Obs")
  Frame <- Frame[, !names(Frame) %in% "Numero.d.identifiant"]
  }

if (campaign == "23-11" | campaign=="24-03") { 
  Frame <- Frame %>% rename("Identifiant"="Numero.d.identifiant")
}

print(unique(Frame$Identifiant))

class(Frame$Identifiant)
### Dictionnaire id ayant changé ------------

Frame$Identifiant[Frame$Identifiant == "PS004" ] <- "LE255"
Frame$Identifiant[Frame$Identifiant == "LE148" ] <- "PS284"
Frame$Identifiant[Frame$Identifiant == "LE195" ] <- "PS285"
Frame$Identifiant[Frame$Identifiant == "LE088" ] <- "PS286"
Frame$Identifiant[Frame$Identifiant == "LE207" ] <- "PS287"
Frame$Identifiant[Frame$Identifiant == "LE093" ] <- "PS288"
print(unique(Frame$Identifiant))


if (campaign == "22-11" | campaign == "23-02" ) {
Frame <-Frame %>%
  mutate(Identifiant = gsub("-CCAS \\(inclus Pôle emploi et SPF\\)", "-CCAS", Identifiant))}


Frame$Identifiant[ Frame$Identifiant=="8447-CCAS" ] <- "8747-CCAS"
Frame$Identifiant[ Frame$Identifiant=="PE19-CCAS" ] <- "PE019-CCAS"
Frame$Identifiant[ Frame$Identifiant=="1564-Epimut" ]<- "1654-Epimut"
Frame$Identifiant[ Frame$Identifiant=="P E013-CCAS" ] <- "PE013-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP032-CCAS" ] <- "SP040-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP-052-CCAS" ] <- "SP052-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP-017-CCAS" ] <- "SP017-CCAS"
Frame$Identifiant[ Frame$Identifiant=="pe003-CCAS" ]<- "pe003-CCAS"

Frame_bis <- Frame

### HArmonisation des données socio-démographiques entre les campagnes

# CREATION DE METADATA ------------------------------------

  ## Extraction des variables d'intèret de Frame-----------
# Utilisation de subset pour sélectionner les colonnes d'intérêt
metadata <- Frame_bis[, c("Identifiant", "Sexe", "Quel.age.avez.vous.", "Quel.est.votre.pays.de.naissance.",
                          "Combien.de.personnes.vivent.dans.votre.foyer", "Quelle.est.votre.situation.matrimoniale.",
                          "Avez.vous.des.enfants.a.charge.", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans",
                          "De.15.a.17.ans", "De.18.ans.et.plus", "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois.",
                          "Part.de.votre.budget.alimentaire.consacree.aux.produits.alimentaires.biologiques.",
                          "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.",
                          "Quelle.est.votre.situation.professionnelle.actuelle.", "La.semaine.dernierevous.arrive.t.il.de.jeter.des.restes.apres.un.repas.",
                          "Si.ouien.avez.vous.jete.tous.les.jours.", "Si.noncombien.de.fois.la.semaine.derniere.",
                          "Gaspillage.restes.", "Gapillage.produits.non.entames", "Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.",
                          "Revenu.mensuel", "Budget.mensuel.alimentation.", "Budget.hebdomadaire.alimentation.",
                          "Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.",
                          "Les.avez.vous.utilises.", "Qu.avez.vous.achete.avec.",
                          "Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.",
                          "Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.",
                          "Date.de.saisie")]



if (campaign == "24-03") {
  questionnaire_nov_23 <- questionnaire_nov_23 %>% 
    rename(
      Identifiant = "247..Q1",
      Sexe = "12..Q2",
      `Quel.age.avez.vous.` = "13..Q3", 
      `Combien.de.personnes.vivent.dans.votre.foyer` = "15..Q5", 
      `Quelle.est.votre.situation.matrimoniale.` = "14..Q6",
      `Avez.vous.des.enfants.a.charge.` = "19..Q7", 
      `De.moins.de.3.ans` = "20..Q71a",
      `De.3.a.10.ans` = "21..Q71b",
      `De.11.a.14.ans` = "22..Q71c",
      `De.15.a.17.ans` = "23..Q71d", 
      `De.18.ans.et.plus` = "24..Q71e"
    )
  
    metadata_bis <- questionnaire_nov_23 %>%
      dplyr::select(
        Identifiant, Sexe, `Quel.age.avez.vous.`,
        `Combien.de.personnes.vivent.dans.votre.foyer`,
        `Quelle.est.votre.situation.matrimoniale.`,
        `Avez.vous.des.enfants.a.charge.`,
        `De.moins.de.3.ans`, `De.3.a.10.ans`, `De.11.a.14.ans`,
        `De.15.a.17.ans`, `De.18.ans.et.plus`
      )

    metadata <- merge(metadata_bis, metadata, by = "Identifiant", all.x = TRUE)
    
    # Select columns and rename as needed
    metadata <- metadata %>%
      dplyr::select(
        -matches("\\.y$")  # Remove columns ending with .y
      ) %>%
      dplyr::rename_with(
        ~ gsub("\\.x$", "", .), ends_with(".x")  # Remove .x suffix from column names
      )
}



if (campaign == "23-02") {
  # Créez la nouvelle colonne identifiant
  questionnaire_nov_22 <- questionnaire_nov_22 %>%
    mutate(Identifiant = paste(`1..Numéro.d'identifiant.:`, `2..Nom.de.l'épicerie.:`, sep = "-"))
  
  questionnaire_nov_22  <- questionnaire_nov_22 %>% 
    rename(
      `Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu` = "224..Quel.est.le.diplôme.d'enseignement.général.ou.technique.le.plus.élevé.que.vous.ayez.obtenu.?")
  
  metadata_bis <- questionnaire_nov_22 %>%
    dplyr::select(
      Identifiant,`Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu` )
  
  metadata <- merge(metadata_bis, metadata, by = "Identifiant", all.x = TRUE)
  
  # Select columns and rename as needed
  metadata <- metadata %>%
    dplyr::select(
      -matches("\\.y$")  # Remove columns ending with .y
    ) %>%
    dplyr::rename_with(
      ~ gsub("\\.x$", "", .), ends_with(".x")  # Remove .x suffix from column names
    )
}





metadata$Identifiant[ metadata$Identifiant=="8447-CCAS" ] <- "8747-CCAS"
metadata$Identifiant[ metadata$Identifiant=="PE19-CCAS" ] <- "PE019-CCAS"
metadata$Identifiant[ metadata$Identifiant=="1564-Epimut" ]<- "1654-Epimut"
metadata$Identifiant[ metadata$Identifiant=="P E013-CCAS" ] <- "PE013-CCAS"
metadata$Identifiant[ metadata$Identifiant=="SP032-CCAS" ] <- "SP040-CCAS"
metadata$Identifiant[ metadata$Identifiant=="SP-052-CCAS" ] <- "SP052-CCAS"
metadata$Identifiant[ metadata$Identifiant=="SP-017-CCAS" ] <- "SP017-CCAS"
metadata$Identifiant[ metadata$Identifiant=="pe003-CCAS" ]<- "pe003-CCAS"
metadata$Identifiant[metadata$Identifiant == "PS004" ] <- "LE255"
metadata$Identifiant[metadata$Identifiant == "LE148" ] <- "PS284"
metadata$Identifiant[metadata$Identifiant == "LE195" ] <- "PS285"
metadata$Identifiant[metadata$Identifiant == "LE088" ] <- "PS286"
metadata$Identifiant[metadata$Identifiant == "LE207" ] <- "PS287"
metadata$Identifiant[metadata$Identifiant == "LE093" ] <- "PS288"
print(unique(Frame$Identifiant))


#AJUSTEMENT DES UC
# Calcul de la somme du nombre d'enfants
metadata$De.moins.de.3.ans <- as.numeric(metadata$De.moins.de.3.ans)
metadata$De.3.a.10.ans <- as.numeric(metadata$De.3.a.10.ans)
metadata$De.11.a.14.ans <- as.numeric(metadata$De.11.a.14.ans)
metadata$De.15.a.17.ans <- as.numeric(metadata$De.15.a.17.ans)
metadata$De.18.ans.et.plus <- as.numeric(metadata$De.18.ans.et.plus)

# Remplacer les NA par 0 dans toutes les colonnes spécifiées
metadata$De.moins.de.3.ans[is.na(metadata$De.moins.de.3.ans)] <- 0
metadata$De.3.a.10.ans[is.na(metadata$De.3.a.10.ans)] <- 0
metadata$De.11.a.14.ans[is.na(metadata$De.11.a.14.ans)] <- 0
metadata$De.15.a.17.ans[is.na(metadata$De.15.a.17.ans)] <- 0
metadata$De.18.ans.et.plus[is.na(metadata$De.18.ans.et.plus)] <- 0
metadata$somme_enfants <- (metadata$De.moins.de.3.ans +
                             metadata$De.3.a.10.ans +
                             metadata$De.11.a.14.ans +
                             metadata$De.15.a.17.ans +
                             metadata$De.18.ans.et.plus)
metadata$adultes_mat <- ifelse(metadata$Quelle.est.votre.situation.matrimoniale.=="En couple non marié (PACS, concubinage…)"|
                                 metadata$Quelle.est.votre.situation.matrimoniale.=="Marié(e)" ,(2), (1))
metadata$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(metadata$Combien.de.personnes.vivent.dans.votre.foyer)
metadata$somme_enfants <- as.numeric(metadata$somme_enfants)
metadata$adultes_mat <- as.numeric(metadata$adultes_mat)
metadata$adultes <- ifelse(metadata$Combien.de.personnes.vivent.dans.votre.foyer == 1, 
                           1, 
                           pmax(metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$somme_enfants, 
                                metadata$adultes_mat, 
                                na.rm = TRUE))

metadata$enfant_18_cor <- ifelse(
  (metadata$adultes + metadata$De.moins.de.3.ans + 
     metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
     metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus - metadata$Combien.de.personnes.vivent.dans.votre.foyer) == 0,
  metadata$De.18.ans.et.plus,
  ifelse(
    metadata$De.18.ans.et.plus > 0,
    pmax(0, metadata$De.18.ans.et.plus - (metadata$adultes + metadata$De.moins.de.3.ans + 
                                            metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
                                            metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus - metadata$Combien.de.personnes.vivent.dans.votre.foyer)),
    ifelse(
      !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.18.ans.et.plus / (metadata$De.moins.de.3.ans + 
                                                                                                                          metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
                                                                                                                          metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus)),
      (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.18.ans.et.plus / (metadata$De.moins.de.3.ans + 
                                                                                                                   metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
                                                                                                                   metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus),
      0
    )
  )
)

#Moins de 3 ans corrigé 
metadata$enfant_moins_3_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.moins.de.3.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.moins.de.3.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.moins.de.3.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

#Enfant 3 -10 ans 
metadata$enfant_3_10_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.3.a.10.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.3.a.10.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.3.a.10.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

#Enfants 11-14 ans 
metadata$enfant_11_14_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.11.a.14.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.11.a.14.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.11.a.14.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

#Enfants 15-17 
metadata$enfant_15_17_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.15.a.17.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.15.a.17.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.15.a.17.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

metadata$UC_TI <- metadata$adultes + metadata$enfant_15_17_ans_cor+ metadata$enfant_18_cor + 0.5*(metadata$enfant_moins_3_ans_cor + metadata$enfant_3_10_ans_cor+ metadata$enfant_11_14_ans_cor)
metadata$UC_INSEE <- ifelse(
  metadata$adultes == 1, 
  1, 
  1 + 0.5 * (metadata$adultes - 1)
) + 
  (metadata$enfant_moins_3_ans_cor + metadata$enfant_3_10_ans_cor + metadata$enfant_11_14_ans_cor) * 0.3 + 
  (metadata$De.15.a.17.ans + metadata$enfant_18_cor) * 0.5

## Calcul des UC et du reveny / UC------------
metadata$Income_UC_INSEE <- as.numeric(metadata$Revenu.mensuel)/as.numeric(metadata$UC_INSEE)
metadata <- subset(metadata, !(is.na(Sexe)))



##Determiner les classes d'âge 
calculate_central_values <- function(classes) {
  # Suppression des caractères non numériques et extraction des bornes
  lower_bounds <- as.numeric(gsub("\\D*(\\d+)-\\d+\\D*", "\\1", classes))
  upper_bounds <- as.numeric(gsub("\\D*\\d+-(\\d+)\\D*", "\\1", classes))
  
  # Calcul des valeurs centrales
  central_values <- (lower_bounds + upper_bounds) / 2
  
  return(central_values)
}

  ## Appliquer la fonction aux classes d'âge à metadata--------------
metadata$Age_Central <- calculate_central_values(metadata$Quel.age.avez.vous.)


#Correction des initulés de variables démographiques

metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "BTS, DUT, DEST, DEUG (y compris formation paramédicale ou sociale)"),("BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat général"),("Baccalauréat"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "2ème ou 3ème cycle universitaire, grande école"),("2e ou 3e cycle universitaire, grande école"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))

metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Autre inactif invalide, handicapé, en congé maladie > 3 mois, titulaire d’une pension de réversion"),("Autre inactif (invalide, handicapé, en congé maladie > 3 mois, titulaire d’une pension de réversion)"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))
metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Femme ou homme au foyer y compris congé parental"),("Femme ou homme au foyer (y compris congé parental)"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))
metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Retraité(e) (ancien salarié) ou préretraité(e)"),("Retraitée ancien salarié ou préretraitée"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))

metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu. <- ifelse((metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Minimas sociaux RSA, Allocations familiales..."),("Minimas sociaux (RSA, Allocations familiales...)"),(metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))
metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu. <- ifelse((metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Travail salarié, autoentrepreneur..."),("Travail (salarié, autoentrepreneur...)"),(metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))

metadata$Quel.est.votre.pays.de.naissance. <- ifelse((metadata$Quel.est.votre.pays.de.naissance. == "Afrique Sub-saharienne ou Moyen-Orient (jusqu'en Iran)"),("Afrique sub-saharienne ou Moyen-Orient jusqu’à l’Iran"),(metadata$Quel.est.votre.pays.de.naissance. ))

metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Divorcée ou séparée"),("Divorcé(e) ou séparé(e)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "En couple non marié PACS, concubinage…"),("En couple non marié (PACS, concubinage…)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Mariée"),("Marié(e)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Veufve"),("Veuf(ve)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))

  ##Déterminer les foyers uniparentaux -----------
metadata$Foyer_monoparental <- ifelse((metadata$Avez.vous.des.enfants.a.charge. =="Oui" & (metadata$Quelle.est.votre.situation.matrimoniale. == "Veuf(ve)" |metadata$Quelle.est.votre.situation.matrimoniale. == "Célibataire" |metadata$Quelle.est.votre.situation.matrimoniale. == "Divorce(e) ou séparé(e)")), (1), (0))

#Correction des budget alimentaires-------------------------
if (campaign == "22-11") {
  metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "6354-Episourire"),(250),(metadata$Budget.mensuel.alimentation.))
  metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "770-Epimut"),(200),(metadata$Budget.mensuel.alimentation.))
  
  }else{ 
  if (campaign == "23-02") {
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "10499-Episourire"),(200),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "1280-Epimut"),(100),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "1530-Epimut"),(150),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "5455-Episourire"),(100),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "5894-CCAS"),(100),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "PE023-CCAS"),(150),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "6222-Episourire"),(200),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "1730-Epimut"),(200),(metadata$Budget.mensuel.alimentation.))
      } else {
    if (campaign == "24-03") {
      metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "PS009"),(200),(metadata$Budget.mensuel.alimentation.))
      metadata$Budget.hebdomadaire.alimentation. <- ifelse((metadata$Identifiant == "PS213"),(180),(metadata$Budget.hebdomadaire.alimentation.))
      }}}

  ## Remplacer 0 par NA--------------------
metadata$Income_UC_INSEE[metadata$Income_UC_INSEE == 0] <- NA

#Gaspillage 
# Conversion en 7 si "Oui", sinon NA
metadata$Si.ouien.avez.vous.jete.tous.les.jours. <- ifelse(metadata$Si.ouien.avez.vous.jete.tous.les.jours. == "Oui", 7, NA)
metadata$Si.noncombien.de.fois.la.semaine.derniere. <- ifelse(is.na(metadata$Si.noncombien.de.fois.la.semaine.derniere.), 
                                                              metadata$Si.ouien.avez.vous.jete.tous.les.jours., 
                                                              metadata$Si.noncombien.de.fois.la.semaine.derniere.)

# Conversion en numérique si ce n'est pas déjà fait
metadata$Si.noncombien.de.fois.la.semaine.derniere. <- as.numeric(metadata$Si.noncombien.de.fois.la.semaine.derniere.)
metadata$Gapillage.produits.non.entames  <- ifelse(!is.na(as.numeric(metadata$Gapillage.produits.non.entames )), metadata$Gapillage.produits.non.entames , "NA")
metadata$Gapillage.produits.non.entames <- as.numeric(metadata$Gapillage.produits.non.entames)
metadata$freq_hebdo_gaspillage <- metadata$Si.noncombien.de.fois.la.semaine.derniere. + metadata$Gapillage.produits.non.entames
tert <- quantile(metadata$freq_hebdo_gaspillage, probs = c(1/3, 2/3), na.rm = TRUE)



metadata<- metadata[, !names(metadata) %in% c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans",
                                              "De.15.a.17.ans", "De.15.a.17.ans", "De.18.ans.et.plus",
                                              "La.semaine.dernierevous.arrive.t.il.de.jeter.des.restes.apres.un.repas.",
                                              "Si.ouien.avez.vous.jete.tous.les.jours.", " Si.noncombien.de.fois.la.semaine.derniere.",
                                              "Gaspillage.restes.", "Gapillage.produits.non.entames")]



print(unique(metadata$Identifiant))

#Ajouter les données de SP041 à MARS23 
if (campaign == "23-02") {
  file_path <- "C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_22.xlsx"
  metadata22 <- read_excel(file_path, sheet = "Metadata") 
  ligne_SP041_CCAS <- metadata22 %>% filter(Identifiant == "SP041-CCAS")
  # Extraire les colonnes de metadata22 et ligne_SP041_CCAS
  colonnes_manquantes <- setdiff(colnames(metadata), colnames(ligne_SP041_CCAS))
  for (col in colonnes_manquantes) {
    ligne_SP041_CCAS[[col]] <- NA
  }
  #ligne_SP041_CCAS <- ligne_SP041_CCAS %>%
  #  select(all_of(colnames(metadata)))
  ligne_SP041_CCAS <- ligne_SP041_CCAS[ , colnames(metadata), drop = FALSE]
  
  
  metadata <- bind_rows(metadata, ligne_SP041_CCAS)
}

## CALCUL DES COEFF DE CORRECTION POUR CORRIGER LES FREQUENCES -----------------------------------------
#
## Fonction pour remplacer les NA par zéro ---------
replace_na_with_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
## Création d'une fonction permettant d'extraire les données numériques dans le tableau---------------
FREQ_intake <- function(data, x) {
  result <- as.numeric(data[[x]])
  result[is.na(result)] <- 0
  return(result)
}


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

### Application de la fonction pour remplacer les tailles de portions par les
# Transformation de la table Taille_Portion en format long
#La fonction pivot_longer convertit les colonnes spécifiées (toutes les colonnes sauf Aliment) en un format long. Cela signifie que les colonnes seront "empilées" dans deux nouvelles colonnes nommées Taille (pour les noms des anciennes colonnes) et Poids (pour les valeurs des anciennes colonnes).
#La fonction na.omit supprime les lignes qui contiennent des valeurs manquantes (NA).
#La fonction filter exclut les lignes où la colonne Taille est égale à "Catégorie" ou "Catégorie2".


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


Taille_Portion_long <- Taille_Portion %>%
  pivot_longer(cols = -Aliment, names_to = "Taille", values_to = "Poids") %>%
  na.omit() %>%
  filter(Taille != "Catégorie" & Taille != "Catégorie2")


    ### Remplacement des tailles par les poids en appliquant la fonction remplacer_poids---------------------
Poids_modifie <- Poids
for (col in names(Poids_modifie)) {
  Poids_modifie[[col]] <- sapply(Poids_modifie[[col]], function(taille) remplacer_poids(taille, col))
}

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

print(unique(Frame$Identifiant))


# Finalisation du tableau
Poids_modifie <- cbind(Frame$Identifiant, Poids_modifie)
names(Poids_modifie)[1] <- "Identifiant"
Poids_modifie[,-1] <- lapply(Poids_modifie[,-1], as.numeric)



# CALCUL DU POIDS DES ALIMENTS CONSOMMES    -------------------------------
#Dans cette étape on calcule le poids des aliments en multipliant les fréquences par les tailles de portion pour tous les aliments
FFQ_POIDS <-data.frame(Frame$Identifiant)
names(FFQ_POIDS)[1] = "Identifiant"

FFQ_POIDS_Int <-data.frame(Frame$Identifiant)
names(FFQ_POIDS_Int )[1] = "Identifiant"

  ##ALCOOL_FFQ ---------------
FFQ_POIDS$ALCOOL_FFQ <- rep(0, nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")

terms <- numeric(nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) / 7
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$ALCOOL_FFQ <- terms



##CAFE_THE_FFQ---------------------
FFQ_POIDS$CAFE_THE_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.cafe.y.compris.decafeine", colnames(Frame))
ncol2 <- grep("de.the",colnames(Frame))
term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) *  Poids_modifie$de.cafe.y.compris.decafeine )
term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.the  )
FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2 
FFQ_POIDS_Int$de.cafe.y.compris.decafeine <- term1
FFQ_POIDS_Int$de.the <- term2



  ##CEREALES_PD_FFQ---------------
FFQ_POIDS$CEREALES_PD_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli",colnames(Frame))
FFQ_POIDS$CEREALES_PD_FFQ  <- FREQ_intake(Frame,ncol1)*Poids_modifie$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli
FFQ_POIDS_Int$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli <- FFQ_POIDS$CEREALES_PD_FFQ

#CHARCUTERIE_HORS_JBc_FFQ
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- rep(0,nrow(Frame))
categories <- c("du.saucisson.sec.ou.salamiy.compris.a.l.aperitif", "du.cervelas.ou.de.la.mortadelle",
                "du.pate.ou.des.rillettes", "du.jambon.crubacon",  "des.saucisses.fraiches.ou.fumees.y.compris.merguez")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- terms

  ##DESSERTS_LACTES_FFQ----------------
FFQ_POIDS$DESSERTS_LACTES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.glace", "des.entremets.cremes.desserts.de.type.Danetteliegeoismoussesflans.",
"des.entremets.au.soja.ou.yaourts.au.soja.ou.autres.yaourts.aux.laits.vegetaux")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$DESSERTS_LACTES_FFQ <- terms

  ##EAU_FFQ--------------
FFQ_POIDS$EAU_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("d.eau.en.bouteille.ou.bonbonne.verre", colnames(Frame))

if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$d.eau.en.bouteille.ou.bonbonne.verre) 
  FFQ_POIDS$EAU_FFQ <- term1 
  FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1 

}else{ 
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)) 
  FFQ_POIDS$EAU_FFQ <- term1 
  FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1 
}


  ##FEC_NON_RAF_FFQ----------------------
FFQ_POIDS$FEC_NON_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("du.painspeciaux.hors.petit.dejeuner.","du.pain.complet.et.autres.pains.speciaux.au.petit.dejeuner",
                "du.mais.ou.de.la.polenta","des.pommes.de.terre.a.l.eau.ou.au.four","des.pommes.de.terre.rissolees.ou.sautees",
                "de.la.puree.de.pomme.de.terre","d.autres.feculents.quinoamaniocbanane.plantainigname.",
                "des.pates.completes.ou.semi.completes","du.riz.complet.ou.semi.complet","du.mais1")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FEC_NON_RAF_FFQ <- terms

  ##FEC_RAF_FFQ---------------------
FFQ_POIDS$FEC_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.semouledu.ble.tabouleen.accompagnement.autre.que.dans.un.couscousEbly",
                "du.riz.blanc", "des.pates.macaronisspaghettiscoquillettes", "du.pain.blancde.mie.hors.petit.dejeuner.",
                "des.biscottesdes.craquottesdes.pains.grilles.au.petit.dejeuner","des.biscottesdes.craquottesdes.pains.grilles.type.suedois.hors.petit.dejeuner",
                "du.pain.blanc.au.petit.dejeuner")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FEC_RAF_FFQ <- terms

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

# Affichage des catégories non reconnues
if (length(non_reconnues) > 0) {
  warning("Colonnes non reconnues dans 'Frame' :\n", paste(non_reconnues, collapse = "\n"))
}

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

  ##FRUITS_JUS_FFQ---------------
FFQ_POIDS$FRUITS_JUS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre", colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  FFQ_POIDS$FRUITS_JUS_FFQ<-  replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre)) 
  FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ
} else {
  FFQ_POIDS$FRUITS_JUS_FFQ <-  replace_na_with_zero(FREQ_intake(Frame, ncol1)) 
  FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ
}

  ##FRUITS_SECS_FFQ ------------------
FFQ_POIDS$FRUITS_SECS_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.seches.abricotsdattesfiguespruneaux.",colnames(Frame))
FFQ_POIDS$FRUITS_SECS_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.seches.abricotsdattesfiguespruneaux.)
FFQ_POIDS_Int$des.fruits.seches.abricotsdattesfiguespruneaux. <- FFQ_POIDS$FRUITS_SECS_FFQ
  
  ##JAMBON_BLANC-------------------
FFQ_POIDS$JAMBON_BLANC_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("du.jambon.blanc",colnames(Frame))
FFQ_POIDS$JAMBON_BLANC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.jambon.blanc)
FFQ_POIDS_Int$du.jambon.blanc <- FFQ_POIDS$JAMBON_BLANC_FFQ


  ##LAIT_FFQ---------------------
FFQ_POIDS$LAIT_FFQ <- rep(0,nrow(Frame))
  ncol1 <- grep("de.lait.entier.", colnames(Frame))
  ncol2 <- grep("de.lait.demi.ecreme",colnames(Frame))
  ncol3 <- grep("de.lait.ecreme",colnames(Frame))
  ncol4 <- grep("du.cacao.ou.chocolat.en.poudre",colnames(Frame))
  if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) *  Poids_modifie$de.lait.entier. )
  term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.lait.demi.ecreme  )
  term3 <-replace_na_with_zero(FREQ_intake(Frame,ncol3)*Poids_modifie$de.lait.ecreme ) 
  term4 <-replace_na_with_zero(FREQ_intake(Frame,ncol4)*Poids_modifie$du.cacao.ou.chocolat.en.poudre)
  FFQ_POIDS$LAIT_FFQ <- term1 + term2 + term3 + term4 
  FFQ_POIDS_Int$de.lait.entier. <- term1
  FFQ_POIDS_Int$de.lait.demi.ecreme <- term2
  FFQ_POIDS_Int$de.lait.ecreme <- term3
  FFQ_POIDS_Int$du.cacao.ou.chocolat.en.poudre <- term4
}else{ 
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1))
  term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS$LAIT_FFQ <- term1 + term2
  FFQ_POIDS_Int$de.lait.entier. <- term1
  FFQ_POIDS_Int$de.lait.demi.ecreme <- term2
}

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

  ##LEGUMES_FFQ-------------------
FFQ_POIDS$LEGUMES_FFQ <- rep(0,nrow(Frame))
categories <- c("des.haricots.verts","des.endivesdes.epinardsdu.cresson","des.poireaux",
                "du.chou.vertchou.fleurBruxellesbrocolis","des.carottes.cuites","des.courgettesdes.auberginesdes.poivronsdes.tomates.cuites.ratatouille.",
                "des.petits.pois","des.artichautsdu.fenouildes.aspergesdu.celeri","des.champignons","du.potirondes.patates.douces","de.la.soupe.de.legumes",
                "de.la.salade.vertede.la.machede.la.roquettedes.epinardsdu.cresson","des.carottes.rapees","de.l.avocat.au.moins.un.demi.avocat"
                ,"d.autres.crudites","Des.salades.composees.uniquement.de.plusieurs.legumes.crus.tomates.et.concombrescarottes.et.salade.verte.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LEGUMES_FFQ <- terms
if (campaign == "23-11" | campaign == "24-03" ) {
  ncol1 <- grep("de.l.oignon.2", colnames(Frame))
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) * 0.05)
  FFQ_POIDS$LEGUMES_FFQ <- FFQ_POIDS$LEGUMES_FFQ + term1
  FFQ_POIDS_Int$de.l.oignon.2 <- term1
}
  
  ##MGA_FFQ -----------------------
FFQ_POIDS$MGA_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates.",colnames(Frame))
ncol2 <- grep("de.la.creme.fraiche",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. )
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.la.creme.fraiche)
FFQ_POIDS$MGA_FFQ <- term1 + term2 
FFQ_POIDS_Int$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. <- term1
FFQ_POIDS_Int$de.la.creme.fraiche <- term2

  ##MGV_FFQ------------------
FFQ_POIDS$MGV_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.huile.de.tournesold.arachide","de.la.margarine",
                "de.l.huile.melangee","de.l.huile.de.colzanoix","de.l.huile.d.olive.hors.vinaigrette")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$MGV_FFQ <- terms

  ##NOIX_FFQ----------------
FFQ_POIDS$NOIX_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.a.coque.noixnoisettesamandes.",colnames(Frame))
FFQ_POIDS$NOIX_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)
FFQ_POIDS_Int$des.fruits.a.coque.noixnoisettesamandes. <- FFQ_POIDS$NOIX_FFQ 

  ##OEUFS_FFQ-----------------
FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
  categories <- c("des.oeufssur.le.plat.en.omelette1","des.oeufspochesdurs.ou.a.la.coque.1")
  terms <- numeric(nrow(Frame))
  for (i in seq_along(categories)) {
    ncol <- grep(categories[i], colnames(Frame))
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
    terms <- terms + term
    FFQ_POIDS_Int[[categories[i]]] <- term
  }
  FFQ_POIDS$OEUFS_FFQ <- terms
  
}else{
  ncol1 <- grep("des.oeufssur.le.plat.en.omelette2",colnames(Frame))
  ncol2 <- grep("des.oeufspochesdurs.ou.a.la.coque.2",colnames(Frame))
  ncol3 <- grep ("Quand.vous.mangez.des.oeufscombien.en.mangez.vous.par.repas.omeletteau.platdur", colnames(Frame))
  term1 <-  replace_na_with_zero((FREQ_intake(Frame,ncol1)*replace_na_with_zero(FREQ_intake(Frame,ncol3))*0.06)) 
  term2 <-  replace_na_with_zero((FREQ_intake(Frame,ncol2)*replace_na_with_zero(FREQ_intake(Frame,ncol3))*0.06))
  FFQ_POIDS$OEUFS_FFQ <- term1 + term2  
  FFQ_POIDS_Int$des.oeufssur.le.plat.en.omelette2 <- term1
  FFQ_POIDS_Int$des.oeufspochesdurs.ou.a.la.coque.2<- term2
}


  ##PDTS_SUCRES_FFQ ---------------------
FFQ_POIDS$PDTS_SUCRES_FFQ <- rep(0,nrow(Frame))
  categories <- c("de.la.tarte.aux.fruitsau.flan.","de.la.patisserie.maison.tartegateau.au.chocolatcrepe.","de.la.briochedu.cakedu.quatre.quarts",
    "des.biscuitspur.beurresecsa.la.confiturefourresau.chocolat.","Des.gateaux.patissiers.tout.faits.browniecrepepain.d.epice.","des.gateux.patissiers.au.chocolata.la.creme.",
    "de.la.pate.a.tartiner.au.chocolat.type.Nutella","des.barres.chocolatees.MarsBounty.","des.barres.de.cereales.Granny.","des.bonbons","des.viennoiseries.croissantspains.au.chocolat.",
    "du.chocolat.noirau.laitaux.noisettes.","du.mielde.la.confitureou.marmelade","du.sorbet","d.autres.types.de.produits.sucres")
  terms <- numeric(nrow(Frame))
  for (i in seq_along(categories)) {
    ncol <- grep(categories[i], colnames(Frame))
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
    terms <- terms + term
    FFQ_POIDS_Int[[categories[i]]] <- term
  }
  FFQ_POIDS$PDTS_SUCRES_FFQ <- terms
  if (campaign == "23-11" | campaign == "24-03" ) { 
    ncol1 <- grep("Lorsque.vous.buvez.du.cafe.the.ou.mangez.un.yaourt.fromage.blanccombien.de.cuilleres.ou.carres.de.sucre.rajoutez.vous.",colnames(Frame))
    term1 <-  replace_na_with_zero((FREQ_intake(Frame,ncol1)*0.07))
    FFQ_POIDS$PDTS_SUCRES_FFQ <- FFQ_POIDS$PDTS_SUCRES_FFQ + term1
    FFQ_POIDS_Int$Lorsque.vous.buvez.du.cafe.the.ou.mangez.un.yaourt.fromage.blanccombien.de.cuilleres.ou.carres.de.sucre.rajoutez.vous. <- term1 }
  
  ##PLAT_PREP_FFQ------------------
FFQ_POIDS$PLATS_PREP_CARNES_FFQ  <- rep(0,nrow(Frame))
categories <- c("des.raviolislasagnespates.fourrees","du.cassoulet","du.couscous","des.salades.composees.toutes.faites.avec.feculents.et.viande",
                "de.la.paella","de.la.choucroute.avec.de.la.charcuterie", "du.chili.con.carne", "des.plats.cuisines.alleges","des.plats.cuisines.a.base.de.poisson")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PLATS_PREP_CARNES_FFQ <- terms

  ##PLAT_PREP_VEGETARIENS_FFQ---------------
FFQ_POIDS$PLATS_PREP_VEGETARIENS_FFQ  <- rep(0,nrow(Frame))
categories <- c("du.gratin.dauphinois", "des.raviolislasagnespates.fourrees.sans.viande",
                "des.salades.composees.toutes.faites.avec.feculents.sans.viande","des.salades.composees.toutes.faites.seulement.de.legumes",
                "du.taboule.tout.fait")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PLATS_PREP_VEGETARIENS_FFQ <- terms

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

  ##PORC_FFQ-----------------
FFQ_POIDS$PORC_FFQ<- rep(0,nrow(Frame))
ncol1 <- grep("de.la.viande.de.porc.sauf.charcuterie",colnames(Frame))
FFQ_POIDS$PORC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.viande.de.porc.sauf.charcuterie)
FFQ_POIDS_Int$de.la.viande.de.porc.sauf.charcuterie <- FFQ_POIDS$PORC_FFQ

  ##POULET_FFQ------------------
FFQ_POIDS$POULET_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.la.volaille.pouletdinde.du.lapin",colnames(Frame))
FFQ_POIDS$POULET_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.volaille.pouletdinde.du.lapin)
FFQ_POIDS_Int$de.la.volaille.pouletdinde.du.lapin <- FFQ_POIDS$POULET_FFQ

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

  ##SODAS_LIGHT_FFQ-------------------
FFQ_POIDS$SODAS_LIGHT_FFQ <- rep(0,nrow(Frame))
  ncol1 <- grep("de.lait.vegetal.sojarizavoine.", colnames(Frame))
  ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre", colnames(Frame))
  if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.lait.vegetal.sojarizavoine.) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre)
  FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
  FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2
}else{ 
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
  FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2
}

  ##SODAS_SUCRES_FFQ------------------
FFQ_POIDS$SODAS_SUCRES_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.sirop.verre", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre", colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.sirop.verre) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre)
  FFQ_POIDS_Int$de.sirop.verre <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2
}else{ 
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS_Int$de.sirop.verre <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2
}
FFQ_POIDS$SODAS_SUCRES_FFQ <- term1 + term2

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

#Somme 
FFQ_POIDS$POIDS_TOTAL_FFQ <- rowSums(FFQ_POIDS[,2:34])
FFQ_POIDS$POIDS_HORS_BOISSON_FFQ <- replace_na_with_zero(FFQ_POIDS$POIDS_TOTAL_FFQ- 
                                                           FFQ_POIDS$ALCOOL_FFQ - 
                                                           FFQ_POIDS$FRUITS_JUS_FFQ - 
                                                           FFQ_POIDS$LAIT_FFQ -
                                                           FFQ_POIDS$EAU_FFQ -
                                                           FFQ_POIDS$SODAS_LIGHT_FFQ -
                                                           FFQ_POIDS$SODAS_SUCRES_FFQ - 
                                                           FFQ_POIDS$CAFE_THE_FFQ)




#Ajout suffixe _Poids
FFQ_POIDS <- FFQ_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_FFQ$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )
#CALCUL DE  KCAL -----------------------------------
df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")
print(unique(df_long$Identifiant))



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
FFQ_KCAL$KCAL_TOTAL <- rowSums(FFQ_KCAL[, 2:ncol(FFQ_KCAL)], na.rm = TRUE)


# Calculer la somme des colonnes hors boisson
FFQ_KCAL$KCAL_HORS_BOISSON <- with(FFQ_KCAL, KCAL_TOTAL - 
                                                ALCOOL - 
                                                FRUITS_JUS - 
                                                LAIT - 
                                                EAU - 
                                                SODAS_LIGHT - 
                                                SODAS_SUCRES - 
                                                CAFE_THE)


# Ajouter le suffixe _FFQ_KCAL à chaque catégorie
FFQ_KCAL <- FFQ_KCAL %>%
  rename_with(
    ~ paste0(.x, "_FFQ_Kcal"),
    .cols = -Identifiant
  )




FFQ_KCAL$UC_TI <- NULL


#Constitution du tableau final ------------------------------------------------
FFQ_id <- metadata  
#On établit les listes de traitement en fonction du respect de l'envoi de chèques

if (campaign == "22-11" |campaign == "23-11") { FFQ_id$Periode <-0   }
if (campaign == "23-02" |campaign == "24-03") { FFQ_id$Periode <-1   }
FFQ_id$Mesure <- "FFQ"
FFQ_id <- inner_join(FFQ_id, FFQ_POIDS, by='Identifiant')
FFQ_id <- inner_join(FFQ_id, FFQ_KCAL, by='Identifiant')



# TELECHARGEMENT ----------------------------

# Créer un nouvel objet workbook
wb <- createWorkbook()

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", FFQ_id)

addWorksheet(wb, "Metadata")
writeData(wb, sheet = "Metadata", metadata)


addWorksheet(wb, "Frequences_corrigées")
writeData(wb, sheet = "Frequences_corrigées", Frame)

addWorksheet(wb, "Poids_corrigés")
writeData(wb, sheet = "Poids_corrigés", FFQ_POIDS_Int)

if (campaign == "22-11") {
  saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_22.xlsx")))
}else{ 
  if (campaign == "23-02") {
    saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_mars_23.xlsx"))) 
  } else {
    if (campaign == "23-11") {
      saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_nov_23.xlsx"))) 
    } else { 
      saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers prétraités/FFQ_Tableaux_mars_24.xlsx")))  
    }}}




