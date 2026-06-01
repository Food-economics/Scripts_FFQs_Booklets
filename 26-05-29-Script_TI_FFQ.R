# 1. LOADING THE WORK ENVIRONMENT ----
## Package imports ----

rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(readxl);library(dplyr);library(broom);library(scales)
library(modelsummary);library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");
library("dplyr");library("tidyr");library("ggplot2");library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library(questionr)

## Data import ----

    ### Choose campaign ---------------------
campaign<- "24-03" #22-11 #23-02 #23-11 #24/03 

    ### Import Nov_22 ------------------
questionnaire_nov_22<- read.xlsx(paste("22-11_FFQ.xlsx",sep=""))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\"","",.))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\\(", "",.))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\\)", "",.))

    ### Import March_2023 data ------------------
questionnaire_mars_23<- read.xlsx(paste("23-02_FFQ.xlsx",sep=""))
questionnaire_mars_23<- questionnaire_mars_23%>% mutate_all(~gsub("\"","",.))

    ### Import Nov_2023 data -------------------
questionnaire_nov_23<- read.xlsx(paste("23-11_FFQ.xlsx",sep=""))
questionnaire_nov_23<- questionnaire_nov_23%>% mutate_all(~gsub("\"","",.))

    ### Import March_2024 data --------------------
questionnaire_mars_24<- read.xlsx(paste("24-03_FFQ.xlsx",sep=""))
questionnaire_mars_24<- questionnaire_mars_24%>% mutate_all(~gsub("\"","",.))

### Import ancillary reference tables ----
# Load nutritional references, frequency encoding, portion sizes, and voucher summary tables
CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_FFQ.xlsx")
Encodage <- read_xlsx(paste("Freq_FFQ.xlsx"))
Taille_Portion <- read_xlsx(paste("Taille portion.xlsx"))
Recap_envoi_cheques <- read_excel("Recap_envoi_cheque.xlsx")



# 2. BUILDING A UNIFORM TABLE ACROSS ALL CAMPAIGNS ----
## Define helper functions for variable recoding ----
### Function to retrieve the column name based on the campaign ----
# get_column_name returns the name of the questionnaire column corresponding to a given campaign
get_column_name <- function(campaign) {
  switch(campaign,
         "22-11" = "questionnaire_nov_22","23-02" = "questionnaire_mars_23","23-11" = "questionnaire_nov_23","24-03" = "questionnaire_mars_24",stop("Campaign non reconnue"))}

### Function to recode variables based on the campaign ----
# recoder_variables renames columns in a data frame based on an encoding table and a specific campaign.
# data: a data frame whose columns are to be renamed.
# Encodage: a data frame containing the mapping between old and new column names,
#   with one column per campaign and a column "Aliment" containing the new names.
# campaign: a character string specifying the campaign.
# The function starts by copying data into tableau_recodé to avoid modifying the original.
# It then calls get_column_name(campaign) to retrieve the column name for the campaign in Encodage.
recoder_variables <- function(data, Encodage, campaign) {
  tableau_recodé <- data
  column_name <- get_column_name(campaign)
  
  # For each row in Encodage:
  # ancien_nom: the original column name for this row and campaign.
  # nouveau_nom: the new name to assign to this column.
  # grep is used to find all columns in tableau_recodé whose name contains ancien_nom.
  for (i in 1:nrow(Encodage)) {
    ancien_nom <- Encodage[[column_name]][i]
    nouveau_nom <- Encodage$Aliment[i]
    colonnes_similaires <- grep(ancien_nom, names(tableau_recodé), value = TRUE)
    # If matching columns are found, each is renamed using make.names to ensure the new name is valid.
    # The function returns tableau_recodé with the renamed columns.
    if (length(colonnes_similaires) > 0) {
      for (col in colonnes_similaires) {
        names(tableau_recodé)[names(tableau_recodé) == col] <- make.names(nouveau_nom)
      }} }
  return(tableau_recodé)}

## Apply the recoding function to produce a first data table with uniform variable names across campaigns ----
if (campaign == "22-11") { data <- questionnaire_nov_22 }else{ 
  if (campaign == "23-02") { data <- questionnaire_mars_23  } else {
    if (campaign == "23-11") {data <- questionnaire_nov_23 } else {  data <- questionnaire_mars_24  }}}
data <- recoder_variables(data, Encodage , campaign)

## Create a table that consolidates all variables from the different campaigns ----
# Extract the first column of Encodage (the reference food item names)
colonne_chaligne <- Encodage[, 1]
# t(colonne_chaligne) transposes the vector, converting rows to columns.
# data.frame(t(colonne_chaligne)) creates a new data frame Frame from this transposition.
Frame <- data.frame(t(colonne_chaligne))
# Frame[1, ] extracts the first row, which contains the desired column names.
# names(Frame) <- new_column_names assigns these values as column names of Frame.
new_column_names <- Frame[1,]
names(Frame) <- new_column_names
# Remove the first row of Frame
Frame <- Frame[-1, ]

## Impute values from data into the new Frame table ----
# If Frame has a different number of rows than data, adjust Frame to match.
# intersect(names(data), names(Frame)) finds column names common to both data and Frame.
# Copy values from common columns of data into Frame.
if (nrow(Frame) != nrow(data)) { Frame <- Frame[1:nrow(data), ]}
colonnes_communes <- intersect(names(data), names(Frame))
Frame[colonnes_communes] <- data[colonnes_communes]


# 3. TRANSLATING CONSUMPTION FREQUENCIES INTO NUMERIC VALUES ----
# Values in data are initially recorded as character strings.
# The following step translates these consumption frequencies into numeric values.
### Dictionary for food items ----
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

### Dictionary for beverages (glasses) ----
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

### Dictionary for beverages (cups/bowls) ----
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

### Standardise identifiers in Frame ----

### Function to create identifiers ----
# Concatenates a participant number and a store name to form a unique identifier
create_identifiant <- function(Frame, num_col, store_col) { 
  Frame$Identifiant <- paste(Frame[[num_col]], Frame[[store_col]], sep = "-")
  return(Frame)}

### Harmonise identifiers ----
# For campaigns 22-11 and 23-02: build the identifier from two separate columns
if (campaign == "22-11" | campaign == "23-02" ) {
  Frame  <- create_identifiant(Frame, "Numero.d.identifiant", "Nom.de.l.epicerie")
  Frame <- Frame %>% 
    relocate("Identifiant", .after = "N.Obs")
  Frame <- Frame[, !names(Frame) %in% "Numero.d.identifiant"]
}

# For campaigns 23-11 and 24-03: rename the identifier column directly
if (campaign == "23-11" | campaign=="24-03") { 
  Frame <- Frame %>% rename("Identifiant"="Numero.d.identifiant")
}

print(unique(Frame$Identifiant))

class(Frame$Identifiant)
### Identifier dictionary for participants whose ID changed ----
# Correct known identifier changes across campaigns
Frame$Identifiant[Frame$Identifiant == "PS004" ] <- "LE255"
Frame$Identifiant[Frame$Identifiant == "LE148" ] <- "PS284"
Frame$Identifiant[Frame$Identifiant == "LE195" ] <- "PS285"
Frame$Identifiant[Frame$Identifiant == "LE088" ] <- "PS286"
Frame$Identifiant[Frame$Identifiant == "LE207" ] <- "PS287"
Frame$Identifiant[Frame$Identifiant == "LE093" ] <- "PS288"
print(unique(Frame$Identifiant))


# Standardise the CCAS label for early campaigns
if (campaign == "22-11" | campaign == "23-02" ) {
  Frame <-Frame %>%
    mutate(Identifiant = gsub("-CCAS \\(inclus Pôle emploi et SPF\\)", "-CCAS", Identifiant))}


# Fix additional identifier typos
Frame$Identifiant[ Frame$Identifiant=="8447-CCAS" ] <- "8747-CCAS"
Frame$Identifiant[ Frame$Identifiant=="PE19-CCAS" ] <- "PE019-CCAS"
Frame$Identifiant[ Frame$Identifiant=="1564-Epimut" ]<- "1654-Epimut"
Frame$Identifiant[ Frame$Identifiant=="P E013-CCAS" ] <- "PE013-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP032-CCAS" ] <- "SP040-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP-052-CCAS" ] <- "SP052-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP-017-CCAS" ] <- "SP017-CCAS"
Frame$Identifiant[ Frame$Identifiant=="pe003-CCAS" ]<- "pe003-CCAS"

Frame_bis <- Frame

### Harmonise socio-demographic variables across campaigns ----

# 4. CREATING THE METADATA TABLE ----

## Extract variables of interest from Frame ----
# Use subset to select the columns of interest
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


# For the 24-03 campaign, household composition data are taken from the Nov 2023 questionnaire
# and merged with the current campaign metadata
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


# For the 23-02 campaign, the education level variable is extracted from the Nov 2022 questionnaire
# and merged with the current campaign metadata
if (campaign == "23-02") {
  # Create the new identifier column
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


# Fix identifier typos in metadata
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


# 5. COMPUTING CONSUMPTION UNIT (UC) COEFFICIENTS ----
# Convert child age-group columns to numeric and replace NA with 0
metadata$De.moins.de.3.ans <- as.numeric(metadata$De.moins.de.3.ans)
metadata$De.3.a.10.ans <- as.numeric(metadata$De.3.a.10.ans)
metadata$De.11.a.14.ans <- as.numeric(metadata$De.11.a.14.ans)
metadata$De.15.a.17.ans <- as.numeric(metadata$De.15.a.17.ans)
metadata$De.18.ans.et.plus <- as.numeric(metadata$De.18.ans.et.plus)

# Replace NAs with 0 in all specified columns
metadata$De.moins.de.3.ans[is.na(metadata$De.moins.de.3.ans)] <- 0
metadata$De.3.a.10.ans[is.na(metadata$De.3.a.10.ans)] <- 0
metadata$De.11.a.14.ans[is.na(metadata$De.11.a.14.ans)] <- 0
metadata$De.15.a.17.ans[is.na(metadata$De.15.a.17.ans)] <- 0
metadata$De.18.ans.et.plus[is.na(metadata$De.18.ans.et.plus)] <- 0
# Compute total number of children across all age groups
metadata$somme_enfants <- (metadata$De.moins.de.3.ans +
                             metadata$De.3.a.10.ans +
                             metadata$De.11.a.14.ans +
                             metadata$De.15.a.17.ans +
                             metadata$De.18.ans.et.plus)
# Determine the number of adults based on marital status (2 if couple, 1 otherwise)
metadata$adultes_mat <- ifelse(metadata$Quelle.est.votre.situation.matrimoniale.=="En couple non marié (PACS, concubinage…)"|
                                 metadata$Quelle.est.votre.situation.matrimoniale.=="Marié(e)" ,(2), (1))
metadata$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(metadata$Combien.de.personnes.vivent.dans.votre.foyer)
metadata$somme_enfants <- as.numeric(metadata$somme_enfants)
metadata$adultes_mat <- as.numeric(metadata$adultes_mat)
# Compute the number of adults, taking into account household size and marital status
metadata$adultes <- ifelse(metadata$Combien.de.personnes.vivent.dans.votre.foyer == 1, 
                           1, 
                           pmax(metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$somme_enfants, 
                                metadata$adultes_mat, 
                                na.rm = TRUE))

# Correct the count of children aged 18+ to avoid double-counting with the adult count
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

# Corrected count of children under 3 years old
metadata$enfant_moins_3_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.moins.de.3.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.moins.de.3.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.moins.de.3.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

# Corrected count of children aged 3-10
metadata$enfant_3_10_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.3.a.10.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.3.a.10.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.3.a.10.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

# Corrected count of children aged 11-14
metadata$enfant_11_14_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.11.a.14.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.11.a.14.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.11.a.14.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

# Corrected count of children aged 15-17
metadata$enfant_15_17_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.15.a.17.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.15.a.17.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.15.a.17.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

# Compute TI consumption units (UC_TI): adults and 18+ children count as 1; younger children as 0.5
metadata$UC_TI <- metadata$adultes + metadata$enfant_15_17_ans_cor+ metadata$enfant_18_cor + 0.5*(metadata$enfant_moins_3_ans_cor + metadata$enfant_3_10_ans_cor+ metadata$enfant_11_14_ans_cor)
# Compute INSEE consumption units (UC_INSEE) using the official equivalence scale
metadata$UC_INSEE <- ifelse(
  metadata$adultes == 1, 
  1, 
  1 + 0.5 * (metadata$adultes - 1)
) + 
  (metadata$enfant_moins_3_ans_cor + metadata$enfant_3_10_ans_cor + metadata$enfant_11_14_ans_cor) * 0.3 + 
  (metadata$De.15.a.17.ans + metadata$enfant_18_cor) * 0.5

## Compute UC and income per UC ----
metadata$Income_UC_INSEE <- as.numeric(metadata$Revenu.mensuel)/as.numeric(metadata$UC_INSEE)
# Remove rows with no sex information (incomplete records)
metadata <- subset(metadata, !(is.na(Sexe)))


## Compute age class central values ----
# Extract lower and upper bounds from age class labels and compute midpoints
calculate_central_values <- function(classes) {
  # Remove non-numeric characters and extract bounds
  lower_bounds <- as.numeric(gsub("\\D*(\\d+)-\\d+\\D*", "\\1", classes))
  upper_bounds <- as.numeric(gsub("\\D*\\d+-(\\d+)\\D*", "\\1", classes))
  
  # Compute central values
  central_values <- (lower_bounds + upper_bounds) / 2
  
  return(central_values)
}

## Apply the age class function to metadata ----
metadata$Age_Central <- calculate_central_values(metadata$Quel.age.avez.vous.)


# Standardise demographic variable labels across campaigns
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "BTS, DUT, DEST, DEUG (y compris formation paramédicale ou sociale)"),("BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat général"),("Baccalauréat"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "2ème ou 3ème cycle universitaire, grande école"),("2e ou 3e cycle universitaire, grande école"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))

metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Autre inactif invalide, handicapé, en congé maladie > 3 mois, titulaire d'une pension de réversion"),("Autre inactif (invalide, handicapé, en congé maladie > 3 mois, titulaire d'une pension de réversion)"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))
metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Femme ou homme au foyer y compris congé parental"),("Femme ou homme au foyer (y compris congé parental)"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))
metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Retraité(e) (ancien salarié) ou préretraité(e)"),("Retraitée ancien salarié ou préretraitée"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))

metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu. <- ifelse((metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Minimas sociaux RSA, Allocations familiales..."),("Minimas sociaux (RSA, Allocations familiales...)"),(metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))
metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu. <- ifelse((metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Travail salarié, autoentrepreneur..."),("Travail (salarié, autoentrepreneur...)"),(metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))

metadata$Quel.est.votre.pays.de.naissance. <- ifelse((metadata$Quel.est.votre.pays.de.naissance. == "Afrique Sub-saharienne ou Moyen-Orient (jusqu'en Iran)"),("Afrique sub-saharienne ou Moyen-Orient jusqu'à l'Iran"),(metadata$Quel.est.votre.pays.de.naissance. ))

metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Divorcée ou séparée"),("Divorcé(e) ou séparé(e)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "En couple non marié PACS, concubinage…"),("En couple non marié (PACS, concubinage…)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Mariée"),("Marié(e)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Veufve"),("Veuf(ve)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))

## Identify single-parent households ----
# A household is flagged as single-parent if children are present and the respondent is widowed, single, or divorced
metadata$Foyer_monoparental <- ifelse((metadata$Avez.vous.des.enfants.a.charge. =="Oui" & (metadata$Quelle.est.votre.situation.matrimoniale. == "Veuf(ve)" |metadata$Quelle.est.votre.situation.matrimoniale. == "Célibataire" |metadata$Quelle.est.votre.situation.matrimoniale. == "Divorce(e) ou séparé(e)")), (1), (0))

# Correction of food budgets ----
# Campaign-specific manual corrections for clearly erroneous declared food budgets
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

## Replace 0 with NA ----
metadata$Income_UC_INSEE[metadata$Income_UC_INSEE == 0] <- NA

# Compute food waste frequency ----
# Convert "every day" to 7 occurrences, otherwise NA
metadata$Si.ouien.avez.vous.jete.tous.les.jours. <- ifelse(metadata$Si.ouien.avez.vous.jete.tous.les.jours. == "Oui", 7, NA)
# Merge the two waste variables: use the daily value if the other is missing
metadata$Si.noncombien.de.fois.la.semaine.derniere. <- ifelse(is.na(metadata$Si.noncombien.de.fois.la.semaine.derniere.), 
                                                              metadata$Si.ouien.avez.vous.jete.tous.les.jours., 
                                                              metadata$Si.noncombien.de.fois.la.semaine.derniere.)

# Convert to numeric if not already done
metadata$Si.noncombien.de.fois.la.semaine.derniere. <- as.numeric(metadata$Si.noncombien.de.fois.la.semaine.derniere.)
# Standardise the unopened product waste column (remove non-numeric entries)
metadata$Gapillage.produits.non.entames  <- ifelse(!is.na(as.numeric(metadata$Gapillage.produits.non.entames )), metadata$Gapillage.produits.non.entames , "NA")
metadata$Gapillage.produits.non.entames <- as.numeric(metadata$Gapillage.produits.non.entames)
# Compute total weekly food waste frequency
metadata$freq_hebdo_gaspillage <- metadata$Si.noncombien.de.fois.la.semaine.derniere. + metadata$Gapillage.produits.non.entames
# Compute tertile thresholds for weekly waste frequency
tert <- quantile(metadata$freq_hebdo_gaspillage, probs = c(1/3, 2/3), na.rm = TRUE)


# Remove intermediate columns that are no longer needed
metadata<- metadata[, !names(metadata) %in% c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans",
                                              "De.15.a.17.ans", "De.15.a.17.ans", "De.18.ans.et.plus",
                                              "La.semaine.dernierevous.arrive.t.il.de.jeter.des.restes.apres.un.repas.",
                                              "Si.ouien.avez.vous.jete.tous.les.jours.", " Si.noncombien.de.fois.la.semaine.derniere.",
                                              "Gaspillage.restes.", "Gapillage.produits.non.entames")]

print(unique(metadata$Identifiant))

# Add SP041 data from Nov 2022 to the March 2023 campaign
if (campaign == "23-02") {
  file_path <- "FFQ_Tableaux_nov_22.xlsx"
  metadata22 <- read_excel(file_path, sheet = "Metadata") 
  ligne_SP041_CCAS <- metadata22 %>% filter(Identifiant == "SP041-CCAS")
  # Extract column names from metadata22 and ligne_SP041_CCAS
  colonnes_manquantes <- setdiff(colnames(metadata), colnames(ligne_SP041_CCAS))
  for (col in colonnes_manquantes) {
    ligne_SP041_CCAS[[col]] <- NA
  }
  #ligne_SP041_CCAS <- ligne_SP041_CCAS %>%
  #  select(all_of(colnames(metadata)))
  ligne_SP041_CCAS <- ligne_SP041_CCAS[ , colnames(metadata), drop = FALSE]
  
  
  metadata <- bind_rows(metadata, ligne_SP041_CCAS)
}

# 6. COMPUTING CORRECTION COEFFICIENTS FOR FREQUENCY ADJUSTMENT ----

## Helper function to replace NAs with zero ----
replace_na_with_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
## Function to extract numeric values from the data table ----
FREQ_intake <- function(data, x) {
  result <- as.numeric(data[[x]])
  result[is.na(result)] <- 0
  return(result)
}


# 7. ASSIGNING PORTION SIZES ----
## Define the function to replace portion codes with weights ----
# remplacer_poids takes two arguments (taille and aliment) and does the following:
# Displays a message indicating which food and size are being processed.
# Filters Taille_Portion_long to find the row matching the food and size, then extracts the weight.
# If no match is found, displays a message and returns NA; otherwise returns the extracted weight.
remplacer_poids <- function(taille, aliment) {message("Traitement de l'aliment: ", aliment, " et de la taille: ", taille)
  poids <- Taille_Portion_long %>%
    filter(Aliment == aliment, Taille == taille) %>%
    pull(Poids)
  if (length(poids) == 0) {message("Pas de correspondance trouvée pour ", aliment, " avec la taille ", taille)
    return(NA)}
  return(poids)}

## Build the portion weight table ----
### Filter portion columns ----
# Some food groups have a portion size (vegetables, raw vegetables, fish, steaks, sweet/savoury pies...)
# Isolate these columns and create a new dataframe "data_duplicated" containing only these values.
colonnes_portion <- grep("portion$", names(Frame), value = TRUE)
Frame_duplicated <- subset(Frame, select = c("Identifiant", colonnes_portion))

### Duplicate portion columns as many times as a food belongs to a general category ----
# The loop iterates over each unique category in Taille_Portion$Catégorie.
# For each category: skip NA categories; select columns in Frame whose names start with the category name;
# compute how many times the category appears in Taille_Portion; duplicate and append those columns.
for (categorie in unique(Taille_Portion$Catégorie)) {
  if (is.na(categorie)) next 
  # Find columns whose name starts with the value of categorie
  column_names <- grep(paste0("^", categorie), names(Frame), value = TRUE)
  
  # Select the corresponding columns
  columns <- Frame[, column_names, drop = FALSE]
  
  nb_repeats <- sum(Taille_Portion$Catégorie == categorie, na.rm = TRUE)
  for (i in 1:nb_repeats) {
    Frame_duplicated <- cbind(Frame_duplicated, columns)
  }
}

# This step aims to harmonise the structure of the dataframe.
# Each row corresponds to one identifier, and each category has been copied
# as many times as there are specific foods within it.
# Initialise Poids with data from Frame_duplicated
# Display the dataframe with duplicated columns
Poids <- print(Frame_duplicated)

# Separate the first two columns from the rest
debut <- Poids[, 1]
fin <- Poids[, -c(1)]

# Sort remaining columns alphabetically
fin_trie <- fin[, order(names(fin))]

# Merge the two parts back together
Poids <- cbind(debut, fin_trie)

# Identify columns ending with a digit
colonnes_a_garder <- grep("\\d$", names(Poids), value = TRUE)

# Keep only columns ending with a digit
Poids <- Poids[, colonnes_a_garder]

# Identify column names
colnames_data <- names(Poids)

# Count occurrences of each column name
occurrences <- table(colnames_data)

# Identify duplicated columns not ending with a digit
colonnes_a_supprimer <- names(occurrences[occurrences > 1])
colonnes_a_supprimer <- colonnes_a_supprimer[!grepl("\\d$", colonnes_a_supprimer)]
# Remove duplicated columns not ending with a digit
data_filtre <- Poids[, !names(Poids) %in% colonnes_a_supprimer]
# Display the filtered dataframe
print("DataFrame filtré:")
Poids <- print(data_filtre)

### Rename copied columns with the specific food name ----
# This step renames the copied portion columns using the specific food names from Taille_Portion
new_column_names <- Taille_Portion$Aliment[match(names(Poids), Taille_Portion$Catégorie2)]
names(Poids) <- new_column_names

### Apply the function to replace portion sizes with weights ----
# Convert Taille_Portion to long format:
# pivot_longer stacks all columns except Aliment into two new columns: Taille (former column names)
# and Poids (former values). na.omit removes rows with NA. filter excludes "Catégorie" and "Catégorie2" rows.

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


### Replace portion sizes with weights using the remplacer_poids function ----
Poids_modifie <- Poids
for (col in names(Poids_modifie)) {
  Poids_modifie[[col]] <- sapply(Poids_modifie[[col]], function(taille) remplacer_poids(taille, col))
}

### Add unit weights for foods with a fixed portion size ----
# Filtering: select only rows in Taille_Portion_long where Taille == "Poids_unitaire".
# Loop: for each row in filtered_df1, extract the food name and weight value,
# then assign this fixed weight to the corresponding column in Poids_modifie.

filtered_df1 <- subset(Taille_Portion_long, Taille == "Poids_unitaire")
for (i in 1:nrow(filtered_df1)) {
  aliment <- filtered_df1$Aliment[i]
  valeur <- filtered_df1$Poids[i]
  Poids_modifie[[aliment]] <- valeur
}

print(unique(Frame$Identifiant))


# Finalise the table
Poids_modifie <- cbind(Frame$Identifiant, Poids_modifie)
names(Poids_modifie)[1] <- "Identifiant"
Poids_modifie[,-1] <- lapply(Poids_modifie[,-1], as.numeric)


# 8. COMPUTING CONSUMED FOOD WEIGHTS ----
# Compute food weights by multiplying consumption frequencies by portion sizes for all foods
FFQ_POIDS <-data.frame(Frame$Identifiant)
names(FFQ_POIDS)[1] = "Identifiant"

FFQ_POIDS_Int <-data.frame(Frame$Identifiant)
names(FFQ_POIDS_Int )[1] = "Identifiant"

##ALCOOL_FFQ ----
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


##CAFE_THE_FFQ ----
FFQ_POIDS$CAFE_THE_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.cafe.y.compris.decafeine", colnames(Frame))
ncol2 <- grep("de.the",colnames(Frame))
term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) *  Poids_modifie$de.cafe.y.compris.decafeine )
term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.the  )
FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2 
FFQ_POIDS_Int$de.cafe.y.compris.decafeine <- term1
FFQ_POIDS_Int$de.the <- term2


##CEREALES_PD_FFQ ----
FFQ_POIDS$CEREALES_PD_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli",colnames(Frame))
FFQ_POIDS$CEREALES_PD_FFQ  <- FREQ_intake(Frame,ncol1)*Poids_modifie$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli
FFQ_POIDS_Int$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli <- FFQ_POIDS$CEREALES_PD_FFQ

#CHARCUTERIE_HORS_JBc_FFQ ----
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

##DESSERTS_LACTES_FFQ ----
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

##EAU_FFQ ----
# For campaigns 22-11 and 23-02: multiply frequency by portion size;
# for later campaigns, water quantity is already expressed in litres
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


##FEC_NON_RAF_FFQ ----
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

##FEC_RAF_FFQ ----
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

##FROMAGES_FFQ ----
FFQ_POIDS$FROMAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.en.morceaux","du.Roquefortdu.Bleu.quelle.qu.en.soit.l.origine",
                "du.fromage.de.chevre","autres.types.de.fromages.camembertbrie.","de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.rape.sur.les.plats.patesriz.",
                "du.fromage.a.pate.molle.camembertcoulommiersbrie.","du.fromage.a.tartiner.cancoillotteSaint.MoretVache.qui.rit.","de.la.mozzarella")
terms <- numeric(nrow(Frame))
non_reconnues <- c()

for (cat in categories) {
  ncol <- grep(cat, colnames(Frame))
  
  if (length(ncol) == 0) {
    # Add to the list of unrecognised categories
    non_reconnues <- c(non_reconnues, cat)
  } else {
    # Apply the formula if the column is found
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[cat]])
    terms <- terms + term
    FFQ_POIDS_Int[[cat]] <- term
  }
}

FFQ_POIDS$FROMAGES_FFQ <- terms

# Display unrecognised categories
if (length(non_reconnues) > 0) {
  warning("Colonnes non reconnues dans 'Frame' :\n", paste(non_reconnues, collapse = "\n"))
}

##FRUITS_FFQ ----
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

##FRUITS_JUS_FFQ ----
# For early campaigns: multiply frequency by portion size; for later ones the value is already in litres
FFQ_POIDS$FRUITS_JUS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre", colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  FFQ_POIDS$FRUITS_JUS_FFQ<-  replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre)) 
  FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ
} else {
  FFQ_POIDS$FRUITS_JUS_FFQ <-  replace_na_with_zero(FREQ_intake(Frame, ncol1)) 
  FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ
}

##FRUITS_SECS_FFQ ----
FFQ_POIDS$FRUITS_SECS_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.seches.abricotsdattesfiguespruneaux.",colnames(Frame))
FFQ_POIDS$FRUITS_SECS_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.seches.abricotsdattesfiguespruneaux.)
FFQ_POIDS_Int$des.fruits.seches.abricotsdattesfiguespruneaux. <- FFQ_POIDS$FRUITS_SECS_FFQ

##JAMBON_BLANC ----
FFQ_POIDS$JAMBON_BLANC_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("du.jambon.blanc",colnames(Frame))
FFQ_POIDS$JAMBON_BLANC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.jambon.blanc)
FFQ_POIDS_Int$du.jambon.blanc <- FFQ_POIDS$JAMBON_BLANC_FFQ


##LAIT_FFQ ----
# For campaigns 22-11 and 23-02: multiply by portion size; for later campaigns value is already in litres
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

##LAITAGES_FFQ ----
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

##LEG_SECS_FFQ ----
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

##LEGUMES_FFQ ----
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
# Add onion for campaigns 23-11 and 24-03 (fixed portion of 50 g)
if (campaign == "23-11" | campaign == "24-03" ) {
  ncol1 <- grep("de.l.oignon.2", colnames(Frame))
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) * 0.05)
  FFQ_POIDS$LEGUMES_FFQ <- FFQ_POIDS$LEGUMES_FFQ + term1
  FFQ_POIDS_Int$de.l.oignon.2 <- term1
}

##MGA_FFQ ----
FFQ_POIDS$MGA_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates.",colnames(Frame))
ncol2 <- grep("de.la.creme.fraiche",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. )
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.la.creme.fraiche)
FFQ_POIDS$MGA_FFQ <- term1 + term2 
FFQ_POIDS_Int$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. <- term1
FFQ_POIDS_Int$de.la.creme.fraiche <- term2

##MGV_FFQ ----
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

##NOIX_FFQ ----
FFQ_POIDS$NOIX_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.a.coque.noixnoisettesamandes.",colnames(Frame))
FFQ_POIDS$NOIX_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)
FFQ_POIDS_Int$des.fruits.a.coque.noixnoisettesamandes. <- FFQ_POIDS$NOIX_FFQ 

##OEUFS_FFQ ----
# For campaigns 22-11 and 23-02: use fixed portion weight;
# for later campaigns: multiply by the number of eggs per meal and a unit weight of 60 g
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


##PDTS_SUCRES_FFQ ----
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
# For campaigns 23-11 and 24-03: add added sugar (spoons/cubes) at 7 g per unit
if (campaign == "23-11" | campaign == "24-03" ) { 
  ncol1 <- grep("Lorsque.vous.buvez.du.cafe.the.ou.mangez.un.yaourt.fromage.blanccombien.de.cuilleres.ou.carres.de.sucre.rajoutez.vous.",colnames(Frame))
  term1 <-  replace_na_with_zero((FREQ_intake(Frame,ncol1)*0.07))
  FFQ_POIDS$PDTS_SUCRES_FFQ <- FFQ_POIDS$PDTS_SUCRES_FFQ + term1
  FFQ_POIDS_Int$Lorsque.vous.buvez.du.cafe.the.ou.mangez.un.yaourt.fromage.blanccombien.de.cuilleres.ou.carres.de.sucre.rajoutez.vous. <- term1 }

##PLAT_PREP_FFQ ----
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

##PLAT_PREP_VEGETARIENS_FFQ ----
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

##POISSONS_FFQ ----
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

##PORC_FFQ ----
FFQ_POIDS$PORC_FFQ<- rep(0,nrow(Frame))
ncol1 <- grep("de.la.viande.de.porc.sauf.charcuterie",colnames(Frame))
FFQ_POIDS$PORC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.viande.de.porc.sauf.charcuterie)
FFQ_POIDS_Int$de.la.viande.de.porc.sauf.charcuterie <- FFQ_POIDS$PORC_FFQ

##POULET_FFQ ----
FFQ_POIDS$POULET_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.la.volaille.pouletdinde.du.lapin",colnames(Frame))
FFQ_POIDS$POULET_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.volaille.pouletdinde.du.lapin)
FFQ_POIDS_Int$de.la.volaille.pouletdinde.du.lapin <- FFQ_POIDS$POULET_FFQ

##QUICHES_PIZZAS_TARTES_SALLEES ----
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

##SAUCES_FFQ ----
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

##SNACKS_AUTRES_FFQ ----
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

##SODAS_LIGHT_FFQ ----
# For early campaigns: multiply by portion size; for later ones the quantity is already expressed in litres
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

##SODAS_SUCRES_FFQ ----
# For early campaigns: multiply by portion size; for later ones the quantity is already expressed in litres
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

##VIANDE_ROUGE_FFQ ----
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

# Compute total weight and weight excluding beverages
FFQ_POIDS$POIDS_TOTAL_FFQ <- rowSums(FFQ_POIDS[,2:34])
FFQ_POIDS$POIDS_HORS_BOISSON_FFQ <- replace_na_with_zero(FFQ_POIDS$POIDS_TOTAL_FFQ- 
                                                           FFQ_POIDS$ALCOOL_FFQ - 
                                                           FFQ_POIDS$FRUITS_JUS_FFQ - 
                                                           FFQ_POIDS$LAIT_FFQ -
                                                           FFQ_POIDS$EAU_FFQ -
                                                           FFQ_POIDS$SODAS_LIGHT_FFQ -
                                                           FFQ_POIDS$SODAS_SUCRES_FFQ - 
                                                           FFQ_POIDS$CAFE_THE_FFQ)


# Add _Poids suffix to all _FFQ columns
FFQ_POIDS <- FFQ_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_FFQ$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )

# 9. COMPUTING KILOCALORIES ----
# Pivot FFQ_POIDS_Int to long format and join with CALNUT to get nutritional values per food item
df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")
print(unique(df_long$Identifiant))


# Compute kilocalories per food per TI group
df_long$nrj_kcal_alim <- df_long$nrj_kcal*df_long$Poids*10
FFQ_KCAL <- aggregate(nrj_kcal_alim ~  Identifiant + groupe_TI_TdC   , df_long, FUN = sum)
# Pivot to wide format: one column per TI food group
FFQ_KCAL<- pivot_wider(
  FFQ_KCAL,
  id_cols = Identifiant,
  names_from = groupe_TI_TdC,
  values_from = nrj_kcal_alim
)


# Compute total calories and calories excluding beverages for each participant
FFQ_KCAL$KCAL_TOTAL <- rowSums(FFQ_KCAL[, 2:ncol(FFQ_KCAL)], na.rm = TRUE)

FFQ_KCAL$KCAL_HORS_BOISSON <- with(FFQ_KCAL, KCAL_TOTAL - 
                                     ALCOOL - 
                                     FRUITS_JUS - 
                                     LAIT - 
                                     EAU - 
                                     SODAS_LIGHT - 
                                     SODAS_SUCRES - 
                                     CAFE_THE)


# Add the _FFQ_KCAL suffix to each food group column
FFQ_KCAL <- FFQ_KCAL %>%
  rename_with(
    ~ paste0(.x, "_FFQ_Kcal"),
    .cols = -Identifiant
  )

FFQ_KCAL$UC_TI <- NULL

# 10. BUILDING THE FINAL OUTPUT TABLE ----
FFQ_id <- metadata  
# Assign a period indicator based on the campaign (0 = pre, 1 = post)
if (campaign == "22-11" |campaign == "23-11") { FFQ_id$Periode <-0   }
if (campaign == "23-02" |campaign == "24-03") { FFQ_id$Periode <-1   }
FFQ_id$Mesure <- "FFQ"
# Join weight and kilocalorie indicator tables to the metadata
FFQ_id <- inner_join(FFQ_id, FFQ_POIDS, by='Identifiant')
FFQ_id <- inner_join(FFQ_id, FFQ_KCAL, by='Identifiant')


# 11. EXPORT ----

# Create a new workbook object
wb <- createWorkbook()

# Add each dataframe as a separate sheet
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", FFQ_id)

addWorksheet(wb, "Metadata")
writeData(wb, sheet = "Metadata", metadata)


addWorksheet(wb, "Frequences_corrigées")
writeData(wb, sheet = "Frequences_corrigées", Frame)

addWorksheet(wb, "Poids_corrigés")
writeData(wb, sheet = "Poids_corrigés", FFQ_POIDS_Int)

# Save the workbook to the appropriate file path based on the campaign
if (campaign == "22-11") {
  saveWorkbook(wb,(paste0("FFQ_Tableaux_nov_22.xlsx")))
}else{ 
  if (campaign == "23-02") {
    saveWorkbook(wb,(paste0("FFQ_Tableaux_mars_23.xlsx"))) 
  } else {
    if (campaign == "23-11") {
      saveWorkbook(wb,(paste0("FFQ_Tableaux_nov_23.xlsx"))) 
    } else { 
      saveWorkbook(wb,(paste0("FFQ_Tableaux_mars_24.xlsx")))  
    }}}
