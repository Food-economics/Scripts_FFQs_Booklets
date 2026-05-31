# SCRIPT: CSGA Food Supply Booklet — Cleaning, Imputation, and Indicator Calculation
# PURPOSE: Process raw CSGA food purchase records (COD-Achats) to produce
#          daily food weight (kg/person/day) and energy (kcal/person/day)
#          indicators per participant and per TI food group.
# OUTPUT: Carnets_CSGA.xlsx — indicator table and cleaned raw data.



# 1. Package loading and working environment setup ----
# WORKING ENVIRONMENT SETUP
## Package import
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx); library(readxl);library(dplyr);
library(broom);library(scales);library(modelsummary);library(ggplot2);library(effsize);library(lfe);
library(ggpubr);library(vtable);library;library("openxlsx");
library("dplyr"); library("tidyr");library("ggplot2");library("gridExtra");library(lubridate);
library("RColorBrewer");library(reshape2);library(Metrics);library(questionr);library(zoo)
## Data import
researcher<-""
if (researcher == "") { setwd <- paste0("")} else {
  setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))}

### Enter the campaign date
campaign<-"23-02"
Nj <- 28 #"Number of recording days


# 2. Import raw CSGA data and reference tables ----
### Import CSGA data
researcher<-""
if (researcher == "") {
  setwd <- paste0("")
} else {
  setwd(paste0(""))
}
# Main food purchase records table (one row per food item purchased).
# UPDATE this path to match your local file system.
resultats_codachats <- read_excel(
  path  = "Données_CSGA.xlsx",
  sheet = "Table_appli"
)

# Corrected FFQ data — used to add gender and food budget to the purchase records.
#CORRECTED FFQ DATA
CSGA_FFQ <- read_excel(
  path  = "Fichiers prétraités/FFQ_CSGA.xlsx",
  sheet = "Frequences_corrigées"
)

# Full FFQ file (all sheets) — loaded for participant ID reference.
CSGA_FFQ_id <- read_excel(
  path  = "Fichiers prétraités/FFQ_CSGA.xlsx",
)

# Quick check: proportion of missing values in price columns.
describe(is.na(resultats_codachats$Prix_detail_VF))
describe(is.na(resultats_codachats$Prix_global_VF))
### Import reference tables
# CALNUT: nutritional composition table linked by CIQUAL food code.
# magasins: store classification table (store name → Lieu1 / Lieu2 type).
# resultats_pondérés: weighted average nutritional and environmental values by TI food group.
# Poids_unitaires_manquants: unit weights for foods with no standard portion.
CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_CARNET.xlsx")
magasins <- read_excel("Reclassement_magasins.xlsx")

resultats_pondérés <- read_excel("moyennes_pondérées.xlsx")
Poids_unitaires_manquants <- read_excel("poids_unitaire_manquants.xlsx")
resultats_pondérés <- read_excel("resultats_pondérés.xlsx")


# 3. Date parsing and participant-level enrichment ----
# CLEANING COD-ACHATS FILE: LOCATIONS / DATES / CUSTOM LABEL / CIQUAL LABEL

# Convert the Date column to proper Date format.
resultats_codachats$Date <- as.Date(
  resultats_codachats$Date,
  format = "%Y-%m-%d"
)


# Add gender and monthly food budget from the FFQ dataset.
#Add gender and monthly income
Ajout <- CSGA_FFQ[, c("Identifiant", "Sexe", "Budget.mensuel.alimentation.")]
names(resultats_codachats)[1] <- "Identifiant"
resultats_codachats <- left_join(Ajout, resultats_codachats,  by = "Identifiant")
# Remove redundant intermediate columns produced by the join.
#REMOVE EXTRA COLUMNS
resultats_codachats <- resultats_codachats[, -c(15:25)]
resultats_codachats <- resultats_codachats[, -c(22:30)]


# 4. Store name standardization and location join ----
### Join food supply locations and store classification by type "Lieu 1" / "Lieu 2"

# Rename column 4 to a clean final name.
names(resultats_codachats)[4] <- "Lieu_vf"
# Standardize store name variants to a single canonical label.
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
# Join the store classification table to add Lieu1 / Lieu2 type columns.
resultats_codachats <- left_join(resultats_codachats, magasins, by=c("Lieu_vf"))


# 5. Join with the CALNUT nutritional reference table ----
## Join COD-Achats and CALNUT
# MERGE the CALNUT reference table with the purchase records (resultats_codachats).
# Rename "CodeCIQUAL" to "CODACHATS_alim_code" and "Categorie1" to "groupe_TI_TdC".
# resultats_codachats and CALNUT are joined on CODACHATS_alim_code.
colnames(resultats_codachats)[colnames(resultats_codachats) == 'CodeCIQUAL'] <- 'CODACHATS_alim_code'
resultats_codachats <- left_join(resultats_codachats, CALNUT, by=c("CODACHATS_alim_code"))
#Rename "Category1" with groupe_TI_TdC
colnames(resultats_codachats)[colnames(resultats_codachats) == 'groupe_TI_TdC'] <- 'groupe_TI_TdC1'


# 6. Cleaning: quantities, prices, and units ----
# CLEANING COD-ACHATS FILE: WEIGHT / PRICE / UNITS
# If weight equals 0 grams, apply a conversion to units.
names(resultats_codachats)[9] <- "Unite"
names(resultats_codachats)[26] <- "LibelleCIQUAL"

# Reclassify white ham (JAMBON_BLANC) into the cold cuts category (CHARCUTERIE_HORS_JB).
resultats_codachats$groupe_TI_TdC1[resultats_codachats$groupe_TI_TdC1 == "JAMBON_BLANC"] <- "CHARCUTERIE_HORS_JB"

# Compute the first recorded date per participant (used to calculate the day number).
resultats_codachats <- resultats_codachats %>%
  group_by(Identifiant) %>%
  mutate(
    date_starting = min(Date, na.rm = TRUE)
  ) %>%
  ungroup()

# Remove receipts recorded beyond 28 days from the participant's start date.
#Remove receipts with more than 28 recording days
resultats_codachats$jour_num <-
  as.numeric(resultats_codachats$Date - resultats_codachats$date_starting) + 1
table(resultats_codachats$jour_num)
resultats_codachats <- resultats_codachats[resultats_codachats$jour_num <= 28, ]


# 7. Unit corrections — general rules ----
# CLEANING COD-ACHATS FILE: WEIGHT / PRICE / UNITS

# If quantity > 30 units and the food is not eggs, coffee/tea, or compote, convert to grams.
# (CODAPPRO assigns -1 for "don't know" → replace with NA)
resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = ifelse(
      # original condition…
      Nb > 30 &
        Unite == "unités" &
        # …and the food is NOT compote
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

# If quantity, price, PrixMenu or appreciation is ≤ 0 (CODAPPRO assigns -1 for "don't know"), set to NA.
resultats_codachats$Prix[resultats_codachats$Prix < 0 ] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu < 0 ] <- NA
resultats_codachats$Nb[resultats_codachats$Nb <= 0 ] <- NA
resultats_codachats$Appreciation[resultats_codachats$Appreciation < 0 ] <- NA
# If price or PrixMenu equals 0 and the location is not a donation, set to NA.
resultats_codachats$Prix[resultats_codachats$Prix == 0 & resultats_codachats$Lieu1 != "dons" ] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu == 0 & resultats_codachats$Lieu1 != "dons" ] <- NA
# If quantity equals 0, set to NA.
resultats_codachats$Nb[resultats_codachats$Nb == 0 ] <- NA

## Correction of main conversion errors
# If quantity < 10g and not spices, multiply by 1000 (weight was entered in kilos).
# If price > 100€ and food is not alcohol, set to NA.
resultats_codachats$Nb <- ifelse((resultats_codachats$Unite == "grammes" & resultats_codachats$Nb < 10 & resultats_codachats$groupe_TI_TdC1 != "EPICES_CONDIMENTS" & resultats_codachats$Lieu2 != "RHD"), (resultats_codachats$Nb*1000 ), (resultats_codachats$Nb))
resultats_codachats$Prix <- ifelse((resultats_codachats$Prix > 100 & resultats_codachats$groupe_TI_TdC1 != "ALCOOL"), (NA), (resultats_codachats$Prix))

# If quantity > 50 kilos → convert to grams.
# If 10g < quantity < 20.5g → convert to units.
# If quantity < 1 cl → convert to litres.
# If quantity > 1000 kilos → convert to grams.
resultats_codachats$Unite <- with(resultats_codachats,
                                  ifelse( Nb > 50 & Unite == "kilos", "grammes",
                                          ifelse(Nb > 10 & Nb < 20.5 & Unite == "grammes", "unités",
                                                 ifelse( Nb < 1 & Unite == "centilitres", "litres",
                                                         ifelse(Nb > 1000 & Unite == "kilos", "grammes", Unite)))))


## Adjustment when price is low and quantity is high
resultats_codachats$Nb <- with(resultats_codachats,ifelse(Prix < 3 & Nb > 17 & Unite == "litres", Nb /10,Nb))
## Case-by-case corrections

# Add a price per kg column (used as a plausibility check in subsequent corrections).
#Add price per kg
resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


# 8. Case-by-case quantity corrections ----
### New condition: multiply Nb by 10 for specific foods if Nb <= 20g
# (indicates the quantity was entered in units of 10g rather than grams)
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

### New condition: divide price by 10 if Nb >= 100g and price >= €10 for specific foods
# (indicates price was entered 10× too high)
aliments_specifiques <- c("Saumon fumé", "Barres chocolatées", "Pâte à tartiner chocolat et noisette", "Rosette ou Fuseau", "Pâtisserie (aliment moyen)",
                          "Pomme de terre de conservation, crue", "Chocolat, en tablette (aliment moyen)", "Mélange apéritif graine non salée fruit séché",
                          "Mozzarella au lait de vache", "Sandwich baguette, jambon emmental")
resultats_codachats$Prix <- ifelse(
  resultats_codachats$LibelleCIQUAL %in% aliments_specifiques &
    resultats_codachats$Nb >= 100 &
    resultats_codachats$Prix >= 10 & resultats_codachats$Prix_kg > 0.05 & resultats_codachats$Unite == "grammes",
  resultats_codachats$Prix / 10,
  resultats_codachats$Prix)

### New condition: convert unit to centilitres if Nb < 100g for specific liquid foods
aliments_specifiques <- c( "Bière \"de spécialités\" ou d'abbaye, régionales ou d'une brasserie (degré d'alcool variable)",
                           "Boisson gazeuse, sans jus de fruit, sucrée","Jus de fruits (aliment moyen)", "Boisson préparée à partir de sirop à diluer type menthe, fraise, etc., sucré, dilué dans l'eau",
                           "Huile de pépins de raisin")
resultats_codachats <- within(resultats_codachats, {
  Unite <- ifelse(
    LibelleCIQUAL %in% aliments_specifiques & Nb < 100 & Unite == "grammes" & Prix_kg > 0.05 ,
    "centilitres",
    Unite
  )})

### Case-by-case corrections on quantities
print(unique(resultats_codachats$Identifiant))

# Assign a unit weight of 63g per egg (standard French egg weight).
#Assign unit weight of 63 grams to eggs
resultats_codachats <- resultats_codachats %>%
  mutate(
    # convert quantity
    Nb = if_else(
      groupe_TI_TdC1 == "OEUFS" & Unite == "unités",
      Nb * 0.063,
      Nb
    ),
    # update unit
    Unite = if_else(
      groupe_TI_TdC1 == "OEUFS" & Unite == "unités" & !is.na(Nb),
      "kilos",
      Unite
    )
  )
oeufs <- resultats_codachats[resultats_codachats$groupe_TI_TdC1 == "OEUFS", ]

# Individual aubergine correction: 75g at €3.98 is clearly a full bag → 750g.
resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Aubergine, crue" &
    resultats_codachats$Nb == 75 &
    resultats_codachats$Prix == 3.98,
  750,
  resultats_codachats$Nb
)


# Assign unit weight of 100g per compote pot (1 pot = 100g).
#1 compote pot = 100g
resultats_codachats <- resultats_codachats %>%
  mutate(
    # convert quantity
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
    # update unit
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


# Correct a misclassified almond entry: 0.2 centilitres → kilos.
resultats_codachats$Unite <- ifelse(
  (resultats_codachats$LibelleCIQUAL == "Amande, avec peau" &
     resultats_codachats$Nb == 0.2 &
     resultats_codachats$Unite == "centilitres"),
  "kilos",
  resultats_codachats$Unite
)

df_libelles <- unique(resultats_codachats$LibelleCIQUAL)

# Keep only labels starting with "Su" (used to inspect suspicious sugar-related entries).
# Keep only those starting with "S"
libelles_S <- df_libelles[grepl("^Su", df_libelles)]


# Unit weight for sushi = 0.04 kg (40g per piece).
# Unit weight for sushi = 0.04 kg (i.e. 40g)
resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
    resultats_codachats$Unite      == "unités",
  resultats_codachats$Nb * 0.04,  # multiply by 0.04 (kg)
  resultats_codachats$Nb
)

# Convert unit from "unités" to "kilos" for sushi.
# Change unit from "unités" to "kilos"
resultats_codachats$Unite <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
    resultats_codachats$Unite      == "unités",
  "kilos",
  resultats_codachats$Unite
)

# Inspect all sushi rows for verification.
# Select rows where LibelleCIQUAL matches exactly this text
subset_sushi <- resultats_codachats %>%
  filter(LibelleCIQUAL == "Sushi ou maki aux produits de la mer")


# Further individual quantity corrections.
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


# 9. Case-by-case unit corrections ----
### Case-by-case corrections on units
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


# 10. Case-by-case price corrections ----
### Case-by-case corrections on prices
# If price equals 100, set to NA (likely a CODAPPRO default value).
resultats_codachats$Prix <- ifelse((resultats_codachats$LibelleCIQUAL == "Oeuf, à la coque" & resultats_codachats$Prix==100),(NA), (resultats_codachats$Prix))
aliments_specifiques <- c("Pâtes sèches standard, cuites, non salées","Pâtes sèches standard, crues")
resultats_codachats$Prix <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Prix==100, 1, resultats_codachats$Prix)
resultats_codachats$Prix <- ifelse((resultats_codachats$LibelleCIQUAL == "Ravioli chinois vapeur à la crevette" & resultats_codachats$Nb==105 & resultats_codachats$Prix==7.5),(NA), (resultats_codachats$Prix))
aliments_specifiques <- c("Abat, cru (aliment moyen)", "Abat, cuit (aliment moyen)",
                          "Thon, cru ", "Accra de poisson")
resultats_codachats$Prix <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Prix == 100, 10, resultats_codachats$Prix)


# 11. Imputation of missing weights and prices ----
# IMPUTATION OF MISSING WEIGHTS AND PRICES
## Correction of remaining price data
# During the November 2023 campaign, some participants entered in-store purchases
# as "PrixMenu". These purchases must not be treated as out-of-home meals.
# We set PrixMenu to NA for these cases — the weight/price imputation procedure
# will be applied to the same foods later.
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="commerce" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="Epicerie" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="commerce" | resultats_codachats$Lieu2 =="Epicerie"), (NA), (resultats_codachats$Menu))
# If the product comes from out-of-home catering (RHD): Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(NA),(resultats_codachats$Prix))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="RHD" ), ("Oui"), (resultats_codachats$Menu))
resultats_codachats$Nb <-ifelse(((resultats_codachats$Lieu2 =="RHD") & resultats_codachats$Nb <9 & resultats_codachats$Unite =="grammes"    ),(NA),(resultats_codachats$Nb ))
resultats_codachats$Unite <-ifelse(((resultats_codachats$Lieu2 =="RHD") & is.na(resultats_codachats$Nb) & resultats_codachats$Unite =="grammes" ),(NA),(resultats_codachats$Unite ))

# If the product is a donation: Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="dons")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="dons")),(NA),(resultats_codachats$Prix))
# If not a menu item, set PrixMenu to NA.
# If not a Menu, set prixMenu to NA
resultats_codachats$PrixMenu <-ifelse((is.na(resultats_codachats$Menu)),(NA),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse((is.na(resultats_codachats$PrixMenu)),(resultats_codachats$Prix),(NA))

# Divide the menu price by the total number of food items purchased together.
# Group purchases by date and supply location
resultats_codachats <- resultats_codachats %>% arrange(Date)
grouped_data <- resultats_codachats %>% group_by(Date, Lieu_vf)
# Copy the menu price to all rows of the same receipt.
# Copy menu price to all rows on the same receipt
resultats_codachats <- grouped_data %>% mutate(PrixMenu = ifelse(row_number() == 1, PrixMenu / n(), PrixMenu))
# Compute the individual price per food item.
# Calculate the individual price for each food item
resultats_codachats <- grouped_data %>% mutate(PrixMenu = ifelse(PrixMenu, PrixMenu[1] / n(), PrixMenu))
# If at this stage RHD prices are zero, set Menu to NA.
#If RHD food prices are zero at this point, assign NA
resultats_codachats$Menu[resultats_codachats$PrixMenu == 0 ] <- NA
resultats_codachats$Menu[resultats_codachats$Menu == 0 ] <- NA


# 12. Create harmonized weight (kg) and price (€) variables ----
## Create Prix_all (€) and Poids (kg) variables
# Two unified variables bringing all weight and price data under the same unit.
resultats_codachats$Poids <- resultats_codachats$Unite
resultats_codachats$Prix_all <- resultats_codachats$Prix
# Fill Prix_all with PrixMenu where Prix is missing.
resultats_codachats$Prix_all <-ifelse((is.na(resultats_codachats$Prix_all)),(resultats_codachats$PrixMenu),(resultats_codachats$Prix_all))
# Convert all quantity/unit combinations to kilograms.
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
# Spot check: verify compote entries after conversion.
resultats_codachats %>%
  filter(LibelleCIQUAL == "Compote de fruits, allégée en sucres") %>%
  select(Nb, Unite)

# Replace zeros with NA for Poids, Nb, and Prix_all.
resultats_codachats$Poids <-ifelse((resultats_codachats$Unite == "unités" & is.na(resultats_codachats$Poids) ),(NA),(resultats_codachats$Poids))
resultats_codachats$Nb <- ifelse((resultats_codachats$Nb== 0 ), (NA), (resultats_codachats$Nb))
resultats_codachats$Poids <- ifelse((resultats_codachats$Poids == 0 ), (NA), (resultats_codachats$Poids))
resultats_codachats$Prix_all <- ifelse((resultats_codachats$Prix == 0 & resultats_codachats$Lieu1!= "dons" ), (NA), (resultats_codachats$Prix_all ))

# Quick check: proportion of missing weight and price values.
describe(is.na(resultats_codachats$Poids))
describe(is.na(resultats_codachats$Prix_all))

## Calculate price per kg for all food items
resultats_codachats$Prix_kg <- resultats_codachats$Prix_all / resultats_codachats$Poids
print(unique(resultats_codachats$Prix_kg))


# 13. Compute mean price/kg for imputation — three levels ----
## Calculate mean price per kg and mean weight for imputation by CIQUAL code × Lieu1
# Calculate the mean price per unit of weight for each food item.
prix_moyen_par_poids1 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # no NA
    str_trim(LibelleCIQUAL) != "",   # no empty string
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  summarise(
    prix_moyen_ciqual1     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual1 = n(),
    .groups = "drop"
  )


# Join the mean price data to the main table.
#Join mean price data to the main table
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids1, by = c("LibelleCIQUAL", "Lieu1"))

# prix_calculé_ciqual1: computed price using Poids × prix_moyen_ciqual1.
# poids_calculé_ciqual1: computed weight using Prix_all / prix_moyen_ciqual1.
resultats_codachats$prix_calculé_ciqual1  <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual1
resultats_codachats$poids_calculé_ciqual1 <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual1

# Count the number of observations per group.
#Count number of observations per group
resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  mutate( nombre_donnees_ciqual1 = n())

## Calculate mean price per kg and mean weight for imputation by CIQUAL code × Lieu2
# Calculate the mean price per unit of weight for each food item.
prix_moyen_par_poids2 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # no NA
    str_trim(LibelleCIQUAL) != "",   # no empty string
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  summarise(
    prix_moyen_ciqual2     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual2 = n(),
    .groups = "drop"
  )


# Impute missing values using the na.aggregate function.
# Impute missing values using na.aggregate
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids2,
            by = c("LibelleCIQUAL", "Lieu2"))

# prix_calculé_ciqual2 and poids_calculé_ciqual2: same logic as level 1, using Lieu2 averages.
resultats_codachats$prix_calculé_ciqual2 <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual2
resultats_codachats$poids_calculé_ciqual2  <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual2

resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  mutate(nombre_donnees_ciqual2 = n())

## Calculate mean price per kg and mean weight for imputation by TI group × Lieu2
# Calculate the mean price per unit of weight for each food item, excluding "Epices_condiments".
prix_moyen_par_poids3 <- resultats_codachats %>%
  filter(!is.na(Prix_all), !is.na(Poids),
         groupe_TI_TdC1 != "Epices_condiments") %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  summarise(
    prix_moyen_groupe_TI_TdC1     = mean(Prix_kg),
    nombre_donnees_groupe_TI_TdC1 = n(),
    .groups = "drop"
  )

# Join the mean prices to the main dataset.
# Join mean prices to the original dataset
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids3,
            by = c("groupe_TI_TdC1", "Lieu2"))

# Compute imputed price and weight for all groups except "Epices_condiments".
# Calculate imputed price and weight for groups except "Epices_condiments"
resultats_codachats <- resultats_codachats %>%
  mutate(prix_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Poids * prix_moyen_groupe_TI_TdC1, NA_real_),
         poids_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Prix_all / prix_moyen_groupe_TI_TdC1, NA_real_))

# Count the number of data points per TI group and location.
# Calculate number of data points per group and location
resultats_codachats <- resultats_codachats %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  mutate(nombre_donnees_groupe_TI_TdC1 = n()) %>%
  ungroup()

# Spot check for participant "39-Epimut".
bis <- resultats_codachats %>%
  filter(Identifiant == "39-Epimut") %>%
  select(Date, LibelleCIQUAL, groupe_TI_TdC1 , Poids, Prix_all, prix_moyen_ciqual1, prix_moyen_ciqual2,prix_moyen_groupe_TI_TdC1
  ) %>%
  print(n = Inf)


# 14. Final imputation of Poids_vf and Prix_vf ----
## Final imputation
# Initialize final columns from the original observed values.
# Initialize final columns from original columns
resultats_codachats$Poids_vf <- resultats_codachats$Poids
resultats_codachats$Prix_vf  <- resultats_codachats$Prix_all

# Function calculate_vf: fills Poids_vf and Prix_vf from computed imputed values
# when the observed value is missing AND the group has at least 10 observations.
# Does not overwrite values already imputed in a previous pass.
# Corrected function: takes the current state of Poids_vf / Prix_vf into account
calculate_vf <- function(data, prix_calc_col, poids_calc_col, nombre_col) {
  # Start from already-imputed values
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

# Apply imputation in three passes without overwriting prior imputations.
# Apply in three passes, without overwriting previous imputations
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

# Spot check for "Piment, cru" after imputation.
data_filtré <- resultats_codachats %>%
  filter(LibelleCIQUAL == "Piment, cru")

# Impute donations: for donated foods with a missing price, assign a price of zero.
#Impute donations
#The only change made is on the price. If NA, impute a value of zero.
resultats_codachats$Prix_vf <- ifelse((resultats_codachats$Lieu1 =="dons" & is.na(resultats_codachats$Prix_vf)), (0), (resultats_codachats$Prix_vf))

### Impute weights for RHD (out-of-home catering) food items
####RHD_COL / RHD_COM
# Use the PoidsRHD column (pre-calculated standard RHD portion weight) where Poids_vf is missing.
resultats_codachats$Poids_vf <- ifelse(
  is.na(resultats_codachats$Poids_vf),
  resultats_codachats$PoidsRHD,
  resultats_codachats$Poids_vf
)

## Post-imputation verification of price per kg
# Compute price per kg after imputation. Zero price → 0; zero weight → NA.
#Calculate price per kg.
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # if price is zero → 0
      Poids_vf == 0          ~ NA_real_,    # if weight is zero → NA
      TRUE                    ~ Prix_vf / Poids_vf
    )
  )

# Quick checks on remaining missing values after imputation.
describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))
describe(is.na(resultats_codachats$Prix_global_VF))
describe(is.na(resultats_codachats$Poids_VF))

# Store rows with missing post-imputation price for inspection.
lignes_vide <- resultats_codachats[is.na(resultats_codachats$Prix_Kg_post_imput), ]


# 15. Participant exclusion — underspending ----
## Remove participants whose recorded spending is below 25% of their declared food budget
# Calculate total spending (sum of Prix_vf) per participant.
# Calculate total sum of prix_vf per participant
somme_prix_vf <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(somme_prix_vf = sum(Prix_vf, na.rm = TRUE),)

# Calculate one quarter of the declared monthly food budget per participant.
# Calculate one quarter of the food budget
valeurs_uniques_budget <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(budget_unique = unique(Budget.mensuel.alimentation.))

# Join spending and budget, compute thresholds.
Comparaison <- inner_join(somme_prix_vf , valeurs_uniques_budget, by="Identifiant")
Comparaison$quart_budget <- as.numeric(Comparaison$budget_unique)/4
Comparaison$dix_budget <- as.numeric(Comparaison$budget_unique)*10

print(unique(resultats_codachats$Identifiant))
# Flag participants whose total spending is below the budget threshold.
# Display alert message for participants where sum of prix_vf < budget_unique
Comparaison <- Comparaison %>%
  mutate(
    message_alerte = ifelse( quart_budget > somme_prix_vf,
                             paste("Alert: sum of prix_vf (", somme_prix_vf, ") is below one quarter of declared food budget (", budget_unique, ")"),
                             "No alert"))
# Collect IDs of participants to exclude.
identifiants_alerte <- Comparaison %>%
  filter(somme_prix_vf < quart_budget )%>%
  pull(Identifiant)

# Remove flagged participants from the dataset.
# Remove participants flagged with an alert message from resultats_codachats
resultats_codachats<- resultats_codachats%>%
  filter(!Identifiant %in% identifiants_alerte )

# Remove food categories that cannot be compared across cohorts.
#Remove incomparable categories
resultats_codachats <- subset(
  resultats_codachats,
  !groupe_TI_TdC1 %in% c( "EPICES_CONDIMENTS", "MGV",  "PLATS_PREP_VEGETARIENS")
)


# 16. Imputation of nutritional and environmental values ----
# IMPUTATION OF NUTRITIONAL AND ENVIRONMENTAL DATA
## Aggregate variables of interest by TI food group from the CALNUT data frame
# Rows with only a TI category code have no individual nutritional / environmental
# values. These are filled with the weighted group-level means from resultats_pondérés.
# - filter(!is.na(Poids_vf) & Poids_vf != 0): removes rows with missing or zero weight.
# - group_by(groupe_TI_TdC1): groups by TI food category.
# - summarise: computes the weighted mean of yield_factor per group.
resultats_codachats$Unite <- ifelse(is.na(resultats_codachats$Unite),(resultats_codachats$Unite=="unités"), (resultats_codachats$Unite))
# List of nutritional and environmental columns to fill with group-level means.
colonnes_a_transformer<- c("yield_factor","pct_conso", "retinol_mcg", "nrj_kcal", "proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
                           "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
                           "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                           "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                           "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "DQR", "EF", "climat", "couche_ozone", "ions",
                           "ozone", "partic", "acid", "eutro_terr", "eutro_eau",
                           "eutro_mer", "sol", "toxi_eau", "ress_eau", "ress_ener", "ress_min")

# The idea is to assign average nutrient values based on the most representative
# foods per category.
## Join the TI reference table to resultats_codachats
resultats_codachats <-left_join(resultats_codachats,resultats_pondérés,by="groupe_TI_TdC1")
# Replace missing individual values with the group-level mean from resultats_pondérés.
# Replace missing values with the corresponding mean
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer),
                ~ ifelse(is.na(.x), get(paste0("mean_", cur_column())), .x)))

## Remove mean_ columns after replacement if needed
resultats_codachats <- resultats_codachats[, setdiff(names(resultats_codachats), grep("^mean_", names(resultats_codachats), value = TRUE))]

# Set the consumption unit variable to 1 for all CSGA participants.
resultats_codachats$UC_TI <- 1


# 17. Compute consumed weight and energy per food item ----
# CALCULATE INDICATORS
## Weight (kg/person/day)
# To link COD-Appro quantities to FFQ data, multiply supply weight by
# yield_factor × pct_conso to obtain the consumed weight.
# This applies to all items except those from out-of-home catering (RHD),
# where the recorded weight is already the consumed portion.
resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD"),(resultats_codachats$Poids_vf*resultats_codachats$yield_factor*resultats_codachats$pct_conso),(resultats_codachats$Poids_vf))
# Compute energy per food item: nrj_kcal is in kcal/100g, Poids in kg → multiply by 10.
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf


# Conversion for coffee and tea:
# (reference: ISO 3103 — 2g of tea per litre of water;
#  55g of coffee per litre of water)
# Adjust consumed weight for CAFE_THE using the dry-to-liquid conversion factor (Sec_Vol).
#resultats_codachats$nrj_kcal <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_vf))

resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_consomme_vf))
# Inspect coffee/tea entries after conversion.
resultats_codachats_CAFE_THE <- resultats_codachats %>%
  filter(groupe_TI_TdC1 == "CAFE_THE")

# Adjust kcal for coffee and tea (divide back by Sec_Vol to avoid double-counting).
resultats_codachats$kcal_aliment_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$kcal_aliment_vf/ resultats_codachats$Sec_Vol),(resultats_codachats$kcal_aliment_vf))


# 18. Build the Carnet_POIDS weight summary table ----
###GENERAL WEIGHT TABLE
# Create a long-format table with one row per participant × food item, then pivot wide.
Nourriture_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$Poids_consomme_vf  )
names(Nourriture_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","Poids_vf_consommee")

#print(unique(Nourriture_consommee$groupe_TI_TdC))
# List of food categories.
# List of food categories
categories <- unique(Nourriture_consommee$groupe_TI_TdC)
# Loop to create one column per food category in Nourriture_consommee.
# Loop to create corresponding columns in Nourriture_consommee
for (categorie in categories) {
  Nourriture_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Nourriture_consommee$groupe_TI_TdC == categorie, Nourriture_consommee$Poids_vf_consommee, 0)}
# Remove the group and weight columns (now redundant after pivoting).
# Remove "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Nourriture_consommee <- subset(Nourriture_consommee, select = -c(groupe_TI_TdC, Poids_vf_consommee, NA_CARNET))
# Aggregate to one row per participant (sum of weights per food category).
# Aggregate for Carnet_POIDS data frame
Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate.
# List of column names to aggregate
colonnes <- names(Nourriture_consommee)[-1] # Exclude the "Identifiant" column
# Loop to aggregate data by column.
# Loop to aggregate data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Nourriture_consommee, FUN = sum)
  Carnet_POIDS <- left_join(Carnet_POIDS, Temp, by = "Identifiant")
}

Carnet_POIDS$AUTRE_CARNET <- NULL

# Divide each food column by UC_TI × Nj to get kg/person/day.
# Divide columns by UC multiplied by number of recording days
Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$UC_TI*Nj)
# Compute total daily weight (sum of all food group columns).
# Calculate the sum of columns for each row
Carnet_POIDS$POIDS_TOTAL_CARNET <- rowSums(Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE)
# Compute total weight excluding beverages.
# Calculate the sum of columns excluding beverages
Carnet_POIDS$POIDS_HORS_BOISSON_CARNET <- with(Carnet_POIDS, POIDS_TOTAL_CARNET -
                                                 ALCOOL_CARNET -
                                                 FRUITS_JUS_CARNET -
                                                 LAIT_CARNET -
                                                 EAU_CARNET -
                                                 SODAS_LIGHT_CARNET -
                                                 SODAS_SUCRES_CARNET -
                                                 CAFE_THE_CARNET)


# Add the _Poids suffix to all _CARNET columns.
#Add _Poids suffix
Carnet_POIDS <- Carnet_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_CARNET$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )


# 19. Build the Carnet_KCAL energy summary table ----
## Kcal (kcal/person/day)
# For each food item, the energy value (kcal/kg from CALNUT) has already been
# applied to the consumed weight (kcal_aliment_vf).
# resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf
# For each consumed food, we impute its nutritional value in kJ/kg based on consumed weight.
Kcal_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$kcal_aliment_vf  )
names(Kcal_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","kcal_aliment_vf")
# List of food categories.
# List of food categories
categories <- unique(Kcal_consommee$groupe_TI_TdC)
# Loop to create one kcal column per food category.
# Loop to create corresponding columns in Kcal_consommee
for (categorie in categories) {
  Kcal_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Kcal_consommee$groupe_TI_TdC == categorie,
                                                           Kcal_consommee$kcal_aliment_vf, 0)}
# Remove helper columns after pivoting.
# Remove "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Kcal_consommee <- subset(Kcal_consommee, select = -c(groupe_TI_TdC, kcal_aliment_vf, NA_CARNET))

# Aggregate to one row per participant (sum of kcal per food category).
# Aggregate for Carnet_KCAL data frame

#Carnet_KCAL <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
Carnet_KCAL <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate.
# List of column names to aggregate
colonnes <- names(Kcal_consommee)[-1] # Exclude the "Identifiant" column
# Loop to aggregate data by column.
# Loop to aggregate data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Kcal_consommee, FUN = sum)
  Carnet_KCAL <- left_join(Carnet_KCAL, Temp, by = "Identifiant")}
# Divide by UC_TI × Nj to get kcal/person/day.
# Divide columns by UC multiplied by Nj
Carnet_KCAL$AUTRE_CARNET <- NULL
Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$UC_TI* Nj)
# Compute total daily energy (sum of all food group columns).
# Calculate the sum of columns for each row
Carnet_KCAL$KCAL_TOTAL_CARNET <- rowSums(Carnet_KCAL[, 3:ncol(Carnet_KCAL)], na.rm = TRUE)
# Compute total energy excluding beverages.
# Calculate the sum of columns excluding beverages
Carnet_KCAL$KCAL_HORS_BOISSON_CARNET <- with(Carnet_KCAL, KCAL_TOTAL_CARNET -
                                               ALCOOL_CARNET -
                                               FRUITS_JUS_CARNET -
                                               LAIT_CARNET -
                                               EAU_CARNET -
                                               SODAS_LIGHT_CARNET -
                                               SODAS_SUCRES_CARNET -
                                               CAFE_THE_CARNET)


# Add the _Kcal suffix to all _CARNET columns.
#Add _KCAL suffix
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


# 20. Build final indicator and cleaned data tables ----
# Build final tables
# Carnet_id: join weight and energy tables; tag rows as booklet (Carnet) records.
Carnet_id <- Carnet_POIDS
Carnet_id$Mesure <- "Carnet"
Carnet_id <- left_join(Carnet_id, Carnet_KCAL, by='Identifiant')


# Build cleaned raw data file (one row per purchase line, selected columns only).
# Build final cleaned data file
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


# 21. Export to Excel ----
# DOWNLOAD

# Create a new workbook object
wb <- createWorkbook()

# Write indicator table (Carnet_id: one row per participant with daily indicators).
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", Carnet_id  )

# Write cleaned raw purchase data (one row per food item per receipt).
addWorksheet(wb, "Données_brutes_nettoyées")
writeData(wb, sheet = "Données_brutes_nettoyées", fichier_nettoyé )

# UPDATE this path before running.
saveWorkbook(wb,(paste0("Fichiers prétraités/Carnets_CSGA.xlsx")))
