# LOADING THE WORKING ENVIRONMENT  --------------
## Importing packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx); library(readxl);library(dplyr);
library(broom);library(scales);library(modelsummary);library(ggplot2);library(effsize);library(lfe);
library(ggpubr);library(vtable);library;library("openxlsx");
library("dplyr"); library("tidyr");library("ggplot2");library("gridExtra");library(lubridate);
library("RColorBrewer");library(reshape2);library(Metrics);library(questionr);library(zoo)

### Enter the campaign date ---------------------
campaign<-"23-02" 
Nj <- 28 #"Number of entry days

### Importing CSGA data -------------------

resultats_codachats <- read_excel(
  path  = "Données_CSGA.xlsx",
  sheet = "Table_appli"
)

#CORRECTED FFQ DATA
CSGA_FFQ <- read_excel(
  path  = "FFQ_CSGA.xlsx",
  sheet = "Frequences_corrigées"
)

CSGA_FFQ_id <- read_excel(
  path  = "FFQ_CSGA.xlsx",
)

describe(is.na(resultats_codachats$Prix_detail_VF))
describe(is.na(resultats_codachats$Prix_global_VF))
### Importing the appendix tables data ----------------
CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_CARNET.xlsx")
magasins <- read_excel("Reclassement_magasins.xlsx")

resultats_pondérés <- read_excel("resultats_pondérés.xlsx")

# CLEANING THE COD_ACHATS FILE: LOCATIONS / DATES / LIBELLE_CUSTOM / LIBELLE_CIQUAL-----------------
resultats_codachats$Date <- as.Date(
  resultats_codachats$Date,
  format = "%Y-%m-%d"
)



#Adding gender and monthly income
Ajout <- CSGA_FFQ[, c("Identifiant", "Sexe", "Budget.mensuel.alimentation.")]
names(resultats_codachats)[1] <- "Identifiant"
resultats_codachats <- left_join(Ajout, resultats_codachats,  by = "Identifiant")
#REMOVE THE EXTRA COLUMNS
resultats_codachats <- resultats_codachats[, -c(15:25)]
resultats_codachats <- resultats_codachats[, -c(22:30)]


### Joining the sourcing locations and the classification by store type "Lieu 1" / "Lieu2"----------

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



## Associating Cod-Achats and Calnut ---------------------------
#MERGING the CALNUT reference table and the entry file "resultats_Codachats"
#On the entry file, we rename: "CodeCIQUAL" to "CODACHATS_alim_code" and "Categorie1" to "groupe_TI_TdC".
# resultats_codachats and CALNUT are linked by CODACHATS_alim_code
colnames(resultats_codachats)[colnames(resultats_codachats) == 'CodeCIQUAL'] <- 'CODACHATS_alim_code'
resultats_codachats <- left_join(resultats_codachats, CALNUT, by=c("CODACHATS_alim_code"))
#Rename "Category1" with groupe_TI_TdC
colnames(resultats_codachats)[colnames(resultats_codachats) == 'groupe_TI_TdC'] <- 'groupe_TI_TdC1'


# CLEANING THE COD_ACHATS FILE:  WEIGHT / PRICE / UNITS ----------------------------
#-	If the weight equals 0 grams, apply a conversion to units
names(resultats_codachats)[9] <- "Unite"
names(resultats_codachats)[26] <- "LibelleCIQUAL"



resultats_codachats$groupe_TI_TdC1[resultats_codachats$groupe_TI_TdC1 == "JAMBON_BLANC"] <- "CHARCUTERIE_HORS_JB"


resultats_codachats <- resultats_codachats %>%
  group_by(Identifiant) %>% 
  mutate(
    date_starting = min(Date, na.rm = TRUE)
  ) %>%
  ungroup()

#Remove tickets with more than 28 days of entries
resultats_codachats$jour_num <- 
  as.numeric(resultats_codachats$Date - resultats_codachats$date_starting) + 1
table(resultats_codachats$jour_num)
resultats_codachats <- resultats_codachats[resultats_codachats$jour_num <= 28, ]




# CLEANING THE COD_ACHATS FILE:  WEIGHT / PRICE / UNITS ----------------------------
## ================================================================

## Changes compared to the original script:
##   1) Each "aliments_specifiques" list was renamed with a unique
##      and explicit name, because the variable was overwritten 4 times
##      in a row in the original script (risk of using the wrong list
##      by mistake in a rule further down).
##   2) A verifier_etape() function is called after each major
##      step: it displays the number of rows modified, the number
##      of NAs created, and the descriptive stats of Nb / Prix / Unite.
## ================================================================

library(dplyr)

## ---- Utility check function -----------------------------
## Compares a "before" and an "after" on the key columns and displays
## a summary of the changes. To be called after each step.
verifier_etape <- function(avant, apres, nom_etape, cols = c("Nb", "Unite", "Prix", "PrixMenu")) {
  cat("\n================ CONTROLE :", nom_etape, "================\n")
  
  # Rows modified on at least one of the tracked columns
  # (proper handling of NA values for the comparison)
  diff_logique <- Reduce(`|`, lapply(cols, function(col) {
    a <- avant[[col]]
    b <- apres[[col]]
    !( (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b) )
  }))
  nb_modifiees <- sum(diff_logique, na.rm = TRUE)
  cat("Lignes modifiées :", nb_modifiees, "/", nrow(avant),
      sprintf("(%.2f%%)\n", 100 * nb_modifiees / nrow(avant)))
  
  # NAs created / removed, column by column
  for (col in cols) {
    na_avant <- sum(is.na(avant[[col]]))
    na_apres <- sum(is.na(apres[[col]]))
    if (na_avant != na_apres) {
      cat(sprintf("  - %s : NA avant = %d | NA après = %d (delta = %+d)\n",
                  col, na_avant, na_apres, na_apres - na_avant))
    }
  }
  
  # Quick stats on Nb and Prix if numeric
  if ("Nb" %in% cols) {
    cat("  - Nb   : avant [min=", round(min(avant$Nb, na.rm = TRUE), 3),
        " médiane=", round(median(avant$Nb, na.rm = TRUE), 3),
        " max=", round(max(avant$Nb, na.rm = TRUE), 3), "]",
        " -> après [min=", round(min(apres$Nb, na.rm = TRUE), 3),
        " médiane=", round(median(apres$Nb, na.rm = TRUE), 3),
        " max=", round(max(apres$Nb, na.rm = TRUE), 3), "]\n", sep = "")
  }
  if ("Prix" %in% cols) {
    cat("  - Prix : avant [min=", round(min(avant$Prix, na.rm = TRUE), 3),
        " médiane=", round(median(avant$Prix, na.rm = TRUE), 3),
        " max=", round(max(avant$Prix, na.rm = TRUE), 3), "]",
        " -> après [min=", round(min(apres$Prix, na.rm = TRUE), 3),
        " médiane=", round(median(apres$Prix, na.rm = TRUE), 3),
        " max=", round(max(apres$Prix, na.rm = TRUE), 3), "]\n", sep = "")
  }
  if ("Unite" %in% cols) {
    # We replace the NAs with an explicit string BEFORE tabulating:
    # otherwise table() gives an NA name (rather than the string "NA") to the
    # missing category, which breaks the [[ ]] indexing further below.
    unite_avant <- ifelse(is.na(avant$Unite), "(manquant)", avant$Unite)
    unite_apres <- ifelse(is.na(apres$Unite), "(manquant)", apres$Unite)
    
    tab_avant <- table(unite_avant)
    tab_apres <- table(unite_apres)
    toutes_unites <- union(names(tab_avant), names(tab_apres))
    for (u in toutes_unites) {
      va <- if (u %in% names(tab_avant)) tab_avant[[u]] else 0
      vp <- if (u %in% names(tab_apres)) tab_apres[[u]] else 0
      if (va != vp) cat(sprintf("  - Unite '%s' : %d -> %d (%+d)\n", u, va, vp, vp - va))
    }
  }
  cat("=====================================================\n")
}

## Starting snapshot, used for the very first check
snapshot_initial <- resultats_codachats


## ================================================================
## STEP 1 — Assigning the "grammes" (grams) unit (Nb > 30, units)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      Nb > 30 &
        Unite == "unités" &
        LibelleCIQUAL != "Compote de fruits, allégée en sucres" &
        LibelleCIQUAL != "Compote de pomme" &
        LibelleCIQUAL != "Compote de fruits" &
        LibelleCIQUAL != "Sushi ou maki aux produits de la mer" &
        groupe_TI_TdC1 != "OEUFS" &
        groupe_TI_TdC1 != "CAFE_THE" ~ "grammes",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "1. Attribution grammes (Nb>30)", cols = "Unite")


## ================================================================
## STEP 2 — Missing values for Prix/PrixMenu/Nb/Appreciation <= 0
## ================================================================
avant <- resultats_codachats

resultats_codachats$Prix[resultats_codachats$Prix < 0] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu < 0] <- NA
resultats_codachats$Nb[resultats_codachats$Nb <= 0] <- NA
resultats_codachats$Appreciation[resultats_codachats$Appreciation < 0] <- NA

resultats_codachats$Prix[resultats_codachats$Prix == 0 & resultats_codachats$Lieu1 != "dons"] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu == 0 & resultats_codachats$Lieu1 != "dons"] <- NA

resultats_codachats$Nb[resultats_codachats$Nb == 0] <- NA

verifier_etape(avant, resultats_codachats, "2. Valeurs manquantes (<=0)",
               cols = c("Nb", "Prix", "PrixMenu", "Appreciation"))


## ================================================================
## STEP 3 — Corrections of the main conversion errors
## (Nb<10g -> x1000 ; Prix>100 excluding alcohol/red meat -> NA)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      Unite == "grammes" &
        Nb < 10 &
        groupe_TI_TdC1 != "EPICES_CONDIMENTS" &
        Lieu2 != "RHD" ~ Nb * 1000,
      TRUE ~ Nb
    )
  )
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix = case_when(
      Prix > 100 &
        groupe_TI_TdC1 != "ALCOOL" &
        groupe_TI_TdC1 != "VIANDE_ROUGE" ~ NA_real_,
      TRUE ~ Prix
    )
  )
verifier_etape(avant, resultats_codachats, "3. Corrections erreurs de conversion (Nb x1000 / Prix>100 -> NA)",
               cols = c("Nb", "Prix"))


## ================================================================
## STEP 4 — Readjusting the units (kilos/grams/litres/centilitres)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      Nb > 50 & Unite == "kilos" ~ "grammes",
      Nb > 10 & Nb < 20.5 & Unite == "grammes" ~ "unités",
      Nb < 1 & Unite == "centilitres" ~ "litres",
      Nb > 1000 & Unite == "kilos" ~ "grammes",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "4. Réajustement des unités", cols = "Unite")


## ================================================================
## STEP 5 — Readjusting Nb when price is low and quantity is high (litres)
## ================================================================
avant <- resultats_codachats

library(dplyr)

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      Prix < 3 &
        Nb > 17 &
        Unite == "litres" &
        groupe_TI_TdC1 != "EAU" ~ Nb / 10,
      TRUE ~ Nb
    )
  )
verifier_etape(avant, resultats_codachats, "5. Réajustement Nb (prix bas / quantité élevée, litres)", cols = "Nb")

## Price per kg — recalculated for the following steps
resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


## ================================================================
## STEP 6 — Multiplying Nb by 10 for low-weight foods
## (renamed list: aliments_poids_faible_x10)
## ================================================================
avant <- resultats_codachats

aliments_poids_faible_x10 <- c(
  "Bonbon/ bouchée chocolat fourrage gaufrettes/ biscuit", "Bonbons, tout type", "Champignon, tout type, cru",
  "Champignons à la grecque, appertisés", "Barre chocolatée biscuitée", "Fromage à pâte molle et croûte fleurie double crème environ 30% MG",
  "Miel", "Sucre blanc", "Crème fraîche, 15 à 20% MG, UHT", "Rillettes de poulet",
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
  mutate(
    Nb = case_when(
      LibelleCIQUAL %in% aliments_poids_faible_x10 &
        Nb <= 20 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      TRUE ~ Nb
    )
  )
verifier_etape(avant, resultats_codachats, "6. Nb x10 pour aliments à faible grammage", cols = "Nb")

resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


## ================================================================
## STEP 7 — Dividing Prix by 10 for certain foods
## (renamed list: aliments_prix_eleve_div10)
## ================================================================
avant <- resultats_codachats

aliments_prix_eleve_div10 <- c("Saumon fumé", "Barres chocolatées", "Pâte à tartiner chocolat et noisette", "Rosette ou Fuseau", "Pâtisserie (aliment moyen)",
                               "Pomme de terre de conservation, crue", "Chocolat, en tablette (aliment moyen)", "Mélange apéritif graine non salée fruit séché",
                               "Mozzarella au lait de vache", "Sandwich baguette, jambon emmental")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix = case_when(
      LibelleCIQUAL %in% aliments_prix_eleve_div10 &
        Nb >= 100 &
        Prix >= 10 &
        Prix_kg > 0.05 &
        Unite == "grammes" ~ Prix / 10,
      TRUE ~ Prix
    )
  )

verifier_etape(avant, resultats_codachats, "7. Prix / 10 pour aliments à prix élevé", cols = "Prix")

resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


## ================================================================
## STEP 8 — Converting certain liquid foods to centilitres
## (renamed list: aliments_liquides_centilitres)
## ================================================================
avant <- resultats_codachats

aliments_liquides_centilitres <- c(
  "Bière \"de spécialités\" ou d'abbaye, régionales ou d'une brasserie (degré d'alcool variable)",
  "Boisson gazeuse, sans jus de fruit, sucrée", "Jus de fruits (aliment moyen)",
  "Boisson préparée à partir de sirop à diluer type menthe, fraise, etc., sucré, dilué dans l'eau",
  "Huile de pépins de raisin")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      LibelleCIQUAL %in% aliments_liquides_centilitres &
        Nb < 100 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ "centilitres",
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "8. Conversion en centilitres (aliments liquides)", cols = "Unite")


## ================================================================
## STEP 9 — Unit weight of eggs (63 g / unit)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" ~ Nb * 0.063,
      TRUE ~ Nb
    ),
    Unite = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" &
        !is.na(Nb) ~ "kilos",
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "9. Poids unitaire des œufs (63g)", cols = c("Nb", "Unite"))


## ================================================================
## STEP 10 — One-off correction: raw eggplant (single case)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" ~ Nb * 0.063,
      LibelleCIQUAL == "Aubergine, crue" &
        Nb == 75 &
        Prix == 3.98 ~ 750,
      TRUE ~ Nb
    ),
    Unite = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" ~ "kilos",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "10. Correction ponctuelle Aubergine crue", cols = "Nb")


## ================================================================
## STEP 11 — Applesauce/compotes: 1 pot = 100 g
## ================================================================
avant <- resultats_codachats

compotes <- c(
  "Compote de fruits allégée en sucres rayon frais",
  "Compote de pomme",
  "Compote de fruits",
  "Compote de fruits, allégée en sucres",
  "Compote (aliment moyen)"
)

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL %in% compotes &
        Unite == "unités" ~ Nb * 0.1,
      TRUE ~ Nb
    ),
    Unite = case_when(
      LibelleCIQUAL %in% compotes &
        Unite == "unités" ~ "kilos",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "11. Compotes (1 pot = 100g)", cols = c("Nb", "Unite"))


## ================================================================
## STEP 12 — Various one-off corrections (Nb / Unite)
## ================================================================
avant <- resultats_codachats

library(dplyr)

# Corrections before recalculating the price per kg
resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
        Unite == "unités" ~ Nb * 0.04,
      TRUE ~ Nb
    ),
    Unite = case_when(
      LibelleCIQUAL == "Amande, avec peau" &
        Nb == 0.2 &
        Unite == "centilitres" ~ "kilos",
      
      LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
        Unite == "unités" ~ "kilos",
      
      TRUE ~ Unite
    )
  )

# Recalculating the price per kg
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_kg = Prix / Nb
  )

# Corrections using Prix_kg
resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL == "Oeuf, cru" &
        Nb == 30 ~ 300,
      
      LibelleCIQUAL == "Nem ou Pâté impérial" &
        Unite == "unités" ~ Nb * 0.1,
      
      LibelleCIQUAL == "Champignon, morille, crue" &
        Nb == 30 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 300,
      
      LibelleCIQUAL == "Champignon, lentin ou shiitaké, séché" &
        Nb == 20 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 200,
      
      LibelleCIQUAL == "Yaourt à la grecque, nature" &
        Nb == 12 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 120,
      
      LibelleCIQUAL == "Yaourt aromatisé, avec édulcorants, 0% MG" &
        Nb == 12 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 120,
      
      LibelleCIQUAL == "Yaourt aux fruits, sucré" &
        Nb == 16 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 160,
      
      LibelleCIQUAL == "Purée de tomate" &
        Nb == 15000 ~ 15,
      
      LibelleCIQUAL == "Fruits de mer (aliment moyen), cru" &
        Nb == 12 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 120,
      
      LibelleCIQUAL == "Pizza 4 fromages" &
        Nb == 16 &
        Unite == "unités" ~ 1,
      
      LibelleCIQUAL == "Terrine de canard" &
        Nb == 130 &
        Prix == 7.6 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Sandwich baguette, jambon emmental" &
        Prix < 0.45 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ NA_real_,
      
      LibelleCIQUAL == "Toasts ou Canapés salés, garnitures diverses, préemballés" &
        Prix == 8.05 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Rillettes de poulet" &
        Nb == 180 &
        Prix == 8.8 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Bœuf, fauxfilet, cru" &
        Nb == 240 &
        Prix == 12.48 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Bœuf, fauxfilet, cru" &
        Nb == 110 &
        Prix == 7.5 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Moutarde" &
        Nb < 200 &
        Unite == "grammes" &
        Prix < 0.05 &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "banane, crue" &
        Nb > 1070 ~ 1.07,
      
      TRUE ~ Nb
    ),
    Unite = case_when(
      LibelleCIQUAL == "Nem ou Pâté impérial" &
        Unite == "unités" ~ "kilos",
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "12. Corrections ponctuelles diverses (Nb/Unite)", cols = c("Nb", "Unite"))


## ================================================================
## STEP 13 — One-off corrections to the units
## (renamed list: aliments_petit_volume_litres)
## ================================================================
avant <- resultats_codachats

aliments_petit_volume_litres <- c("Sauce pour nems à base de nuocmam dilué, préemballée", "Sauce soja, préemballée",
                                  "Bière \"spéciale\" (56° alcool)", "Bière \"spéciale\" (5-6° alcool)")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      LibelleCIQUAL == "Pomme, crue" &
        Nb == 1 &
        Unite == "centilitres" ~ "kilos",
      
      LibelleCIQUAL == "Tomate, bouillie/cuite à l'eau" &
        Nb == 1600 ~ "unités",
      
      LibelleCIQUAL == "Vin rouge" &
        Nb == 1 &
        Unite == "centilitres" ~ "litres",
      
      LibelleCIQUAL %in% aliments_petit_volume_litres &
        Nb <= 3 &
        Unite == "centilitres" ~ "litres",
      
      LibelleCIQUAL == "Sauce soja, préemballée" &
        Nb == 2.7 &
        Unite == "centilitres" ~ "litres",
      
      LibelleCIQUAL == "Fromage (aliment moyen)" &
        Nb == 1600 &
        Unite == "kilos" ~ "grammes",
      
      LibelleCIQUAL == "Vinaigre" &
        Nb == 1 &
        Unite == "grammes" ~ "litres",
      
      LibelleCIQUAL == "Sel blanc, non iodé, non fluoré" &
        Nb == 0.75 &
        Unite == "grammes" ~ "kilos",
      
      LibelleCIQUAL == "Moutarde" &
        Nb < 200 &
        Unite == "centilitres" ~ "grammes",
      
      LibelleCIQUAL == "Cornichon, au vinaigre" &
        Unite == "litres" ~ "kilos",
      
      LibelleCIQUAL == "Cornichon, au vinaigre" &
        Unite == "centilitres" ~ "grammes",
      
      # As in your original script
      LibelleCIQUAL %in% aliments_petit_volume_litres &
        Nb == 1 &
        Unite == "grammes" ~ "kilos",
      
      LibelleCIQUAL == "Oeuf, cru" &
        Unite == "grammes" &
        Prix == 100 ~ "unités",
      
      LibelleCIQUAL == "Croissant, sans précision" &
        Identifiant == "LE116" &
        Nb == 2 ~ "unités",
      
      LibelleCIQUAL == "Crème de lait, 15 à 20% MG, légère, épaisse, rayon frais" &
        Identifiant == "LE126" &
        Nb == 8 ~ "unités",
      
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "13. Corrections ponctuelles sur les unités", cols = "Unite")


## ================================================================
## STEP 14 — One-off corrections to the prices
## (renamed list: aliments_prix_a_100)
## ================================================================
avant <- resultats_codachats


aliments_prix_a_100_pates <- c("Pâtes sèches standard, cuites, non salées", "Pâtes sèches standard, crues")
aliments_prix_a_100_abats <- c("Abat, cru (aliment moyen)", "Abat, cuit (aliment moyen)", "Thon, cru ", "Accra de poisson")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix = case_when(
      LibelleCIQUAL == "Oeuf, à la coque" &
        Prix == 100 ~ NA_real_,
      
      LibelleCIQUAL %in% aliments_prix_a_100_pates &
        Prix == 100 ~ 1,
      
      LibelleCIQUAL == "Ravioli chinois vapeur à la crevette" &
        Nb == 105 &
        Prix == 7.5 ~ NA_real_,
      
      LibelleCIQUAL %in% aliments_prix_a_100_abats &
        Prix == 100 ~ 10,
      
      TRUE ~ Prix
    )
  )
verifier_etape(avant, resultats_codachats, "14. Corrections ponctuelles sur les prix", cols = "Prix")


## ================================================================
## STEP 15 — Recoding LibelleCIQUAL + hydration coefficient
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    LibelleCIQUAL = case_when(
      LibelleCIQUAL == "Champignon, tout type, cru" &
        Identifiant == "6348-Episourire" ~ "Champignon noir, séché",
      TRUE ~ LibelleCIQUAL
    ),
    Nb = case_when(
      LibelleCIQUAL == "Champignon noir, séché" &
        Unite == "grammes" ~ Nb * 14,
      TRUE ~ Nb
    )
  )
verifier_etape(avant, resultats_codachats, "15. Recodage Champignon noir séché + coeff hydratation", cols = c("Nb", "LibelleCIQUAL"))


## ================================================================
## STEP 16 — Imputing Prix / PrixMenu / RHD / donations
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    PrixMenu = case_when(
      Lieu2 %in% c("commerce", "Epicerie") & Menu == "Oui" ~ NA_real_,
      Lieu2 == "RHD" ~ Prix,
      Lieu2 == "dons" ~ Prix,
      TRUE ~ PrixMenu
    ),
    
    Prix = case_when(
      Lieu2 %in% c("RHD", "dons") ~ NA_real_,
      TRUE ~ Prix
    ),
    
    Menu = case_when(
      Lieu2 %in% c("commerce", "Epicerie") ~ NA_character_,
      Lieu2 == "RHD" ~ "Oui",
      TRUE ~ Menu
    ),
    
    Nb = case_when(
      Lieu2 == "RHD" &
        Nb < 9 &
        Unite == "grammes" ~ NA_real_,
      TRUE ~ Nb
    ),
    
    Unite = case_when(
      Lieu2 == "RHD" &
        is.na(Nb) &
        Unite == "grammes" ~ NA_character_,
      TRUE ~ Unite
    ),
    
    PrixMenu = case_when(
      is.na(Menu) ~ NA_real_,
      TRUE ~ PrixMenu
    ),
    
    Prix = case_when(
      !is.na(PrixMenu) ~ NA_real_,
      TRUE ~ Prix
    )
  )
verifier_etape(avant, resultats_codachats, "16. Imputation Prix/PrixMenu (RHD, dons, commerce)",
               cols = c("Nb", "Unite", "Prix", "PrixMenu"))


## ================================================================
## STEP 17 — Splitting PrixMenu across tickets (Date_vf / Lieu_vf)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  arrange(Date) %>%
  group_by(Date, Lieu_vf) %>%
  mutate(
    PrixMenu = case_when(
      row_number() == 1 ~ PrixMenu / n(),
      TRUE ~ PrixMenu
    )
  ) %>%
  ungroup()

resultats_codachats$Menu[resultats_codachats$PrixMenu == 0] <- NA
resultats_codachats$Menu[resultats_codachats$Menu == 0] <- NA
verifier_etape(avant, resultats_codachats, "17. Répartition PrixMenu par ticket", cols = c("PrixMenu", "Menu"))



# IMPUTING MISSING WEIGHT/PRICE VALUES ------------------------------
## Correcting the last remaining Price data----------------
#During the November 2023 campaign, some people entered as "prix menu" purchases made in-store
#These purchases should not appear the way they do when "Oui" is entered for the RHD_DON variable
#We assign NA to the prices entered for these purchases, since we only have the total price for all purchases, and we will apply the weight/price imputation procedure to these same foods
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="commerce" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="Epicerie" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="commerce" | resultats_codachats$Lieu2 =="Epicerie"), (NA), (resultats_codachats$Menu))
# If the product comes from RHD (food service): Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(NA),(resultats_codachats$Prix))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="RHD" ), ("Oui"), (resultats_codachats$Menu))
resultats_codachats$Nb <-ifelse(((resultats_codachats$Lieu2 =="RHD") & resultats_codachats$Nb <9 & resultats_codachats$Unite =="grammes"    ),(NA),(resultats_codachats$Nb ))
resultats_codachats$Unite <-ifelse(((resultats_codachats$Lieu2 =="RHD") & is.na(resultats_codachats$Nb) & resultats_codachats$Unite =="grammes" ),(NA),(resultats_codachats$Unite ))

# If the product is a donation: Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="dons")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="dons")),(NA),(resultats_codachats$Prix))
# If it is not a Menu, enter NA in the prixMenu variable
resultats_codachats$PrixMenu <-ifelse((is.na(resultats_codachats$Menu)),(NA),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse((is.na(resultats_codachats$PrixMenu)),(resultats_codachats$Prix),(NA))





## Creating the Prix_all (€) and poids (kg) variables-------------------
#two homogeneous variables that bring together all the weight and price data under the same unit
resultats_codachats$Poids <- resultats_codachats$Unite    
resultats_codachats$Prix_all <- resultats_codachats$Prix
#resultats_codachats$Prix_all <-ifelse((is.na(resultats_codachats$Prix_all)),(resultats_codachats$PrixMenu),(resultats_codachats$Prix_all))
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


## Calculating Price / Kg for all foods -------------
resultats_codachats$Prix_kg <- resultats_codachats$Prix_all / resultats_codachats$Poids
#Fprint(unique(resultats_codachats$Prix_kg))


## Calculating the average price per Kg and the average weight for the CODE_CIQUAL X Lieu1 imputation -----------
# Calculating the average price per unit weight for each food
prix_moyen_par_poids1 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # not NA
    str_trim(LibelleCIQUAL) != "",   # not an empty string
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  summarise(
    prix_moyen_ciqual1     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual1 = n(),
    .groups = "drop"
  )




#Joining the Average Price Data to the Main Table
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids1, by = c("LibelleCIQUAL", "Lieu1")) 

#prix_calculé_ciqual1: Calculates a weighted price using Poids and prix_moyen_ciqual1.
#poids_calculé_ciqual1: Calculates a derived weight using Prix_all and prix_moyen_ciqual1.
resultats_codachats$prix_calculé_ciqual1  <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual1
resultats_codachats$poids_calculé_ciqual1 <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual1

#Counting the Number of Observations per Group
resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  mutate( nombre_donnees_ciqual1 = n())

## Calculating the average price per Kg and the average weight for the CODE_CIQUAL X Lieu2 imputation -------------
# Calculating the average price per unit weight for each food
prix_moyen_par_poids2 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # not NA
    str_trim(LibelleCIQUAL) != "",   # not an empty string
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  summarise(
    prix_moyen_ciqual2     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual2 = n(),
    .groups = "drop"
  )



# Imputing missing values using the na.aggregate function
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids2,
            by = c("LibelleCIQUAL", "Lieu2"))

resultats_codachats$prix_calculé_ciqual2 <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual2
resultats_codachats$poids_calculé_ciqual2  <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual2

resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  mutate(nombre_donnees_ciqual2 = n())

## Calculating the average price per Kg and the average weight for the GroupeTI X Lieu2 imputation -------------
# Calculating the average price per unit weight for each food except for "Epices_condiments"
prix_moyen_par_poids3 <- resultats_codachats %>%
  filter(!is.na(Prix_all), !is.na(Poids),
         groupe_TI_TdC1 != "Epices_condiments") %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  summarise(
    prix_moyen_groupe_TI_TdC1     = mean(Prix_kg),
    nombre_donnees_groupe_TI_TdC1 = n(),
    .groups = "drop"
  )

# Joining the average prices to the original dataset
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids3,
            by = c("groupe_TI_TdC1", "Lieu2"))

# Calculating the imputed price and weight for the groups except "Epices_condiments"
resultats_codachats <- resultats_codachats %>%
  mutate(prix_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Poids * prix_moyen_groupe_TI_TdC1, NA_real_),
         poids_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Prix_all / prix_moyen_groupe_TI_TdC1, NA_real_))





# Calculating the number of data points per group and location
resultats_codachats <- resultats_codachats %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  mutate(nombre_donnees_groupe_TI_TdC1 = n()) %>%
  ungroup()



## Final imputation ---------------
# Initializing the final columns from the original columns
resultats_codachats$Poids_vf <- resultats_codachats$Poids
resultats_codachats$Prix_vf  <- resultats_codachats$Prix_all

# Corrected function: takes into account the current state of Poids_vf / Prix_vf
calculate_vf <- function(data, prix_calc_col, poids_calc_col, nombre_col) {
  # We start from the already-imputed values
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

# Applied in three passes, without overwriting the previous imputations
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

resultats_codachats <- resultats_codachats %>%
  filter(
    !is.na(groupe_TI_TdC1),
    !groupe_TI_TdC1 %in% c( "AUTRE", NA)
  )

#Imputing donations
#The only change made is to the price. If NA, we impute a value of zero
resultats_codachats$Prix_vf <- ifelse((resultats_codachats$Lieu1 =="dons" & is.na(resultats_codachats$Prix_vf)), (0), (resultats_codachats$Prix_vf))

### Imputing the weight of RHD (food service) foods----------
####RHD_COL / RHD_COM
resultats_codachats$Poids_vf <- ifelse(
  is.na(resultats_codachats$Poids_vf),
  resultats_codachats$PoidsRHD,
  resultats_codachats$Poids_vf
)

## Checking the price per kg of the imputed data = ---------------------------
#We calculate the price per kg.
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # if the price is zero → 0
      Poids_vf == 0          ~ NA_real_,    # if the weight is zero → NA
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

## Removing identifiers that record spending below 25% of the declared food budget---------
# Calculating the sum of prix_vf per identifier
somme_prix_vf <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(somme_prix_vf = sum(Prix_vf, na.rm = TRUE),)

# Calculating a quarter of the food budget
valeurs_uniques_budget <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(budget_unique = unique(Budget.mensuel.alimentation.))



Comparaison <- inner_join(somme_prix_vf , valeurs_uniques_budget, by="Identifiant")
Comparaison$quart_budget <- as.numeric(Comparaison$budget_unique)/4
Comparaison$dix_budget <- as.numeric(Comparaison$budget_unique)*10

print(unique(resultats_codachats$Identifiant))
# Displaying the alert message for identifiers where sum prix_vf < budget_unique
Comparaison <- Comparaison %>%
  mutate(
    message_alerte = ifelse( quart_budget > somme_prix_vf,
                             paste("Alerte: La somme des prix_vf (", somme_prix_vf, ") est inférieure au quart du budget alimentaire (", budget_unique, ")"),
                             "Aucune alerte"))
identifiants_alerte <- Comparaison %>%
  filter(somme_prix_vf < quart_budget )%>%
  pull(Identifiant)

# Removing the identifiers flagged with an alert message from resultats_codachats
resultats_codachats<- resultats_codachats%>%
  filter(!Identifiant %in% identifiants_alerte )



#Removing incomparable categories
resultats_codachats <- subset(
  resultats_codachats,
  !groupe_TI_TdC1 %in% c( "EPICES_CONDIMENTS", "MGV",  "PLATS_PREP_VEGETARIENS")
) 


# IMPUTING NUTRITIONAL AND ENVIRONMENTAL DATA -----------------
##Aggregating the variables of interest by groupe_TI_TdC from the CALNUT dataframe----------
resultats_codachats$Unite <- ifelse(is.na(resultats_codachats$Unite),(resultats_codachats$Unite=="unités"), (resultats_codachats$Unite))
#Data entered only via a TI category have no imputation for the various nutrients / the environmental data and the pct conso and yield_factor
#We therefore impute these average values based on the CALNUT file by TI category
##Filtering the data: filter(!is.na(Poids_vf) & Poids_vf != 0) filters the rows where Poids_vf is not NA and is not equal to zero (!= 0). This excludes all rows where the weight is missing or zero.
##Aggregating the data: the filtered data is then grouped by groupe_TI_TdC1 using group_by.
##Calculating the weighted average: finally, summarise calculates the weighted average of yield_factor for each group defined by groupe_TI_TdC1, using the valid weights specified by Poids_vf and ignoring NA values.
colonnes_a_transformer<- c("yield_factor","pct_conso", "retinol_mcg", "nrj_kcal", "proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
                           "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
                           "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                           "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                           "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "DQR", "EF", "climat", "couche_ozone", "ions",
                           "ozone", "partic", "acid", "eutro_terr", "eutro_eau", 
                           "eutro_mer", "sol", "toxi_eau", "ress_eau", "ress_ener", "ress_min")

#The idea is to assign the average nutrient values based on the foods most commonly found per category
## Linking the TI table to resultats_codachats ---------
resultats_codachats <-left_join(resultats_codachats,resultats_pondérés,by="groupe_TI_TdC1")
# Replacing missing values with the corresponding average
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer), 
                ~ ifelse(is.na(.x), get(paste0("mean_", cur_column())), .x)))

## Removing the mean_ columns after the replacement if needed
resultats_codachats <- resultats_codachats[, setdiff(names(resultats_codachats), grep("^mean_", names(resultats_codachats), value = TRUE))]

resultats_codachats$UC_TI <- 1

# CALCULATING THE INDICATORS ------------------
## Weight (Kg/person/day) ----------------------------------
#To link the COD-Appro and FFQ quantities, the weight of the supplies must systematically be multiplied
#by yield_factor*pct_conso:
#This gives the weight consumed for the supply logs.
#To link the COD-Appro and FFQ quantities, the weight of the supplies must systematically be multiplied
#by yield_factor*pct_conso:
#This gives the weight consumed for the supply logs.
resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD"),(resultats_codachats$Poids_vf*resultats_codachats$yield_factor*resultats_codachats$pct_conso),(resultats_codachats$Poids_vf))
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf


#Conversion
#https://www.femobook.com/blogs/coffee-knowledge/a-guide-to-the-golden-cup-standard?
#55 g of coffee per 1 L of water
#The ISO 3103 (2g of tea per 1L of water)
#resultats_codachats$nrj_kcal <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_vf))


resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_consomme_vf))
resultats_codachats_CAFE_THE <- resultats_codachats %>%
  filter(groupe_TI_TdC1 == "CAFE_THE")

resultats_codachats$kcal_aliment_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$kcal_aliment_vf/ resultats_codachats$Sec_Vol),(resultats_codachats$kcal_aliment_vf))

###OVERALL WEIGHT ------------------------
Nourriture_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$Poids_consomme_vf  )
names(Nourriture_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","Poids_vf_consommee")

#print(unique(Nourriture_consommee$groupe_TI_TdC))
# List of food categories
categories <- unique(Nourriture_consommee$groupe_TI_TdC)
# Loop to create the corresponding columns in Nourriture_consommee
for (categorie in categories) {
  Nourriture_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Nourriture_consommee$groupe_TI_TdC == categorie, Nourriture_consommee$Poids_vf_consommee, 0)}
# Remove the "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Nourriture_consommee <- subset(Nourriture_consommee, select = -c(groupe_TI_TdC, Poids_vf_consommee))
# Aggregation for the Carnet_POIDS dataframe
Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate
# Exclude the "Identifiant" column
# Loop to aggregate the data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Nourriture_consommee, FUN = sum)
  Carnet_POIDS <- left_join(Carnet_POIDS, Temp, by = "Identifiant")
}

Carnet_POIDS$AUTRE_CARNET <- NULL

# Divide the columns by the UC column multiplied by the number of entry days
Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$UC_TI*Nj)
# Calculate the sum of the columns for each row
Carnet_POIDS$POIDS_TOTAL_CARNET <- rowSums(Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE)
# Calculate the sum of the columns excluding beverages
Carnet_POIDS$POIDS_HORS_BOISSON_CARNET <- with(Carnet_POIDS, POIDS_TOTAL_CARNET - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET -
                                                 CAFE_THE_CARNET)


#Adding the _Poids suffix
Carnet_POIDS <- Carnet_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_CARNET$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )

## Kcal (Kcal/person/day)------------------------------------------
#For each food, we impute its nutritional value in kcal / kg based on the weight consumed of each food.
#resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf
#For each food consumed, we impute its nutritional value in kj / kg based on the weight consumed.
Kcal_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$kcal_aliment_vf  )
names(Kcal_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","kcal_aliment_vf")
# List of food categories
categories <- unique(Kcal_consommee$groupe_TI_TdC)
# Loop to create the corresponding columns in Kcal_consommee
for (categorie in categories) {
  Kcal_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Kcal_consommee$groupe_TI_TdC == categorie,
                                                           Kcal_consommee$kcal_aliment_vf, 0)}
# Remove the "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Kcal_consommee <- subset(Kcal_consommee, select = -c(groupe_TI_TdC, kcal_aliment_vf))

# Aggregation for the Carnet_KCAL dataframe

#Carnet_KCAL <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
Carnet_KCAL <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate
# Exclude the "Identifiant" column
# Loop to aggregate the data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Kcal_consommee, FUN = sum)
  Carnet_KCAL <- left_join(Carnet_KCAL, Temp, by = "Identifiant")}
# Divide the columns by the UC column multiplied by Nj
Carnet_KCAL$AUTRE_CARNET <- NULL
Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$UC_TI* Nj)
# Calculate the sum of the columns  for each row
Carnet_KCAL$KCAL_TOTAL_CARNET <- rowSums(Carnet_KCAL[, 3:ncol(Carnet_KCAL)], na.rm = TRUE)
# Calculate the sum of the columns excluding beverages
Carnet_KCAL$KCAL_HORS_BOISSON_CARNET <- with(Carnet_KCAL, KCAL_TOTAL_CARNET - 
                                               ALCOOL_CARNET - 
                                               FRUITS_JUS_CARNET - 
                                               LAIT_CARNET - 
                                               EAU_CARNET - 
                                               SODAS_LIGHT_CARNET - 
                                               SODAS_SUCRES_CARNET - 
                                               CAFE_THE_CARNET)


#Adding the _KCAL suffix
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
#Building the final tables ------------------------------------------------
Carnet_id <- Carnet_POIDS  
Carnet_id$Mesure <- "Carnet"
Carnet_id <- left_join(Carnet_id, Carnet_KCAL, by='Identifiant')





#Building the final data table ------------------------------------------------
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


# DOWNLOAD ----------------------------

# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", Carnet_id  )

addWorksheet(wb, "Données_brutes_nettoyées")
writeData(wb, sheet = "Données_brutes_nettoyées", fichier_nettoyé )





