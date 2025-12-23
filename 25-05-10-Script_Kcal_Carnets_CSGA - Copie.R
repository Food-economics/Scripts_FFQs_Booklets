#Importation des packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(car);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable)
library("openxlsx");library("dplyr");library("tidyr");library(ggplot2);
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot");library(nlme);library(fixest); library(plm);library(lmtest)
library("htmltools"); library(clubSandwich);library(Matrix); library(lme4)
library(cobalt); library(knitr); library(tableone); library(purrr); library("plotly")
library("htmlwidgets")
library(ggrepel)

#Chargement de l'environnement de travail -----------------
researcher<-"adenieul" #"vbellassen" edumont
if (researcher == "adenieul") { setwd <- paste0("C:/Users/adenieul/ownCloud - Anaelle Denieul@cesaer-datas.inra.fr/TI Dijon/donnees")
} else { setwd(paste0("C:/Users/",researcher,"/Owncloud/TI Dijon/donnees"))}
sgsdata_CSGA  <- read.xlsx((paste("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata.xlsx", sep="")))
sgsdata_TI <-read.xlsx((paste("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata_IT.xlsx", sep="")))
sgsdata_nudges  <- read.xlsx((paste("Données analysées - Article N°4- Nudge/Fichiers_nettoyés/Fichier_traité/sgsdata_nudges.xlsx", sep="")))


FFQ_NOV_23 <- read.xlsx((paste("Données analysées - Article N°1 chèques/Fichiers_bruts/23-11_FFQ.xlsx", sep="")))
sgsdata_TI <- sgsdata_TI 
#Indiquer la campagne
sgsdata_TI <- sgsdata_TI %>%mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"),2,1))

#Completer les données UC si manquant
sgsdata_TI <- sgsdata_TI %>%
  arrange(Identifiant) %>%group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>%ungroup()

#Completer les espaces vides par 0 
fill_zero <- function(df) { df %>%mutate(across(everything(),~ ifelse(is.na(.) | . == "", 0, .)))}

sgsdata_TI             <- fill_zero(sgsdata_TI)
sgsdata_CSGA      <- fill_zero(sgsdata_CSGA)
sgsdata_nudges <- fill_zero(sgsdata_nudges)

#SUpprimer les colonnes semaines 
sgsdata_TI      <- sgsdata_TI      %>% select(-contains("_sem"))
sgsdata_CSGA    <- sgsdata_CSGA    %>% select(-contains("_sem"))
sgsdata_nudges  <- sgsdata_nudges  %>% select(-contains("_sem"))

sgsdata_FFQ_CSGA <- sgsdata_CSGA %>% filter(Mesure != "Carnet") 
sgsdata_Booklet_CSGA <- sgsdata_CSGA %>% filter(Mesure == "Carnet")


#jAMBON BLANC INCLU DANS charcuterie en CSGA / Retrait MGV / Plats prep vege / Sauces et desserts lactés sont sortis car non présents en nudges
sgsdata_FFQ_CSGA <- sgsdata_FFQ_CSGA %>%
  semi_join(sgsdata_Booklet_CSGA, by = "Identifiant") %>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))
sgsdata_FFQ_CSGA$FV_Kcal <- sgsdata_FFQ_CSGA$FRUITS_Kcal + sgsdata_FFQ_CSGA$FRUITS_SECS_Kcal  + sgsdata_FFQ_CSGA$NOIX_Kcal + sgsdata_FFQ_CSGA$LEGUMES_Kcal 
sgsdata_FFQ_CSGA$FEC_Kcal <- sgsdata_FFQ_CSGA$FEC_NON_RAF_Kcal + sgsdata_FFQ_CSGA$FEC_RAF_Kcal
sgsdata_FFQ_CSGA$PDTS_LAITIERS_Kcal <- sgsdata_FFQ_CSGA$LAIT_Kcal + sgsdata_FFQ_CSGA$LAITAGES_Kcal + sgsdata_FFQ_CSGA$FROMAGES_Kcal
sgsdata_FFQ_CSGA$POULET_OEUFS_Kcal <- sgsdata_FFQ_CSGA$POULET_Kcal + sgsdata_FFQ_CSGA$OEUFS_Kcal
sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_FFQ_CSGA$CHARCUTERIE_HORS_JB_Kcal 
sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Kcal <- sgsdata_FFQ_CSGA$VIANDE_ROUGE_Kcal+ sgsdata_FFQ_CSGA$PORC_Kcal
sgsdata_FFQ_CSGA$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_CSGA$SNACKS_AUTRES_Kcal +  sgsdata_FFQ_CSGA$CEREALES_PD_Kcal + sgsdata_FFQ_CSGA$PDTS_SUCRES_Kcal
sgsdata_FFQ_CSGA$SSB_Kcal <-  sgsdata_FFQ_CSGA$SODAS_SUCRES_Kcal + sgsdata_FFQ_CSGA$SODAS_LIGHT_Kcal +sgsdata_FFQ_CSGA$FRUITS_JUS_Kcal 
sgsdata_FFQ_CSGA$SOMME_KCAL_Kcal <- sgsdata_FFQ_CSGA$SOMME_KCAL
sgsdata_FFQ_CSGA$SOMME_HORS_BOISSON_Kcal <-sgsdata_FFQ_CSGA$SOMME_HORS_BOISSON
sgsdata_FFQ_CSGA$SOMME_Poids_Kcal <- sgsdata_FFQ_CSGA$SOMME_POIDS
sgsdata_FFQ_CSGA$VIANDES_Kcal <- sgsdata_FFQ_CSGA$POULET_OEUFS_Kcal + sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Kcal + sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Kcal
sgsdata_Booklet_CSGA <- sgsdata_Booklet_CSGA %>%
  semi_join(sgsdata_FFQ_CSGA, by = "Identifiant") %>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))
sgsdata_Booklet_CSGA$FV_Kcal <- sgsdata_Booklet_CSGA$FRUITS_Kcal + sgsdata_Booklet_CSGA$FRUITS_SECS_Kcal  + sgsdata_Booklet_CSGA$NOIX_Kcal + sgsdata_Booklet_CSGA$LEGUMES_Kcal 
sgsdata_Booklet_CSGA$FEC_Kcal <- sgsdata_Booklet_CSGA$FEC_NON_RAF_Kcal + sgsdata_Booklet_CSGA$FEC_RAF_Kcal
sgsdata_Booklet_CSGA$PDTS_LAITIERS_Kcal <- sgsdata_Booklet_CSGA$LAIT_Kcal + sgsdata_Booklet_CSGA$LAITAGES_Kcal + sgsdata_Booklet_CSGA$FROMAGES_Kcal
sgsdata_Booklet_CSGA$POULET_OEUFS_Kcal <- sgsdata_Booklet_CSGA$POULET_Kcal + sgsdata_Booklet_CSGA$OEUFS_Kcal
sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_Booklet_CSGA$CHARCUTERIE_HORS_JB_Kcal 
sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Kcal <- sgsdata_Booklet_CSGA$VIANDE_ROUGE_Kcal+ sgsdata_Booklet_CSGA$PORC_Kcal
sgsdata_Booklet_CSGA$MG_Kcal <- sgsdata_Booklet_CSGA$MGA_Kcal 
sgsdata_Booklet_CSGA$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_Booklet_CSGA$SNACKS_AUTRES_Kcal +  sgsdata_Booklet_CSGA$CEREALES_PD_Kcal + sgsdata_Booklet_CSGA$PDTS_SUCRES_Kcal 
sgsdata_Booklet_CSGA$SSB_Kcal <-  sgsdata_Booklet_CSGA$SODAS_SUCRES_Kcal + sgsdata_Booklet_CSGA$SODAS_LIGHT_Kcal +sgsdata_Booklet_CSGA$FRUITS_JUS_Kcal 
sgsdata_Booklet_CSGA$SOMME_KCAL_Kcal <- sgsdata_Booklet_CSGA$SOMME_KCAL
sgsdata_Booklet_CSGA$SOMME_HORS_BOISSON_Kcal <-sgsdata_Booklet_CSGA$SOMME_HORS_BOISSON.x
sgsdata_Booklet_CSGA$SOMME_Poids_Kcal <- sgsdata_Booklet_CSGA$SOMME_POIDS.x
sgsdata_Booklet_CSGA$VIANDES_Kcal <- sgsdata_Booklet_CSGA$POULET_OEUFS_Kcal + sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Kcal + sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Kcal



#SELECTION DES DATAFRAMES TI -- Campagne 2 
sgsdata_FFQ_IT <- sgsdata_TI %>%filter(Mesure != "Carnet", UC_TI ==1) 

sgsdata_Booklet_IT <- sgsdata_TI %>% filter(Mesure == "Carnet", UC_TI ==1)

sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  filter(
    # on garde les lignes où AU MOINS une colonne « CARNET » est non‑NA ET non‑zéro
    rowSums(
      across(contains("CARNET"), ~ !is.na(.) & . != 0),
      na.rm = TRUE
    ) > 0
  )


sgsdata_FFQ_IT  <- sgsdata_FFQ_IT  %>% 
  semi_join(sgsdata_Booklet_IT, by = "Identifiant")%>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

sgsdata_FFQ_IT$FV_Kcal <- sgsdata_FFQ_IT$FRUITS_Kcal + sgsdata_FFQ_IT$FRUITS_SECS_Kcal  + sgsdata_FFQ_IT$NOIX_Kcal + sgsdata_FFQ_IT$LEGUMES_Kcal 
sgsdata_FFQ_IT$FEC_Kcal <- sgsdata_FFQ_IT$FEC_NON_RAF_Kcal + sgsdata_FFQ_IT$FEC_RAF_Kcal
sgsdata_FFQ_IT$PDTS_LAITIERS_Kcal <- sgsdata_FFQ_IT$LAIT_Kcal + sgsdata_FFQ_IT$LAITAGES_Kcal + sgsdata_FFQ_IT$FROMAGES_Kcal
sgsdata_FFQ_IT$POULET_OEUFS_Kcal <- sgsdata_FFQ_IT$POULET_Kcal + sgsdata_FFQ_IT$OEUFS_Kcal
sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_FFQ_IT$CHARCUTERIE_HORS_JB_Kcal +  sgsdata_FFQ_IT$JAMBON_BLANC_Kcal 
sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Kcal <- sgsdata_FFQ_IT$VIANDE_ROUGE_Kcal+ sgsdata_FFQ_IT$PORC_Kcal
sgsdata_FFQ_IT$MG_Kcal <- sgsdata_FFQ_IT$MGA_Kcal 
sgsdata_FFQ_IT$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_IT$SNACKS_AUTRES_Kcal +  sgsdata_FFQ_IT$CEREALES_PD_Kcal + sgsdata_FFQ_IT$PDTS_SUCRES_Kcal 
sgsdata_FFQ_IT$SSB_Kcal <-  sgsdata_FFQ_IT$SODAS_SUCRES_Kcal + sgsdata_FFQ_IT$SODAS_LIGHT_Kcal +sgsdata_FFQ_IT$FRUITS_JUS_Kcal 
sgsdata_FFQ_IT$SOMME_KCAL_Kcal <- sgsdata_FFQ_IT$SOMME_KCAL
sgsdata_FFQ_IT$SOMME_HORS_BOISSON_Kcal <-sgsdata_FFQ_IT$SOMME_HORS_BOISSON
sgsdata_FFQ_IT$SOMME_Poids_Kcal <- sgsdata_FFQ_IT$SOMME_POIDS
sgsdata_FFQ_IT$VIANDES_Kcal <- sgsdata_FFQ_IT$POULET_OEUFS_Kcal + sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Kcal + sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Kcal


sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>% 
  semi_join(sgsdata_FFQ_IT , by = "Identifiant")%>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))
sgsdata_Booklet_IT$FV_Kcal <- sgsdata_Booklet_IT$FRUITS_Kcal + sgsdata_Booklet_IT$FRUITS_SECS_Kcal  + sgsdata_Booklet_IT$NOIX_Kcal + sgsdata_Booklet_IT$LEGUMES_Kcal 
sgsdata_Booklet_IT$FEC_Kcal <- sgsdata_Booklet_IT$FEC_NON_RAF_Kcal + sgsdata_Booklet_IT$FEC_RAF_Kcal
sgsdata_Booklet_IT$PDTS_LAITIERS_Kcal <- sgsdata_Booklet_IT$LAIT_Kcal + sgsdata_Booklet_IT$LAITAGES_Kcal + sgsdata_Booklet_IT$FROMAGES_Kcal
sgsdata_Booklet_IT$POULET_OEUFS_Kcal <- sgsdata_Booklet_IT$POULET_Kcal + sgsdata_Booklet_IT$OEUFS_Kcal
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_Kcal 
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Kcal <- sgsdata_Booklet_IT$VIANDE_ROUGE_Kcal+ sgsdata_Booklet_IT$PORC_Kcal
sgsdata_Booklet_IT$MG_Kcal <- sgsdata_Booklet_IT$MGA_Kcal 
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_Booklet_IT$SNACKS_AUTRES_Kcal +  sgsdata_Booklet_IT$CEREALES_PD_Kcal  + sgsdata_Booklet_IT$PDTS_SUCRES_Kcal
sgsdata_Booklet_IT$SSB_Kcal <-  sgsdata_Booklet_IT$SODAS_SUCRES_Kcal + sgsdata_Booklet_IT$SODAS_LIGHT_Kcal +sgsdata_Booklet_IT$FRUITS_JUS_Kcal 
sgsdata_Booklet_IT$SOMME_KCAL_Kcal <- sgsdata_Booklet_IT$SOMME_Kcal
sgsdata_Booklet_IT$SOMME_HORS_BOISSON_Kcal <-sgsdata_Booklet_IT$SOMME_HORS_BOISSON_Kcal
sgsdata_Booklet_IT$SOMME_Poids_Kcal <- sgsdata_Booklet_IT$SOMME_Poids
sgsdata_Booklet_IT$VIANDES_Kcal <- sgsdata_Booklet_IT$POULET_OEUFS_Kcal + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Kcal + sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Kcal



#SELECTION DES DATAFRAMES NUDGES
sgsdata_FFQ_nudges <- sgsdata_nudges %>% filter(Mesure != "Carnet", UC_TI ==1)

sgsdata_Booklet_nudges <- sgsdata_nudges %>% filter(Mesure == "Carnet", UC_TI.x ==1) 

sgsdata_FFQ_nudges  <- sgsdata_FFQ_nudges  %>% 
  semi_join(sgsdata_Booklet_nudges, by = "Identifiant")%>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))
sgsdata_FFQ_nudges$FV_Kcal <- sgsdata_FFQ_nudges$FRUITS_Kcal + sgsdata_FFQ_nudges$FRUITS_SECS_Kcal  + sgsdata_FFQ_nudges$NOIX_Kcal + sgsdata_FFQ_nudges$LEGUMES_Kcal 
sgsdata_FFQ_nudges$FEC_Kcal <- sgsdata_FFQ_nudges$FEC_NON_RAF_Kcal + sgsdata_FFQ_nudges$FEC_RAF_Kcal
sgsdata_FFQ_nudges$PDTS_LAITIERS_Kcal <- sgsdata_FFQ_nudges$LAIT_Kcal + sgsdata_FFQ_nudges$LAITAGES_Kcal + sgsdata_FFQ_nudges$FROMAGES_Kcal
sgsdata_FFQ_nudges$POULET_OEUFS_Kcal <- sgsdata_FFQ_nudges$POULET_Kcal + sgsdata_FFQ_nudges$OEUFS_Kcal
sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_FFQ_nudges$CHARCUTERIE_HORS_JB_Kcal +  sgsdata_FFQ_nudges$JAMBON_BLANC_Kcal 
sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Kcal <- sgsdata_FFQ_nudges$VIANDE_ROUGE_Kcal+ sgsdata_FFQ_nudges$PORC_Kcal
sgsdata_FFQ_nudges$MG_Kcal <- sgsdata_FFQ_nudges$MGA_Kcal 
sgsdata_FFQ_nudges$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_nudges$SNACKS_AUTRES_Kcal +  sgsdata_FFQ_nudges$CEREALES_PD_Kcal  + sgsdata_FFQ_nudges$PDTS_SUCRES_Kcal 
sgsdata_FFQ_nudges$SSB_Kcal <-  sgsdata_FFQ_nudges$SODAS_SUCRES_Kcal + sgsdata_FFQ_nudges$SODAS_LIGHT_Kcal +sgsdata_FFQ_nudges$FRUITS_JUS_Kcal 
sgsdata_FFQ_nudges$SOMME_KCAL_Kcal <- sgsdata_FFQ_nudges$SOMME_KCAL
sgsdata_FFQ_nudges$SOMME_HORS_BOISSON_Kcal <-sgsdata_FFQ_nudges$SOMME_HORS_BOISSON
sgsdata_FFQ_nudges$SOMME_Poids_Kcal <- sgsdata_FFQ_nudges$SOMME_POIDS
sgsdata_FFQ_nudges$VIANDES_Kcal <- sgsdata_FFQ_nudges$POULET_OEUFS_Kcal + sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Kcal + sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Kcal


sgsdata_Booklet_nudges <- sgsdata_Booklet_nudges %>% 
  semi_join(sgsdata_FFQ_nudges, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", "")) #%>%
  #rename_with(~ paste0(.x, "_Kcal"), .cols = 28:61)
sgsdata_Booklet_nudges$FV_Kcal <- sgsdata_Booklet_nudges$FRUITS_Kcal + sgsdata_Booklet_nudges$FRUITS_SECS_Kcal  + sgsdata_Booklet_nudges$NOIX_Kcal + sgsdata_Booklet_nudges$LEGUMES_Kcal 
sgsdata_Booklet_nudges$FEC_Kcal <- sgsdata_Booklet_nudges$FEC_NON_RAF_Kcal + sgsdata_Booklet_nudges$FEC_RAF_Kcal
sgsdata_Booklet_nudges$PDTS_LAITIERS_Kcal <- sgsdata_Booklet_nudges$LAIT_Kcal + sgsdata_Booklet_nudges$LAITAGES_Kcal + sgsdata_Booklet_nudges$FROMAGES_Kcal
sgsdata_Booklet_nudges$POULET_OEUFS_Kcal <- sgsdata_Booklet_nudges$POULET_Kcal + sgsdata_Booklet_nudges$OEUFS_Kcal
sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_Booklet_nudges$CHARCUTERIE_HORS_JB_Kcal 
sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Kcal <- sgsdata_Booklet_nudges$VIANDE_ROUGE_Kcal+ sgsdata_Booklet_nudges$PORC_Kcal
sgsdata_Booklet_nudges$MG_Kcal <- sgsdata_Booklet_nudges$MGA_Kcal 
sgsdata_Booklet_nudges$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_Booklet_nudges$SNACKS_AUTRES_Kcal +  sgsdata_Booklet_nudges$CEREALES_PD_Kcal + sgsdata_Booklet_nudges$PDTS_SUCRES_Kcal 
sgsdata_Booklet_nudges$SSB_Kcal <-  sgsdata_Booklet_nudges$SODAS_SUCRES_Kcal + sgsdata_Booklet_nudges$SODAS_LIGHT_Kcal +sgsdata_Booklet_nudges$FRUITS_JUS_Kcal 
sgsdata_Booklet_nudges$SOMME_KCAL_Kcal <- sgsdata_Booklet_nudges$SOMME_KCAL
sgsdata_Booklet_nudges$SOMME_HORS_BOISSON_Kcal <-sgsdata_Booklet_nudges$SOMME_HORS_BOISSON.x
sgsdata_Booklet_nudges$SOMME_Poids_Kcal <- sgsdata_Booklet_nudges$SOMME_POIDS
sgsdata_Booklet_nudges$VIANDES_Kcal <- sgsdata_Booklet_nudges$POULET_OEUFS_Kcal + sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Kcal + sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Kcal




#cAMPAGNE 1
sgsdata_Booklet_IT11 <- sgsdata_Booklet_IT %>% filter(Campagne == 1 , Periode ==0) 
sgsdata_Booklet_IT21 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode ==1)  
sgsdata_FFQ_IT11 <- sgsdata_FFQ_IT %>% filter(Campagne == 1, Periode ==0) 
sgsdata_FFQ_IT21 <- sgsdata_FFQ_IT %>% filter(Campagne == 1, Periode ==1)

#CAMPAGNE 2
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2 , Periode ==0) 
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode ==1)  
sgsdata_FFQ_IT12 <- sgsdata_FFQ_IT %>% filter(Campagne == 2, Periode ==0) 
sgsdata_FFQ_IT22 <- sgsdata_FFQ_IT %>% filter(Campagne == 2, Periode ==1) 

#NUDGES
sgsdata_Booklet_nudges1 <- sgsdata_Booklet_nudges %>% filter(Periode ==0) 
sgsdata_Booklet_nudges2 <- sgsdata_Booklet_nudges %>% filter(Periode ==1)  
sgsdata_FFQ_nudges1 <- sgsdata_FFQ_nudges %>% filter(Periode ==0) 
sgsdata_FFQ_nudges2 <- sgsdata_FFQ_nudges %>% filter(Periode ==1)

#Faire un gros dataframe pour les FFQ
#ALL
dfs <- list(
  nudges1 = sgsdata_FFQ_nudges1,
  nudges2 = sgsdata_FFQ_nudges2,
  IT11    = sgsdata_FFQ_IT11,
  IT12    = sgsdata_FFQ_IT12,
  IT21    = sgsdata_FFQ_IT21,
  IT22    = sgsdata_FFQ_IT22
)

# 1) Colonnes communes à TOUS les data.frames
common_cols <- Reduce(intersect, lapply(dfs, names))

sgsdata_FFQ_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")#fAIRE en sorte que les mêmes id apparaissent dans chaque paire 



#Faire un gros dataframe pour les CARNET 
#ALL
dfs <- list(
  nudges1 = sgsdata_Booklet_nudges1,
  nudges2 = sgsdata_Booklet_nudges2,
  IT11    = sgsdata_Booklet_IT11,
  IT12    = sgsdata_Booklet_IT12,
  IT21    = sgsdata_Booklet_IT21,
  IT22    = sgsdata_Booklet_IT22
)

# 1) Colonnes communes à TOUS les data.frames
common_cols <- Reduce(intersect, lapply(dfs, names))

sgsdata_Booklet_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")#fAIRE en sorte que les mêmes id apparaissent dans chaque paire 





# Fonction pour filtrer les deux dataframes d'une paire
harmoniser_ids <- function(df1, df2, id_col = "Identifiant") {
  ids_communs <- intersect(df1[[id_col]], df2[[id_col]])
  df1_filtre <- df1 %>% filter(.data[[id_col]] %in% ids_communs)
  df2_filtre <- df2 %>% filter(.data[[id_col]] %in% ids_communs)
  list(df1 = df1_filtre, df2 = df2_filtre)
}



# Application à chaque paire
pair_IT11 <- harmoniser_ids(sgsdata_Booklet_IT11, sgsdata_FFQ_IT11)
sgsdata_Booklet_IT11 <- pair_IT11$df1
sgsdata_FFQ_IT11     <- pair_IT11$df2

pair_IT12 <- harmoniser_ids(sgsdata_Booklet_IT12, sgsdata_FFQ_IT12)
sgsdata_Booklet_IT12 <- pair_IT12$df1
sgsdata_FFQ_IT12     <- pair_IT12$df2

pair_IT21 <- harmoniser_ids(sgsdata_Booklet_IT21, sgsdata_FFQ_IT21)
sgsdata_Booklet_IT21 <- pair_IT21$df1
sgsdata_FFQ_IT21     <- pair_IT21$df2

pair_IT22 <- harmoniser_ids(sgsdata_Booklet_IT22, sgsdata_FFQ_IT22)
sgsdata_Booklet_IT22 <- pair_IT22$df1
sgsdata_FFQ_IT22     <- pair_IT22$df2

pair_nudges1 <- harmoniser_ids(sgsdata_Booklet_nudges1, sgsdata_FFQ_nudges1)
sgsdata_Booklet_nudges1 <- pair_nudges1$df1
sgsdata_FFQ_nudges1     <- pair_nudges1$df2

pair_nudges2 <- harmoniser_ids(sgsdata_Booklet_nudges2, sgsdata_FFQ_nudges2)
sgsdata_Booklet_nudges2 <- pair_nudges2$df1
sgsdata_FFQ_nudges2     <- pair_nudges2$df2




# --- Choisis les clés d’appariement ---
# adapte si besoin (ex. ajoute "variable_en" ou "Wave")
by_keys <- c("Identifiant", "Periode")

# --- Restreindre aux lignes communes des deux jeux ---
ffq_comm    <- sgsdata_FFQ_all     %>% semi_join(sgsdata_Booklet_all, by = by_keys)
booklet_comm<- sgsdata_Booklet_all %>% semi_join(sgsdata_FFQ_all, by = by_keys)

# --- Ordonner pareil pour garantir le même ordre de lignes ---
ffq_comm     <- ffq_comm     %>% arrange(across(all_of(by_keys)))
booklet_comm <- booklet_comm %>% arrange(across(all_of(by_keys)))


pair_all <- harmoniser_ids(booklet_comm , ffq_comm)
sgsdata_Booklet_comm <- pair_all$df1
sgsdata_ffq_comm    <- pair_all$df2 


analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Kcal"),
                              multiplier = 1000,
                              conf_level = 0.95) {
  library(dplyr)
  library(tidyr)
  
  # --- 0) Alignement si Identifiant présent ---
  if ("Identifiant" %in% names(ffq_data) && "Identifiant" %in% names(booklet_data)) {
    commun_ids   <- intersect(ffq_data$Identifiant, booklet_data$Identifiant)
    ffq_data     <- ffq_data     %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
    booklet_data <- booklet_data %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
  }
  if (nrow(ffq_data) != nrow(booklet_data)) {
    stop("ffq_data et booklet_data n'ont pas le même nombre de lignes après alignement.")
  }
  
  # --- 1) Colonnes ciblées par suffixe ---
  motif        <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  vars_ffq     <- grep(motif, names(ffq_data),     value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  
  if (length(vars_ffq) == 0)     stop("Aucune colonne FFQ ne se termine par ", paste(suffixes, collapse = ", "))
  if (length(vars_booklet) == 0) stop("Aucune colonne Booklet ne se termine par ", paste(suffixes, collapse = ", "))
  
  # --- 2) Préparer listes pour transformation (exclure explicitement SOMME_KCAL_Kcal) ---
  Kcal_ffq     <- setdiff(grep("_Kcal$",  vars_ffq,     value = TRUE), "SOMME_KCAL_Kcal")
  Kcal_booklet <- setdiff(grep("_Kcal$",  vars_booklet, value = TRUE), "SOMME_KCAL_Kcal")
  kcal_ffq      <- grep("_Kcal$",  vars_ffq,     value = TRUE)
  kcal_booklet  <- grep("_Kcal$",  vars_booklet, value = TRUE)
  
  # --- 3) Moyennes FFQ (long) avec ajustements d’unités (hors SOMME_KCAL_Kcal) ---
  moy_ffq <- ffq_data %>%
    select(all_of(vars_ffq)) %>%

    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_FFQ")
  
  # --- 4) Moyennes Booklet (long) avec ajustements d’unités (hors SOMME_KCAL_Kcal) ---
  moy_booklet <- booklet_data %>%
    select(all_of(vars_booklet)) %>%

    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_Booklet")
  
  # --- 5) Base jointure pour stats ---
  tableau_base <- left_join(moy_booklet, moy_ffq, by = "variable")
  
  # --- 6) Stats par variable ---
  stats <- lapply(tableau_base$variable, function(var) {
    x_full <- ffq_data[[var]]
    y_full <- booklet_data[[var]]
    
    valid <- !is.na(x_full) & !is.na(y_full)
    x <- x_full[valid]; y <- y_full[valid]
    
    if (length(x) <= 1) {
      return(data.frame(
        variable = var, pct_bias = NA, p_value_diff = NA,
        ci95_FFQ = NA, ci95_Booklet = NA,
        pearson_correlation = NA, pearson_p_value = NA,
        spearman_correlation = NA, spearman_p_value = NA,
        pearson_correlation = NA, pearson_p_value = NA,
        pct_similar_quintile = NA, pct_adjacent_quintile = NA, pct_opposite_quintile = NA
      ))
    }
    
    # 6.1 Ajustements d’unités (sauf SOMME_KCAL_Kcal)
    if (grepl("_Kcal$", var) && var != "SOMME_KCAL_Kcal") { x <- x * multiplier; y <- y * multiplier }
    if (grepl("_Kcal$",  var))                              { x <- x / 100;      y <- y / 100      }
    
    # 6.2 % bias
    pct_bias <- (mean(x) - mean(y)) / mean(y) * 100
    
    # 6.3 Tests t et IC
    t_diff <- t.test(x, y)
    t_ffq  <- t.test(x, conf.level = conf_level)
    t_book <- t.test(y, conf.level = conf_level)
    
    # 6.4 Corrélations brutes
    if (sd(x) > 0 && sd(y) > 0) {
      pearson_tst  <- cor.test(x, y, method = "pearson",  exact = FALSE)
      spearman_tst <- cor.test(x, y, method = "spearman", exact = FALSE)
      pearson_est  <- pearson_tst$estimate;  pearson_p  <- pearson_tst$p.value
      spearman_est <- spearman_tst$estimate; spearman_p <- spearman_tst$p.value
    } else {
      pearson_est <- NA; pearson_p <- NA
      spearman_est<- NA; spearman_p<- NA
    }
    
    # 6.5 Corrélation ajustée sur énergie (energy = SOMME_KCAL_Kcal non transformée)
    if ("SOMME_KCAL_Kcal" %in% names(ffq_data) && "SOMME_KCAL_Kcal" %in% names(booklet_data)) {
      energy_ffq <- ffq_data$SOMME_KCAL_Kcal[valid]
      energy_bk  <- booklet_data$SOMME_KCAL_Kcal[valid]
      # Choix: ajuster les deux sur la même énergie (booklet) pour neutraliser l’apport réel
      lm_x <- lm(x ~ energy_bk)
      lm_y <- lm(y ~ energy_bk)
      resid_x <- residuals(lm_x); resid_y <- residuals(lm_y)
      if (sd(resid_x) > 0 && sd(resid_y) > 0) {
        tst <- cor.test(resid_x, resid_y, method = "pearson", exact = FALSE)
        est <- tst$estimate; p <- tst$p.value
      } else { est <- NA; p <- NA }
    } else { est <- NA; p <- NA }
    
    # 6.6 Accord par quintiles
    qx <- dplyr::ntile(x, 5); qy <- dplyr::ntile(y, 5)
    delta <- qx - qy
    d <- abs(delta)
    pct_similar   <- mean(d == 0,        na.rm = TRUE) * 100
    pct_adjacent  <- mean(d %in% c(1,2),        na.rm = TRUE) * 100
    pct_opposite  <- mean(d %in% c(3,4), na.rm = TRUE) * 100  # sévères (3–4)
    
    data.frame(
      variable = var,
      pct_bias = signif(pct_bias, 2),
      p_value_diff = t_diff$p.value,
      ci95_FFQ     = sprintf("[%.2f, %.2f]", t_ffq$conf.int[1],  t_ffq$conf.int[2]),
      ci95_Booklet = sprintf("[%.2f, %.2f]", t_book$conf.int[1], t_book$conf.int[2]),
      pearson_correlation  = pearson_est,
      pearson_p_value      = pearson_p,
      pearson_correlation = signif(est, 2),
      pearson_p_value     = p,
      spearman_correlation = spearman_est,
      spearman_p_value     = spearman_p,
      pct_similar_quintile  = pct_similar,
      pct_adjacent_quintile = pct_adjacent,
      pct_opposite_quintile = pct_opposite
    )
  }) %>% bind_rows()
  
  # --- 7) Assemblage final ---
  tableau_final <- tableau_base %>%
    left_join(stats, by = "variable") %>%
    select(
      variable,
      moyenne_Booklet, ci95_Booklet,
      moyenne_FFQ,     ci95_FFQ,
      pct_bias, p_value_diff,
      pearson_correlation,  pearson_p_value,
      pearson_correlation, pearson_p_value,
      spearman_correlation, spearman_p_value,
      pct_similar_quintile, pct_adjacent_quintile, pct_opposite_quintile
    ) %>%
    mutate(across(where(is.numeric), ~ signif(.x, 2)))
  
  return(tableau_final)
}




#SAUVEGARGE POUR PUS TARD
sgsdata_FFQ_IT11_bis <- sgsdata_FFQ_IT11 
sgsdata_Booklet_IT11_bis <- sgsdata_Booklet_IT11
sgsdata_FFQ_IT12_bis <- sgsdata_FFQ_IT12 
sgsdata_Booklet_IT12_bis <- sgsdata_Booklet_IT12
sgsdata_Booklet_IT21_bis <- sgsdata_Booklet_IT21
sgsdata_FFQ_IT21_bis <- sgsdata_FFQ_IT21
sgsdata_Booklet_IT22_bis <- sgsdata_Booklet_IT22
sgsdata_FFQ_IT22_bis <- sgsdata_FFQ_IT22
sgsdata_Booklet_nudges1_bis <-sgsdata_Booklet_nudges1
sgsdata_FFQ_nudges1_bis <- sgsdata_FFQ_nudges1
sgsdata_Booklet_nudges2_bis <-sgsdata_Booklet_nudges2
sgsdata_FFQ_nudges2_bis <- sgsdata_FFQ_nudges2
sgsdata_FFQ_CSGA_bis <- sgsdata_FFQ_CSGA
sgsdata_Booklet_CSGA_bis <- sgsdata_Booklet_CSGA
sgsdata_FFQ_com_bis <- ffq_comm 
sgsdata_Booklet_com_bis <- booklet_comm


# Lancer les analyses pour les deux périodes
tableau_final_1 <- analyser_moyennes(sgsdata_FFQ_nudges1, sgsdata_Booklet_nudges1)
tableau_final_1 <- tableau_final_1 %>%
  arrange(variable)

tableau_final_2 <- analyser_moyennes(sgsdata_FFQ_nudges2, sgsdata_Booklet_nudges2)
tableau_final_2 <- tableau_final_2 %>%
  arrange(variable)

tableau_final_3 <- analyser_moyennes(sgsdata_FFQ_IT11, sgsdata_Booklet_IT11)
tableau_final_3 <- tableau_final_3 %>%
  arrange(variable)

tableau_final_4 <- analyser_moyennes(sgsdata_FFQ_IT21, sgsdata_Booklet_IT21)
tableau_final_4 <- tableau_final_4 %>%
  arrange(variable)


tableau_final_5 <- analyser_moyennes(sgsdata_FFQ_IT12, sgsdata_Booklet_IT12)
tableau_final_5 <- tableau_final_5 %>%
  arrange(variable)

tableau_final_6 <- analyser_moyennes(sgsdata_FFQ_IT22, sgsdata_Booklet_IT22)
tableau_final_6 <- tableau_final_6 %>%
  arrange(variable)


tableau_final_7 <- analyser_moyennes(sgsdata_FFQ_CSGA, sgsdata_Booklet_CSGA)
tableau_final_7 <- tableau_final_7 %>%
  arrange(variable)



tableau_final_8 <- analyser_moyennes(ffq_comm , booklet_comm )
tableau_final_8 <- tableau_final_8 %>%
  arrange(variable)



# 1) Vecteur complet des variables dans l’ordre voulu
variables <- c(
  "VIANDES_Kcal",
  "CEREALES_PD_Kcal", 
  #"CHARCUTERIE_HORS_JB_Kcal",
  "FEC_NON_RAF_Kcal",
  "FEC_RAF_Kcal",
  "FROMAGES_Kcal",
  "FRUITS_Kcal",
  "FRUITS_SECS_Kcal", 
  #"JAMBON_BLANC_Kcal", 
  "LAITAGES_Kcal",
  "LEGUMES_Kcal",
  "LEG_SECS_Kcal",
  "NOIX_Kcal",
  "OEUFS_Kcal", 
  "PDTS_SUCRES_Kcal", 
  "PORC_Kcal", 
  "POULET_Kcal", 
  "QUICHES_PIZZAS_TARTES_SALEES_Kcal", 
  "SNACKS_AUTRES_Kcal",
  "VIANDE_ROUGE_Kcal",  
  "FRUITS_JUS_Kcal", 
  "LAIT_Kcal", 
  "SODAS_LIGHT_Kcal", 
  "SODAS_SUCRES_Kcal",
  "MGA_Kcal", 
  "MGV_Kcal",
  "FV_Kcal",
  "FEC_Kcal",
  "PDTS_LAITIERS_Kcal",
  "POULET_OEUFS_Kcal", 
  "AUTRE_PDTS_ANIMAUX_Kcal",
  "VIANDE_ROUGE_PORC_Kcal",
  "PDTS_DISCRETIONNAIRES_Kcal",  
  "SSB_Kcal" ,
  "PLATS_PREP_VEGETARIENS_Kcal", 
  "PLATS_PREP_CARNES_Kcal",
  "POISSONS_Kcal",
  "ALCOOL_Kcal",  
  "LEG_SECS_Kcal",
  "SAUCES_Kcal",
  "DESSERTS_LACTES_Kcal",
  "EAU_Kcal",
  "SOMME_HORS_BOISSON_Kcal",
  "SOMME_Poids_Kcal",
  "SOMME_KCAL_Kcal"

)

# 2) Créer un template (data.frame ou tibble) qu'on joindra à chaque tableau
library(dplyr)
template <- tibble(variable = variables)

# 3) Fonction de réordonnancement + calcul de Delta + placement de Delta en 6ᵉ position
reorder_and_delta <- function(df){
  df_ord <- template %>%
    left_join(df, by = "variable") %>%                    # garde toutes les variables
    mutate(Delta = moyenne_FFQ - moyenne_Booklet) %>%     # calcule Delta
    relocate(Delta, .after = 5)                           # place en 6ᵉ colonne
  return(df_ord)
}

# 4) Application aux trois tableaux
tableau1_ord <- reorder_and_delta(tableau_final_1)
tableau2_ord <- reorder_and_delta(tableau_final_2)
tableau3_ord <- reorder_and_delta(tableau_final_3)
tableau4_ord <- reorder_and_delta(tableau_final_4)
tableau5_ord <- reorder_and_delta(tableau_final_5)
tableau6_ord <- reorder_and_delta(tableau_final_6)
tableau7_ord <- reorder_and_delta(tableau_final_7)
tableau8_ord <- reorder_and_delta(tableau_final_8)



### Graphs correlations Kcal ----------------------------------------------------------
tableau1_ord <- tableau1_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )

    

tableau2_ord <- tableau2_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )



tableau3_ord <- tableau3_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )


tableau4_ord <- tableau4_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )



tableau5_ord <- tableau5_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )

tableau6_ord <- tableau6_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )



tableau7_ord <- tableau7_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )

tableau8_ord <- tableau8_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |(spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )

# 1) Ajout de la période et combinaison
df1 <- tableau1_ord %>% mutate(periode = "Nov21_TI")
df2 <- tableau2_ord %>% mutate(periode = "March22_TI")
df3 <- tableau3_ord %>% mutate(periode = "Nov22_TI")
df4 <- tableau4_ord %>% mutate(periode = "March23_TI")
df5 <- tableau5_ord %>% mutate(periode = "Nov23_TI")
df6 <- tableau6_ord %>% mutate(periode = "March24_TI")
df7 <- tableau7_ord %>% mutate(periode = "Nov22 (CSGA)")
df8 <- tableau8_ord %>% mutate(periode = "All")

# —————————————————————————
# Données
# (adapte si besoin : ici on reprend ta construction)
heat_df <- bind_rows(df1, df2, df3, df4, df5, df6, df7) %>%
  filter(!str_ends(variable, "_Poids"), variable != "SOMME_Poids")

# —————————————————————————
# Niveaux et palette
levels_classe <- c(
  "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different",
  "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different",
  "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different",
  "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different",
  "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different",
  "Weakly. No correlation coefficient ≥ 0.4.",
  "Missing"   # <- libellé unique pour les manquants
)

palette_custom_named <- c(
  "Weakly. No correlation coefficient ≥ 0.4." = "firebrick",
  "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" = "goldenrod",
  "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" = "#C7E9C0",
  "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" = "#A1D97B",
  "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" = "#74C499",
  "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" = "#31A354",
  "Missing" = "grey"   # <- tuiles blanches pour les manquants
)

# —————————————————————————
# Périodes & labels X
period_levels <- c(
  "Nov21_TI","March22_TI",
  "Nov22_TI","March23_TI",
  "Nov23_TI","March24_TI",
  "Nov22 (CSGA)"
)

labels_x <- setNames(
  c("Nov 21","Mar 22","Nov 22","Mar 23","Nov 23","Mar 24","Nov22"),
  period_levels
)

# —————————————————————————
# Labels variables (anglais)
labels_EN <- c(
  CEREALES_PD_Kcal = "Breakfast cereals",
  DESSERTS_LACTES_Kcal = "Dairy Desserts",
  FEC_NON_RAF_Kcal = "Unrefined Starches",
  FEC_RAF_Kcal = "Refined Starches",
  FROMAGES_Kcal = "Cheeses",
  FRUITS_Kcal = "Fruits",
  FRUITS_SECS_Kcal = "Dried Fruits",
  LAITAGES_Kcal = "Dairy Products",
  LEGUMES_Kcal = "Vegetables",
  LEG_SECS_Kcal = "Legumes",
  MGA_Kcal = "Animal Fats",
  MGV_Kcal = "Vegetable Fats",
  NOIX_Kcal = "Nuts",
  OEUFS_Kcal = "Eggs",
  PDTS_SUCRES_Kcal = "Sweet products",
  PLATS_PREP_CARNES_Kcal = "Meat Based Prepared Dishes",
  PLATS_PREP_VEGETARIENS_Kcal = "Vegetarian Prepared Dishes",
  POISSONS_Kcal = "Fish",
  PORC_Kcal = "Pork",
  POULET_Kcal = "Chicken",
  QUICHES_PIZZAS_TARTES_SALEES_Kcal = "Quiches, Pizzas & Savoury Pies",
  SAUCES_Kcal = "Sauces",
  SNACKS_AUTRES_Kcal = "Other Snacks",
  VIANDE_ROUGE_Kcal = "Red Meat",
  ALCOOL_Kcal = "Alcohol",
  EAU_Kcal = "Water",
  FRUITS_JUS_Kcal = "Fruit Juices",
  LAIT_Kcal = "Milk",
  SODAS_LIGHT_Kcal = "Diet Sodas",
  SODAS_SUCRES_Kcal = "Sugary Sodas",
  FV_Kcal  = "Fruits and vegetables",
  FEC_Kcal = "Starchy food",
  PDTS_LAITIERS_Kcal = "Dairy products",
  POULET_OEUFS_Kcal = "Eggs / chicken",
  AUTRE_PDTS_ANIMAUX_Kcal = "Cold cuts",
  VIANDE_ROUGE_PORC_Kcal = "Red meat/Pork",
  PDTS_DISCRETIONNAIRES_Kcal = "Discretionnary food",
  SSB_Kcal = "Sugary sweet beverages",
  SOMME_HORS_BOISSON_Kcal = "Total weight without beverages",
  SOMME_Poids_Kcal = "Total weight",
  SOMME_KCAL_Kcal = "Total kilocalories",
  VIANDES_Kcal = "Meats"
)

# —————————————————————————
# Préparation df_all : facettes, périodes, recodage Missing
df_all <- heat_df %>%
  mutate(
    # catégories pour les facettes
    category = case_when(
      variable %in% c("SOMME_HORS_BOISSON_Kcal", "SOMME_Poids_Kcal", "SOMME_KCAL_Kcal") ~ "General\nindicator",
      variable %in% c(#"AUTRE_PDTS_ANIMAUX_Kcal",
                      "FV_Kcal","FEC_Kcal","PDTS_LAITIERS_Kcal",
                      #"POULET_OEUFS_Kcal",
                      #"VIANDE_ROUGE_PORC_Kcal",
                      "VIANDES_Kcal",
                      "PDTS_DISCRETIONNAIRES_Kcal","SSB_Kcal") ~ "General\nfood item",
      TRUE ~ "Specific\nfood item"
    ) %>% factor(levels = c("General\nindicator","General\nfood item","Specific\nfood item")),
    # vagues pour facettes colonnes
    wave = case_when(
      periode %in% c("Nov21_TI","March22_TI") ~ "Weekly FFQ 1",
      periode %in% c("Nov22_TI","March23_TI") ~ "Weekly FFQ 2",
      periode %in% c("Nov23_TI","March24_TI") ~ "Weekly FFQ 3",
      periode == "Nov22 (CSGA)"               ~ "Monthly FFQ 1"#,
      #periode =="All"   ~ "All"
    ) %>% factor(levels = c("Weekly FFQ 1","Weekly FFQ 2","Weekly FFQ 3","Monthly FFQ 1")),
    # ordre des périodes
    periode = factor(periode, levels = period_levels),
    # recoder NA -> "Missing" (vraie modalité)
    classe  = fct_explicit_na(classe, na_level = "Missing")
  )

# —————————————————————————
# Ordre des variables (du meilleur au pire, Missing à la fin)
df_all <- df_all %>%
  mutate(classe_num = as.integer(factor(classe, levels = levels_classe)))

var_ord <- df_all %>%
  group_by(variable) %>%
  summarise(score = mean(classe_num, na.rm = TRUE), .groups = "drop") %>%
  arrange(score) %>%
  pull(variable)

df_all <- df_all %>%
  mutate(variable = factor(variable, levels = var_ord))

# —————————————————————————
# (Optionnel) Vérifs rapides :
print(setdiff(levels(df_all$classe), names(palette_custom_named)))  # doit renvoyer character(0)

# —————————————————————————
# Graphique
p <- ggplot(df_all, aes(x = periode, y = variable, fill = classe)) +
  geom_tile(colour = "white") +
  facet_grid(
    category ~ wave,
    scales  = "free",
    space   = "free",
    switch  = "y",
    labeller = labeller(
      wave     = label_wrap_gen(width = 10),
      category = label_value
    )
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(labels_x[x], width = 6),
    expand = c(0, 0)
  ) +
  scale_y_discrete(labels = labels_EN) +
  scale_fill_manual(
    name   = "Correlation scale",
    values = palette_custom_named,
    limits = levels_classe,      # ordre fixe + évite le gris
    drop   = FALSE,
    labels = function(x) str_wrap(x, width = 30)
  ) +
  labs(title = "Correlation of contribution to proportion of energy intake", x = NULL, y = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.margin        = unit(c(1, 1, 1, 4), "lines"),
    axis.text.y        = element_text(size = 9, angle = 0, hjust = 1),
    panel.spacing.y    = unit(1, "lines"),
    strip.placement    = "outside",
    strip.text.y.left  = element_blank(),
    strip.background.y = element_blank(),
    strip.background.x = element_blank(),
    strip.text.x       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.text        = element_text(size = 8, lineheight = 0.9),
    panel.grid         = element_blank(),
    plot.title         = element_text(color = "black", face = "bold", hjust = 0.5, size = 12)
  )

print(p)


# 1) Regrouper les résultats de vos 7 campagnes
all_stats <- bind_rows(
  tableau_final_1 %>% mutate(periode = "Nov21_TI"),
  tableau_final_2 %>% mutate(periode = "March22_TI"),
  tableau_final_3 %>% mutate(periode = "Nov22_TI"),
  tableau_final_4 %>% mutate(periode = "March23_TI"),
  tableau_final_5 %>% mutate(periode = "Nov23_TI"),
  tableau_final_6 %>% mutate(periode = "March24_TI"),
  tableau_final_7 %>% mutate(periode = "Nov22_CSGA")#,
  #tableau_final_8 %>% mutate(periode = "All")
)
lab_map <- c(
  Nov21_TI   = "Weekly\nFFQ 1",
  March22_TI = "Weekly\nFFQ 1",
  Nov22_TI   = "Weekly\nFFQ 2",
  March23_TI = "Weekly\nFFQ 2",
  Nov23_TI   = "Weekly\nFFQ 3",
  March24_TI = "Weekly\nFFQ 3",
  Nov22_CSGA = "Monthly\nFFQ 1"#,
  #All = "All"
  
)


# 2) Préparer le data.frame long : un pourcentage par variable × période × classification
plot_df_vars <- all_stats %>%
  select(variable, periode,
         pct_similar_quintile,
         pct_adjacent_quintile,
         pct_opposite_quintile) %>%
  pivot_longer(
    cols      = starts_with("pct_"),
    names_to  = "classification",
    values_to = "percentage"
  ) %>%
  mutate(
    classification = recode_factor(
      classification,
      pct_similar_quintile  = "Same quintile",
      pct_adjacent_quintile = "Adjacent quintiles",
      pct_opposite_quintile = "Opposite quintiles"
    ),
    classification = factor(classification,
                            levels = c("Same quintile","Adjacent quintiles","Opposite quintiles")),
    periode = factor(periode,
                     levels = c("Nov21_TI","Nov22_TI",
                                "Nov23_TI","Nov22_CSGA"))
  )

# 3) Mapping code → libellé anglais
labels_EN <- c(
  VIANDES_Kcal = "Meats",
  CEREALES_PD_Kcal                          = "Breakfast cereals",
  FEC_NON_RAF_Kcal                          = "Unrefined starches",
  FEC_RAF_Kcal                              = "Refined starches",
  FROMAGES_Kcal                             = "Cheeses",
  FRUITS_Kcal                               = "Fruits",
  FRUITS_SECS_Kcal                          = "Dried fruits",
  LAITAGES_Kcal                             = "Dairy products",
  LEGUMES_Kcal                              = "Vegetables",
  LEG_SECS_Kcal                             = "Legumes",
  NOIX_Kcal                                 = "Nuts",
  OEUFS_Kcal                                = "Eggs",
  PDTS_SUCRES_Kcal                          = "Sweet products",
  PORC_Kcal                                 = "Pork",
  POULET_Kcal                               = "Chicken",
  QUICHES_PIZZAS_TARTES_SALEES_Kcal         = "Savory pies & pizzas",
  SNACKS_AUTRES_Kcal                        = "Other snacks",
  VIANDE_ROUGE_Kcal                         = "Red meat",
  FRUITS_JUS_Kcal                           = "Fruit juices",
  LAIT_Kcal                                 = "Milk",
  SODAS_LIGHT_Kcal                          = "Diet sodas",
  SODAS_SUCRES_Kcal                         = "Sugary sodas",
  MGA_Kcal                                  = "Animal fats",
  MGV_Kcal                                  = "Vegetable fats",
  FV_Kcal                                   = "Fruits & vegetables",
  FEC_Kcal                                  = "Starchy foods",
  PDTS_LAITIERS_Kcal                        = "Dairy products",
  POULET_OEUFS_Kcal                         = "Eggs/Chicken",
  AUTRE_PDTS_ANIMAUX_Kcal                   = "Other animal products",
  VIANDE_ROUGE_PORC_Kcal                    = "Red meat & pork",
  PDTS_DISCRETIONNAIRES_Kcal                = "Discretionary foods",
  SSB_Kcal                                  = "Sugary beverages",
  PLATS_PREP_VEGETARIENS_Kcal               = "Vegetarian dishes",
  PLATS_PREP_CARNES_Kcal                    = "Meat dishes",
  POISSONS_Kcal                             = "Fish",
  ALCOOL_Kcal                               = "Alcohol",
  SAUCES_Kcal                               = "Sauces",
  DESSERTS_LACTES_Kcal                      = "Dairy desserts",
  EAU_Kcal                                  = "Water",
  SOMME_HORS_BOISSON_Kcal                   = "Total weight (no beverages)",
  SOMME_Kcal_Kcal                          = "Total weight",
  SOMME_KCAL_Kcal                           = "Total calories"
)


# 0) Définir les groupes
general_indicators <- c("SOMME_HORS_BOISSON_Kcal","SOMME_Kcal_Kcal","SOMME_KCAL_Kcal")

general_food_items <- c("FV_Kcal","VIANDES_Kcal","FEC_Kcal","PDTS_LAITIERS_Kcal",#"POULET_OEUFS_Kcal",
                        #"VIANDE_ROUGE_PORC_Kcal",
                        "PDTS_DISCRETIONNAIRES_Kcal","SSB_Kcal")

# tout le reste sera "Specific items"
plot_quintile_by_campaign <- function(df,
                                      keep_vars   = NULL,
                                      remove_vars = NULL,
                                      label_map   = NULL,
                                      n_cols      = 7,
                                      label_min   = 3,
                                      label_dec   = 0) {
  general_indicators <- c("SOMME_HORS_BOISSON_Kcal","SOMME_Kcal_Kcal","SOMME_KCAL_Kcal")
  general_food_items <- c("FV_Kcal","FEC_Kcal","PDTS_LAITIERS_Kcal","POULET_OEUFS_Kcal",
                          "VIANDE_ROUGE_PORC_Kcal","PDTS_DISCRETIONNAIRES_Kcal","SSB_Kcal")
  
  df2 <- df
  if (!is.null(keep_vars))   df2 <- dplyr::filter(df2, variable %in% keep_vars)
  if (!is.null(remove_vars)) df2 <- dplyr::filter(df2, !variable %in% remove_vars)
  
  df2 <- df2 %>%
    dplyr::mutate(
      group = dplyr::case_when(
        variable %in% general_indicators ~ "General\nindicator",
        variable %in% general_food_items ~ "General\nfood items",
        TRUE                             ~ "Specific\nitems"
      ),
      group = factor(group, levels = c("General\nindicator","General\nfood items","Specific\nitems")),
      var_en = ifelse(!is.null(label_map) & variable %in% names(label_map),
                      label_map[variable], variable),
      var_en = factor(var_en, levels = unique(var_en)),
      periode = {
        desired <- c("Nov21_TI","March22_TI","Nov22_TI","March23_TI",
                     "Nov23_TI","March24_TI","Nov22_CSGA")
        lvls <- desired[desired %in% unique(periode)]
        factor(periode, levels = lvls)
      }
    ) %>%
    dplyr::filter(!is.na(periode), !is.na(var_en), !is.na(classification), !is.na(percentage)) %>%
    droplevels()
  
  ggplot(df2, aes(x = var_en, y = percentage, fill = classification)) +
    geom_col(position = "fill", width = 0.8) +
    geom_text(
      aes(label = ifelse(percentage >= label_min,
                         scales::number(percentage, accuracy = 1), "")),
      position = position_fill(vjust = 0.5),
      size = 2.5, color = "black", fontface = "bold"
    ) +
    coord_flip() +
    facet_grid(
      group ~ periode,
      scales = "free_y",
      space  = "free_y",
      labeller = labeller(
        periode = as_labeller(lab_map),
        group   = function(x) rep("", length(x)) # supprime titres des lignes
      )
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(
      name   = "Classification",
      values = c("Same quintile"="#31A354","Adjacent quintiles"="#C7E9C0","Opposite quintiles"="yellow")
    ) +
    labs(x = NULL, y = "% Individuals", title = "Quintile classification agreement by campaign") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y        = element_text(size = 8),
      strip.text.y.left  = element_blank(), # enlève le texte de strip vertical
      strip.text.x       = element_text(face = "bold"),
      panel.spacing.y    = unit(0.5, "lines"),
      panel.spacing.x    = unit(0.2, "lines"),
      legend.position    = "bottom"
    )
}


p_by_campaign <- plot_quintile_by_campaign(
  df          = plot_df_vars,
  keep_vars   = c(
    "SOMME_HORS_BOISSON_Kcal",
    "SOMME_Kcal_Kcal",
    "SOMME_KCAL_Kcal",
     "FV_Kcal",
    "FEC_Kcal",
    "PDTS_LAITIERS_Kcal",
    "POULET_OEUFS_Kcal",
    "VIANDES_Kcal",
    #"AUTRE_PDTS_ANIMAUX_Kcal",
    #"VIANDE_ROUGE_PORC_Kcal",
    #"PDTS_DISCRETIONNAIRES_Kcal",
    "SSB_Kcal"  ,
    "CEREALES_PD_Kcal",     
    "FEC_NON_RAF_Kcal",      
    "FEC_RAF_Kcal",     
    "FROMAGES_Kcal",      
    "FRUITS_Kcal",     
    "FRUITS_SECS_Kcal",      
    "LAITAGES_Kcal",     
    "LEGUMES_Kcal",      
    "LEG_SECS_Kcal",     
    "NOIX_Kcal",      
    "OEUFS_Kcal",     
    "PDTS_SUCRES_Kcal",      
    "PORC_Kcal",     
    "POULET_Kcal",      
    "QUICHES_PIZZAS_TARTES_SALEES_Kcal",     
    "SNACKS_AUTRES_Kcal",      
    "VIANDE_ROUGE_Kcal",     
    "FRUITS_JUS_Kcal",      
    "LAIT_Kcal",     
    "SODAS_LIGHT_Kcal",      
    "SODAS_SUCRES_Kcal",     
    "MGA_Kcal",      
    "MGV_Kcal",     
    "PLATS_PREP_VEGETARIENS_Kcal",      
    "PLATS_PREP_CARNES_Kcal",     
    "POISSONS_Kcal",      
    "ALCOOL_Kcal",     
    "SAUCES_Kcal",      
    "DESSERTS_LACTES_Kcal",     
    "EAU_Kcal"     
    
    ),
  label_map   = labels_EN,
  n_cols      = 4   # 7 campagnes → 7 colonnes
)
print(p_by_campaign)





#ANALYSE PAR PETIT GROS CONSO------------------

add_tertiles_id <- function(df, base, id_col = "Identifiant", seed = NULL) {
  Kcal_vars <- grep("(?i)_Kcal$", names(df), value = TRUE, perl = TRUE)
  
  for (var in Kcal_vars) {
    num <- as.numeric(as.character(df[[var]]))
    
    # Pré‐initialisation des colonnes T1/T2/T3
    df[[paste0(var, "_T1")]] <- NA_character_
    df[[paste0(var, "_T2")]] <- NA_character_
    df[[paste0(var, "_T3")]] <- NA_character_
    

      # Règle standard : découpe 20% / 60% / 20%, même quand il y a beaucoup de zéros
      
      # Optionnel : reproductibilité
      if (!is.null(seed)) set.seed(seed)
      
      # indices valides
      valid <- which(!is.na(num))
      n_tot <- length(valid)
      n1    <- floor(0.2 * n_tot)
      n2    <- floor(0.8 * n_tot)
      
      # ordre croissant + jitter pour "casser" les ties (ex. les zéros)
      rj    <- runif(n_tot)
      ord   <- valid[order(num[valid], rj)]
      
      # répartir
      idx1 <- ord[seq_len(n1)]
      idx2 <- ord[(n1 + 1):n2]
      idx3 <- ord[(n2 + 1):n_tot]
      
      df[[paste0(var, "_T1")]][idx1] <- df[[id_col]][idx1]
      df[[paste0(var, "_T2")]][idx2] <- df[[id_col]][idx2]
      df[[paste0(var, "_T3")]][idx3] <- df[[id_col]][idx3]
    
  }
  
  return(df)
}


# Liste de vos data‑frames initiales
dfs <- list(
  nudges1 = sgsdata_Booklet_nudges1_bis,
  nudges2 = sgsdata_Booklet_nudges2_bis,
  IT11  = sgsdata_Booklet_IT11_bis,
  IT12  = sgsdata_Booklet_IT12_bis,
  IT21  = sgsdata_Booklet_IT21_bis,
  IT22  = sgsdata_Booklet_IT22_bis,
  CSGA  = sgsdata_Booklet_CSGA_bis,
  com = sgsdata_Booklet_com_bis
)

for (nm in names(dfs)) {
  full_df <- add_tertiles_id(dfs[[nm]], base = nm)  # <-- base est maintenant passé
  
  assign(paste0("sgsdata_Booklet_", nm, "_bis"), full_df, envir = .GlobalEnv)
  
  # Pour chaque k = 1,2,3 :
  for (k in 1:3) {
    suffix <- paste0("_T", k)
    # Sélection des colonnes du tertile k
    subset_df <- full_df %>%
      select(ends_with(suffix)) %>%
      rename_with(~ sub(paste0(suffix, "$"), "", .x), ends_with(suffix))
    
    assign(
      paste0("sgsdata_Booklet_", nm, "_T", k),
      subset_df,
      envir = .GlobalEnv
    )
  }
}


# Vos suffixes
bases <- c("IT11","IT12","IT21","IT22","nudges1","nudges2","CSGA","com")

for (base in bases) {
  # 1) DF complet
  df_full <- get(paste0("sgsdata_Booklet_", base, "_bis"))
  
  # 2) Repère toutes les colonnes *_Kcal
  Kcal_vars <- grep("(?i)_Kcal$", names(df_full), value = TRUE, perl = TRUE)
  
  # 3) Boucle sur chaque tertile (1,2,3)
  for (k in 1:3) {
    # 3a) le mask correspondant (contient les mêmes noms de colonnes *_Kcal)
    mask_df <- get(paste0("sgsdata_Booklet_", base, "_T", k))
    
    # 3b) copie de df_full avec Identifiant + colonnes Kcal
    new_df <- df_full %>%
      select(Identifiant, all_of(Kcal_vars))
    
    # 3c) pour chaque variable Kcal, on met NA si mask_df[[var]] est NA
    for (var in Kcal_vars) {
      if (! var %in% names(mask_df)) {
        stop("Le masque n’a pas de colonne ‘", var, "’ pour ", base, "_T", k)
      }
      new_df[[var]] <- ifelse(!is.na(mask_df[[var]]),
                              new_df[[var]],
                              NA)
    }
    
    # 3d) on assigne dans l’environnement global
    assign(paste0("sgsdata_Booklet_", base, "_Kcal_T", k),
           new_df,
           envir = .GlobalEnv)
  }
}



bases <- c("IT11","IT12","IT21","IT22","nudges1","nudges2","CSGA","com")

for (base in bases) {
  df_ffq_full <- get(paste0("sgsdata_FFQ_", base, "_bis"))
  Kcal_vars  <- grep("(?i)_Kcal$", names(df_ffq_full), value = TRUE, perl = TRUE)
  
  for (k in 1:3) {
    mask_booklet <- get(paste0("sgsdata_Booklet_", base, "_Kcal_T", k))
    
    # Colonnes communes *_Kcal
    common_vars <- intersect(Kcal_vars, names(mask_booklet))
    if (length(common_vars)==0) next
    
    # 1) On filtre le FFQ pour ne garder que les IDs du masque
    df_ffq <- df_ffq_full %>%
      filter(Identifiant %in% mask_booklet$Identifiant) %>%
      select(Identifiant, all_of(common_vars))
    
    # 2) On prépare le sous‐masque Booklet, joint par Identifiant
    mask_sub <- mask_booklet %>%
      select(Identifiant, all_of(common_vars))
    
    # 3) On fait la jointure : chaque ligne FFQ récupère les colonnes *_Kcal.mask
    df_join <- df_ffq %>%
      left_join(mask_sub, by = "Identifiant", suffix = c("", ".mask"))
    
    # 4) Pour chaque var commune, on ne garde la valeur FFQ que si la colonne .mask n'est pas NA
    for (var in common_vars) {
      df_join[[var]] <- ifelse(
        !is.na(df_join[[paste0(var, ".mask")]]),
        df_join[[var]],
        NA_real_
      )
    }
    
    # 5) On supprime toutes les colonnes *.mask
    df_sel <- df_join %>%
      select(Identifiant, all_of(common_vars))
    
    # 6) On assigne le résultat
    assign(
      paste0("sgsdata_FFQ_", base, "_Kcal_T", k),
      df_sel,
      envir = .GlobalEnv
    )
  }
}



analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Kcal"),
                              multiplier = 1000,
                              conf_level = 0.95) {
  library(dplyr)
  library(tidyr)
  library(stringr)

  # 1) repérer les colonnes d’intérêt
  motif        <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  vars_ffq     <- grep(motif, names(ffq_data),    value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  if (!length(vars_ffq))     stop("Pas de colonnes FFQ en ", motif)
  if (!length(vars_booklet)) stop("Pas de colonnes Booklet en ", motif)

  # 2) moyennes FFQ (0 comptés comme 0)
  moy_ffq <- ffq_data %>%
    select(all_of(vars_ffq)) %>%

    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "moyenne_FFQ")

  # 3) moyennes Booklet (idem)
  moy_booklet <- booklet_data %>%
    select(all_of(vars_booklet)) %>%

    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(),
                 names_to  = "variable",
                 values_to = "moyenne_Booklet")

  # base commune
  tableau_base <- left_join(moy_booklet, moy_ffq, by = "variable")

  # 4) stats ligne à ligne
  stats <- lapply(tableau_base$variable, function(var) {
    x_raw <- as.numeric(ffq_data[[var]])
    y_raw <- as.numeric(booklet_data[[var]])
    idx   <- which(!is.na(x_raw) & !is.na(y_raw))
    n_valid <- length(idx)
    xv <- x_raw[idx]; yv <- y_raw[idx]
    if (grepl("_Kcal$", var)) { xv <- xv * multiplier; yv <- yv * multiplier }
    if (grepl("_Kcal$",  var)) { xv <- xv / 100;     yv <- yv / 100     }

    # init
    p_diff      <- NA_real_; ciFFQ_str <- NA_character_; ciBook_str <- NA_character_
    pearson_est <- NA_real_; pearson_p <- NA_real_
    spearman_est<- NA_real_; spearman_p<- NA_real_

    # t.test apparié dès 2 obs
    if (n_valid >= 2) {
      tt  <- tryCatch(t.test(xv, yv, paired=TRUE, conf.level=conf_level),
                      error=function(e) NULL)
      tt2 <- tryCatch(t.test(yv, conf.level=conf_level),
                      error=function(e) NULL)
      if (!is.null(tt)) {
        p_diff    <- tt$p.value
        ciFFQ_str <- sprintf("[%.2f, %.2f]", tt$conf.int[1], tt$conf.int[2])
      }
      if (!is.null(tt2)) {
        ciBook_str <- sprintf("[%.2f, %.2f]", tt2$conf.int[1], tt2$conf.int[2])
      }
    }

    # corrélations dès 2 obs
    if (n_valid >= 2) {
      pr <- tryCatch(cor.test(xv, yv, method="pearson", exact=FALSE),
                     error=function(e) NULL)
      sp <- tryCatch(cor.test(xv, yv, method="spearman",exact=FALSE),
                     error=function(e) NULL)
      if (!is.null(pr)) {
        pearson_est <- pr$estimate; pearson_p <- pr$p.value
      }
      if (!is.null(sp)) {
        spearman_est <- sp$estimate; spearman_p <- sp$p.value
      }
    }

    data.frame(
      variable             = var,
      n_valid              = n_valid,
      p_value_diff         = signif(p_diff,       2),
      ci95_FFQ             = ciFFQ_str,
      ci95_Booklet         = ciBook_str,
      pearson_correlation  = signif(pearson_est,  2),
      pearson_p_value      = signif(pearson_p,    2),
      spearman_correlation = signif(spearman_est, 2),
      spearman_p_value     = signif(spearman_p,   2),
      stringsAsFactors     = FALSE
    )
  }) %>% bind_rows()

  # 5) assemblage final
  tableau_final <- tableau_base %>%
    left_join(stats, by = "variable") %>%
    select(
      variable, n_valid,
      moyenne_Booklet, ci95_Booklet,
      moyenne_FFQ,     ci95_FFQ,
      p_value_diff,
      pearson_correlation, pearson_p_value,
      spearman_correlation, spearman_p_value
    )

  return(tableau_final)
}


tableau_final_nudges_T1 <- analyser_moyennes(sgsdata_FFQ_nudges1_Kcal_T1, sgsdata_Booklet_nudges1_Kcal_T1)
tableau_final_nudges_T1 <- tableau_final_nudges_T1 %>%
  arrange(variable)
tableau_final_nudges_T2 <- analyser_moyennes(sgsdata_Booklet_nudges1_Kcal_T2, sgsdata_Booklet_nudges1_Kcal_T2)
tableau_final_nudges_T2 <- tableau_final_nudges_T2 %>%
  arrange(variable)
tableau_final_nudges_T3 <- analyser_moyennes(sgsdata_FFQ_nudges1_Kcal_T3, sgsdata_Booklet_nudges1_Kcal_T3)
tableau_final_nudges_T3 <- tableau_final_nudges_T3 %>%
  arrange(variable)

tableau_final_IT11_T1 <- analyser_moyennes(sgsdata_FFQ_IT11_Kcal_T1, sgsdata_Booklet_IT11_Kcal_T1)
tableau_final_IT11_T1 <- tableau_final_IT11_T1 %>%
  arrange(variable)
tableau_final_IT11_T2 <- analyser_moyennes(sgsdata_FFQ_IT11_Kcal_T2, sgsdata_Booklet_IT11_Kcal_T2)
tableau_final_IT11_T2 <- tableau_final_IT11_T2 %>%
  arrange(variable)
tableau_final_IT11_T3 <- analyser_moyennes(sgsdata_FFQ_IT11_Kcal_T3, sgsdata_Booklet_IT11_Kcal_T3)
tableau_final_IT11_T3 <- tableau_final_IT11_T3 %>%
  arrange(variable)

tableau_final_IT12_T1 <- analyser_moyennes(sgsdata_FFQ_IT12_Kcal_T1, sgsdata_Booklet_IT12_Kcal_T1)
tableau_final_IT12_T1 <- tableau_final_IT12_T1 %>%
  arrange(variable)
tableau_final_IT12_T2 <- analyser_moyennes(sgsdata_FFQ_IT12_Kcal_T2, sgsdata_Booklet_IT12_Kcal_T2)
tableau_final_IT12_T2 <- tableau_final_IT12_T2 %>%
  arrange(variable)
tableau_final_IT12_T3 <- analyser_moyennes(sgsdata_FFQ_IT12_Kcal_T3, sgsdata_Booklet_IT12_Kcal_T3)
tableau_final_IT12_T3 <- tableau_final_IT12_T3 %>%
  arrange(variable)

tableau_final_CSGA_T1 <- analyser_moyennes(sgsdata_FFQ_CSGA_Kcal_T1, sgsdata_Booklet_CSGA_Kcal_T1)
tableau_final_CSGA_T1 <- tableau_final_CSGA_T1 %>%
  arrange(variable)
tableau_final_CSGA_T2 <- analyser_moyennes(sgsdata_FFQ_CSGA_Kcal_T2, sgsdata_Booklet_CSGA_Kcal_T2)
tableau_final_CSGA_T2 <- tableau_final_CSGA_T2 %>%
  arrange(variable)
tableau_final_CSGA_T3 <- analyser_moyennes(sgsdata_FFQ_CSGA_Kcal_T3, sgsdata_Booklet_CSGA_Kcal_T3)
tableau_final_CSGA_T3 <- tableau_final_CSGA_T3 %>%
  arrange(variable)

tableau_final_com_T1 <- analyser_moyennes(sgsdata_FFQ_com_Kcal_T1, sgsdata_Booklet_com_Kcal_T1)
tableau_final_com_T1 <- tableau_final_com_T1 %>%
  arrange(variable)
tableau_final_com_T2 <- analyser_moyennes(sgsdata_FFQ_com_Kcal_T2, sgsdata_Booklet_com_Kcal_T2)
tableau_final_com_T2 <- tableau_final_com_T2 %>%
  arrange(variable)
tableau_final_com_T3 <- analyser_moyennes(sgsdata_FFQ_com_Kcal_T3, sgsdata_Booklet_com_Kcal_T3)
tableau_final_com_T3 <- tableau_final_com_T3 %>%
  arrange(variable)

library(dplyr)

bases <- c("nudges1","nudges2","IT11","IT12","IT21","IT22","CSGA","com")

for (base in bases) {
  for (k in 1:3) {
    # 1) Détermine les noms de tes dataframes Booklet & FFQ
    df_b <- get(paste0("sgsdata_Booklet_", base, "_Kcal_T", k))
    df_f <- get(paste0("sgsdata_FFQ_",      base, "_Kcal_T", k))
    
    # 2) Calcule et ordonne
    tbl_ord <- analyser_moyennes( df_f, df_b) %>%
      arrange(variable) %>%
      mutate(
        classe = case_when(
          pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" ,
          (pearson_correlation >= 0.4 |  spearman_correlation >= 0.4) & (pearson_correlation >= 0.6 |  spearman_correlation >= 0.6)  & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" ,
          pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" ,
          (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" ,
          (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" ,
          spearman_correlation < 0.4 &  pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient ≥ 0.4.",
          is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
            "Valeur manquante"
        )
      )
    
    # 3) Stocke sous un nom unique
    assign(
      paste0("tableau_final_", base, "_T", k, "_ord"),
      tbl_ord,
      envir = .GlobalEnv
    )
  }
}



# 1) Récupération et empilement de tous les tableaux_final_*_ord
tbl_names <- ls(pattern = "^tableau_final_.*_ord$")

df_all <- lapply(tbl_names, function(nm) {
  df <- get(nm)
  # Extraction de base et tertile depuis le nom
  parts <- str_match(nm, "^tableau_final_([^_]+)_T([123])_ord$")
  base  <- parts[2]
  tert  <- as.integer(parts[3])
  # Construction de la période selon base & tertile
  periode <- switch(base,
                    IT11    = c("Nov22_TI","Nov22_TI","Nov22_TI")[tert],
                    IT12    = c("March23_TI","March23_TI","March23_TI")[tert],
                    IT21    = c("Nov23_TI","Nov23_TI","Nov23_TI")[tert],
                    IT22    = c("March24_TI","March24_TI","March24_TI")[tert],
                    nudges1 = c("Nov21_TI", "Nov21_TI", "Nov21_TI")[tert],
                    nudges2 = c("March22_TI","March22_TI","March22_TI")[tert],
                    CSGA    = c("Nov22 (CSGA)","Nov22 (CSGA)", "Nov22 (CSGA)")[tert],
                    com    = c("All","All", "All")[tert]
  )
  df %>%
    mutate(
      base    = base,
      tertile = paste0("T", tert),
      periode = periode
    )
}) %>%
  bind_rows() %>%
  # Filtrage pour ne garder que les Kcal (sans Kcal ni total Kcal)
  filter(
    !str_ends(variable, "_Kcal"),
    variable != "SOMME_KCALTOT"
  )

# 2) Définition des niveaux de classe et palette
levels_classe <- c(
  "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different",
  "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different",
  "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different",
  "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different",
  "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different",
  "Weakly. No correlation coefficient ≥ 0.4.",
  "Missing value"
)

palette_custom <- c(
  "Weakly. No correlation coefficient ≥ 0.4." =
    "firebrick",
  "Poorly. At least one of the two correlation coefficients is ≥ 0.4, but the means are statistically different" =
    "goldenrod",
  "Moderatly. At least one of the two correlation coefficients is ≥ 0.4 and the means are not significantly different" =
    "#C7E9C0",
  "Substantialy. The two correlation coefficients are ≥ 0.4 and the means are not significantly different" =
    "#A1D97B",
  "Strongly. One of the two correlation coefficients is ≥ 0.6 and the other ≥ 0.4, and the averages are not significantly different" =
    "#74C499",
  "Intensely. The two correlation coefficients are >= 0.6 and the averages are not significantly different" =
    "#31A354",
  "Missing value" = "grey80"
)

# 3) Traduction des variables pour l'axe y
labels_EN <- c(
  CEREALES_PD_Kcal                          = "Breakfast cereals",
  CHARCUTERIE_HORS_JB_Kcal                   = "Cold cuts",
  DESSERTS_LACTES_Kcal                       = "Dairy Desserts",
  FEC_NON_RAF_Kcal                           = "Unrefined Starches",
  FEC_RAF_Kcal                               = "Refined Starches",
  FROMAGES_Kcal                              = "Cheeses",
  FRUITS_Kcal                                = "Fruits",
  FRUITS_SECS_Kcal                           = "Dried Fruits",
  JAMBON_BLANC_Kcal                          = "White Ham",
  LAITAGES_Kcal                              = "Dairy Products",
  LEGUMES_Kcal                               = "Vegetables",
  LEG_SECS_Kcal                              = "Legumes",
  MGA_Kcal                                   = "Animal Fats",
  MGV_Kcal                                   = "Vegetable Fats",
  NOIX_Kcal                                  = "Nuts",
  OEUFS_Kcal                                 = "Eggs",
  PDTS_SUCRES_Kcal                           = "Sweet products",
  PLATS_PREP_CARNES_Kcal                     = "Meat Based Prepared Dishes",
  PLATS_PREP_VEGETARIENS_Kcal                = "Vegetarian Prepared Dishes",
  POISSONS_Kcal                              = "Fish",
  PORC_Kcal                                  = "Pork",
  POULET_Kcal                                = "Chicken",
  QUICHES_PIZZAS_TARTES_SALEES_Kcal          = "Quiches, Pizzas & Savoury Pies",
  SAUCES_Kcal                                = "Sauces",
  SNACKS_AUTRES_Kcal                         = "Other Snacks",
  VIANDE_ROUGE_Kcal                          = "Red Meat",
  ALCOOL_Kcal                                = "Alcohol",
  EAU_Kcal                                   = "Water",
  FRUITS_JUS_Kcal                            = "Fruit Juices",
  LAIT_Kcal                                  = "Milk",
  SODAS_LIGHT_Kcal                           = "Diet Sodas",
  SODAS_SUCRES_Kcal                          = "Sugary Sodas",
  FV_Kcal                                    = "Fruits and vegetables",
  FEC_Kcal                                   = "Starchy food",
  PDTS_LAITIERS_Kcal                         = "Dairy products",
  POULET_OEUFS_Kcal                          = "Eggs / chicken",
  AUTRE_PDTS_ANIMAUX_Kcal                    = "Cold cuts",
  PLATS_PREP_Kcal                            = "Prepared dishes",
  VIANDE_ROUGE_PORC_Kcal                     = "Red meat/Pork",
  MG_Kcal                                    = "Added fats",
  PDTS_DISCRETIONNAIRES_Kcal                 = "Discretionary food",
  SSB_Kcal                                   = "Sugary sweet beverages",
  SOMME_KCAL_Kcal                            = "Total Kcal",
  SOMME_Kcal_Kcal                           = "Total weight",
  SOMME_HORS_BOISSON_Kcal                    = "Total weight without beverages"
)

# 4) Définition des périodes et labels
period_levels <- c(
  "Nov21_TI","March22_TI","Nov22_TI",
  "March23_TI","Nov23_TI","March24_TI",
  "Nov22 (CSGA)", "All"
)
labels_x <- setNames(
  c("Nov 21","Mar 22","Nov 22","Mar 23","Nov 23","Mar 24","Nov 22", "All"),
  period_levels
)



# 5) Ajout des colonnes ‘category’ et ‘wave’
df_plot <- df_all %>%
  mutate(
    periode = factor(periode, levels = period_levels),
    category = case_when(
      variable %in% c("SOMME_HORS_BOISSON_Kcal","SOMME_Kcal_Kcal","SOMME_KCAL_Kcal") ~
        "General\nindicator",
      variable %in% c("FV_Kcal","FEC_Kcal","PDTS_LAITIERS_Kcal",
                      #"POULET_OEUFS_Kcal","VIANDE_ROUGE_PORC_Kcal",
                      "PDTS_DISCRETIONNAIRES_Kcal","SSB_Kcal", "VIANDES_Kcal"#"AUTRE_PDTS_ANIMAUX_Kcal"
                      ) ~
        "General\nfood item",
      TRUE ~ "Specific\nfood item"
    ) %>%
      factor(levels = c("General\nindicator","General\nfood item","Specific\nfood item")),
    wave = case_when(
      periode %in% c("Nov21_TI","March22_TI") ~ "Weekly FFQ 1",
      periode %in% c("Nov22_TI","March23_TI") ~ "Weekly FFQ 2",
      periode %in% c("Nov23_TI","March24_TI") ~ "Weekly FFQ 3",
      periode == "Nov22 (CSGA)"               ~ "Monthly FFQ 1"
    ) %>%
      factor(levels = c("Weekly FFQ 1","Weekly FFQ 2","Weekly FFQ 3","Monthly FFQ 1")),
    classe = factor(classe, levels = levels_classe)
  )

vars_a_supprimer <- c("CAFE_THE_Kcal", "UC_TI_Kcal", "EPICES_CONDIMENTS_Kcal", "SOMME_Kcal"
                      #,
                      #"CHARCUTERIE_HORS_JB_Kcal", "JAMBON_BLANC_Kcal",
                      #"CEREALES_PD_Kcal", "DESSERTS_LACTES_Kcal",
                      #"FEC_NON_RAF_Kcal","FEC_RAF_Kcal","FROMAGES_Kcal",
                      #"FRUITS_Kcal","FRUITS_SECS_Kcal","LAITAGES_Kcal",
                      #"LEGUMES_Kcal", "LEG_SECS_Kcal","MGA_Kcal","MGV_Kcal",
                      #"NOIX_Kcal", "OEUFS_Kcal", "PDTS_SUCRES_Kcal",
                      #"PLATS_PREP_CARNES_Kcal","PLATS_PREP_VEGETARIENS_Kcal",
                      #"POISSONS_Poid","PORC_Kcal","POULET_Kcal","QUICHES_PIZZAS_TARTES_SALEES_Kcal",
                      #"SAUCES_Kcal", "SNACKS_AUTRES_Kcal","VIANDE_ROUGE_Kcal",
                      #"ALCOOL_Kcal","EAU_Kcal", "FRUITS_JUS_Kcal","LAIT_Kcal",
                      #"SODAS_LIGHT_Kcal","SODAS_SUCRES_Kcal", "POISSONS_Kcal", "MG_Kcal"#,
                      #"SOMME_KCAL_Kcal",
                      #"SOMME_Kcal_Kcal",                           
                      #"SOMME_HORS_BOISSON_Kcal"                

)

df_plot <- df_plot %>%
  filter(!variable %in% vars_a_supprimer)


# 1) Nettoyage des labels de période + bold Markdown pour wave
df_plot <- df_plot %>%
  mutate(
    # 1a. Enlever le suffixe _TI
    periode = sub("_TI$", "", periode),
    # 1b. Ajouter un espace entre letters et chiffres
    periode = sub("([A-Za-z]+)([0-9]{2}$)", "\\1 \\2", periode),
    # 1c. Remettre en factor avec les niveaux propres
    periode = factor(
      periode,
      levels = c("Nov 21", "March 22", "Nov 22", "March 23",
                 "Nov 23", "March 24", "Nov 22 (CSGA)")
    ),
    
    # 1d. Transformer wave en bold Markdown
    wave = factor(
      wave,
      levels = c("Weekly FFQ 1", "Weekly FFQ 2", "Weekly FFQ 3", "Monthly FFQ 1"),
      labels = c("**Weekly FFQ 1**", "**Weekly FFQ 2**",
                 "**Weekly FFQ 3**", "**Monthly FFQ 1**")
    )
  )

library(forcats)

# 1) Transforme les NA en un niveau “Missing value”
df_plot <- df_plot %>%
  mutate(
    classe = fct_explicit_na(classe, na_level = "Missing value")
  )


###% de saturation---------------------------------
library(dplyr)

# 1) Récupérer les noms de tous les tableaux commençant par "tableau_final_"
tbl_names <- ls(pattern = "^tableau_final_")

# 2) Les charger dans une liste
tbl_list <- mget(tbl_names, envir = .GlobalEnv)

# 3) Empiler tous les data.frames en un seul tibble, en gardant le nom d’origine
tableau_final <- bind_rows(tbl_list, .id = "source")

# 4) (Optionnel) Si vous voulez extraire base et tertile du nom de chaque source :
library(tidyr)
tableau_final <- tableau_final %>%
  separate(
    col   = source,
    into  = c("discard","base","tertile"),
    sep   = "_",
    extra = "merge"
  ) %>%
  select(-discard)


# filtrage avec une expression régulière
tableau_final <- tableau_final[ grepl("_ord$", tableau_final$tertile), ]

# vector of target categories
cibles <- c("FV_Kcal",
            "FEC_Kcal",
            "PDTS_LAITIERS_Kcal",
            #"POULET_OEUFS_Kcal",
            #"VIANDE_ROUGE_PORC_Kcal",
            "PDTS_DISCRETIONNAIRES_Kcal",
            "SSB_Kcal",
            #"AUTRE_PDTS_ANIMAUX_Kcal"
            "VIANDES_Kcal"
            )

tableau_final  <- tableau_final[tableau_final$variable %in% cibles, ]
tableau_final$delta <- tableau_final$moyenne_FFQ - tableau_final$moyenne_Booklet

tableau_final <- tableau_final %>%
  select(tertile, variable, moyenne_Booklet, moyenne_FFQ, delta)

tableau_final <- tableau_final %>%
  filter(str_detect(tertile, "nudges1|nudges2|IT11|IT12|IT21|IT22|CSGA|com")) #"nudges2|IT12|IT22" #"nudges1|IT11|IT21|CSGA"




df_plot_all <- tableau_final %>%
  filter(!is.na(moyenne_Booklet), !is.na(moyenne_FFQ), !is.na(tertile)) %>%
  mutate(
    # extraction du préfixe
    TertileTypeRaw = str_replace(tertile, "_T[123]_ord$", ""),
    # recodage en libellés complets
    TertileLabel = recode_factor(TertileTypeRaw,
                                 nudges1 = "Weekly FFQ 1 (Nov 21)",
                                 nudges2 = "Weekly FFQ 1 (March 22)",
                                 IT11    = "Weekly FFQ 2 (Nov 22)",
                                 IT12    = "Weekly FFQ 2 (March 23)",
                                 IT21    = "Weekly FFQ 3 (Nov 23)",
                                 IT22    = "Weekly FFQ 3 (March 24)",
                                 CSGA    = "Monthly FFQ 1",
                                 com    = "All"
    ),
    # traduction des catégories
    variable_en = case_when(
      variable == "FV_Kcal"                    ~ "Fruits & Vegetables",
      variable == "FEC_Kcal"                   ~ "Starchy Foods",
      variable == "PDTS_LAITIERS_Kcal"         ~ "Dairy Products",
      #variable == "POULET_OEUFS_Kcal"          ~ "Poultry & Eggs",
      variable == "SSB_Kcal"                   ~ "Sugary Sweet Beverages",
      variable == "PDTS_DISCRETIONNAIRES_Kcal" ~ "Discretionary Foods",
      #variable == "VIANDE_ROUGE_PORC_Kcal"     ~ "Red Meat & Pork",
      #variable == "AUTRE_PDTS_ANIMAUX_Kcal"    ~ "Cold cuts",
      variable == "VIANDES_Kcal" ~ "Meats",
      TRUE                                      ~ variable
    ) %>% factor(levels = c(
      "Fruits & Vegetables","Starchy Foods","Dairy Products",
      #"Poultry & Eggs",
      "Sugary Sweet Beverages","Discretionary Foods","Meats"
      #"Red Meat & Pork","Cold cuts"
    )),
    # label T1/T2/T3 en pourcentages
    Campaign = case_when(
      str_detect(tertile, "_T1_ord$") ~ "≤ 20 %",
      str_detect(tertile, "_T2_ord$") ~ "20 %–80 %",
      str_detect(tertile, "_T3_ord$") ~ "> 80 %"
    ) %>% factor(levels = c("≤ 20 %", "20 %–80 %", "> 80 %")),
    # calcul de l'over‑estimation
    diff = moyenne_FFQ - moyenne_Booklet
  )




# ---- 1) Préparer positions avec un "gap" entre aliments ----
gap <- 2.2   # >1 = plus d’espace entre aliments (essaie 2.0–2.8)
off <- 0.25  # écart entre Waves dans un même aliment


df_v <- df_plot_all %>%
  mutate(
    Bin      = Campaign,
    Wave     = as.factor(TertileLabel),
    var_fac  = factor(variable_en),
    var_ord  = as.numeric(var_fac),
    W        = dplyr::n_distinct(Wave),
    x_base   = var_ord * gap,
    x_pos    = x_base + (as.numeric(Wave) - (W + 1)/2) * off,
    delta    = moyenne_FFQ - moyenne_Booklet,
    has_change = !is.na(delta) & abs(delta) >= 1   # seuil anti-bruit
  )

# 2) Breaks/labels et séparateurs
x_breaks <- df_v %>%
  distinct(var_ord, x_base) %>%
  arrange(var_ord) %>%
  pull(x_base)

x_labels <- levels(df_v$var_fac) |> str_wrap(width = 16)
x_labels_spaced <- str_replace_all(x_labels, " ", "\u00A0\u00A0")  # 2 NBSP

# >>> ICI on crée bien 'seps' (milieux entre breaks) <<<
seps <- tibble(x = (x_breaks[-1] + x_breaks[-length(x_breaks)]) / 2)

# 3) Palette daltonien-friendly (Okabe–Ito)
okabe_ito <- c("#000000","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")
pal <- setNames(okabe_ito[seq_along(levels(df_v$Wave))], levels(df_v$Wave))

# 0) Ordonner les niveaux de Bin
df_v <- df_v %>%
  dplyr::mutate(
    Bin = forcats::fct_relevel(Bin, "≤ 20 %", "20 %-80 %", "> 80 %")
  )

# 1) Graphique
ggplot(df_v, aes(color = Wave, group = Wave)) +
  geom_vline(data = seps, aes(xintercept = x),
             linetype = "twodash", linewidth = 0.4,
             color = "#7F7F7F", alpha = 0.7) +
  geom_segment(
    data = dplyr::filter(df_v, has_change),
    aes(x = x_pos, xend = x_pos, y = moyenne_Booklet, yend = moyenne_FFQ),
    linewidth = 0.9, lineend = "round", alpha = 0.9,
    arrow = arrow(ends = "last", type = "closed", angle = 14, length = unit(2.4, "mm"))
  ) +
  facet_wrap(~ Bin, ncol = 1, scales = "fixed", strip.position = "left") +
  scale_color_manual(values = pal, name = "Wave") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels_spaced,
                     expand = expansion(mult = c(0.04, 0.10))) +
  scale_y_continuous(name = "Energy (Kcal/d/CU)",
                     labels = scales::label_number(accuracy = 1, big.mark = " "),
                     breaks = scales::breaks_pretty(n = 5),
                     expand = expansion(mult = c(0.03, 0.08))) +
  labs(title = "FFQ vs Booklet by Food Category", x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title          = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    legend.position     = "bottom",
    legend.title        = element_text(face = "bold"),
    axis.text.x         = element_text(size = 9, angle = 45, hjust = 1, vjust = 1),
    axis.text.y         = element_text(size = 9.5),
    strip.text.y.left   = element_text(face = "bold"),
    panel.spacing.y     = unit(6, "mm"),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_line(linewidth = 0.3, colour = "grey85")
  )


library(dplyr)

df_delta <- df_v %>%
  transmute(
    Bin, Wave, variable_en, x_pos,
    delta = moyenne_FFQ-moyenne_Booklet,
    denom = moyenne_FFQ
  ) %>%
  group_by(Bin) %>%
  mutate(
    pct       = if_else(is.na(denom) | denom == 0, NA_real_, 100 * delta / denom),
    label_pct = if_else(is.na(pct), NA_character_, sprintf("%+d%%", round(pct))),
    off       = 0.04 * diff(range(delta, na.rm = TRUE)),
    # si delta==0, on pousse un peu le label pour qu'il ne colle pas à la ligne 0
    label_y   = if_else(delta == 0, delta + off * 0.8, delta + sign(delta) * off),
    hjust_lb  = if_else(delta < 0, 1, 0),
    has_change = delta != 0 & !is.na(delta)
  ) %>%
  ungroup()


ggplot(df_delta, aes(x = x_pos, y = delta, color = Wave)) +
  geom_hline(yintercept = 0, linewidth = 0.7, linetype = 2, color = "grey65") +
  # flèches uniquement si delta != 0
  geom_segment(
    data = dplyr::filter(df_delta, has_change),
    aes(x = x_pos, xend = x_pos, y = 0, yend = delta),
    position = position_dodge(width = 0.2),
    linewidth = 0.9,
    lineend = "round",
    arrow = arrow(ends = "last", type = "closed", angle = 8, length = unit(3, "mm")),
    na.rm = TRUE
  ) +
  # étiquettes en % (en gras)
  geom_text(
    aes(y = label_y, label = label_pct, hjust = hjust_lb),
    position = position_dodge(width = 0.2),
    fontface = "bold",
    size = 2.7,
    lineheight = 0.9,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  facet_wrap(~ Bin, nrow = 1, scales = "fixed") +
  scale_color_brewer(palette = "Set2", name = "Wave") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels,
                     expand = expansion(mult = c(0.03, 0.06))) +
  labs(title = "Bias (FFQ − Booklet) by Food Category", x = NULL, y = "Δ (kcal/d/CU)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    legend.position = "bottom",
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9.5),
    strip.text      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(linetype = 1, shape = NA)))

tableau_final <- tableau_final %>%
  mutate(
    # delta déjà calculé ci‑dessus = moyenne_FFQ - moyenne_Booklet
    pct_delta = delta / moyenne_Booklet * 100
  )


 ##GRAPH de différence par mesure--------------------------------------
sgsdata_complet <-read.xlsx((paste("Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata_IT.xlsx", sep="")))
sgsdata_nudges_complet  <- read.xlsx((paste("Données analysées - Article N°4- Nudge/Fichiers_nettoyés/Fichier_traité/sgsdata_nudges.xlsx", sep="")))
sgsdata_complet <- sgsdata_complet %>%mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"),2,1))
sgsdata_complet<- sgsdata_complet %>% arrange(Identifiant) %>%group_by(Identifiant) %>% fill(UC_TI, .direction = "downup") %>%ungroup()
sgsdata_nudges_complet<- sgsdata_nudges_complet %>%  arrange(Identifiant) %>%group_by(Identifiant) %>% fill(UC_TI, .direction = "downup") %>%ungroup()
fill_zero <- function(df) { df %>%mutate(across(everything(),~ ifelse(is.na(.) | . == "", 0, .)))}
sgsdata_complet            <- fill_zero(sgsdata_complet)
sgsdata_nudges_complet      <- fill_zero(sgsdata_nudges_complet)



### SGSDATA Complet carnet -----------------------http://127.0.0.1:27837/graphics/8d000470-044b-44ee-9a45-a9d4d9f89e41.png
sgsdata_Booklet_IT <- sgsdata_complet %>%
  select(Identifiant, UC_TI ,Mesure,Campagne, Periode, groupe, ends_with("_CARNET_Kcal"), SOMME_CARNET_HORS_BOISSON, SOMME_FFQ_HORS_BOISSON)
#Verif groupe
sgsdata_Booklet_IT$FV_CARNET_Kcal <- sgsdata_Booklet_IT$FRUITS_CARNET_Kcal + sgsdata_Booklet_IT$FRUITS_SECS_CARNET_Kcal  + sgsdata_Booklet_IT$NOIX_CARNET_Kcal + sgsdata_Booklet_IT$LEGUMES_CARNET_Kcal 
sgsdata_Booklet_IT$FEC_CARNET_Kcal <- sgsdata_Booklet_IT$FEC_NON_RAF_CARNET_Kcal + sgsdata_Booklet_IT$FEC_RAF_CARNET_Kcal
sgsdata_Booklet_IT$PDTS_LAITIERS_CARNET_Kcal <- sgsdata_Booklet_IT$LAIT_CARNET_Kcal + sgsdata_Booklet_IT$LAITAGES_CARNET_Kcal + sgsdata_Booklet_IT$FROMAGES_CARNET_Kcal
sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Kcal <- sgsdata_Booklet_IT$POULET_CARNET_Kcal + sgsdata_Booklet_IT$OEUFS_CARNET_Kcal
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Kcal <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_CARNET_Kcal  + sgsdata_Booklet_IT$JAMBON_BLANC_CARNET_Kcal 
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_CARNET_Kcal <- sgsdata_Booklet_IT$VIANDE_ROUGE_CARNET_Kcal+ sgsdata_Booklet_IT$PORC_CARNET_Kcal
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_CARNET_Kcal <- sgsdata_Booklet_IT$SNACKS_AUTRES_CARNET_Kcal +  sgsdata_Booklet_IT$CEREALES_PD_CARNET_Kcal  + sgsdata_Booklet_IT$PDTS_SUCRES_CARNET_Kcal 
sgsdata_Booklet_IT$SSB_CARNET_Kcal <-  sgsdata_Booklet_IT$SODAS_SUCRES_CARNET_Kcal + sgsdata_Booklet_IT$SODAS_LIGHT_CARNET_Kcal +sgsdata_Booklet_IT$FRUITS_JUS_CARNET_Kcal 

sgsdata_Booklet_IT$SOMME_HB_CARNET_Kcal <-sgsdata_Booklet_IT$FV_CARNET_Kcal + sgsdata_Booklet_IT$FEC_CARNET_Kcal +
  sgsdata_Booklet_IT$PDTS_LAITIERS_CARNET_Kcal + sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Kcal +sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Kcal +
  sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_CARNET_Kcal + sgsdata_Booklet_IT$DESSERTS_LACTES_CARNET_Kcal +
  sgsdata_Booklet_IT$QUICHES_PIZZAS_TARTES_SALEES_CARNET_Kcal + sgsdata_Booklet_IT$MGA_CARNET_Kcal + sgsdata_Booklet_IT$MGV_CARNET_Kcal +
  sgsdata_Booklet_IT$POISSONS_CARNET_Kcal + sgsdata_Booklet_IT$LEG_SECS_CARNET_Kcal + sgsdata_Booklet_IT$PLATS_PREP_CARNES_CARNET_Kcal + sgsdata_Booklet_IT$PLATS_PREP_VEGETARIENS_CARNET_Kcal + sgsdata_Booklet_IT$SAUCES_CARNET_Kcal
sgsdata_Booklet_IT$VIANDES_CARNET_Kcal  <- sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Kcal + sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Kcal + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_CARNET_Kcal 

sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  rename_with(
    ~ str_remove_all(.x, "_CARNET"),  .cols = everything())  %>%
  filter(if_any(ends_with("_Kcal"), ~ . != 0))

###SGSDATA COMPLET FFQ -----------------------------------
sgsdata_complet_FFQ <- sgsdata_complet%>%
  select(Identifiant, UC_TI ,Mesure,Campagne, Periode, groupe, ends_with("_FFQ_Kcal"), SOMME_FFQ_HORS_BOISSON)
sgsdata_complet_FFQ$FV_FFQ_Kcal <- sgsdata_complet_FFQ$FRUITS_FFQ_Kcal + sgsdata_complet_FFQ$FRUITS_SECS_FFQ_Kcal  + sgsdata_complet_FFQ$NOIX_FFQ_Kcal + sgsdata_complet_FFQ$LEGUMES_FFQ_Kcal 
sgsdata_complet_FFQ$FEC_FFQ_Kcal <- sgsdata_complet_FFQ$FEC_NON_RAF_FFQ_Kcal + sgsdata_complet_FFQ$FEC_RAF_FFQ_Kcal
sgsdata_complet_FFQ$PDTS_LAITIERS_FFQ_Kcal <- sgsdata_complet_FFQ$LAIT_FFQ_Kcal + sgsdata_complet_FFQ$LAITAGES_FFQ_Kcal + sgsdata_complet_FFQ$FROMAGES_FFQ_Kcal
sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Kcal <- sgsdata_complet_FFQ$POULET_FFQ_Kcal + sgsdata_complet_FFQ$OEUFS_FFQ_Kcal
sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Kcal <- sgsdata_complet_FFQ$CHARCUTERIE_HORS_JB_FFQ_Kcal  + sgsdata_complet_FFQ$JAMBON_BLANC_FFQ_Kcal 
sgsdata_complet_FFQ$VIANDE_ROUGE_PORC_FFQ_Kcal <- sgsdata_complet_FFQ$VIANDE_ROUGE_FFQ_Kcal+ sgsdata_complet_FFQ$PORC_FFQ_Kcal
sgsdata_complet_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Kcal <- sgsdata_complet_FFQ$SNACKS_AUTRES_FFQ_Kcal +  sgsdata_complet_FFQ$CEREALES_PD_FFQ_Kcal  + sgsdata_complet_FFQ$PDTS_SUCRES_FFQ_Kcal
sgsdata_complet_FFQ$SSB_FFQ_Kcal <-  sgsdata_complet_FFQ$SODAS_SUCRES_FFQ_Kcal + sgsdata_complet_FFQ$SODAS_LIGHT_FFQ_Kcal +sgsdata_complet_FFQ$FRUITS_JUS_FFQ_Kcal 
sgsdata_complet_FFQ$SOMME_HB_FFQ_Kcal <-sgsdata_complet_FFQ$FV_FFQ_Kcal + sgsdata_complet_FFQ$FEC_FFQ_Kcal +
  sgsdata_complet_FFQ$PDTS_LAITIERS_FFQ_Kcal + sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Kcal +sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Kcal +
  sgsdata_complet_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Kcal + sgsdata_complet_FFQ$DESSERTS_LACTES_FFQ_Kcal +
  sgsdata_complet_FFQ$QUICHES_PIZZAS_TARTES_SALEES_FFQ_Kcal + sgsdata_complet_FFQ$MGA_FFQ_Kcal + sgsdata_complet_FFQ$MGV_FFQ_Kcal +
  sgsdata_complet_FFQ$POISSONS_FFQ_Kcal + sgsdata_complet_FFQ$LEG_SECS_FFQ_Kcal + sgsdata_complet_FFQ$PLATS_PREP_CARNES_FFQ_Kcal + sgsdata_complet_FFQ$PLATS_PREP_VEGETARIENS_FFQ_Kcal + sgsdata_complet_FFQ$SAUCES_FFQ_Kcal

sgsdata_complet_FFQ$VIANDES_FFQ_Kcal  <- sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Kcal + sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Kcal + sgsdata_complet_FFQ$VIANDE_ROUGE_PORC_FFQ_Kcal 


sgsdata_complet_FFQ <- sgsdata_complet_FFQ %>%
  rename_with(
    ~ str_remove_all(.x, "_FFQ"),
    .cols = everything()
  )  %>%
  # on ne garde que les lignes où au moins une colonne _Kcal est non-zéro
  filter(
    if_any(ends_with("_Kcal"), ~ . != 0))

### SGSDATA NUDGES CARNET -------------------------------
sgsdata_nudges_Carnet <- sgsdata_nudges_complet %>%
  select(Identifiant,UC_TI , Mesure,Periode, groupe, ends_with("_Kcal"), SOMME_CARNET_POIDS, SOMME_CARNET_HORS_BOISSON.x ) %>%
  rename_with(
    ~ str_replace(.x, "_Kcal$", "_CARNET_Kcal"))#,
  #  ends_with("_CARNET")) %>%
  ## on ne garde que les lignes où au moins une colonne _Kcal est non-zéro
  #filter(
  #  if_any(ends_with("_Kcal"), ~ . != 0))
    
sgsdata_nudges_Carnet$FV_CARNET_Kcal <- sgsdata_nudges_Carnet$FRUITS_CARNET_Kcal + sgsdata_nudges_Carnet$FRUITS_SECS_CARNET_Kcal  + sgsdata_nudges_Carnet$NOIX_CARNET_Kcal + sgsdata_nudges_Carnet$LEGUMES_CARNET_Kcal 
sgsdata_nudges_Carnet$FEC_CARNET_Kcal <- sgsdata_nudges_Carnet$FEC_NON_RAF_CARNET_Kcal + sgsdata_nudges_Carnet$FEC_RAF_CARNET_Kcal
sgsdata_nudges_Carnet$PDTS_LAITIERS_CARNET_Kcal <- sgsdata_nudges_Carnet$LAIT_CARNET_Kcal + sgsdata_nudges_Carnet$LAITAGES_CARNET_Kcal + sgsdata_nudges_Carnet$FROMAGES_CARNET_Kcal
sgsdata_nudges_Carnet$POULET_OEUFS_CARNET_Kcal <- sgsdata_nudges_Carnet$POULET_CARNET_Kcal + sgsdata_nudges_Carnet$OEUFS_CARNET_Kcal
sgsdata_nudges_Carnet$AUTRE_PDTS_ANIMAUX_CARNET_Kcal <- sgsdata_nudges_Carnet$CHARCUTERIE_HORS_JB_CARNET_Kcal  + sgsdata_nudges_Carnet$JAMBON_BLANC_CARNET_Kcal 
sgsdata_nudges_Carnet$VIANDE_ROUGE_PORC_CARNET_Kcal <- sgsdata_nudges_Carnet$VIANDE_ROUGE_CARNET_Kcal+ sgsdata_nudges_Carnet$PORC_CARNET_Kcal
sgsdata_nudges_Carnet$PDTS_DISCRETIONNAIRES_CARNET_Kcal <- sgsdata_nudges_Carnet$SNACKS_AUTRES_CARNET_Kcal +  sgsdata_nudges_Carnet$CEREALES_PD_CARNET_Kcal  + sgsdata_nudges_Carnet$PDTS_SUCRES_CARNET_Kcal 
sgsdata_nudges_Carnet$SSB_CARNET_Kcal <-  sgsdata_nudges_Carnet$SODAS_SUCRES_CARNET_Kcal + sgsdata_nudges_Carnet$SODAS_LIGHT_CARNET_Kcal +sgsdata_nudges_Carnet$FRUITS_JUS_CARNET_Kcal 

sgsdata_nudges_Carnet$SOMME_HB_CARNET_Kcal <-sgsdata_nudges_Carnet$FV_CARNET_Kcal + sgsdata_nudges_Carnet$FEC_CARNET_Kcal +
  sgsdata_nudges_Carnet$PDTS_LAITIERS_CARNET_Kcal + sgsdata_nudges_Carnet$POULET_OEUFS_CARNET_Kcal +sgsdata_nudges_Carnet$AUTRE_PDTS_ANIMAUX_CARNET_Kcal +
  sgsdata_nudges_Carnet$PDTS_DISCRETIONNAIRES_CARNET_Kcal + sgsdata_nudges_Carnet$DESSERTS_LACTES_CARNET_Kcal +
  sgsdata_nudges_Carnet$QUICHES_PIZZAS_TARTES_SALEES_CARNET_Kcal + sgsdata_nudges_Carnet$MGA_CARNET_Kcal + sgsdata_nudges_Carnet$MGV_CARNET_Kcal +
  sgsdata_nudges_Carnet$POISSONS_CARNET_Kcal + sgsdata_nudges_Carnet$LEG_SECS_CARNET_Kcal + sgsdata_nudges_Carnet$PLATS_PREP_CARNES_CARNET_Kcal + sgsdata_nudges_Carnet$PLATS_PREP_VEGETARIENS_CARNET_Kcal + sgsdata_nudges_Carnet$SAUCES_CARNET_Kcal

sgsdata_nudges_Carnet$VIANDES_CARNET_Kcal  <- sgsdata_nudges_Carnet$AUTRE_PDTS_ANIMAUX_CARNET_Kcal + sgsdata_nudges_Carnet$POULET_OEUFS_CARNET_Kcal + sgsdata_nudges_Carnet$VIANDE_ROUGE_PORC_CARNET_Kcal 


sgsdata_nudges_Carnet <- sgsdata_nudges_Carnet %>%
  rename_with(
    ~ str_remove_all(.x, "_CARNET"),
    .cols = everything()
  ) %>%
  # on ne garde que les lignes où au moins une colonne _Kcal est non-zéro
  filter(
    if_any(ends_with("_Kcal"), ~ . != 0))

### SGSDATA NUDGES FFQ -------------------------------
sgsdata_nudges_FFQ <- sgsdata_nudges_complet  %>%
  select(Identifiant, UC_TI ,Mesure, Periode, groupe, ends_with("_Kcal"), SOMME_FFQ_HORS_BOISSON )%>%
  rename_with(
    ~ str_replace(.x, "_Kcal$", "_FFQ_Kcal"))



sgsdata_nudges_FFQ$FV_FFQ_Kcal <- sgsdata_nudges_FFQ$FRUITS_FFQ_Kcal + sgsdata_nudges_FFQ$FRUITS_SECS_FFQ_Kcal  + sgsdata_nudges_FFQ$NOIX_FFQ_Kcal + sgsdata_nudges_FFQ$LEGUMES_FFQ_Kcal 
sgsdata_nudges_FFQ$FEC_FFQ_Kcal <- sgsdata_nudges_FFQ$FEC_NON_RAF_FFQ_Kcal + sgsdata_nudges_FFQ$FEC_RAF_FFQ_Kcal
sgsdata_nudges_FFQ$PDTS_LAITIERS_FFQ_Kcal <- sgsdata_nudges_FFQ$LAIT_FFQ_Kcal + sgsdata_nudges_FFQ$LAITAGES_FFQ_Kcal + sgsdata_nudges_FFQ$FROMAGES_FFQ_Kcal
sgsdata_nudges_FFQ$POULET_OEUFS_FFQ_Kcal <- sgsdata_nudges_FFQ$POULET_FFQ_Kcal + sgsdata_nudges_FFQ$OEUFS_FFQ_Kcal
sgsdata_nudges_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Kcal <- sgsdata_nudges_FFQ$CHARCUTERIE_HORS_JB_FFQ_Kcal  + sgsdata_nudges_FFQ$JAMBON_BLANC_FFQ_Kcal 
sgsdata_nudges_FFQ$VIANDE_ROUGE_PORC_FFQ_Kcal <- sgsdata_nudges_FFQ$VIANDE_ROUGE_FFQ_Kcal+ sgsdata_nudges_FFQ$PORC_FFQ_Kcal
sgsdata_nudges_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Kcal <- sgsdata_nudges_FFQ$SNACKS_AUTRES_FFQ_Kcal +  sgsdata_nudges_FFQ$CEREALES_PD_FFQ_Kcal  + sgsdata_nudges_FFQ$PDTS_SUCRES_FFQ_Kcal 
sgsdata_nudges_FFQ$SSB_FFQ_Kcal <-  sgsdata_nudges_FFQ$SODAS_SUCRES_FFQ_Kcal + sgsdata_nudges_FFQ$SODAS_LIGHT_FFQ_Kcal +sgsdata_nudges_FFQ$FRUITS_JUS_FFQ_Kcal 

sgsdata_nudges_FFQ$SOMME_HB_FFQ_Kcal <-sgsdata_nudges_FFQ$FV_FFQ_Kcal + sgsdata_nudges_FFQ$FEC_FFQ_Kcal +
  sgsdata_nudges_FFQ$PDTS_LAITIERS_FFQ_Kcal + sgsdata_nudges_FFQ$POULET_OEUFS_FFQ_Kcal +sgsdata_nudges_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Kcal +
  sgsdata_nudges_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Kcal + sgsdata_nudges_FFQ$DESSERTS_LACTES_FFQ_Kcal +
  sgsdata_nudges_FFQ$QUICHES_PIZZAS_TARTES_SALEES_FFQ_Kcal + sgsdata_nudges_FFQ$MGA_FFQ_Kcal + sgsdata_nudges_FFQ$MGV_FFQ_Kcal +
  sgsdata_nudges_FFQ$POISSONS_FFQ_Kcal + sgsdata_nudges_FFQ$LEG_SECS_FFQ_Kcal + sgsdata_nudges_FFQ$PLATS_PREP_CARNES_FFQ_Kcal + sgsdata_nudges_FFQ$PLATS_PREP_VEGETARIENS_FFQ_Kcal + sgsdata_nudges_FFQ$SAUCES_FFQ_Kcal

sgsdata_nudges_FFQ$VIANDES_FFQ_Kcal <- sgsdata_nudges_FFQ$POULET_OEUFS_FFQ_Kcal  + sgsdata_nudges_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Kcal  + sgsdata_nudges_FFQ$VIANDE_ROUGE_PORC_FFQ_Kcal



sgsdata_nudges_FFQ <- sgsdata_nudges_FFQ %>%
  rename_with(
    ~ str_remove_all(.x, "_FFQ"),
    .cols = everything()
  ) %>%
  # on ne garde que les lignes où au moins une colonne _Kcal est non-zéro
  filter(
    if_any(ends_with("_Kcal"), ~ . != 0))

#cAMPAGNE 1
sgsdata_Booklet_IT11  <- sgsdata_Booklet_IT %>% filter( Campagne == 1 , Periode ==0, groupe==0, Mesure== "Carnet") 
sgsdata_Booklet_IT21 <-sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode ==1, groupe==0, Mesure== "Carnet")  
sgsdata_Booklet_IT1 <- left_join(sgsdata_Booklet_IT11, sgsdata_Booklet_IT21, by="Identifiant")
sgsdata_Booklet_IT1 <- sgsdata_Booklet_IT1 %>%
  select(matches("Kcal|Identifiant")) %>%
  # on ne garde que les lignes où au moins une colonne _Kcal est non-zéro
  filter(
    if_any(ends_with("_Kcal"), ~ . != 0))

#FFQ CAMP 1 
sgsdata_FFQ_IT11 <- sgsdata_complet_FFQ %>% filter(Campagne == 1, Periode ==0, groupe==0, Mesure != "Carnet") 
sgsdata_FFQ_IT21 <- sgsdata_complet_FFQ  %>% filter(Campagne == 1, Periode ==1 ,groupe==0, Mesure != "Carnet")
sgsdata_FFQ_IT1 <- left_join(sgsdata_FFQ_IT11, sgsdata_FFQ_IT21, by="Identifiant")
sgsdata_FFQ_IT1<- sgsdata_FFQ_IT1 %>%
  select(matches("Kcal|Identifiant"))

sgsdata_FFQ_IT1 <- sgsdata_FFQ_IT1 %>%
  semi_join(sgsdata_Booklet_IT1 , by ="Identifiant")

sgsdata_Booklet_IT1  <-sgsdata_Booklet_IT1 %>%
  semi_join(sgsdata_FFQ_IT1 , by ="Identifiant")

#CAMP 2 
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2 , Periode ==0, groupe==0, Mesure== "Carnet")  
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode ==1, groupe==0, Mesure== "Carnet")   
sgsdata_Booklet_IT2 <- left_join(sgsdata_Booklet_IT12, sgsdata_Booklet_IT22, by="Identifiant")
sgsdata_Booklet_IT2 <- sgsdata_Booklet_IT2 %>%
  select(matches("Kcal|Identifiant"))

sgsdata_FFQ_IT12<- sgsdata_complet_FFQ %>% filter(Campagne == 2, Periode ==0, groupe==0, Mesure != "Carnet")  
sgsdata_FFQ_IT22 <-sgsdata_complet_FFQ %>% filter(Campagne == 2, Periode ==1 ,groupe==0,  Mesure != "Carnet") 
sgsdata_FFQ_IT2 <- left_join(sgsdata_FFQ_IT12, sgsdata_FFQ_IT22, by="Identifiant")
sgsdata_FFQ_IT2 <- sgsdata_FFQ_IT2 %>%
  select(matches("Kcal|Identifiant"))

sgsdata_FFQ_IT2 <- sgsdata_FFQ_IT2 %>%
  semi_join(sgsdata_Booklet_IT2 , by ="Identifiant")

sgsdata_Booklet_IT2 <-sgsdata_Booklet_IT2 %>%
  semi_join(sgsdata_FFQ_IT2 , by ="Identifiant")

sgsdata_Booklet_nudges1 <- sgsdata_nudges_Carnet %>% filter(Periode ==0,str_detect(Identifiant, "Epimut", ),  Mesure== "Carnet")
sgsdata_Booklet_nudges2 <-sgsdata_nudges_Carnet %>% filter(Periode ==1,str_detect(Identifiant, "Epimut"), Mesure== "Carnet")  
sgsdata_Booklet_nudges_vf <- left_join(sgsdata_Booklet_nudges1, sgsdata_Booklet_nudges2, by="Identifiant")
sgsdata_Booklet_nudges_vf <- sgsdata_Booklet_nudges_vf %>%
  select(matches("Kcal|Identifiant"))

sgsdata_FFQ_nudges1 <- sgsdata_nudges_FFQ %>% filter( Periode ==0,str_detect(Identifiant, "Epimut"), Mesure != "Carnet")
sgsdata_FFQ_nudges2 <-sgsdata_nudges_FFQ %>% filter(Periode ==1,str_detect(Identifiant, "Epimut") , Mesure != "Carnet") 
sgsdata_FFQ_nudges_vf <- left_join(sgsdata_FFQ_nudges1, sgsdata_FFQ_nudges2, by="Identifiant")
sgsdata_FFQ_nudges_vf <- sgsdata_FFQ_nudges_vf %>%
  select(matches("Kcal|Identifiant"))

sgsdata_FFQ_nudges_vf <- sgsdata_FFQ_nudges_vf %>%
  semi_join(sgsdata_Booklet_nudges_vf , by ="Identifiant")

sgsdata_Booklet_nudges_vf <- sgsdata_Booklet_nudges_vf %>%
  semi_join(sgsdata_FFQ_nudges_vf , by ="Identifiant")



sgsdata_FFQ_nudges_vf <-sgsdata_FFQ_nudges_vf  %>%
  semi_join(sgsdata_Booklet_nudges_vf , by ="Identifiant")
sgsdata_FFQ_IT1 <-sgsdata_FFQ_IT1 %>%
  semi_join(sgsdata_Booklet_IT1, by ="Identifiant")
sgsdata_FFQ_IT2 <-sgsdata_FFQ_IT2 %>%
  semi_join(sgsdata_Booklet_IT2, by ="Identifiant")


process_data <- function(df) {
  # lookup pour renommer les colonnes Kcal
  traductions <- c(
    VIANDES_Kcal = "Meats",
    CEREALES_PD_Kcal                          = "Breakfast cereals",
    CHARCUTERIE_HORS_JB_Kcal                   = "Cold cuts excluding white ham",
    DESSERTS_LACTES_Kcal                       = "Dairy Desserts",
    FEC_NON_RAF_Kcal                           = "Unrefined Starches",
    FEC_RAF_Kcal                               = "Refined Starches",
    FROMAGES_Kcal                              = "Cheeses",
    FRUITS_Kcal                                = "Fruits",
    FRUITS_SECS_Kcal                           = "Dried Fruits",
    JAMBON_BLANC_Kcal                          = "White Ham",
    LAITAGES_Kcal                              = "Dairy Products",
    LEGUMES_Kcal                               = "Vegetables",
    LEG_SECS_Kcal                              = "Legumes",
    MGA_Kcal                                   = "Animal Fats",
    MGV_Kcal                                   = "Vegetable Fats",
    NOIX_Kcal                                  = "Nuts",
    OEUFS_Kcal                                 = "Eggs",
    PDTS_SUCRES_Kcal                           = "Sweet products",
    PLATS_PREP_CARNES_Kcal                     = "Meat Based Prepared Dishes",
    PLATS_PREP_VEGETARIENS_Kcal                = "Vegetarian Prepared Dishes",
    POISSONS_Kcal                              = "Fish",
    PORC_Kcal                                  = "Pork",
    POULET_Kcal                                = "Chicken",
    QUICHES_PIZZAS_TARTES_SALEES_Kcal          = "Quiches, Pizzas & Savoury Pies",
    SAUCES_Kcal                                = "Sauces",
    SNACKS_AUTRES_Kcal                         = "Other Snacks",
    VIANDE_ROUGE_Kcal                          = "Red Meat",
    ALCOOL_Kcal                                = "Alcohol",
    EAU_Kcal                                   = "Water",
    FRUITS_JUS_Kcal                            = "Fruit Juices",
    LAIT_Kcal                                  = "Milk",
    SODAS_LIGHT_Kcal                           = "Diet Sodas",
    SODAS_SUCRES_Kcal                          = "Sugary Sodas",
    FV_Kcal  = "Fruits and vegetables",
    FEC_Kcal  = "Starchy food",
    PDTS_LAITIERS_Kcal = "Dairy products",
    POULET_OEUFS_Kcal = "Eggs / chicken",
    AUTRE_PDTS_ANIMAUX_Kcal ="Cold cuts",
    PLATS_PREP_Kcal ="Prepared dishes",
    VIANDE_ROUGE_PORC_Kcal ="Red meat/Pork",
    MG_Kcal = "Added fats",
    PDTS_DISCRETIONNAIRES_Kcal  = "Discretionnary food",
    SSB_Kcal  ="Sugary sweet beverages",
    SOMME_Kcal_HB = "Total without beverages",
    SOMME_Kcal_HB = "Total without beverages",
    SOMME_Kcal  = "Sum",
    SOMME_Pois  = "Sum"
    
    
  )
  
  df %>%
    # 1) ne conserver que Kcal + Identifiant
    select(matches("Kcal|Identifiant|KCAL")) %>%
    # 2) moyenne des colonnes Kcal
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    # 3) repérer les bases .x/.y puis calculer les diffs
    { 
      bases_x <- sub("\\.x$", "", grep("\\.x$", names(.), value = TRUE))
      bases_y <- sub("\\.y$", "", grep("\\.y$", names(.), value = TRUE))
      bases   <- intersect(bases_x, bases_y)
      for(b in bases) {
        .[[paste0(b, "_diff")]] <- .[[paste0(b, ".y")]] - .[[paste0(b, ".x")]]
      }
      .
    } %>%
    # 4) Supprimer les colonnes .y et tout le reste indésirable
    select(  
    -ends_with(".y"),
     -starts_with("SOMME_POURCENT_Kcal"),
  # -starts_with("total_Kcal"),
  # -starts_with("SOMME_HB_Kcal"),
  #-starts_with("SOMME_Kcal_HB"),
  # -starts_with("SOMME_Kcal"),
  #-starts_with("SOMME_Kcal"),
  #  -starts_with("SOMME_FFQ_KCAL"),
  #  -starts_with("SOMME_CARNET_KCAL"),
    -starts_with("PROP_Kcal_EPIC"),
    -starts_with("Kcal_EPIC"),
    #-starts_with("SOMME_FFQ"),
    #-starts_with("SOMME_CARNET"),
     #-starts_with("SSB_Kcal"),
     #-starts_with("FEC_Kcal"),
     #-starts_with("PDTS_DISCRETIONNAIRES_Kcal"),
     #-starts_with("FV_Kcal"),
     -starts_with("MG_Kcal"),
    -starts_with("AUTRE_PDTS_ANIMAUX_Kcal"),
    # -starts_with("PDTS_LAITIERS_Kcal"),
     -starts_with("PLATS_PREP_Kcal"),
     -starts_with("POULET_OEUFS_Kcal"),
     -starts_with("VIANDE_ROUGE_PORC_Kcal"),
    -starts_with("POISSONS_Kcal"),
      -starts_with("ALCOOL_Kcal"),
     -starts_with("EPICES_CONDIMENTS_Kcal"),
     -starts_with("CAFE_THE_Kcal"),
     -starts_with("EAU_Kcal"),
     -starts_with("LAIT_Kcal"),
  -starts_with("FRUITS_JUS_Kcal"),
  -starts_with("SODAS_LIGHT_Kcal"),
  -starts_with("SODAS_SUCRES_Kcal"),
  #-starts_with("VIANDES_Kcal"),
  -starts_with("SAUCES_Kcal"), 
  -starts_with("CHARCUTERIE_HORS_JB_Kcal"),
   -starts_with("PLATS_PREP_CARNES_Kcal"),
  -starts_with("PLATS_PREP_VEGETARIENS_Kcal"),
  -starts_with("QUICHES_PIZZAS_TARTES_SALEES_Kcal"),
  -starts_with("PORC_Kcal"),
  -starts_with("MGA_Kcal"),
  -starts_with("MGV_Kcal"),
  -starts_with("OEUFS_Kcal"),
  -starts_with("JAMBON_BLANC_Kcal"),
  -starts_with("CEREALES_PD_Kcal"),
  -starts_with("NOIX_Kcal"),
  -starts_with("SNACKS_AUTRES_Kcal"),
  -starts_with("LAITAGES_Kcal"),
  -starts_with("FROMAGES_Kcal"),
  -starts_with("FRUITS_Kcal"),
 -starts_with("DESSERTS_LACTES_Kcal"),
-starts_with("FEC_NON_RAF_Kcal"), 
-starts_with("PDTS_SUCRES_Kcal"),
-starts_with("LEGUMES_Kcal"),
  -starts_with("VIANDE_ROUGE_Kcal"),
  -starts_with("FEC_RAF_Kcal"),
  -starts_with("POULET_Kcal"),
 -starts_with("LEG_SECS_Kcal"),
  -starts_with("FRUITS_SECS_Kcal")
      
    ) %>%
    # 5) Renommer selon la lookup, en traduisant si le début du nom correspond
    rename_with(
      .cols = everything(),
      .fn = function(x) {
        vapply(x, function(col) {
          key <- names(traductions)[vapply(names(traductions), function(k) startsWith(col, k), logical(1))]
          if (length(key)) {
            # remplacer le préfixe key par sa traduction
            sub(paste0("^", key), traductions[key], col)
          } else {
            col
          }
        }, character(1))
      }
    ) 
}





sgsdata_Booklet_IT1     <- process_data(sgsdata_Booklet_IT1)
sgsdata_FFQ_IT1         <- process_data(sgsdata_FFQ_IT1)
sgsdata_Booklet_IT2     <- process_data(sgsdata_Booklet_IT2)
sgsdata_FFQ_IT2         <- process_data(sgsdata_FFQ_IT2)
sgsdata_Booklet_nudges_vf <- process_data(sgsdata_Booklet_nudges_vf)
sgsdata_FFQ_nudges_vf <- process_data(sgsdata_FFQ_nudges_vf)


make_df_plot <- function(df_summary) {
  library(dplyr)
  library(stringr)
  library(tibble)
  
  # 1) On repère les racines de colonnes en .x
  bases <- names(df_summary) %>%
    str_subset("\\.x$") %>%
    str_remove("\\.x$")
  
  # 2) On construit la trame brute
  df_plot <- tibble(
    base_raw   = bases,
    x          = unlist(df_summary[paste0(bases, ".x")],    use.names = FALSE),
    diff       = unlist(df_summary[paste0(bases, "_diff")], use.names = FALSE)
  ) %>%
    mutate(
      base_clean  = base_raw %>%
        str_remove("_Kcal$") %>%
        str_remove("Kcal$") %>%
        str_replace_all("_", " ") %>%
        str_to_lower() %>%
        str_to_sentence(),
      signe       = if_else(diff >= 0, "Rise", "Decrease"),
      diff_label  = sprintf("%+.2f", diff),
      x_label_pos = max(x + diff, na.rm = TRUE) * 1.02
    )
  
  # 3) On ordonne le facteur base selon l’ordre décroissant de |diff|
  if (nrow(df_plot) > 1) {
    ordered_levels <- df_plot %>%
      arrange(desc(abs(diff))) %>%
      pull(base_clean) %>%
      unique()   # <-- on retire d’éventuels doublons
  } else {
    ordered_levels <- df_plot$base_clean
  }
  
  # 4) On crée la variable factorielle sans niveau dupliqué
  df_plot %>%
    mutate(
      base = factor(base_clean, levels = ordered_levels)
    ) %>%
    select(base, x, diff, signe, diff_label, x_label_pos)
}


sgsdata_Booklet_IT1     <- make_df_plot(sgsdata_Booklet_IT1)
sgsdata_FFQ_IT1         <- make_df_plot(sgsdata_FFQ_IT1)
sgsdata_Booklet_IT2     <- make_df_plot(sgsdata_Booklet_IT2)
sgsdata_FFQ_IT2         <- make_df_plot(sgsdata_FFQ_IT2)
sgsdata_Booklet_nudges_vf <- make_df_plot(sgsdata_Booklet_nudges_vf)
sgsdata_FFQ_nudges_vf <- make_df_plot(sgsdata_FFQ_nudges_vf)

# 1) On regroupe vos 6 data.frames df_plot dans une liste nommée
list_df_plot <- list(
  Booklet_Winter_23    = sgsdata_Booklet_IT1,
  FFQ_Winter_23        = sgsdata_FFQ_IT1,
  Booklet_Winter_24    = sgsdata_Booklet_IT2,
  FFQ_Winter_24        = sgsdata_FFQ_IT2,
  Booklet_Winter_22  = sgsdata_Booklet_nudges_vf,
  FFQ_22   = sgsdata_FFQ_nudges_vf
)



# on crée 3 petits tables avec la colonne campaign
df_booklet_22 <- sgsdata_Booklet_nudges_vf %>%  mutate(campaign = "Booklet Winter 22")
df_booklet_23 <- sgsdata_Booklet_IT1 %>%        mutate(campaign = "Booklet Winter 23")
df_booklet_24 <- sgsdata_Booklet_IT2 %>%        mutate(campaign = "Booklet Winter 24")

# on fusionne
df_booklet_all <- bind_rows(df_booklet_22,
                            df_booklet_23,
                            df_booklet_24)
df_ffq_22 <- sgsdata_FFQ_nudges_vf %>% mutate(campaign = "FFQ 22")
df_ffq_23 <- sgsdata_FFQ_IT1       %>% mutate(campaign = "FFQ Winter 23")
df_ffq_24 <- sgsdata_FFQ_IT2       %>% mutate(campaign = "FFQ Winter 24")

df_ffq_all <- bind_rows(df_ffq_22,
                        df_ffq_23,
                        df_ffq_24)
make_plot_multi <- function(
    df, titre, campaign_colors, offset = 5,
    arrow_len_mm = 4, arrow_angle = 12, arrow_size = 1.4,
    show_origin_point = TRUE,
    show_zero_line = FALSE
) {
  base_levels <- unique(df$base)
  
  df2 <- df %>%
    mutate(
      base     = factor(base, levels = base_levels),
      bar_end  = x + diff,
      label_y  = if_else(diff < 0,
                         pmax(bar_end - offset, -offset * 0.5),
                         bar_end + offset),
      hjust_lb = if_else(diff < 0, 1, 0),
      # ---- POURCENTAGE DANS LES ÉTIQUETTES ----
      diff_ratio = if_else(x != 0, diff / x, NA_real_),
      diff_label = case_when(
        is.na(diff_ratio) ~ "",
        diff_ratio >= 0   ~ paste0("+", scales::percent(diff_ratio, accuracy = 1, decimal.mark = ",")),
        TRUE              ~ scales::percent(diff_ratio, accuracy = 1, decimal.mark = ",")
      )
    )
  
  dodge <- position_dodge(width = 0.8)
  
  p <- ggplot(df2, aes(x = base, y = x, group = campaign, color = campaign)) +
    { if (show_origin_point)
      geom_point(position = dodge, size = 2.2, stroke = 0.7) else NULL } +
    geom_segment(
      aes(y = x, yend = bar_end),
      position = dodge,
      linewidth = arrow_size, lineend = "round",
      arrow = arrow(ends = "last", type = "closed",
                    angle = arrow_angle, length = unit(arrow_len_mm, "mm"))
    ) +
    geom_segment(aes(y = bar_end, yend = label_y), position = dodge, linewidth = 0.6) +
    geom_label(
      aes(y = label_y, label = diff_label, hjust = hjust_lb),
      position      = dodge,
      fill          = "white",
      label.padding = unit(0.1, "lines"),
      label.size    = 0.25,
      size          = 3,
      fontface      = "bold",
      show.legend   = FALSE,
      color         = "black"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 1, decimal.mark = ",", big.mark = " "),
      expand = expansion(add = c(offset * 1.5, 0))
    ) +
    scale_color_manual("Campaign", values = campaign_colors) +
    labs(title = titre, x = NULL, y = "Consumption [kcal/d/CU]") +
    theme_minimal(base_size = 9) +
    theme(
      plot.title         = element_text(face = "bold", hjust = 0),
      axis.text.y        = element_text(face = "bold", size = 10, margin = margin(r = 20)),
      axis.text.x        = element_text(face = "bold", size = 10),
      axis.title.y       = element_text(face = "bold", size = 10),
      legend.title       = element_text(face = "bold", size = 10),
      legend.text        = element_text(face = "bold", size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.ticks.y       = element_blank(),
      legend.position    = "bottom",
      plot.margin        = margin(5, 20, 5, 20)
    )
  
  if (show_zero_line) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1)
  }
  p
}

# Exemple d’utilisation
campaign_cols_booklet <- c(
  "Booklet Winter 22" = "#1b9e77",
  "Booklet Winter 23" = "#d95f02",
  "Booklet Winter 24" = "#7570b3"
)

p_booklet <- make_plot_multi(
  df               = df_booklet_all,
  titre            = "Booklet Comparisons",
  campaign_colors  = campaign_cols_booklet,
  offset           = 10
) +
  theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank(),
    plot.margin        = margin(t = 5, r = 50, b = 5, l = 5)
  )




print(p_booklet)

# 1) Palette pour les FFQ
campaign_cols_ffq <- c(
  "FFQ 22"            = "#1b9e77",
  "FFQ Winter 23"     = "#d95f02",
  "FFQ Winter 24"     = "#7570b3"
)

p_ffq <- make_plot_multi(
  df              = df_ffq_all,
  titre           = "FFQ Comparisons",
  campaign_colors = campaign_cols_ffq,
  offset          = 10
) +
  theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank(),
    plot.margin        = margin(t = 5, r = 50, b = 5, l = 5)
  )

# 3) Affichage
print(p_ffq)





# 1) On regroupe les 6 data-frames “df_plot” (avec base, x, diff, …) dans une liste:
tbls_plot <- list(
  Booklet_IT1       = sgsdata_Booklet_IT1,
  FFQ_IT1           = sgsdata_FFQ_IT1,
  Booklet_IT2       = sgsdata_Booklet_IT2,
  FFQ_IT2           = sgsdata_FFQ_IT2,
  Booklet_nudges_vf = sgsdata_Booklet_nudges_vf,
  FFQ_nudges_vf     = sgsdata_FFQ_nudges_vf
)

library(dplyr)
library(purrr)

# Supposons que `wide_pct` est obtenu comme tu l'avais :
wide_pct <- imap_dfr(
  tbls_plot,
  ~ .x %>%
    mutate(
      table_type = .y,
      pct        = diff / x * 100
    )
)






# TELECHARGEMENT -------------------------------------


###TELECHARGEMENT----------------------------------------------------
wb <- createWorkbook()

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df1")
writeData(wb, sheet = "df1", df1)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df2")
writeData(wb, sheet = "df2", df2 )

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df3")
writeData(wb, sheet = "df3", df3)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df4")
writeData(wb, sheet = "df4", df4)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df5")
writeData(wb, sheet = "df5", df5)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df6")
writeData(wb, sheet = "df6", df6)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df7")
writeData(wb, sheet = "df7", df7)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "df8")
writeData(wb, sheet = "df8", df7)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_nudges_T1")
writeData(wb, sheet = "tableau_final_nudges_T1", tableau_final_nudges_T1)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_nudges_T2")
writeData(wb, sheet = "tableau_final_nudges_T2", tableau_final_nudges_T2 )

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_nudges_T3")
writeData(wb, sheet = "tableau_final_nudges_T3",tableau_final_nudges_T3)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_IT11_T1")
writeData(wb, sheet = "tableau_final_IT11_T1", tableau_final_IT11_T1)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_IT11_T2")
writeData(wb, sheet = "tableau_final_IT11_T2", tableau_final_IT11_T2)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_IT11_T3")
writeData(wb, sheet = "tableau_final_IT11_T3", tableau_final_IT11_T3)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_IT12_T1")
writeData(wb, sheet = "tableau_final_IT12_T1", tableau_final_IT12_T1)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_IT12_T2")
writeData(wb, sheet = "tableau_final_IT12_T2", tableau_final_IT12_T2)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_IT12_T3")
writeData(wb, sheet = "tableau_final_IT12_T3", tableau_final_IT12_T3)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_CSGA_T1")
writeData(wb, sheet = "tableau_final_CSGA_T1", tableau_final_CSGA_T1)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_CSGA_T2")
writeData(wb, sheet = "tableau_final_CSGA_T2", tableau_final_CSGA_T2)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_CSGA_T3")
writeData(wb, sheet = "tableau_final_CSGA_T3", tableau_final_CSGA_T3)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_com_T1")
writeData(wb, sheet = "tableau_final_com_T1", tableau_final_com_T1)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_com_T2")
writeData(wb, sheet = "tableau_final_com_T2", tableau_final_com_T2)

# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final_com_T3")
writeData(wb, sheet = "tableau_final_com_T3", tableau_final_com_T3)


# Ajouter chaque dataframe dans un onglet différent
addWorksheet(wb, "tableau_final")
writeData(wb, sheet = "tableau_final", tableau_final)


addWorksheet(wb, "wide_pct")
writeData(wb, sheet = "wide_pct", wide_pct)



saveWorkbook(wb,(paste0("Données analyses - Article N°2 FFQvsCarnets/Comparaison_kcal.xlsx")))
