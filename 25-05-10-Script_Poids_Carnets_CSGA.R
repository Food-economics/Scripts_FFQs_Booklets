#Importation des packages -------------------
rm(list = ls())

#install.packages("modelsummary")

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
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(purrr)
library(ggplot2)
library(scales)

#"C:\Users\denieul-barbot\Dropbox\Thèse\Article_3\Données analyses - Article N°2 FFQvsCarnets\Fichiers nettoyés\Fichiers traités\sgsdata.xlsx"
#Chargement de l'environnement de travail -----------------
#researcher<-"denieul-barbot" 
#if (researcher == "denieul-barbot") { setwd <- paste0("C:/Users/denieul-barbot/Dropbox/Thèse/")
#} else { setwd(paste0("C:/Users/",researcher,"/Dropbox/Thèse/"))}

sgsdata_CSGA  <- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata.xlsx", sep="")))
sgsdata_TI <-read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata_IT.xlsx", sep="")))
sgsdata_nudges  <- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_4/Fichiers_nettoyés/Fichier_traité/sgsdata_nudges.xlsx", sep="")))

FFQ_NOV_23 <- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_1/Données analysées - Article N°1 chèques/Fichiers_bruts/23-11_FFQ.xlsx", sep="")))
sgsdata_TI <- sgsdata_TI 
#Indiquer la campagne
sgsdata_TI <- sgsdata_TI %>%mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"),2,1))

#Completer les données UC si manquant
sgsdata_TI <- sgsdata_TI %>%
  arrange(Identifiant) %>%group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>%ungroup()

#Completer les espaces vides par 0 
fill_zero <- function(df) { df %>%mutate(across(everything(),~ ifelse(is.na(.) | . == "", 0, .)))}


#TEST 
#sgsdata_CSGA <- sgsdata_CSGA %>%
#  filter(!str_detect(Identifiant, "azvpkt"))
#

sgsdata_TI             <- fill_zero(sgsdata_TI)
sgsdata_CSGA      <- fill_zero(sgsdata_CSGA)
sgsdata_nudges <- fill_zero(sgsdata_nudges)



sgsdata_FFQ_CSGA <- sgsdata_CSGA %>% filter(Mesure != "Carnet") 
sgsdata_Booklet_CSGA <- sgsdata_CSGA %>% filter(Mesure == "Carnet")


#jAMBON BLANC INCLU DANS charcuterie en CSGA / Retrait MGV / Plats prep vege / Sauces et desserts lactés sont sortis car non présents en nudges
sgsdata_FFQ_CSGA <- sgsdata_FFQ_CSGA %>%
  semi_join(sgsdata_Booklet_CSGA, by = "Identifiant") %>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))
sgsdata_FFQ_CSGA$FV_Poids <- sgsdata_FFQ_CSGA$FRUITS_Poids + sgsdata_FFQ_CSGA$FRUITS_SECS_Poids  + sgsdata_FFQ_CSGA$NOIX_Poids + sgsdata_FFQ_CSGA$LEGUMES_Poids 
sgsdata_FFQ_CSGA$FEC_Poids <- sgsdata_FFQ_CSGA$FEC_NON_RAF_Poids + sgsdata_FFQ_CSGA$FEC_RAF_Poids
sgsdata_FFQ_CSGA$PDTS_LAITIERS_Poids <- sgsdata_FFQ_CSGA$LAIT_Poids + sgsdata_FFQ_CSGA$LAITAGES_Poids + sgsdata_FFQ_CSGA$FROMAGES_Poids
sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Poids <- sgsdata_FFQ_CSGA$CHARCUTERIE_HORS_JB_Poids 
sgsdata_FFQ_CSGA$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_FFQ_CSGA$SNACKS_AUTRES_Poids +  sgsdata_FFQ_CSGA$CEREALES_PD_Poids + sgsdata_FFQ_CSGA$PDTS_SUCRES_Poids
sgsdata_FFQ_CSGA$POULET_OEUFS_Poids <- sgsdata_FFQ_CSGA$POULET_Poids + sgsdata_FFQ_CSGA$OEUFS_Poids
sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Poids <- sgsdata_FFQ_CSGA$VIANDE_ROUGE_Poids+ sgsdata_FFQ_CSGA$PORC_Poids

sgsdata_FFQ_CSGA$SSB_Poids <-  sgsdata_FFQ_CSGA$SODAS_SUCRES_Poids + sgsdata_FFQ_CSGA$SODAS_LIGHT_Poids +sgsdata_FFQ_CSGA$FRUITS_JUS_Poids 
sgsdata_FFQ_CSGA$VIANDES_Poids <- sgsdata_FFQ_CSGA$POULET_OEUFS_Poids   + sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Poids + sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Poids


sgsdata_FFQ_CSGA$SOMME_KCAL_Poids <- sgsdata_FFQ_CSGA$KCAL_TOTAL_Kcal
sgsdata_FFQ_CSGA$SOMME_HORS_BOISSON_Poids <-sgsdata_FFQ_CSGA$POIDS_HORS_BOISSON_Poids
sgsdata_FFQ_CSGA$SOMME_POIDS_Poids <- sgsdata_FFQ_CSGA$POIDS_TOTAL_Poids



sgsdata_Booklet_CSGA <- sgsdata_Booklet_CSGA %>%
  semi_join(sgsdata_FFQ_CSGA, by = "Identifiant") %>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))
sgsdata_Booklet_CSGA$FV_Poids <- sgsdata_Booklet_CSGA$FRUITS_Poids + sgsdata_Booklet_CSGA$FRUITS_SECS_Poids  + sgsdata_Booklet_CSGA$NOIX_Poids + sgsdata_Booklet_CSGA$LEGUMES_Poids 
sgsdata_Booklet_CSGA$FEC_Poids <- sgsdata_Booklet_CSGA$FEC_NON_RAF_Poids + sgsdata_Booklet_CSGA$FEC_RAF_Poids
sgsdata_Booklet_CSGA$PDTS_LAITIERS_Poids <- sgsdata_Booklet_CSGA$LAIT_Poids + sgsdata_Booklet_CSGA$LAITAGES_Poids + sgsdata_Booklet_CSGA$FROMAGES_Poids
sgsdata_Booklet_CSGA$POULET_OEUFS_Poids <- sgsdata_Booklet_CSGA$POULET_Poids + sgsdata_Booklet_CSGA$OEUFS_Poids
sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Poids <- sgsdata_Booklet_CSGA$CHARCUTERIE_HORS_JB_Poids 
sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Poids <- sgsdata_Booklet_CSGA$VIANDE_ROUGE_Poids+ sgsdata_Booklet_CSGA$PORC_Poids
sgsdata_Booklet_CSGA$MG_Poids <- sgsdata_Booklet_CSGA$MGA_Poids 
sgsdata_Booklet_CSGA$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_Booklet_CSGA$SNACKS_AUTRES_Poids +  sgsdata_Booklet_CSGA$CEREALES_PD_Poids + sgsdata_Booklet_CSGA$PDTS_SUCRES_Poids 
sgsdata_Booklet_CSGA$SSB_Poids <-  sgsdata_Booklet_CSGA$SODAS_SUCRES_Poids + sgsdata_Booklet_CSGA$SODAS_LIGHT_Poids +sgsdata_Booklet_CSGA$FRUITS_JUS_Poids 

sgsdata_Booklet_CSGA$SOMME_KCAL_Poids <- sgsdata_Booklet_CSGA$KCAL_TOTAL_Kcal
sgsdata_Booklet_CSGA$SOMME_HORS_BOISSON_Poids <-sgsdata_Booklet_CSGA$POIDS_HORS_BOISSON_Poids
sgsdata_Booklet_CSGA$SOMME_POIDS_Poids <- sgsdata_Booklet_CSGA$POIDS_TOTAL_Poids
sgsdata_Booklet_CSGA$VIANDES_Poids <- sgsdata_Booklet_CSGA$POULET_OEUFS_Poids + sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Poids + sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Poids



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

sgsdata_FFQ_IT$FV_Poids <- sgsdata_FFQ_IT$FRUITS_Poids + sgsdata_FFQ_IT$FRUITS_SECS_Poids  + sgsdata_FFQ_IT$NOIX_Poids + sgsdata_FFQ_IT$LEGUMES_Poids 
sgsdata_FFQ_IT$FEC_Poids <- sgsdata_FFQ_IT$FEC_NON_RAF_Poids + sgsdata_FFQ_IT$FEC_RAF_Poids
sgsdata_FFQ_IT$PDTS_LAITIERS_Poids <- sgsdata_FFQ_IT$LAIT_Poids + sgsdata_FFQ_IT$LAITAGES_Poids + sgsdata_FFQ_IT$FROMAGES_Poids
sgsdata_FFQ_IT$POULET_OEUFS_Poids <- sgsdata_FFQ_IT$POULET_Poids + sgsdata_FFQ_IT$OEUFS_Poids
sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Poids <- sgsdata_FFQ_IT$CHARCUTERIE_HORS_JB_Poids +  sgsdata_FFQ_IT$JAMBON_BLANC_Poids 
sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Poids <- sgsdata_FFQ_IT$VIANDE_ROUGE_Poids+ sgsdata_FFQ_IT$PORC_Poids
sgsdata_FFQ_IT$MG_Poids <- sgsdata_FFQ_IT$MGA_Poids 
sgsdata_FFQ_IT$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_FFQ_IT$SNACKS_AUTRES_Poids +  sgsdata_FFQ_IT$CEREALES_PD_Poids + sgsdata_FFQ_IT$PDTS_SUCRES_Poids 
sgsdata_FFQ_IT$SSB_Poids <-  sgsdata_FFQ_IT$SODAS_SUCRES_Poids + sgsdata_FFQ_IT$SODAS_LIGHT_Poids +sgsdata_FFQ_IT$FRUITS_JUS_Poids 
sgsdata_FFQ_IT$SOMME_KCAL_Poids <- sgsdata_FFQ_IT$KCAL_TOTAL_Kcal
sgsdata_FFQ_IT$SOMME_HORS_BOISSON_Poids <- sgsdata_FFQ_IT$POIDS_HORS_BOISSON_Poids
sgsdata_FFQ_IT$SOMME_POIDS_Poids <- sgsdata_FFQ_IT$POIDS_TOTAL_Poids
sgsdata_FFQ_IT$VIANDES_Poids <- sgsdata_FFQ_IT$POULET_OEUFS_Poids + sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Poids + sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Poids


sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>% 
  semi_join(sgsdata_FFQ_IT , by = "Identifiant")%>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))
sgsdata_Booklet_IT$FV_Poids <- sgsdata_Booklet_IT$FRUITS_Poids + sgsdata_Booklet_IT$FRUITS_SECS_Poids  + sgsdata_Booklet_IT$NOIX_Poids + sgsdata_Booklet_IT$LEGUMES_Poids 
sgsdata_Booklet_IT$FEC_Poids <- sgsdata_Booklet_IT$FEC_NON_RAF_Poids + sgsdata_Booklet_IT$FEC_RAF_Poids
sgsdata_Booklet_IT$PDTS_LAITIERS_Poids <- sgsdata_Booklet_IT$LAIT_Poids + sgsdata_Booklet_IT$LAITAGES_Poids + sgsdata_Booklet_IT$FROMAGES_Poids
sgsdata_Booklet_IT$POULET_OEUFS_Poids <- sgsdata_Booklet_IT$POULET_Poids + sgsdata_Booklet_IT$OEUFS_Poids
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Poids <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_Poids 
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Poids <- sgsdata_Booklet_IT$VIANDE_ROUGE_Poids+ sgsdata_Booklet_IT$PORC_Poids
sgsdata_Booklet_IT$MG_Poids <- sgsdata_Booklet_IT$MGA_Poids 
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_Booklet_IT$SNACKS_AUTRES_Poids +  sgsdata_Booklet_IT$CEREALES_PD_Poids  + sgsdata_Booklet_IT$PDTS_SUCRES_Poids
sgsdata_Booklet_IT$SSB_Poids <-  sgsdata_Booklet_IT$SODAS_SUCRES_Poids + sgsdata_Booklet_IT$SODAS_LIGHT_Poids +sgsdata_Booklet_IT$FRUITS_JUS_Poids 
sgsdata_Booklet_IT$SOMME_KCAL_Poids <- sgsdata_Booklet_IT$KCAL_TOTAL_Kcal
sgsdata_Booklet_IT$SOMME_HORS_BOISSON_Poids <- sgsdata_Booklet_IT$POIDS_HORS_BOISSON_Poids
sgsdata_Booklet_IT$SOMME_POIDS_Poids <- sgsdata_Booklet_IT$POIDS_TOTAL_Poids
sgsdata_Booklet_IT$VIANDES_Poids <- sgsdata_Booklet_IT$POULET_OEUFS_Poids + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Poids + sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Poids



#SELECTION DES DATAFRAMES NUDGES
sgsdata_FFQ_nudges <- sgsdata_nudges %>% filter(Mesure != "Carnet", UC_TI ==1)

sgsdata_Booklet_nudges <- sgsdata_nudges %>% filter(Mesure == "Carnet", UC_TI.x ==1) 

sgsdata_FFQ_nudges  <- sgsdata_FFQ_nudges  %>% 
  semi_join(sgsdata_Booklet_nudges, by = "Identifiant")%>%
  select(Identifiant,where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))
sgsdata_FFQ_nudges$FV_Poids <- sgsdata_FFQ_nudges$FRUITS_Poids + sgsdata_FFQ_nudges$FRUITS_SECS_Poids  + sgsdata_FFQ_nudges$NOIX_Poids + sgsdata_FFQ_nudges$LEGUMES_Poids 
sgsdata_FFQ_nudges$FEC_Poids <- sgsdata_FFQ_nudges$FEC_NON_RAF_Poids + sgsdata_FFQ_nudges$FEC_RAF_Poids
sgsdata_FFQ_nudges$PDTS_LAITIERS_Poids <- sgsdata_FFQ_nudges$LAIT_Poids + sgsdata_FFQ_nudges$LAITAGES_Poids + sgsdata_FFQ_nudges$FROMAGES_Poids
sgsdata_FFQ_nudges$POULET_OEUFS_Poids <- sgsdata_FFQ_nudges$POULET_Poids + sgsdata_FFQ_nudges$OEUFS_Poids
sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Poids <- sgsdata_FFQ_nudges$CHARCUTERIE_HORS_JB_Poids +  sgsdata_FFQ_nudges$JAMBON_BLANC_Poids 
sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Poids <- sgsdata_FFQ_nudges$VIANDE_ROUGE_Poids+ sgsdata_FFQ_nudges$PORC_Poids
sgsdata_FFQ_nudges$MG_Poids <- sgsdata_FFQ_nudges$MGA_Poids 
sgsdata_FFQ_nudges$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_FFQ_nudges$SNACKS_AUTRES_Poids +  sgsdata_FFQ_nudges$CEREALES_PD_Poids  + sgsdata_FFQ_nudges$PDTS_SUCRES_Poids 
sgsdata_FFQ_nudges$SSB_Poids <-  sgsdata_FFQ_nudges$SODAS_SUCRES_Poids + sgsdata_FFQ_nudges$SODAS_LIGHT_Poids +sgsdata_FFQ_nudges$FRUITS_JUS_Poids 
sgsdata_FFQ_nudges$SOMME_KCAL_Poids <- sgsdata_FFQ_nudges$SOMME_KCAL
sgsdata_FFQ_nudges$SOMME_HORS_BOISSON_Poids <-sgsdata_FFQ_nudges$SOMME_HORS_BOISSON
sgsdata_FFQ_nudges$SOMME_POIDS_Poids <- sgsdata_FFQ_nudges$SOMME_POIDS
sgsdata_FFQ_nudges$VIANDES_Poids <- sgsdata_FFQ_nudges$POULET_OEUFS_Poids + sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Poids + sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Poids


sgsdata_Booklet_nudges <- sgsdata_Booklet_nudges %>% 
  semi_join(sgsdata_FFQ_nudges, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", "")) #%>%
  #rename_with(~ paste0(.x, "_Poids"), .cols = 28:61)
sgsdata_Booklet_nudges$FV_Poids <- sgsdata_Booklet_nudges$FRUITS_Poids + sgsdata_Booklet_nudges$FRUITS_SECS_Poids  + sgsdata_Booklet_nudges$NOIX_Poids + sgsdata_Booklet_nudges$LEGUMES_Poids 
sgsdata_Booklet_nudges$FEC_Poids <- sgsdata_Booklet_nudges$FEC_NON_RAF_Poids + sgsdata_Booklet_nudges$FEC_RAF_Poids
sgsdata_Booklet_nudges$PDTS_LAITIERS_Poids <- sgsdata_Booklet_nudges$LAIT_Poids + sgsdata_Booklet_nudges$LAITAGES_Poids + sgsdata_Booklet_nudges$FROMAGES_Poids
sgsdata_Booklet_nudges$POULET_OEUFS_Poids <- sgsdata_Booklet_nudges$POULET_Poids + sgsdata_Booklet_nudges$OEUFS_Poids
sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Poids <- sgsdata_Booklet_nudges$CHARCUTERIE_HORS_JB_Poids 
sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Poids <- sgsdata_Booklet_nudges$VIANDE_ROUGE_Poids+ sgsdata_Booklet_nudges$PORC_Poids
sgsdata_Booklet_nudges$MG_Poids <- sgsdata_Booklet_nudges$MGA_Poids 
sgsdata_Booklet_nudges$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_Booklet_nudges$SNACKS_AUTRES_Poids +  sgsdata_Booklet_nudges$CEREALES_PD_Poids + sgsdata_Booklet_nudges$PDTS_SUCRES_Poids 
sgsdata_Booklet_nudges$SSB_Poids <-  sgsdata_Booklet_nudges$SODAS_SUCRES_Poids + sgsdata_Booklet_nudges$SODAS_LIGHT_Poids +sgsdata_Booklet_nudges$FRUITS_JUS_Poids 
sgsdata_Booklet_nudges$SOMME_KCAL_Poids <- sgsdata_Booklet_nudges$SOMME_KCAL
sgsdata_Booklet_nudges$SOMME_HORS_BOISSON_Poids <-sgsdata_Booklet_nudges$SOMME_HORS_BOISSON.x
sgsdata_Booklet_nudges$SOMME_POIDS_Poids <- sgsdata_Booklet_nudges$SOMME_POIDS
sgsdata_Booklet_nudges$VIANDES_Poids <- sgsdata_Booklet_nudges$POULET_OEUFS_Poids + sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Poids + sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Poids




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
by_keys <- c("source", "Identifiant", "Periode")

ffq_comm <- sgsdata_FFQ_all %>% 
  semi_join(sgsdata_Booklet_all, by = by_keys) %>%
  arrange(across(all_of(by_keys)))

booklet_comm <- sgsdata_Booklet_all %>% 
  semi_join(sgsdata_FFQ_all, by = by_keys) %>%
  arrange(across(all_of(by_keys)))


pair_all <- harmoniser_ids(booklet_comm , ffq_comm)
sgsdata_Booklet_comm <- pair_all$df1
sgsdata_ffq_comm    <- pair_all$df2 


analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Poids"),
                              multiplier = 1000,
                              conf_level = 0.95) {
  
  library(dplyr)
  library(tidyr)
  
  # --- 0) Alignement si Identifiant présent ---
  if ("Identifiant" %in% names(ffq_data) && "Identifiant" %in% names(booklet_data)) {
    
    commun_ids <- intersect(ffq_data$Identifiant, booklet_data$Identifiant)
    
    ffq_data <- ffq_data %>%
      filter(Identifiant %in% commun_ids) %>%
      arrange(Identifiant)
    
    booklet_data <- booklet_data %>%
      filter(Identifiant %in% commun_ids) %>%
      arrange(Identifiant)
  }
  
  if (nrow(ffq_data) != nrow(booklet_data)) {
    stop("ffq_data et booklet_data n'ont pas le même nombre de lignes après alignement.")
  }
  
  # --- 1) Colonnes ciblées par suffixe ---
  motif <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  
  vars_ffq <- grep(motif, names(ffq_data), value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  
  if (length(vars_ffq) == 0) {
    stop("Aucune colonne FFQ ne se termine par ", paste(suffixes, collapse = ", "))
  }
  
  if (length(vars_booklet) == 0) {
    stop("Aucune colonne Booklet ne se termine par ", paste(suffixes, collapse = ", "))
  }
  
  # --- 2) Variables communes aux deux bases ---
  vars_communes <- intersect(vars_ffq, vars_booklet)
  
  if (length(vars_communes) == 0) {
    stop("Aucune variable commune entre ffq_data et booklet_data avec les suffixes indiqués.")
  }
  
  tableau_base <- tibble(variable = vars_communes)
  
  # --- 3) Statistiques par variable, sur paires complètes ---
  stats <- lapply(tableau_base$variable, function(var) {
    
    x_full <- ffq_data[[var]]
    y_full <- booklet_data[[var]]
    
    # Garder uniquement les individus avec FFQ ET Booklet non manquants
    valid <- !is.na(x_full) & !is.na(y_full)
    
    x <- x_full[valid] # FFQ
    y <- y_full[valid] # Booklet
    
    n_pairs <- length(x)
    
    # Si pas assez d'observations
    if (n_pairs <= 1) {
      return(data.frame(
        variable = var,
        n_pairs = n_pairs,
        moyenne_Booklet = NA,
        ci95_Booklet = NA,
        moyenne_FFQ = NA,
        ci95_FFQ = NA,
        pct_bias = NA,
        p_value_diff = NA,
        pearson_correlation = NA,
        pearson_p_value = NA,
        spearman_correlation = NA,
        spearman_p_value = NA,
        pct_similar_quintile = NA,
        pct_adjacent_quintile = NA,
        pct_opposite_quintile = NA
      ))
    }
    
    # --- 3.1) Ajustement d'unité ---
    # Conversion kg -> g pour les variables de poids,
    # sauf SOMME_KCAL_Poids qui ne doit pas être multipliée par 1000.
    if (grepl("_Poids$", var) && var != "SOMME_KCAL_Poids") {
      x <- x * multiplier
      y <- y * multiplier
    }
    
    # --- 3.2) Moyennes sur les mêmes individus ---
    moyenne_FFQ <- mean(x)
    moyenne_Booklet <- mean(y)
    
    # --- 3.3) Pourcentage de biais : FFQ vs Booklet ---
    pct_bias <- ifelse(
      moyenne_Booklet == 0,
      NA,
      (moyenne_FFQ - moyenne_Booklet) / moyenne_Booklet * 100
    )
    
    # --- 3.4) Tests t et intervalles de confiance ---
    # Test apparié : les deux mesures viennent des mêmes individus
    t_diff <- t.test(x, y, paired = TRUE)
    
    # IC séparés des moyennes FFQ et Booklet
    t_ffq <- t.test(x, conf.level = conf_level)
    t_book <- t.test(y, conf.level = conf_level)
    
    # --- 3.5) Corrélations Pearson et Spearman ---
    if (sd(x) > 0 && sd(y) > 0) {
      
      pearson_tst <- cor.test(x, y, method = "pearson", exact = FALSE)
      spearman_tst <- cor.test(x, y, method = "spearman", exact = FALSE)
      
      pearson_est <- unname(pearson_tst$estimate)
      pearson_p <- pearson_tst$p.value
      
      spearman_est <- unname(spearman_tst$estimate)
      spearman_p <- spearman_tst$p.value
      
    } else {
      
      pearson_est <- NA
      pearson_p <- NA
      spearman_est <- NA
      spearman_p <- NA
    }
    
    # --- 3.6) Accord par quintiles ---
    qx <- dplyr::ntile(x, 5)
    qy <- dplyr::ntile(y, 5)
    
    d <- abs(qx - qy)
    
    pct_similar <- mean(d == 0, na.rm = TRUE) * 100
    
    # Ici, tu gardes ta définition large :
    # écart de 1 ou 2 quintiles.
    # Si tu veux "strictly adjacent", remplace c(1, 2) par 1.
    pct_adjacent <- mean(d %in% c(1, 2), na.rm = TRUE) * 100
    
    pct_opposite <- mean(d %in% c(3, 4), na.rm = TRUE) * 100
    
    # --- 3.7) Résultat pour la variable ---
    data.frame(
      variable = var,
      n_pairs = n_pairs,
      
      moyenne_Booklet = moyenne_Booklet,
      ci95_Booklet = sprintf("[%.2f, %.2f]",
                             t_book$conf.int[1],
                             t_book$conf.int[2]),
      
      moyenne_FFQ = moyenne_FFQ,
      ci95_FFQ = sprintf("[%.2f, %.2f]",
                         t_ffq$conf.int[1],
                         t_ffq$conf.int[2]),
      
      pct_bias = pct_bias,
      p_value_diff = t_diff$p.value,
      
      pearson_correlation = pearson_est,
      pearson_p_value = pearson_p,
      
      spearman_correlation = spearman_est,
      spearman_p_value = spearman_p,
      
      pct_similar_quintile = pct_similar,
      pct_adjacent_quintile = pct_adjacent,
      pct_opposite_quintile = pct_opposite
    )
  }) %>%
    bind_rows()
  
  # --- 4) Tableau final ---
  tableau_final <- stats %>%
    select(
      variable,
      n_pairs,
      moyenne_Booklet, ci95_Booklet,
      moyenne_FFQ, ci95_FFQ,
      pct_bias, p_value_diff,
      pearson_correlation, pearson_p_value,
      spearman_correlation, spearman_p_value,
      pct_similar_quintile,
      pct_adjacent_quintile,
      pct_opposite_quintile
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



# 1) Vecteur complet des variables dans l’ordre voulu
variables <- c(
  "VIANDES_Poids",
  "CEREALES_PD_Poids", 
  "FEC_NON_RAF_Poids",
  "FEC_RAF_Poids",
  "FROMAGES_Poids",
  "FRUITS_Poids",
  "FRUITS_SECS_Poids", 
  "LAITAGES_Poids",
  "LEGUMES_Poids",
  "LEG_SECS_Poids",
  "NOIX_Poids",
  "OEUFS_Poids", 
  "PDTS_SUCRES_Poids", 
  "PORC_Poids", 
  "POULET_Poids", 
  "QUICHES_PIZZAS_TARTES_SALEES_Poids", 
  "SNACKS_AUTRES_Poids",
  "VIANDE_ROUGE_Poids",  
  "FRUITS_JUS_Poids", 
  "LAIT_Poids", 
  "SODAS_LIGHT_Poids", 
  "SODAS_SUCRES_Poids",
  "MGA_Poids", 
  "MGV_Poids",
  "FV_Poids",
  "FEC_Poids",
  "PDTS_LAITIERS_Poids",
  "AUTRE_PDTS_ANIMAUX_Poids",
  "PDTS_DISCRETIONNAIRES_Poids",  
  "SSB_Poids" ,
  "PLATS_PREP_VEGETARIENS_Poids", 
  "PLATS_PREP_CARNES_Poids",
  "POISSONS_Poids",
  "ALCOOL_Poids",  
  "LEG_SECS_Poids",
  "SAUCES_Poids",
  "DESSERTS_LACTES_Poids",
  "EAU_Poids",
  "CAFE_THE_Poids",
  "SOMME_HORS_BOISSON_Poids",
  "SOMME_POIDS_Poids",
  "SOMME_KCAL_Poids"

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



### Graphs correlations poids ----------------------------------------------------------
tableau1_ord <- tableau1_ord %>% 
  mutate(
    classe = case_when(
     pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
     ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
     pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
     (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )

    

tableau2_ord <- tableau2_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )




tableau3_ord <- tableau3_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    )
  )






tableau4_ord <- tableau4_ord %>% 
  mutate(
      classe = case_when(
        pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
        ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
        pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
        (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
        (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
        spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
        
        # 3) Valeur manquante : l'une des stats est NA
        is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
          "Missing"
      )
    )
    


tableau5_ord <- tableau5_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    ))
  



tableau6_ord <- tableau6_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    ))
    


tableau7_ord <- tableau7_ord %>% 
  mutate(
    classe = case_when(
      pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" ,
      ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" ,
      pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" ,
      (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" ,
      spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
      
      # 3) Valeur manquante : l'une des stats est NA
      is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ 
        "Missing"
    ))




# 1) Ajout de la période et combinaison
df1 <- tableau1_ord %>% mutate(periode = "Nov21_TI")
df2 <- tableau2_ord %>% mutate(periode = "March22_TI")
df3 <- tableau3_ord %>% mutate(periode = "Nov22_TI")
df4 <- tableau4_ord %>% mutate(periode = "March23_TI")
df5 <- tableau5_ord %>% mutate(periode = "Nov23_TI")
df6 <- tableau6_ord %>% mutate(periode = "March24_TI")
df7 <- tableau7_ord %>% mutate(periode = "Nov22 (CSGA)")


# —————————————————————————
# Données
# (adapte si besoin : ici on reprend ta construction)
heat_df <- bind_rows(df1, df2, df3, df4, df5, df6, df7) %>%
  filter(!str_ends(variable, "_Kcal"), variable != "SOMME_KCALTOT")

# —————————————————————————
# Niveaux et palette
levels_classe <- c(
  "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  "Weakly. No correlation coefficient at least 0.4.",
  "Missing"   # <- libellé unique pour les manquants
)

palette_custom_named <- c(
  "Weakly. No correlation coefficient at least 0.4." = "firebrick",
  "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different" = "goldenrod",
  "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different" = "#C7E9C0",
  "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different" = "#A1D97B",
  "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different" = "#74C499",
  "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different" = "#31A354",
  "Missing" = "grey"   # <- tuiles blanches pour les manquants
)

# —————————————————————————
# Périodes & labels X
period_levels <- c(
  "Nov21_TI","March22_TI",
  "Nov22_TI","March23_TI",
  "Nov23_TI","March24_TI",
  "Nov22 (CSGA)"#, "All"
)

labels_x <- setNames(
  c("Nov 21","Mar 22","Nov 22","Mar 23","Nov 23","Mar 24","Nov22"),
  period_levels
)

# —————————————————————————
# Labels variables (anglais)
labels_EN <- c(
  CEREALES_PD_Poids = "Breakfast cereals",
  CAFE_THE_Poids = "Coffee/tea",
  DESSERTS_LACTES_Poids = "Dairy Desserts",
  FEC_NON_RAF_Poids = "Unrefined Starches",
  FEC_RAF_Poids = "Refined Starches",
  FROMAGES_Poids = "Cheeses",
  FRUITS_Poids = "Fruits",
  FRUITS_SECS_Poids = "Dried Fruits",
  LAITAGES_Poids = "Dairy Products",
  LEGUMES_Poids = "Vegetables",
  LEG_SECS_Poids = "Legumes",
  MGA_Poids = "Animal Fats",
  MGV_Poids = "Vegetable Fats",
  NOIX_Poids = "Nuts",
  OEUFS_Poids = "Eggs",
  PDTS_SUCRES_Poids = "Sweet products",
  PLATS_PREP_CARNES_Poids = "Meat Based Prepared Dishes",
  PLATS_PREP_VEGETARIENS_Poids = "Vegetarian Prepared Dishes",
  POISSONS_Poids = "Fish",
  PORC_Poids = "Pork",
  POULET_Poids = "Chicken",
  QUICHES_PIZZAS_TARTES_SALEES_Poids = "Quiches/ Pizzas/ Savoury Pies",
  SAUCES_Poids = "Sauces",
  SNACKS_AUTRES_Poids = "Other Snacks",
  VIANDE_ROUGE_Poids = "Red Meat",
  ALCOOL_Poids = "Alcohol",
  EAU_Poids = "Water",
  FRUITS_JUS_Poids = "Fruit Juices",
  LAIT_Poids = "Milk",
  SODAS_LIGHT_Poids = "Diet Sodas",
  SODAS_SUCRES_Poids = "Sugary Sodas",
  FV_Poids  = "Fruits and vegetables",
  FEC_Poids = "Starchy foods",
  PDTS_LAITIERS_Poids = "Dairy products",
  POULET_OEUFS_Poids = "Eggs / chicken",
  AUTRE_PDTS_ANIMAUX_Poids = "Cold cuts",
  VIANDE_ROUGE_PORC_Poids = "Red meat/Pork",
  PDTS_DISCRETIONNAIRES_Poids = "Discretionary foods",
  SSB_Poids = "Sugary sweet beverages",
  SOMME_HORS_BOISSON_Poids = "Total weight without beverages",
  SOMME_POIDS_Poids = "Total weight",
  SOMME_KCAL_Poids = "Total kilocalories",
  VIANDES_Poids = "Meats"
)

# —————————————————————————
# Préparation df_all : facettes, périodes, recodage Missing
df_all <- heat_df %>%
  mutate(
    # catégories pour les facettes
    category = case_when(
      variable %in% c("SOMME_HORS_BOISSON_Poids", "SOMME_POIDS_Poids", "SOMME_KCAL_Poids") ~ "General\nindicator",
      variable %in% c("FV_Poids","FEC_Poids","PDTS_LAITIERS_Poids",
                      "VIANDES_Poids",
                      "PDTS_DISCRETIONNAIRES_Poids","SSB_Poids") ~ "General\nfood item",
      TRUE ~ "Specific\nfood item"
    ) %>% factor(levels = c("General\nindicator","General\nfood item","Specific\nfood item")),
    # vagues pour facettes colonnes
    wave = case_when(
      periode %in% c("Nov21_TI","March22_TI") ~ "Weekly FFQ 1",
      periode %in% c("Nov22_TI","March23_TI") ~ "Weekly FFQ 2",
      periode %in% c("Nov23_TI","March24_TI") ~ "Weekly FFQ 3",
      periode == "Nov22 (CSGA)"               ~ "Monthly FFQ 1"#,
      #periode =="All"   ~ "All"
    ) %>% factor(levels = c("Weekly FFQ 1","Weekly FFQ 2","Weekly FFQ 3","Monthly FFQ 1", "All")),
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
  labs(title = "Correlation of Weight Variables", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin        = unit(c(1, 1, 1, 4), "lines"),
    axis.text.y        = element_text( angle = 0, hjust = 1, size=11),
    panel.spacing.y    = unit(1, "lines"),
    strip.placement    = "outside",
    strip.text.y.left  = element_blank(),
    strip.background.y = element_blank(),
    strip.background.x = element_blank(),
    strip.text.x       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1, size=11),
    legend.text        = element_text(size = 12, lineheight = 1),
    panel.grid         = element_blank(),
    plot.title         = element_text(color = "black", face = "bold", hjust = 0.5, size = 12)
  )

print(p)

###ANALYSE PAR QUINTILE ----------------------------------

# 1) Regrouper les résultats de vos 7 campagnes
all_stats <- bind_rows(
  tableau_final_1 %>% mutate(periode = "Nov21_TI"),
  tableau_final_2 %>% mutate(periode = "March22_TI"),
  tableau_final_3 %>% mutate(periode = "Nov22_TI"),
  tableau_final_4 %>% mutate(periode = "March23_TI"),
  tableau_final_5 %>% mutate(periode = "Nov23_TI"),
  tableau_final_6 %>% mutate(periode = "March24_TI"),
  tableau_final_7 %>% mutate(periode = "Nov22_CSGA")
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
  VIANDES_Poids = "Meats",
  CAFE_THE_Poids = "Coffee / Tea",
  CEREALES_PD_Poids                          = "Breakfast cereals",
  FEC_NON_RAF_Poids                          = "Unrefined starches",
  FEC_RAF_Poids                              = "Refined starches",
  FROMAGES_Poids                             = "Cheeses",
  FRUITS_Poids                               = "Fruits",
  FRUITS_SECS_Poids                          = "Dried fruits",
  LAITAGES_Poids                             = "Dairy products",
  LEGUMES_Poids                              = "Vegetables",
  LEG_SECS_Poids                             = "Legumes",
  NOIX_Poids                                 = "Nuts",
  OEUFS_Poids                                = "Eggs",
  PDTS_SUCRES_Poids                          = "Sweet products",
  PORC_Poids                                 = "Pork",
  POULET_Poids                               = "Chicken",
  QUICHES_PIZZAS_TARTES_SALEES_Poids         = "Savory pies & pizzas",
  SNACKS_AUTRES_Poids                        = "Other snacks",
  VIANDE_ROUGE_Poids                         = "Red meat",
  FRUITS_JUS_Poids                           = "Fruit juices",
  LAIT_Poids                                 = "Milk",
  SODAS_LIGHT_Poids                          = "Diet sodas",
  SODAS_SUCRES_Poids                         = "Sugary sodas",
  MGA_Poids                                  = "Animal fats",
  MGV_Poids                                  = "Vegetable fats",
  FV_Poids                                   = "Fruits and vegetables",
  FEC_Poids                                  = "Starchy foods",
  PDTS_LAITIERS_Poids                        = "Dairy products",
  #POULET_OEUFS_Poids                         = "Eggs/Chicken",
  AUTRE_PDTS_ANIMAUX_Poids                   = "Cold cuts",
  #VIANDE_ROUGE_PORC_Poids                    = "Red meat & pork",
  PDTS_DISCRETIONNAIRES_Poids                = "Discretionary foods",
  SSB_Poids                                  = "Sugary sweet beverages",
  PLATS_PREP_VEGETARIENS_Poids               = "Vegetarian dishes",
  PLATS_PREP_CARNES_Poids                    = "Meat dishes",
  POISSONS_Poids                             = "Fish",
  ALCOOL_Poids                               = "Alcohol",
  SAUCES_Poids                               = "Sauces",
  DESSERTS_LACTES_Poids                      = "Dairy desserts",
  EAU_Poids                                  = "Water",
  SOMME_HORS_BOISSON_Poids                   = "Total weight (no beverages)",
  SOMME_POIDS_Poids                          = "Total weight",
  SOMME_KCAL_Poids                           = "Total calories"
)


# 0) Définir les groupes
general_indicators <- c("SOMME_HORS_BOISSON_Poids","SOMME_POIDS_Poids","SOMME_KCAL_Poids")

general_food_items <- c("FV_Poids","FEC_Poids","PDTS_LAITIERS_Poids",
                          "VIANDES_Poids","PDTS_DISCRETIONNAIRES_Poids","SSB_Poids")

# tout le reste sera "Specific items"
plot_quintile_by_campaign <- function(df,
                                      keep_vars   = NULL,
                                      remove_vars = NULL,
                                      label_map   = NULL,
                                      n_cols      = 7,
                                      label_min   = 3,
                                      label_dec   = 0) {
  general_indicators <- c("SOMME_HORS_BOISSON_Poids","SOMME_POIDS_Poids","SOMME_KCAL_Poids")
  general_food_items <- c("FV_Poids","FEC_Poids","PDTS_LAITIERS_Poids",
                          "VIANDES_Poids","PDTS_DISCRETIONNAIRES_Poids","SSB_Poids")
  
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
      legend.position    = "bottom",
      plot.title        = element_text(hjust = 0.5),  # centre le titre principal
      legend.title      = element_text(hjust = 0.5),  # centre le texte du titre
      legend.title.align = 0.5                        # centre le titre par rapport aux clés
    )
}


p_by_campaign <- plot_quintile_by_campaign(
  df          = plot_df_vars,
  keep_vars   = c(
    "SOMME_HORS_BOISSON_Poids",
    "SOMME_POIDS_Poids",
    "SOMME_KCAL_Poids",
     "FV_Poids",
    "CAFE_THE_Poids",
    "FEC_Poids",
    "PDTS_LAITIERS_Poids",
    #"POULET_OEUFS_Poids",
    "VIANDES_Poids",
    "AUTRE_PDTS_ANIMAUX_Poids",
    #"VIANDE_ROUGE_PORC_Poids",
    "PDTS_DISCRETIONNAIRES_Poids",
    "SSB_Poids"  ,
    "CEREALES_PD_Poids",     
    "FEC_NON_RAF_Poids",      
    "FEC_RAF_Poids",     
    "FROMAGES_Poids",      
    "FRUITS_Poids",     
    "FRUITS_SECS_Poids",      
    "LAITAGES_Poids",     
    "LEGUMES_Poids",      
    "LEG_SECS_Poids",     
    "NOIX_Poids",      
    "OEUFS_Poids",     
    "PDTS_SUCRES_Poids",      
    "PORC_Poids",     
    "POULET_Poids",      
    "QUICHES_PIZZAS_TARTES_SALEES_Poids",     
    "SNACKS_AUTRES_Poids",      
    "VIANDE_ROUGE_Poids",     
    "FRUITS_JUS_Poids",      
    "LAIT_Poids",     
    "SODAS_LIGHT_Poids",      
    "SODAS_SUCRES_Poids",     
    "MGA_Poids",      
    "MGV_Poids",     
    "PLATS_PREP_VEGETARIENS_Poids",      
    "PLATS_PREP_CARNES_Poids",     
    "POISSONS_Poids",      
    "ALCOOL_Poids",     
    "SAUCES_Poids",      
    "DESSERTS_LACTES_Poids",     
    "EAU_Poids"     
    
    ),
  label_map   = labels_EN,
  n_cols      = 4   # 7 campagnes → 7 colonnes
)
print(p_by_campaign)




# --- 1) Fonctions utilitaires -------------------------------------------------
# --- 1) Fonctions utilitaires -------------------------------------------------

# Découpe les individus en trois groupes selon leur position dans la distribution
# des quantités observées dans les données de supply/booklet :
# - T1 : 20% les plus faibles
# - T2 : 60% intermédiaires
# - T3 : 20% les plus élevés
#
# La découpe est réalisée séparément pour chaque variable *_Poids.
# Les individus sont triés par quantité croissante ; en cas d'ex æquo,
# l'identifiant est utilisé pour obtenir un tri déterministe.
# La fonction crée, pour chaque variable, trois colonnes supplémentaires :
# var_T1, var_T2 et var_T3, contenant l'identifiant si l'individu appartient
# au groupe correspondant, et NA sinon.
# Découpe T1/T2/T3 à 20% / 60% / 20% 

add_tertiles_id <- function(df, id_col = "Identifiant") {
  
  poids_vars <- grep("(?i)_Poids$", names(df), value = TRUE, perl = TRUE)
  
  for (var in poids_vars) {
    
    num <- suppressWarnings(as.numeric(as.character(df[[var]])))
    
    # Pré-init colonnes T1/T2/T3
    df[[paste0(var, "_T1")]] <- NA_character_
    df[[paste0(var, "_T2")]] <- NA_character_
    df[[paste0(var, "_T3")]] <- NA_character_
    
    # Positions des individus avec une valeur non manquante
    valid <- which(!is.na(num))
    
    # Nombre d'observations valides
    n_tot <- length(valid)
    
    # Si aucune observation valide, on passe à la variable suivante
    if (n_tot == 0) next
    
    # Découpe 20% / 60% / 20%
    n1 <- floor(0.2 * n_tot)
    n2 <- floor(0.8 * n_tot)
    
    # Tri déterministe :
    # 1. par quantité croissante
    # 2. par Identifiant en cas d'ex æquo
    ord <- valid[order(num[valid], df[[id_col]][valid])]
    
    idx1 <- if (n1 > 0) ord[seq_len(n1)] else integer(0)
    idx2 <- if (n2 > n1) ord[(n1 + 1):n2] else integer(0)
    idx3 <- if (n_tot > n2) ord[(n2 + 1):n_tot] else integer(0)
    
    df[[paste0(var, "_T1")]][idx1] <- df[[id_col]][idx1]
    df[[paste0(var, "_T2")]][idx2] <- df[[id_col]][idx2]
    df[[paste0(var, "_T3")]][idx3] <- df[[id_col]][idx3]
  }
  
  df
}

# Tableau comparatif FFQ vs Booklet : uniquement les moyennes
analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes = c("_Poids"),
                              multiplier = 1000) {
  
  motif <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  
  vars_ffq <- grep(motif, names(ffq_data), value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  vars_communes <- intersect(vars_ffq, vars_booklet)
  
  if (!length(vars_communes)) {
    stop("Aucune variable commune entre FFQ et Booklet.")
  }
  
  stats <- lapply(vars_communes, function(var) {
    
    x <- suppressWarnings(as.numeric(ffq_data[[var]]))
    y <- suppressWarnings(as.numeric(booklet_data[[var]]))
    
    if (grepl("_Poids$", var) && var != "SOMME_KCAL_Poids") {
      x <- x * multiplier
      y <- y * multiplier
    }
    
    valid <- !is.na(x) & !is.na(y)
    
    data.frame(
      variable = var,
      n_pairs = sum(valid),
      moyenne_Booklet = mean(y[valid], na.rm = TRUE),
      moyenne_FFQ = mean(x[valid], na.rm = TRUE)
    )
  }) %>%
    bind_rows() %>%
    mutate(
      Delta = moyenne_FFQ - moyenne_Booklet,
      pct_bias = ifelse(
        moyenne_Booklet == 0,
        NA_real_,
        Delta / moyenne_Booklet * 100
      ),
      across(where(is.numeric), ~ signif(.x, 3))
    )
  
  return(stats)
}



# --- 2) Préparation des tertiles Booklet + masques FFQ ------------------------

# Data-frames initiales Booklet à découper en tertiles
dfs_booklet <- list(
  nudges1 = sgsdata_Booklet_nudges1_bis,
  nudges2 = sgsdata_Booklet_nudges2_bis,
  IT11    = sgsdata_Booklet_IT11_bis,
  IT12    = sgsdata_Booklet_IT12_bis,
  IT21    = sgsdata_Booklet_IT21_bis,
  IT22    = sgsdata_Booklet_IT22_bis,
  CSGA    = sgsdata_Booklet_CSGA_bis,
  com     = sgsdata_Booklet_com_bis   # facultatif si tu veux l'avoir sous la main
)

# 2.1 Ajout des colonnes *_T1/_T2/_T3 (identifiants retenus par tertile)
dfs_booklet_tertiles <- imap(dfs_booklet, ~ add_tertiles_id(.x))

# 2.2 Création des dataframes Booklet filtrés par tertile T1/T2/T3 (mêmes colonnes *_Poids)
for (base in names(dfs_booklet_tertiles)) {
  full_df   <- dfs_booklet_tertiles[[base]]
  poids_vars <- grep("(?i)_poids$", names(full_df), value = TRUE, perl = TRUE)
  
  for (k in 1:3) {
    suffix   <- paste0("_T", k)
    mask_df  <- full_df %>% select(ends_with(suffix)) %>%
      rename_with(~ sub(paste0(suffix, "$"), "", .x), ends_with(suffix))
    
    new_df <- full_df %>%
      select(Identifiant, all_of(poids_vars))
    
    for (var in poids_vars) {
      if (!var %in% names(mask_df))
        stop("Le masque n’a pas la colonne ‘", var, "’ pour ", base, "_T", k)
      new_df[[var]] <- ifelse(!is.na(mask_df[[var]]), new_df[[var]], NA)
    }
    
    assign(paste0("sgsdata_Booklet_", base, "_poids_T", k), new_df, envir = .GlobalEnv)
  }
}

# 2.3 Création des masques correspondants côté FFQ (on garde mêmes IDs & colonnes communes)
bases_ffq <- c("nudges1","nudges2","IT11","IT12","IT21","IT22","CSGA") # (pas "com" sauf si FFQ_com_bis existe)
for (base in bases_ffq) {
  df_ffq_full <- get(paste0("sgsdata_FFQ_", base, "_bis"))
  poids_vars  <- grep("(?i)_poids$", names(df_ffq_full), value = TRUE, perl = TRUE)
  
  for (k in 1:3) {
    mask_booklet <- get(paste0("sgsdata_Booklet_", base, "_poids_T", k))
    common_vars  <- intersect(poids_vars, names(mask_booklet))
    if (!length(common_vars)) next
    
    df_ffq <- df_ffq_full %>%
      filter(Identifiant %in% mask_booklet$Identifiant) %>%
      select(Identifiant, all_of(common_vars))
    
    mask_sub <- mask_booklet %>% select(Identifiant, all_of(common_vars))
    
    df_join <- df_ffq %>%
      left_join(mask_sub, by = "Identifiant", suffix = c("", ".mask"))
    
    for (var in common_vars) {
      df_join[[var]] <- ifelse(!is.na(df_join[[paste0(var, ".mask")]]), df_join[[var]], NA_real_)
    }
    
    df_sel <- df_join %>% select(Identifiant, all_of(common_vars))
    
    assign(paste0("sgsdata_FFQ_", base, "_poids_T", k), df_sel, envir = .GlobalEnv)
  }
}

# --- 3) Calcul des tableaux comparatifs ---------------------------------------

# 3.1 TERTILES : calcul programmatique pour toutes les bases et T1/T2/T3
tab_tertiles <- list()
for (base in bases_ffq) {
  for (k in 1:3) {
    df_b <- get(paste0("sgsdata_Booklet_", base, "_poids_T", k))
    df_f <- get(paste0("sgsdata_FFQ_",      base, "_poids_T", k))
    nm   <- paste0("tableau_final_", base, "_T", k, "_ord")
    
    tab_tertiles[[nm]] <- analyser_moyennes(df_f, df_b) %>% arrange(variable)
  }
}

# 3.2 “ALL” (sans découpage tertiles) pour chaque base (hors "com" par défaut)
bases_all <- bases_ffq  # nudges1, nudges2, IT11, IT12, IT21, IT22, CSGA
tab_all <- list()
for (base in bases_all) {
  df_b_full <- get(paste0("sgsdata_Booklet_", base, "_bis"))
  df_f_full <- get(paste0("sgsdata_FFQ_",      base, "_bis"))
  
  poids_b <- grep("(?i)_poids$", names(df_b_full), value = TRUE, perl = TRUE)
  poids_f <- grep("(?i)_poids$", names(df_f_full), value = TRUE, perl = TRUE)
  
  df_b <- df_b_full %>% select(Identifiant, all_of(poids_b))
  df_f <- df_f_full %>% select(Identifiant, all_of(poids_f))
  
  nm <- paste0("tableau_final_", base, "_Tall_ord")
  tab_all[[nm]] <- analyser_moyennes(df_f, df_b) %>% arrange(variable)
}

# 3.3 Empilement de tous les tableaux (T1/T2/T3/Tall)
tbl_list <- c(tab_tertiles, tab_all)
tableau_final <- bind_rows(tbl_list, .id = "source") %>%
  tidyr::separate(col = source, into = c("discard","base","tertile"), sep = "_", extra = "merge") %>%
  select(-discard) %>%
  filter(grepl("_ord$", tertile)) %>%
  # nettoyage variables non voulues
  filter(!str_ends(variable, "_Kcal"), variable != "SOMME_KCALTOT")

# 3.4 Ajout période par base
tableau_final <- bind_rows(tbl_list, .id = "source") %>%
  mutate(source = str_remove(source, "^tableau_final_")) %>%
  tidyr::separate(
    col = source,
    into = c("base", "tertile"),
    sep = "_(?=T)",
    extra = "merge"
  ) %>%
  filter(grepl("_ord$", tertile)) %>%
  filter(!str_ends(variable, "_Kcal"), variable != "SOMME_KCALTOT")


# --- 4) Sélection des cibles & préparation du DF pour le plot -----------------

cibles <- c("FV_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids",
            "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids", "VIANDES_Poids")

df_plot_all <- tableau_final %>%
  filter(variable %in% cibles,
         !is.na(moyenne_Booklet), !is.na(moyenne_FFQ), !is.na(tertile)) %>%
  mutate(
    TertileLabel = fct_recode(
      base,
      "Weekly FFQ\u00A01 (Nov\u00A021)"   = "nudges1",
      "Weekly FFQ\u00A01 (March\u00A022)" = "nudges2",
      "Weekly FFQ\u00A02 (Nov\u00A022)"   = "IT11",
      "Weekly FFQ\u00A02 (March\u00A023)" = "IT12",
      "Weekly FFQ\u00A03 (Nov\u00A023)"   = "IT21",
      "Weekly FFQ\u00A03 (March\u00A024)" = "IT22",
      "Monthly FFQ"                       = "CSGA"
    ),
    variable_en = case_when(
      variable == "FV_Poids"                    ~ "Fruits & Vegetables",
      variable == "FEC_Poids"                   ~ "Starchy Foods",
      variable == "PDTS_LAITIERS_Poids"         ~ "Dairy Products",
      variable == "SSB_Poids"                   ~ "Sugary Sweet Beverages",
      variable == "PDTS_DISCRETIONNAIRES_Poids" ~ "Discretionary Foods",
      variable == "VIANDES_Poids"               ~ "Meats",
      TRUE                                      ~ variable
    ) %>% fct_relevel("Fruits & Vegetables","Starchy Foods","Dairy Products",
                      "Sugary Sweet Beverages","Discretionary Foods","Meats"),
    Campaign = case_when(
      str_detect(tertile, "^T1_ord$")   ~ "Bottom 20% (Supply data distribution)",
      str_detect(tertile, "^T2_ord$")   ~ "Middle 60% (Supply data distribution)",
      str_detect(tertile, "^T3_ord$")   ~ "Top 20% (Supply data distribution)",
      str_detect(tertile, "^Tall_ord$") ~ "All (Supply data distribution)"
    ) %>% factor(levels = c("All (Supply data distribution)",
                            "Top 20% (Supply data distribution)",
                            "Middle 60% (Supply data distribution)", 
                            "Bottom 20% (Supply data distribution)")),
    diff = moyenne_FFQ - moyenne_Booklet
  )
# >>> Ordre désiré des vagues (FFQ1 -> FFQ3 -> Monthly) <<<
wave_levels <- c(
  "Weekly FFQ\u00A01 (Nov\u00A021)",
  "Weekly FFQ\u00A01 (March\u00A022)",
  "Weekly FFQ\u00A02 (Nov\u00A022)",
  "Weekly FFQ\u00A02 (March\u00A023)",
  "Weekly FFQ\u00A03 (Nov\u00A023)",
  "Weekly FFQ\u00A03 (March\u00A024)",
  "Monthly FFQ"
)

# --- 5) Positions/esthétique & tracé -----------------------------------------

gap <- 2.2
off <- 0.25

df_v <- df_plot_all %>%
  mutate(
    Bin     = fct_relevel(Campaign, "All (Supply data distribution)","Top 20% (Supply data distribution)", 
                          "Middle 60% (Supply data distribution)", 
                          "Bottom 20% (Supply data distribution)"),
    Wave    = factor(TertileLabel, levels = wave_levels),  # <-- ordre imposé ici
    var_fac = factor(variable_en),
    var_ord = as.numeric(var_fac),
    W       = n_distinct(Wave),
    x_base  = var_ord * gap,
    x_pos   = x_base + (as.numeric(Wave) - (W + 1)/2) * off,
    delta   = moyenne_FFQ - moyenne_Booklet,
    has_change = !is.na(delta) & abs(delta) >= 1
  )

x_breaks <- df_v %>% distinct(var_ord, x_base) %>% arrange(var_ord) %>% pull(x_base)
x_labels <- levels(df_v$var_fac) %>% str_wrap(width = 16)
x_labels_spaced <- str_replace_all(x_labels, " ", "\u00A0\u00A0")
seps <- tibble(x = (x_breaks[-1] + x_breaks[-length(x_breaks)]) / 2)

okabe_ito <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
pal <- setNames(okabe_ito[seq_along(wave_levels)], wave_levels)  # <-- palette alignée à l’ordre

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
  facet_wrap(~ Bin, ncol = 1, scales = "fixed", strip.position = "top") +
  scale_color_manual(values = pal, breaks = wave_levels, name = "Wave") +  # <-- breaks = ordre
  scale_x_continuous(breaks = x_breaks, labels = x_labels_spaced,
                     expand = expansion(mult = c(0.04, 0.10))) +
  scale_y_continuous(
    name   = "Quantity (g/d/CU)",
    labels = scales::label_number(accuracy = 1, big.mark = " "),
    breaks = scales::breaks_pretty(n = 3),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  labs(title = "Comparison of FFQ and Supply Data Across Aggregated Food Categories", x = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    axis.title.x       = element_blank(),
    axis.text.x        = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, colour = "grey35"),
    axis.text.y        = element_text(size = 12, colour = "grey35"),
    axis.ticks.length  = unit(2, "pt"),
    axis.ticks         = element_line(linewidth = 0.2, colour = "grey70"),
    strip.text.y.left  = element_text(face = "bold"),
    panel.spacing.y    = unit(6, "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85", linetype = "solid")
  )




##GRAPH de différence par mesure--------------------------------------
sgsdata_complet <-read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Fichiers nettoyés/Fichiers traités/sgsdata_IT.xlsx", sep="")))
sgsdata_nudges_complet  <- read.xlsx((paste("C:/Users/denieul-barbot/Dropbox/Thèse/Article_4/Fichiers_nettoyés/Fichier_traité/sgsdata_nudges.xlsx", sep="")))
sgsdata_complet <- sgsdata_complet %>%mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"),2,1))
sgsdata_complet<- sgsdata_complet %>% arrange(Identifiant) %>%group_by(Identifiant) %>% fill(UC_TI, .direction = "downup") %>%ungroup()
sgsdata_nudges_complet<- sgsdata_nudges_complet %>%  arrange(Identifiant) %>%group_by(Identifiant) %>% fill(UC_TI, .direction = "downup") %>%ungroup()
fill_zero <- function(df) { df %>%mutate(across(everything(),~ ifelse(is.na(.) | . == "", 0, .)))}
sgsdata_complet            <- fill_zero(sgsdata_complet)
sgsdata_nudges_complet      <- fill_zero(sgsdata_nudges_complet)

unique (sgsdata_complet$Montant.mensuel.total)
sgsdata_complet <- sgsdata_complet %>%
  mutate(
    groupe = case_when(
      Montant.mensuel.total == "Ok" ~ 1,
      Montant.mensuel.total == "0" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(groupe == 0)

### SGSDATA Complet carnet -----------------------http://127.0.0.1:27837/graphics/8d000470-044b-44ee-9a45-a9d4d9f89e41.png
sgsdata_Booklet_IT <- sgsdata_complet %>%
  select(Identifiant, UC_TI ,Mesure,Campagne, Periode, groupe, ends_with("_CARNET_POIDS"))
#Verif groupe
sgsdata_Booklet_IT$FV_CARNET_Poids <- sgsdata_Booklet_IT$FRUITS_CARNET_Poids + sgsdata_Booklet_IT$FRUITS_SECS_CARNET_Poids  + sgsdata_Booklet_IT$NOIX_CARNET_Poids + sgsdata_Booklet_IT$LEGUMES_CARNET_Poids 
sgsdata_Booklet_IT$FEC_CARNET_Poids <- sgsdata_Booklet_IT$FEC_NON_RAF_CARNET_Poids + sgsdata_Booklet_IT$FEC_RAF_CARNET_Poids
sgsdata_Booklet_IT$PDTS_LAITIERS_CARNET_Poids <- sgsdata_Booklet_IT$LAIT_CARNET_Poids + sgsdata_Booklet_IT$LAITAGES_CARNET_Poids + sgsdata_Booklet_IT$FROMAGES_CARNET_Poids
sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Poids <- sgsdata_Booklet_IT$POULET_CARNET_Poids + sgsdata_Booklet_IT$OEUFS_CARNET_Poids
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Poids <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_CARNET_Poids  + sgsdata_Booklet_IT$JAMBON_BLANC_CARNET_Poids 
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_CARNET_Poids <- sgsdata_Booklet_IT$VIANDE_ROUGE_CARNET_Poids+ sgsdata_Booklet_IT$PORC_CARNET_Poids
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_CARNET_Poids <- sgsdata_Booklet_IT$SNACKS_AUTRES_CARNET_Poids +  sgsdata_Booklet_IT$CEREALES_PD_CARNET_Poids  + sgsdata_Booklet_IT$PDTS_SUCRES_CARNET_Poids 
sgsdata_Booklet_IT$SSB_CARNET_Poids <-  sgsdata_Booklet_IT$SODAS_SUCRES_CARNET_Poids + sgsdata_Booklet_IT$SODAS_LIGHT_CARNET_Poids +sgsdata_Booklet_IT$FRUITS_JUS_CARNET_Poids 

sgsdata_Booklet_IT$SOMME_HB_CARNET_Poids <-sgsdata_Booklet_IT$FV_CARNET_Poids + sgsdata_Booklet_IT$FEC_CARNET_Poids +
  sgsdata_Booklet_IT$PDTS_LAITIERS_CARNET_Poids + sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Poids +sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Poids +
  sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_CARNET_Poids + sgsdata_Booklet_IT$DESSERTS_LACTES_CARNET_Poids +
  sgsdata_Booklet_IT$QUICHES_PIZZAS_TARTES_SALEES_CARNET_Poids + sgsdata_Booklet_IT$MGA_CARNET_Poids + sgsdata_Booklet_IT$MGV_CARNET_Poids +
  sgsdata_Booklet_IT$POISSONS_CARNET_Poids + sgsdata_Booklet_IT$LEG_SECS_CARNET_Poids + sgsdata_Booklet_IT$PLATS_PREP_CARNES_CARNET_Poids + sgsdata_Booklet_IT$PLATS_PREP_VEGETARIENS_CARNET_Poids + sgsdata_Booklet_IT$SAUCES_CARNET_Poids
sgsdata_Booklet_IT$VIANDES_CARNET_Poids  <- sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Poids + sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Poids + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_CARNET_Poids 

sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  rename_with(
    ~ str_remove_all(.x, "_CARNET"),  .cols = everything())  %>%
  filter(if_any(ends_with("_Poids"), ~ . != 0))

###SGSDATA COMPLET FFQ -----------------------------------
sgsdata_complet_FFQ <- sgsdata_complet%>%
  select(Identifiant, UC_TI ,Mesure,Campagne, Periode, groupe, ends_with("_FFQ_Poids"))
sgsdata_complet_FFQ$FV_FFQ_Poids <- sgsdata_complet_FFQ$FRUITS_FFQ_Poids + sgsdata_complet_FFQ$FRUITS_SECS_FFQ_Poids  + sgsdata_complet_FFQ$NOIX_FFQ_Poids + sgsdata_complet_FFQ$LEGUMES_FFQ_Poids 
sgsdata_complet_FFQ$FEC_FFQ_Poids <- sgsdata_complet_FFQ$FEC_NON_RAF_FFQ_Poids + sgsdata_complet_FFQ$FEC_RAF_FFQ_Poids
sgsdata_complet_FFQ$PDTS_LAITIERS_FFQ_Poids <- sgsdata_complet_FFQ$LAIT_FFQ_Poids + sgsdata_complet_FFQ$LAITAGES_FFQ_Poids + sgsdata_complet_FFQ$FROMAGES_FFQ_Poids
sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Poids <- sgsdata_complet_FFQ$POULET_FFQ_Poids + sgsdata_complet_FFQ$OEUFS_FFQ_Poids
sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids <- sgsdata_complet_FFQ$CHARCUTERIE_HORS_JB_FFQ_Poids  + sgsdata_complet_FFQ$JAMBON_BLANC_FFQ_Poids 
sgsdata_complet_FFQ$VIANDE_ROUGE_PORC_FFQ_Poids <- sgsdata_complet_FFQ$VIANDE_ROUGE_FFQ_Poids+ sgsdata_complet_FFQ$PORC_FFQ_Poids
sgsdata_complet_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Poids <- sgsdata_complet_FFQ$SNACKS_AUTRES_FFQ_Poids +  sgsdata_complet_FFQ$CEREALES_PD_FFQ_Poids  + sgsdata_complet_FFQ$PDTS_SUCRES_FFQ_Poids
sgsdata_complet_FFQ$SSB_FFQ_Poids <-  sgsdata_complet_FFQ$SODAS_SUCRES_FFQ_Poids + sgsdata_complet_FFQ$SODAS_LIGHT_FFQ_Poids +sgsdata_complet_FFQ$FRUITS_JUS_FFQ_Poids 
sgsdata_complet_FFQ$SOMME_HB_FFQ_Poids <-sgsdata_complet_FFQ$FV_FFQ_Poids + sgsdata_complet_FFQ$FEC_FFQ_Poids +
  sgsdata_complet_FFQ$PDTS_LAITIERS_FFQ_Poids + sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Poids +sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids +
  sgsdata_complet_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Poids + sgsdata_complet_FFQ$DESSERTS_LACTES_FFQ_Poids +
  sgsdata_complet_FFQ$QUICHES_PIZZAS_TARTES_SALEES_FFQ_Poids + sgsdata_complet_FFQ$MGA_FFQ_Poids + sgsdata_complet_FFQ$MGV_FFQ_Poids +
  sgsdata_complet_FFQ$POISSONS_FFQ_Poids + sgsdata_complet_FFQ$LEG_SECS_FFQ_Poids + sgsdata_complet_FFQ$PLATS_PREP_CARNES_FFQ_Poids + sgsdata_complet_FFQ$PLATS_PREP_VEGETARIENS_FFQ_Poids + sgsdata_complet_FFQ$SAUCES_FFQ_Poids

sgsdata_complet_FFQ$VIANDES_FFQ_Poids  <- sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids + sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Poids + sgsdata_complet_FFQ$VIANDE_ROUGE_PORC_FFQ_Poids 


sgsdata_complet_FFQ <- sgsdata_complet_FFQ %>%
  rename_with(
    ~ str_remove_all(.x, "_FFQ"),
    .cols = everything()
  )  %>%
  # on ne garde que les lignes où au moins une colonne _Poids est non-zéro
  filter(
    if_any(ends_with("_Poids"), ~ . != 0))

### SGSDATA NUDGES CARNET -------------------------------
#sgsdata_nudges_Carnet <- sgsdata_nudges_complet %>%
#  select(Identifiant,UC_TI , Mesure,Periode, groupe, ends_with("_Poids")) %>%
#  rename_with(
#    ~ str_replace(.x, "_Poids$", "_CARNET_Poids"))#,
#  #  ends_with("_CARNET")) %>%
#  ## on ne garde que les lignes où au moins une colonne _Poids est non-zéro
#  #filter(
#  #  if_any(ends_with("_Poids"), ~ . != 0))
#    
#sgsdata_nudges_Carnet$FV_CARNET_Poids <- sgsdata_nudges_Carnet$FRUITS_CARNET_Poids + sgsdata_nudges_Carnet$FRUITS_SECS_CARNET_Poids  + sgsdata_nudges_Carnet$NOIX_CARNET_Poids + sgsdata_nudges_Carnet$LEGUMES_CARNET_Poids 
#sgsdata_nudges_Carnet$FEC_CARNET_Poids <- sgsdata_nudges_Carnet$FEC_NON_RAF_CARNET_Poids + sgsdata_nudges_Carnet$FEC_RAF_CARNET_Poids
#sgsdata_nudges_Carnet$PDTS_LAITIERS_CARNET_Poids <- sgsdata_nudges_Carnet$LAIT_CARNET_Poids + sgsdata_nudges_Carnet$LAITAGES_CARNET_Poids + sgsdata_nudges_Carnet$FROMAGES_CARNET_Poids
#sgsdata_nudges_Carnet$POULET_OEUFS_CARNET_Poids <- sgsdata_nudges_Carnet$POULET_CARNET_Poids + sgsdata_nudges_Carnet$OEUFS_CARNET_Poids
#sgsdata_nudges_Carnet$AUTRE_PDTS_ANIMAUX_CARNET_Poids <- sgsdata_nudges_Carnet$CHARCUTERIE_HORS_JB_CARNET_Poids  + sgsdata_nudges_Carnet$JAMBON_BLANC_CARNET_Poids 
#sgsdata_nudges_Carnet$VIANDE_ROUGE_PORC_CARNET_Poids <- sgsdata_nudges_Carnet$VIANDE_ROUGE_CARNET_Poids+ sgsdata_nudges_Carnet$PORC_CARNET_Poids
#sgsdata_nudges_Carnet$PDTS_DISCRETIONNAIRES_CARNET_Poids <- sgsdata_nudges_Carnet$SNACKS_AUTRES_CARNET_Poids +  sgsdata_nudges_Carnet$CEREALES_PD_CARNET_Poids  + sgsdata_nudges_Carnet$PDTS_SUCRES_CARNET_Poids 
#sgsdata_nudges_Carnet$SSB_CARNET_Poids <-  sgsdata_nudges_Carnet$SODAS_SUCRES_CARNET_Poids + sgsdata_nudges_Carnet$SODAS_LIGHT_CARNET_Poids +sgsdata_nudges_Carnet$FRUITS_JUS_CARNET_Poids 
#
#sgsdata_nudges_Carnet$SOMME_HB_CARNET_Poids <-sgsdata_nudges_Carnet$FV_CARNET_Poids + sgsdata_nudges_Carnet$FEC_CARNET_Poids +
#  sgsdata_nudges_Carnet$PDTS_LAITIERS_CARNET_Poids + sgsdata_nudges_Carnet$POULET_OEUFS_CARNET_Poids +sgsdata_nudges_Carnet$AUTRE_PDTS_ANIMAUX_CARNET_Poids +
#  sgsdata_nudges_Carnet$PDTS_DISCRETIONNAIRES_CARNET_Poids + sgsdata_nudges_Carnet$DESSERTS_LACTES_CARNET_Poids +
#  sgsdata_nudges_Carnet$QUICHES_PIZZAS_TARTES_SALEES_CARNET_Poids + sgsdata_nudges_Carnet$MGA_CARNET_Poids + sgsdata_nudges_Carnet$MGV_CARNET_Poids +
#  sgsdata_nudges_Carnet$POISSONS_CARNET_Poids + sgsdata_nudges_Carnet$LEG_SECS_CARNET_Poids + sgsdata_nudges_Carnet$PLATS_PREP_CARNES_CARNET_Poids + sgsdata_nudges_Carnet$PLATS_PREP_VEGETARIENS_CARNET_Poids + sgsdata_nudges_Carnet$SAUCES_CARNET_Poids
#
#sgsdata_nudges_Carnet$VIANDES_CARNET_Poids  <- sgsdata_nudges_Carnet$AUTRE_PDTS_ANIMAUX_CARNET_Poids + sgsdata_nudges_Carnet$POULET_OEUFS_CARNET_Poids + sgsdata_nudges_Carnet$VIANDE_ROUGE_PORC_CARNET_Poids 
#
#
#sgsdata_nudges_Carnet <- sgsdata_nudges_Carnet %>%
#  rename_with(
#    ~ str_remove_all(.x, "_CARNET"),
#    .cols = everything()
#  ) %>%
#  # on ne garde que les lignes où au moins une colonne _Poids est non-zéro
#  filter(
#    if_any(ends_with("_Poids"), ~ . != 0))
#
#### SGSDATA NUDGES FFQ -------------------------------
#sgsdata_nudges_FFQ <- sgsdata_nudges_complet  %>%
#  select(Identifiant, UC_TI ,Mesure, Periode, groupe, ends_with("_Poids") )%>%
#  rename_with(
#    ~ str_replace(.x, "_Poids$", "_FFQ_Poids"))
#
#
#
#sgsdata_nudges_FFQ$FV_FFQ_Poids <- sgsdata_nudges_FFQ$FRUITS_FFQ_Poids + sgsdata_nudges_FFQ$FRUITS_SECS_FFQ_Poids  + sgsdata_nudges_FFQ$NOIX_FFQ_Poids + sgsdata_nudges_FFQ$LEGUMES_FFQ_Poids 
#sgsdata_nudges_FFQ$FEC_FFQ_Poids <- sgsdata_nudges_FFQ$FEC_NON_RAF_FFQ_Poids + sgsdata_nudges_FFQ$FEC_RAF_FFQ_Poids
#sgsdata_nudges_FFQ$PDTS_LAITIERS_FFQ_Poids <- sgsdata_nudges_FFQ$LAIT_FFQ_Poids + sgsdata_nudges_FFQ$LAITAGES_FFQ_Poids + sgsdata_nudges_FFQ$FROMAGES_FFQ_Poids
#sgsdata_nudges_FFQ$POULET_OEUFS_FFQ_Poids <- sgsdata_nudges_FFQ$POULET_FFQ_Poids + sgsdata_nudges_FFQ$OEUFS_FFQ_Poids
#sgsdata_nudges_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids <- sgsdata_nudges_FFQ$CHARCUTERIE_HORS_JB_FFQ_Poids  + sgsdata_nudges_FFQ$JAMBON_BLANC_FFQ_Poids 
#sgsdata_nudges_FFQ$VIANDE_ROUGE_PORC_FFQ_Poids <- sgsdata_nudges_FFQ$VIANDE_ROUGE_FFQ_Poids+ sgsdata_nudges_FFQ$PORC_FFQ_Poids
#sgsdata_nudges_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Poids <- sgsdata_nudges_FFQ$SNACKS_AUTRES_FFQ_Poids +  sgsdata_nudges_FFQ$CEREALES_PD_FFQ_Poids  + sgsdata_nudges_FFQ$PDTS_SUCRES_FFQ_Poids 
#sgsdata_nudges_FFQ$SSB_FFQ_Poids <-  sgsdata_nudges_FFQ$SODAS_SUCRES_FFQ_Poids + sgsdata_nudges_FFQ$SODAS_LIGHT_FFQ_Poids +sgsdata_nudges_FFQ$FRUITS_JUS_FFQ_Poids 
#
#sgsdata_nudges_FFQ$SOMME_HB_FFQ_Poids <-sgsdata_nudges_FFQ$FV_FFQ_Poids + sgsdata_nudges_FFQ$FEC_FFQ_Poids +
#  sgsdata_nudges_FFQ$PDTS_LAITIERS_FFQ_Poids + sgsdata_nudges_FFQ$POULET_OEUFS_FFQ_Poids +sgsdata_nudges_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids +
#  sgsdata_nudges_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Poids + sgsdata_nudges_FFQ$DESSERTS_LACTES_FFQ_Poids +
#  sgsdata_nudges_FFQ$QUICHES_PIZZAS_TARTES_SALEES_FFQ_Poids + sgsdata_nudges_FFQ$MGA_FFQ_Poids + sgsdata_nudges_FFQ$MGV_FFQ_Poids +
#  sgsdata_nudges_FFQ$POISSONS_FFQ_Poids + sgsdata_nudges_FFQ$LEG_SECS_FFQ_Poids + sgsdata_nudges_FFQ$PLATS_PREP_CARNES_FFQ_Poids + sgsdata_nudges_FFQ$PLATS_PREP_VEGETARIENS_FFQ_Poids + sgsdata_nudges_FFQ$SAUCES_FFQ_Poids
#
#sgsdata_nudges_FFQ$VIANDES_FFQ_Poids <- sgsdata_nudges_FFQ$POULET_OEUFS_FFQ_Poids  + sgsdata_nudges_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids  + sgsdata_nudges_FFQ$VIANDE_ROUGE_PORC_FFQ_Poids
#
#
#
#sgsdata_nudges_FFQ <- sgsdata_nudges_FFQ %>%
#  rename_with(
#    ~ str_remove_all(.x, "_FFQ"),
#    .cols = everything()
#  ) %>%
#  # on ne garde que les lignes où au moins une colonne _Poids est non-zéro
#  filter(
#    if_any(ends_with("_Poids"), ~ . != 0))
#
#cAMPAGNE 1
sgsdata_Booklet_IT11  <- sgsdata_Booklet_IT %>% filter( Campagne == 1 , Periode ==0, groupe==0, Mesure== "Carnet",UC_TI ==1) 
sgsdata_Booklet_IT21 <-sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode ==1, groupe==0, Mesure== "Carnet",UC_TI ==1)  
sgsdata_Booklet_IT1 <- left_join(sgsdata_Booklet_IT11, sgsdata_Booklet_IT21, by="Identifiant")
sgsdata_Booklet_IT1 <- sgsdata_Booklet_IT1 %>%
  select(matches("Poids|Identifiant")) %>%
  # on ne garde que les lignes où au moins une colonne _Poids est non-zéro
  filter(if_any(matches("_Poids\\.(x|y)$"), ~ . != 0))

#FFQ CAMP 1 
sgsdata_FFQ_IT11 <- sgsdata_complet_FFQ %>% filter(Campagne == 1, Periode ==0, groupe==0, Mesure != "Carnet",UC_TI ==1) 
sgsdata_FFQ_IT21 <- sgsdata_complet_FFQ  %>% filter(Campagne == 1, Periode ==1 ,groupe==0, Mesure != "Carnet",UC_TI ==1)
sgsdata_FFQ_IT1 <- left_join(sgsdata_FFQ_IT11, sgsdata_FFQ_IT21, by="Identifiant")
sgsdata_FFQ_IT1<- sgsdata_FFQ_IT1 %>%
  select(matches("Poids|Identifiant"))

sgsdata_FFQ_IT1 <- sgsdata_FFQ_IT1 %>%
  filter(if_all(ends_with(".y"), ~ !is.na(.))) %>%
  semi_join(sgsdata_Booklet_IT1, by = "Identifiant")



sgsdata_Booklet_IT1  <-sgsdata_Booklet_IT1 %>%
  filter(if_all(ends_with(".y"), ~ !is.na(.))) %>%
  semi_join(sgsdata_FFQ_IT1, by = "Identifiant")



#CAMP 2 
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2 , Periode ==0, groupe==0, Mesure== "Carnet",UC_TI ==1)  
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode ==1, groupe==0, Mesure== "Carnet",UC_TI ==1)   
sgsdata_Booklet_IT2 <- left_join(sgsdata_Booklet_IT12, sgsdata_Booklet_IT22, by="Identifiant")
sgsdata_Booklet_IT2 <- sgsdata_Booklet_IT2 %>%
  select(matches("Poids|Identifiant")) %>%
  # on ne garde que les lignes où au moins une colonne _Poids est non-zéro
  filter(if_any(matches("_Poids\\.(x|y)$"), ~ . != 0))


sgsdata_FFQ_IT12<- sgsdata_complet_FFQ %>% filter(Campagne == 2, Periode ==0, groupe==0, Mesure != "Carnet",UC_TI ==1)  
sgsdata_FFQ_IT22 <-sgsdata_complet_FFQ %>% filter(Campagne == 2, Periode ==1 ,groupe==0,  Mesure != "Carnet",UC_TI ==1) 
sgsdata_FFQ_IT2 <- left_join(sgsdata_FFQ_IT12, sgsdata_FFQ_IT22, by="Identifiant")
sgsdata_FFQ_IT2 <- sgsdata_FFQ_IT2 %>%
  select(matches("Poids|Identifiant"))

sgsdata_FFQ_IT2 <- sgsdata_FFQ_IT2 %>%
  filter(if_all(ends_with(".y"), ~ !is.na(.))) %>%
  semi_join(sgsdata_Booklet_IT2, by = "Identifiant")



sgsdata_Booklet_IT2 <-sgsdata_Booklet_IT2 %>%
  filter(if_all(ends_with(".y"), ~ !is.na(.))) %>%
  semi_join(sgsdata_FFQ_IT2, by = "Identifiant")

#sgsdata_Booklet_nudges1 <- sgsdata_nudges_Carnet %>% filter(Periode ==0,str_detect(Identifiant, "Epimut", ),  Mesure== "Carnet",UC_TI ==1)
#sgsdata_Booklet_nudges2 <-sgsdata_nudges_Carnet %>% filter(Periode ==1,str_detect(Identifiant, "Epimut"), Mesure== "Carnet",UC_TI ==1)  
#sgsdata_Booklet_nudges_vf <- left_join(sgsdata_Booklet_nudges1, sgsdata_Booklet_nudges2, by="Identifiant")
#sgsdata_Booklet_nudges_vf <- sgsdata_Booklet_nudges_vf %>%
#  select(matches("Poids|Identifiant"))
#
#sgsdata_FFQ_nudges1 <- sgsdata_nudges_FFQ %>% filter( Periode ==0,str_detect(Identifiant, "Epimut"), Mesure != "Carnet",UC_TI ==1)
#sgsdata_FFQ_nudges2 <-sgsdata_nudges_FFQ %>% filter(Periode ==1,str_detect(Identifiant, "Epimut") , Mesure != "Carnet",UC_TI ==1) 
#sgsdata_FFQ_nudges_vf <- left_join(sgsdata_FFQ_nudges1, sgsdata_FFQ_nudges2, by="Identifiant")
#sgsdata_FFQ_nudges_vf <- sgsdata_FFQ_nudges_vf %>%
#  select(matches("Poids|Identifiant"))
#
#sgsdata_FFQ_nudges_vf <- sgsdata_FFQ_nudges_vf %>%
#  semi_join(sgsdata_Booklet_nudges_vf , by ="Identifiant")
#
#sgsdata_Booklet_nudges_vf <- sgsdata_Booklet_nudges_vf %>%
#  semi_join(sgsdata_FFQ_nudges_vf , by ="Identifiant")



#sgsdata_FFQ_nudges_vf <-sgsdata_FFQ_nudges_vf  %>%
#  semi_join(sgsdata_Booklet_nudges_vf , by ="Identifiant")
sgsdata_FFQ_IT1 <-sgsdata_FFQ_IT1 %>%
  semi_join(sgsdata_Booklet_IT1, by = "Identifiant")



sgsdata_FFQ_IT2 <-sgsdata_FFQ_IT2%>%
  semi_join(sgsdata_Booklet_IT2, by = "Identifiant")


process_data <- function(df) {
  # lookup pour renommer les colonnes poids
  traductions <- c(
    VIANDES_Poids = "Meats",
    CAFE_THE_Poids = "Coffee / Tea",
    CEREALES_PD_Poids                          = "Breakfast cereals",
    CHARCUTERIE_HORS_JB_Poids                   = "Cold cuts excluding white ham",
    DESSERTS_LACTES_Poids                       = "Dairy Desserts",
    FEC_NON_RAF_Poids                           = "Unrefined Starches",
    FEC_RAF_Poids                               = "Refined Starches",
    FROMAGES_Poids                              = "Cheeses",
    FRUITS_Poids                                = "Fruits",
    FRUITS_SECS_Poids                           = "Dried Fruits",
    JAMBON_BLANC_Poids                          = "White Ham",
    LAITAGES_Poids                              = "Dairy Products",
    LEGUMES_Poids                               = "Vegetables",
    LEG_SECS_Poids                              = "Legumes",
    MGA_Poids                                   = "Animal Fats",
    MGV_Poids                                   = "Vegetable Fats",
    NOIX_Poids                                  = "Nuts",
    OEUFS_Poids                                 = "Eggs",
    PDTS_SUCRES_Poids                           = "Sweet products",
    PLATS_PREP_CARNES_Poids                     = "Meat Based Prepared Dishes",
    PLATS_PREP_VEGETARIENS_Poids                = "Vegetarian Prepared Dishes",
    POISSONS_Poids                              = "Fish",
    PORC_Poids                                  = "Pork",
    POULET_Poids                                = "Chicken",
    QUICHES_PIZZAS_TARTES_SALEES_Poids          = "Quiches/ Pizzas/ Savoury Pies",
    SAUCES_Poids                                = "Sauces",
    SNACKS_AUTRES_Poids                         = "Other Snacks",
    VIANDE_ROUGE_Poids                          = "Red Meat",
    ALCOOL_Poids                                = "Alcohol",
    EAU_Poids                                   = "Water",
    FRUITS_JUS_Poids                            = "Fruit Juices",
    LAIT_Poids                                  = "Milk",
    SODAS_LIGHT_Poids                           = "Diet Sodas",
    SODAS_SUCRES_Poids                          = "Sugary Sodas",
    FV_Poids  = "Fruits/vegetables",
    FEC_Poids  = "Starchy foods",
    PDTS_LAITIERS_Poids = "Dairy products",
    POULET_OEUFS_Poids = "Eggs / chicken",
    AUTRE_PDTS_ANIMAUX_Poids ="Cold cuts",
    PLATS_PREP_Poids ="Prepared dishes",
    VIANDE_ROUGE_PORC_Poids ="Red meat/Pork",
    MG_Poids = "Added fats",
    PDTS_DISCRETIONNAIRES_Poids  = "Discretionary foods",
    SSB_Poids  ="Sugary sweet beverages",
    SOMME_HB = "Total without beverages"
    
    
    
  )
  
  df %>%
    # 1) ne conserver que Poids + Identifiant
    select(matches("Poids|Identifiant|KCAL")) %>%
    # 2) moyenne des colonnes Poids
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
     -starts_with("SOMME_POURCENT_Poids"),
   -starts_with("total_Poids"),
  -starts_with("SOMME_Poids_HB"),
 -starts_with("SOMME_POIDS_HB"),
   -starts_with("SOMME_Poids"),
  -starts_with("SOMME_POIDS"),
   -starts_with("SOMME_FFQ_KCAL"),
   -starts_with("SOMME_CARNET_KCAL"),
   -starts_with("PROP_Poids_EPIC"),
    -starts_with("POIDS_EPIC"),
    -starts_with("SOMME_FFQ"),
    -starts_with("SOMME_CARNET"),
     #-starts_with("SSB_Poids"),
     #-starts_with("FEC_Poids"),
     #-starts_with("PDTS_DISCRETIONNAIRES_Poids"),
     #-starts_with("FV_Poids"),
     -starts_with("MG_Poids"),
    -starts_with("AUTRE_PDTS_ANIMAUX_Poids"),
    # -starts_with("PDTS_LAITIERS_Poids"),
     -starts_with("PLATS_PREP_Poids"),
     -starts_with("POULET_OEUFS_Poids"),
     -starts_with("VIANDE_ROUGE_PORC_Poids"),
    -starts_with("POISSONS_Poids"),
      -starts_with("ALCOOL_Poids"),
     -starts_with("EPICES_CONDIMENTS_Poids"),
     -starts_with("CAFE_THE_Poids"),
     -starts_with("EAU_Poids"),
     -starts_with("LAIT_Poids"),
  -starts_with("FRUITS_JUS_Poids"),
  -starts_with("SODAS_LIGHT_Poids"),
  -starts_with("SODAS_SUCRES_Poids"),
  #-starts_with("VIANDES_Poids"),
  -starts_with("SAUCES_Poids"), 
  -starts_with("CHARCUTERIE_HORS_JB_Poids"),
   -starts_with("PLATS_PREP_CARNES_Poids"),
  -starts_with("PLATS_PREP_VEGETARIENS_Poids"),
  -starts_with("QUICHES_PIZZAS_TARTES_SALEES_Poids"),
  -starts_with("PORC_Poids"),
  -starts_with("MGA_Poids"),
  -starts_with("MGV_Poids"),
 -starts_with("OEUFS_Poids"),
  -starts_with("JAMBON_BLANC_Poids"),
  -starts_with("CEREALES_PD_Poids"),
  -starts_with("NOIX_Poids"),
  -starts_with("SNACKS_AUTRES_Poids"),
  -starts_with("LAITAGES_Poids"),
  -starts_with("FROMAGES_Poids"),
  -starts_with("FRUITS_Poids"),
 -starts_with("DESSERTS_LACTES_Poids"),
-starts_with("FEC_NON_RAF_Poids"), 
-starts_with("PDTS_SUCRES_Poids"),
-starts_with("LEGUMES_Poids"),
  -starts_with("VIANDE_ROUGE_Poids"),
  -starts_with("FEC_RAF_Poids"),
  -starts_with("POULET_Poids"),
 -starts_with("LEG_SECS_Poids"),
  -starts_with("FRUITS_SECS_Poids"),
-starts_with("SOMME_HB"),
-starts_with("SOMME_POIDS"),
-starts_with("SOMME_HORS_BOISSON")

      
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
    ) %>%
    # 6) Passer en grammes et garder 2 chiffres significatifs
    mutate(across(everything(), ~ signif(.x * 1000, 2))) #Passer en grammes et garder 2 chiffres significatifs) Passer en grammes et garder 2 chiffres significatifs
}





sgsdata_Booklet_IT1     <- process_data(sgsdata_Booklet_IT1)
sgsdata_FFQ_IT1         <- process_data(sgsdata_FFQ_IT1)
sgsdata_Booklet_IT2     <- process_data(sgsdata_Booklet_IT2)
sgsdata_FFQ_IT2         <- process_data(sgsdata_FFQ_IT2)
#sgsdata_Booklet_nudges_vf <- process_data(sgsdata_Booklet_nudges_vf)
#sgsdata_FFQ_nudges_vf <- process_data(sgsdata_FFQ_nudges_vf)
#



make_df_plot <- function(df_summary) {
  library(dplyr); library(stringr); library(tibble)

  bases <- names(df_summary) %>%
    str_subset("\\.x$") %>%
    str_remove("\\.x$")

  if (length(bases) == 0) {
    stop("Aucune colonne '.x' trouvée. Tu as probablement supprimé/renommé les colonnes '.x' dans process_data() ou avant.")
  }

  tibble(
    base_raw = bases,
    x        = unlist(df_summary[paste0(bases, ".x")],    use.names = FALSE),
    diff     = unlist(df_summary[paste0(bases, "_diff")], use.names = FALSE)
  ) %>%
    mutate(
      base_clean  = base_raw %>%
        str_remove("_Poids$") %>%
        str_remove("POIDS$") %>%
        str_replace_all("_", " ") %>%
        str_to_lower() %>%
        str_to_sentence(),
      signe       = if_else(diff >= 0, "Rise", "Decrease"),
      diff_label  = sprintf("%+.2f", diff),
      x_label_pos = max(x + diff, na.rm = TRUE) * 1.02
    ) %>%
    { 
      ordered_levels <- if (nrow(.) > 1) .$base_clean[order(-abs(.$diff))] else .$base_clean
      mutate(., base = factor(base_clean, levels = unique(ordered_levels)))
    } %>%
    select(base, x, diff, signe, diff_label, x_label_pos)
}


sgsdata_Booklet_IT1     <- make_df_plot(sgsdata_Booklet_IT1)
sgsdata_FFQ_IT1         <- make_df_plot(sgsdata_FFQ_IT1)
sgsdata_Booklet_IT2     <- make_df_plot(sgsdata_Booklet_IT2)
sgsdata_FFQ_IT2         <- make_df_plot(sgsdata_FFQ_IT2)
#sgsdata_Booklet_nudges_vf <- make_df_plot(sgsdata_Booklet_nudges_vf)
#sgsdata_FFQ_nudges_vf <- make_df_plot(sgsdata_FFQ_nudges_vf)

# 1) On regroupe vos 6 data.frames df_plot dans une liste nommée
list_df_plot <- list(
  Booklet_Winter_23    = sgsdata_Booklet_IT1,
  FFQ_Winter_23        = sgsdata_FFQ_IT1,
  Booklet_Winter_24    = sgsdata_Booklet_IT2,
  FFQ_Winter_24        = sgsdata_FFQ_IT2#,
  #Booklet_Winter_22  = sgsdata_Booklet_nudges_vf,
  #FFQ_22   = sgsdata_FFQ_nudges_vf
)



# on crée 3 petits tables avec la colonne campaign
#df_booklet_22 <- sgsdata_Booklet_nudges_vf %>%  mutate(campaign = "Booklet Winter 22")
df_booklet_23 <- sgsdata_Booklet_IT1 %>%        mutate(campaign = "Booklet Winter 23")
df_booklet_24 <- sgsdata_Booklet_IT2 %>%        mutate(campaign = "Booklet Winter 24")

# on fusionne
df_booklet_all <- bind_rows(#df_booklet_22,
                            df_booklet_23,
                            df_booklet_24)
#df_ffq_22 <- sgsdata_FFQ_nudges_vf %>% mutate(campaign = "FFQ 22")
df_ffq_23 <- sgsdata_FFQ_IT1       %>% mutate(campaign = "FFQ Winter 23")
df_ffq_24 <- sgsdata_FFQ_IT2       %>% mutate(campaign = "FFQ Winter 24")

df_ffq_all <- bind_rows(#df_ffq_22,
                        df_ffq_23,
                        df_ffq_24)


df_booklet_all <- df_booklet_all %>%
  filter(!str_detect(as.character(base), regex("total|hors boisson|without beverages", ignore_case = TRUE)))

df_ffq_all <- df_ffq_all %>%
  filter(!str_detect(as.character(base), regex("total|hors boisson|without beverages", ignore_case = TRUE)))

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
    labs(title = titre, x = NULL, y = "Consumption [g/d/CU]") +
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
  #"FFQ 22"            = "#1b9e77",
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
  FFQ_IT2           = sgsdata_FFQ_IT2
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
addWorksheet(wb, "tableau_final")
writeData(wb, sheet = "tableau_final", tableau_final)


addWorksheet(wb, "wide_pct")
writeData(wb, sheet = "wide_pct", wide_pct)


saveWorkbook(wb,"C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Comparaison_vf.xlsx")

