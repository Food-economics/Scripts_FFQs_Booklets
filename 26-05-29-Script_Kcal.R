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
sgsdata_FFQ_CSGA$FV_Kcal <- sgsdata_FFQ_CSGA$FRUITS_Kcal + sgsdata_FFQ_CSGA$FRUITS_SECS_Kcal  + sgsdata_FFQ_CSGA$NOIX_Kcal + sgsdata_FFQ_CSGA$LEGUMES_Kcal 
sgsdata_FFQ_CSGA$FEC_Kcal <- sgsdata_FFQ_CSGA$FEC_NON_RAF_Kcal + sgsdata_FFQ_CSGA$FEC_RAF_Kcal
sgsdata_FFQ_CSGA$PDTS_LAITIERS_Kcal <- sgsdata_FFQ_CSGA$LAIT_Kcal + sgsdata_FFQ_CSGA$LAITAGES_Kcal + sgsdata_FFQ_CSGA$FROMAGES_Kcal
sgsdata_FFQ_CSGA$POULET_OEUFS_Kcal <- sgsdata_FFQ_CSGA$POULET_Kcal + sgsdata_FFQ_CSGA$OEUFS_Kcal
sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Kcal <- sgsdata_FFQ_CSGA$CHARCUTERIE_HORS_JB_Kcal 
sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Kcal <- sgsdata_FFQ_CSGA$VIANDE_ROUGE_Kcal+ sgsdata_FFQ_CSGA$PORC_Kcal
sgsdata_FFQ_CSGA$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_CSGA$SNACKS_AUTRES_Kcal +  sgsdata_FFQ_CSGA$CEREALES_PD_Kcal + sgsdata_FFQ_CSGA$PDTS_SUCRES_Kcal
sgsdata_FFQ_CSGA$SSB_Kcal <-  sgsdata_FFQ_CSGA$SODAS_SUCRES_Kcal + sgsdata_FFQ_CSGA$SODAS_LIGHT_Kcal  + sgsdata_FFQ_CSGA$FRUITS_JUS_Kcal 
sgsdata_FFQ_CSGA$SOMME_KCAL_Kcal <- sgsdata_FFQ_CSGA$KCAL_TOTAL_Kcal
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
sgsdata_Booklet_CSGA$SOMME_KCAL_Kcal <- sgsdata_Booklet_CSGA$KCAL_TOTAL_Kcal
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
sgsdata_FFQ_IT$SOMME_KCAL_Kcal <- sgsdata_FFQ_IT$KCAL_TOTAL_Kcal
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
sgsdata_Booklet_IT$SOMME_KCAL_Kcal <- sgsdata_Booklet_IT$KCAL_TOTAL_Kcal
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


analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Kcal"),
                              multiplier = 1,
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
  
  # --- 2) Préparer listes pour transformation (exclure explicitement SOMME_KCAL_Poids) ---
  kcal_ffq      <- grep("_Kcal$",  vars_ffq,     value = TRUE)
  kcal_booklet  <- grep("_Kcal$",  vars_booklet, value = TRUE)
  
  # --- 3) Moyennes FFQ (long) avec ajustements d’unités (hors SOMME_KCAL_Poids) ---
  moy_ffq <- ffq_data %>%
    select(all_of(vars_ffq)) %>%

    {
      if (length(kcal_ffq) > 0)      mutate(., across(all_of(kcal_ffq),  ~ .x))       else .
    } %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_FFQ")
  
  # --- 4) Moyennes Booklet (long) avec ajustements d’unités (hors SOMME_KCAL_Poids) ---
  moy_booklet <- booklet_data %>%
    select(all_of(vars_booklet)) %>%

    {
      if (length(kcal_booklet) > 0)  mutate(., across(all_of(kcal_booklet),  ~ .x ))       else .
    } %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_Booklet")
  
  # --- 5) Base jointure pour stats ---
  tableau_base <- left_join(moy_booklet, moy_ffq, by = "variable")
  
  # --- 6) Stats par variable ---
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
  
  # --- 7) Assemblage final ---
  tableau_final <- stats %>%
    select(
      variable,
      n_pairs,
      moyenne_Booklet, ci95_Booklet,
      moyenne_FFQ,     ci95_FFQ,
      pct_bias, p_value_diff,
      pearson_correlation,  pearson_p_value,
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
  #"POULET_OEUFS_Kcal", 
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
  "CAFE_THE_Kcal",
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
  filter(!str_ends(variable, "_Poids"))

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
  CEREALES_PD_Kcal = "Breakfast cereals",
  CAFE_THE_Kcal = "Coffee and tea",
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
  FEC_Kcal = "Starchy foods",
  PDTS_LAITIERS_Kcal = "Dairy products",
  #POULET_OEUFS_Kcal = "Eggs / chicken",
  AUTRE_PDTS_ANIMAUX_Kcal = "Cold cuts",
  VIANDE_ROUGE_PORC_Kcal = "Red meat/Pork",
  PDTS_DISCRETIONNAIRES_Kcal = "Discretionary foods",
  SSB_Kcal = "Sugary sweet beverages",
  SOMME_KCAL_Kcal = "Total kilocalories",
  VIANDES_Kcal = "Meats"
)

# —————————————————————————
# Préparation df_all : facettes, périodes, recodage Missing
df_all <- heat_df %>%
  mutate(
    # catégories pour les facettes
    category = case_when(
      variable %in% c( "SOMME_KCAL_Kcal") ~ "General\nindicator",
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
  labs(title = "Correlation of energy intake Variables", x = NULL, y = NULL) +
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





saveWorkbook(wb,(paste0("C:/Users/denieul-barbot/Dropbox/Thèse/Article_3/Données analyses - Article N°2 FFQvsCarnets/Comparaison_Kcal_vf.xlsx")))
