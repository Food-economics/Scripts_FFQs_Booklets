#Importing packages -------------------
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


sgsdata_CSGA  <- read.xlsx((paste("sgsdata.xlsx", sep="")))
sgsdata_TI <-read.xlsx((paste("sgsdata_IT.xlsx", sep="")))
sgsdata_nudges  <- read.xlsx((paste("sgsdata_nudges.xlsx", sep="")))

FFQ_NOV_23 <- read.xlsx((paste("23-11_FFQ.xlsx", sep="")))
sgsdata_TI <- sgsdata_TI 
#Indicate the campaign
sgsdata_TI <- sgsdata_TI %>%mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"),2,1))

#Fill in the UC data if missing
sgsdata_TI <- sgsdata_TI %>%
  arrange(Identifiant) %>%group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>%ungroup()

#Fill empty cells with 0
fill_zero <- function(df) { df %>%mutate(across(everything(),~ ifelse(is.na(.) | . == "", 0, .)))}

sgsdata_TI             <- fill_zero(sgsdata_TI)
sgsdata_CSGA      <- fill_zero(sgsdata_CSGA)
sgsdata_nudges <- fill_zero(sgsdata_nudges)


sgsdata_FFQ_CSGA <- sgsdata_CSGA %>% filter(Mesure != "Carnet") 
sgsdata_Booklet_CSGA <- sgsdata_CSGA %>% filter(Mesure == "Carnet")


#WHITE HAM INCLUDED IN deli meats in CSGA / MGV removed / Vegetarian ready meals / Sauces and dairy desserts are removed because they are not present in nudges
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



#SELECTING THE TI DATAFRAMES -- Campaign 2 
sgsdata_FFQ_IT <- sgsdata_TI %>%filter(Mesure != "Carnet", UC_TI ==1) 

sgsdata_Booklet_IT <- sgsdata_TI %>% filter(Mesure == "Carnet", UC_TI ==1)

sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  filter(
    # keep the rows where AT LEAST one "CARNET" column is non-NA AND non-zero
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



#SELECTING THE NUDGES DATAFRAMES
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




#CAMPAIGN 1
sgsdata_Booklet_IT11 <- sgsdata_Booklet_IT %>% filter(Campagne == 1 , Periode ==0) 
sgsdata_Booklet_IT21 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode ==1)  
sgsdata_FFQ_IT11 <- sgsdata_FFQ_IT %>% filter(Campagne == 1, Periode ==0) 
sgsdata_FFQ_IT21 <- sgsdata_FFQ_IT %>% filter(Campagne == 1, Periode ==1)

#CAMPAIGN 2
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2 , Periode ==0) 
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode ==1)  
sgsdata_FFQ_IT12 <- sgsdata_FFQ_IT %>% filter(Campagne == 2, Periode ==0) 
sgsdata_FFQ_IT22 <- sgsdata_FFQ_IT %>% filter(Campagne == 2, Periode ==1) 

#NUDGES
sgsdata_Booklet_nudges1 <- sgsdata_Booklet_nudges %>% filter(Periode ==0) 
sgsdata_Booklet_nudges2 <- sgsdata_Booklet_nudges %>% filter(Periode ==1)  
sgsdata_FFQ_nudges1 <- sgsdata_FFQ_nudges %>% filter(Periode ==0) 
sgsdata_FFQ_nudges2 <- sgsdata_FFQ_nudges %>% filter(Periode ==1)

#Build a large dataframe for the FFQs
#ALL
dfs <- list(
  nudges1 = sgsdata_FFQ_nudges1,
  nudges2 = sgsdata_FFQ_nudges2,
  IT11    = sgsdata_FFQ_IT11,
  IT12    = sgsdata_FFQ_IT12,
  IT21    = sgsdata_FFQ_IT21,
  IT22    = sgsdata_FFQ_IT22
)

# 1) Columns common to ALL data.frames
common_cols <- Reduce(intersect, lapply(dfs, names))

sgsdata_FFQ_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")#mAKE sure the same ids appear in each pair 



#Build a large dataframe for the CARNETs
#ALL
dfs <- list(
  nudges1 = sgsdata_Booklet_nudges1,
  nudges2 = sgsdata_Booklet_nudges2,
  IT11    = sgsdata_Booklet_IT11,
  IT12    = sgsdata_Booklet_IT12,
  IT21    = sgsdata_Booklet_IT21,
  IT22    = sgsdata_Booklet_IT22
)

# 1) Columns common to ALL data.frames
common_cols <- Reduce(intersect, lapply(dfs, names))

sgsdata_Booklet_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")#mAKE sure the same ids appear in each pair 





# Function to filter the two dataframes of a pair
harmoniser_ids <- function(df1, df2, id_col = "Identifiant") {
  ids_communs <- intersect(df1[[id_col]], df2[[id_col]])
  df1_filtre <- df1 %>% filter(.data[[id_col]] %in% ids_communs)
  df2_filtre <- df2 %>% filter(.data[[id_col]] %in% ids_communs)
  list(df1 = df1_filtre, df2 = df2_filtre)
}



# Applying it to each pair
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
  
  # --- 0) Alignment if Identifiant is present ---
  if ("Identifiant" %in% names(ffq_data) && "Identifiant" %in% names(booklet_data)) {
    commun_ids   <- intersect(ffq_data$Identifiant, booklet_data$Identifiant)
    ffq_data     <- ffq_data     %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
    booklet_data <- booklet_data %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
  }
  if (nrow(ffq_data) != nrow(booklet_data)) {
    stop("ffq_data et booklet_data n'ont pas le même nombre de lignes après alignement.")
  }
  
  # --- 1) Columns targeted by suffix ---
  motif        <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  vars_ffq     <- grep(motif, names(ffq_data),     value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  
  if (length(vars_ffq) == 0)     stop("Aucune colonne FFQ ne se termine par ", paste(suffixes, collapse = ", "))
  if (length(vars_booklet) == 0) stop("Aucune colonne Booklet ne se termine par ", paste(suffixes, collapse = ", "))
  
  # --- 2) Preparing lists for transformation (explicitly excluding SOMME_KCAL_Poids) ---
  kcal_ffq      <- grep("_Kcal$",  vars_ffq,     value = TRUE)
  kcal_booklet  <- grep("_Kcal$",  vars_booklet, value = TRUE)
  
  # --- 3) FFQ averages (long) with unit adjustments (excluding SOMME_KCAL_Poids) ---
  moy_ffq <- ffq_data %>%
    select(all_of(vars_ffq)) %>%
    
    {
      if (length(kcal_ffq) > 0)      mutate(., across(all_of(kcal_ffq),  ~ .x))       else .
    } %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_FFQ")
  
  # --- 4) Booklet averages (long) with unit adjustments (excluding SOMME_KCAL_Poids) ---
  moy_booklet <- booklet_data %>%
    select(all_of(vars_booklet)) %>%
    
    {
      if (length(kcal_booklet) > 0)  mutate(., across(all_of(kcal_booklet),  ~ .x ))       else .
    } %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_Booklet")
  
  # --- 5) Join base for the stats ---
  tableau_base <- left_join(moy_booklet, moy_ffq, by = "variable")
  
  # --- 6) Stats per variable ---
  # --- 3) Statistics per variable, on complete pairs ---
  stats <- lapply(tableau_base$variable, function(var) {
    
    x_full <- ffq_data[[var]]
    y_full <- booklet_data[[var]]
    
    # Keep only the individuals with both FFQ AND Booklet not missing
    valid <- !is.na(x_full) & !is.na(y_full)
    
    x <- x_full[valid] # FFQ
    y <- y_full[valid] # Booklet
    
    n_pairs <- length(x)
    
    # If not enough observations
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
    
    # --- 3.1) Unit adjustment ---
    # Converting kg -> g for the weight variables,
    # except SOMME_KCAL_Poids which must not be multiplied by 1000.
    if (grepl("_Poids$", var) && var != "SOMME_KCAL_Poids") {
      x <- x * multiplier
      y <- y * multiplier
    }
    
    # --- 3.2) Averages on the same individuals ---
    moyenne_FFQ <- mean(x)
    moyenne_Booklet <- mean(y)
    
    # --- 3.3) Percentage bias: FFQ vs Booklet ---
    pct_bias <- ifelse(
      moyenne_Booklet == 0,
      NA,
      (moyenne_FFQ - moyenne_Booklet) / moyenne_Booklet * 100
    )
    
    # --- 3.4) t-tests and confidence intervals ---
    # Paired test: both measures come from the same individuals
    t_diff <- t.test(x, y, paired = TRUE)
    
    # Separate CIs for the FFQ and Booklet averages
    t_ffq <- t.test(x, conf.level = conf_level)
    t_book <- t.test(y, conf.level = conf_level)
    
    # --- 3.5) Pearson and Spearman correlations ---
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
    
    # --- 3.6) Quintile agreement ---
    qx <- dplyr::ntile(x, 5)
    qy <- dplyr::ntile(y, 5)
    
    d <- abs(qx - qy)
    
    pct_similar <- mean(d == 0, na.rm = TRUE) * 100
    
    # Here, you keep your broad definition:
    # a gap of 1 or 2 quintiles.
    # If you want "strictly adjacent", replace c(1, 2) with 1.
    pct_adjacent <- mean(d %in% c(1, 2), na.rm = TRUE) * 100
    
    pct_opposite <- mean(d %in% c(3, 4), na.rm = TRUE) * 100
    
    # --- 3.7) Result for the variable ---
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
  
  # --- 7) Final assembly ---
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




#SAVE FOR LATER
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



# Run the analyses for the two periods
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




# 1) Complete vector of the variables in the desired order
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

# 2) Creating a template (data.frame or tibble) to join to each table
library(dplyr)
template <- tibble(variable = variables)

# 3) Function to reorder + calculate Delta + place Delta in 6th position
reorder_and_delta <- function(df){
  df_ord <- template %>%
    left_join(df, by = "variable") %>%                    # keeps all the variables
    mutate(Delta = moyenne_FFQ - moyenne_Booklet) %>%     # calculates Delta
    relocate(Delta, .after = 5)                           # places it in the 6th column
  return(df_ord)
}

# 4) Applying it to the three tables
tableau1_ord <- reorder_and_delta(tableau_final_1)
tableau2_ord <- reorder_and_delta(tableau_final_2)
tableau3_ord <- reorder_and_delta(tableau_final_3)
tableau4_ord <- reorder_and_delta(tableau_final_4)
tableau5_ord <- reorder_and_delta(tableau_final_5)
tableau6_ord <- reorder_and_delta(tableau_final_6)
tableau7_ord <- reorder_and_delta(tableau_final_7)




### Weight correlation graphs ----------------------------------------------------------


# Reusable function to avoid repeating the code across the 7 tables -------------------
classer_correlation <- function(df) {
  df %>%
    mutate(
      classe = case_when(
        pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ 
          "Intensely: both correlation coefficients are x \u2265 0.6, and the means are not statistically different.",
        
        ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4 & spearman_correlation < 0.6) | 
           (spearman_correlation >= 0.6 & pearson_correlation >= 0.4 & pearson_correlation < 0.6)) & 
          p_value_diff >= 0.05 ~ 
          "Strongly: one of the two correlation coefficients is x \u2265 0.6 and the other is 0.4 \u2264 x < 0.6, and the means are not statistically different.",
        
        pearson_correlation >= 0.4 & pearson_correlation < 0.6 & 
          spearman_correlation >= 0.4 & spearman_correlation < 0.6 & 
          p_value_diff >= 0.05 ~ 
          "Substantially: both correlation coefficients are 0.4 \u2264 x < 0.6, and the means are not statistically different.",
        
        (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ 
          "Moderately: at least one correlation coefficient is x \u2265 0.4, and the means are not statistically different.",
        
        (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ 
          "Poorly: at least one correlation coefficient is x \u2265 0.4, but the means are statistically different.",
        
        spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ 
          "Weakly: neither correlation coefficient reaches x \u2265 0.4.",
        
        is.na(p_value_diff) ~ 
          "Missing"
      )
    )
}

tableau1_ord <- classer_correlation(tableau1_ord)
tableau2_ord <- classer_correlation(tableau2_ord)
tableau3_ord <- classer_correlation(tableau3_ord)
tableau4_ord <- classer_correlation(tableau4_ord)
tableau5_ord <- classer_correlation(tableau5_ord)
tableau6_ord <- classer_correlation(tableau6_ord)
tableau7_ord <- classer_correlation(tableau7_ord)


# 1) Adding the period and combining
df1 <- tableau1_ord %>% mutate(periode = "Nov21_TI")
df2 <- tableau2_ord %>% mutate(periode = "March22_TI")
df3 <- tableau3_ord %>% mutate(periode = "Nov22_TI")
df4 <- tableau4_ord %>% mutate(periode = "March23_TI")
df5 <- tableau5_ord %>% mutate(periode = "Nov23_TI")
df6 <- tableau6_ord %>% mutate(periode = "March24_TI")
df7 <- tableau7_ord %>% mutate(periode = "Nov22 (CSGA)")

# —————————————————————————
# Data
heat_df <- bind_rows(df1, df2, df3, df4, df5, df6, df7) %>%
  filter(!str_ends(variable, "_Poids"))

# —————————————————————————
# Levels and palette
levels_classe <- c(
  "Intensely: both correlation coefficients are x \u2265 0.6, and the means are not statistically different.",
  "Strongly: one of the two correlation coefficients is x \u2265 0.6 and the other is 0.4 \u2264 x < 0.6, and the means are not statistically different.",
  "Substantially: both correlation coefficients are 0.4 \u2264 x < 0.6, and the means are not statistically different.",
  "Moderately: at least one correlation coefficient is x \u2265 0.4, and the means are not statistically different.",
  "Poorly: at least one correlation coefficient is x \u2265 0.4, but the means are statistically different.",
  "Weakly: neither correlation coefficient reaches x \u2265 0.4.",
  "Missing"
)

palette_custom_named <- c(
  "Weakly: neither correlation coefficient reaches x \u2265 0.4." = "firebrick",
  "Poorly: at least one correlation coefficient is x \u2265 0.4, but the means are statistically different." = "goldenrod",
  "Moderately: at least one correlation coefficient is x \u2265 0.4, and the means are not statistically different." = "#C7E9C0",
  "Substantially: both correlation coefficients are 0.4 \u2264 x < 0.6, and the means are not statistically different." = "#A1D97B",
  "Strongly: one of the two correlation coefficients is x \u2265 0.6 and the other is 0.4 \u2264 x < 0.6, and the means are not statistically different." = "#74C499",
  "Intensely: both correlation coefficients are x \u2265 0.6, and the means are not statistically different." = "#31A354",
  "Missing" = "grey"
)


# —————————————————————————
# Periods & X labels
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
# Variable labels (English)
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
# Preparing df_all: facets, periods, recoding Missing
df_all <- heat_df %>%
  mutate(
    # categories for the facets
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
    # waves for the column facets
    wave = case_when(
      periode %in% c("Nov21_TI","March22_TI") ~ "Weekly FFQ 1",
      periode %in% c("Nov22_TI","March23_TI") ~ "Weekly FFQ 2",
      periode %in% c("Nov23_TI","March24_TI") ~ "Weekly FFQ 3",
      periode == "Nov22 (CSGA)"               ~ "Monthly FFQ 1"#,
      #periode =="All"   ~ "All"
    ) %>% factor(levels = c("Weekly FFQ 1","Weekly FFQ 2","Weekly FFQ 3","Monthly FFQ 1", "All")),
    # order of the periods
    periode = factor(periode, levels = period_levels),
    # recode NA -> "Missing" (a genuine category)
    classe  = fct_explicit_na(classe, na_level = "Missing")
  )

# —————————————————————————
# Order of the variables (from best to worst, Missing at the end)
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
# (Optional) Quick checks:
print(setdiff(levels(df_all$classe), names(palette_custom_named)))  # should return character(0)

# —————————————————————————
# Chart
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
    limits = levels_classe,      # fixed order + avoids grey
    drop   = FALSE,
    labels = function(x) str_wrap(x, width = 30)
  ) +
  labs(title = "Correlation of energy variables", x = NULL, y = NULL) +
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

###DOWNLOAD----------------------------------------------------
wb <- createWorkbook()

# Add each dataframe to a different sheet
addWorksheet(wb, "df1")
writeData(wb, sheet = "df1", df1)

# Add each dataframe to a different sheet
addWorksheet(wb, "df2")
writeData(wb, sheet = "df2", df2 )

# Add each dataframe to a different sheet
addWorksheet(wb, "df3")
writeData(wb, sheet = "df3", df3)

# Add each dataframe to a different sheet
addWorksheet(wb, "df4")
writeData(wb, sheet = "df4", df4)

# Add each dataframe to a different sheet
addWorksheet(wb, "df5")
writeData(wb, sheet = "df5", df5)

# Add each dataframe to a different sheet
addWorksheet(wb, "df6")
writeData(wb, sheet = "df6", df6)

# Add each dataframe to a different sheet
addWorksheet(wb, "df7")
writeData(wb, sheet = "df7", df7)





saveWorkbook(wb,(paste0("Comparaison_Kcal_vf.xlsx")))