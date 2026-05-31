
# SCRIPT: FFQ vs. Food Supply Booklet — Weight Validation
# PURPOSE: Compare FFQ (Food Frequency Questionnaire) and booklet (food supply
#          diary) data for the same participants across three cohorts:
#          CSGA, TI (IT), and Nudges — over multiple measurement waves.
#


# 1. PACKAGE LOADING----
rm(list = ls())  # Clear the environment before starting

library(tidyverse)
library(haven)
library(readxl)
library(openxlsx)
library(car)
library(broom)
library(scales)
library(modelsummary)
library(effsize)
library(lfe)
library(ggpubr)
library(vtable)
library(gridExtra)
library(RColorBrewer)
library(reshape2)
library(Metrics)
library(poLCA)
library(webshot)
library(nlme)
library(fixest)
library(plm)
library(lmtest)
library(htmltools)
library(clubSandwich)
library(Matrix)
library(lme4)
library(cobalt)
library(knitr)
library(tableone)
library(plotly)
library(htmlwidgets)
library(ggrepel)



# 2. IMPORT RAW DATASETS----

# All four files must be in the working directory (or provide full paths).
sgsdata_CSGA    <- read.xlsx((paste("sgsdata.xlsx", sep="")))
sgsdata_TI      <- read.xlsx((paste("sgsdata_IT.xlsx", sep="")))
sgsdata_nudges  <- read.xlsx((paste("sgsdata_nudges.xlsx", sep="")))
FFQ_NOV_23      <- read.xlsx((paste("23-11_FFQ.xlsx", sep="")))

# Identify the campaign based on participant identifiers.
# Participants whose identifier contains "PS" or "LE" are assigned to campaign 2;
# all others are assigned to campaign 1.
sgsdata_TI <- sgsdata_TI %>% mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"), 2, 1))

# Complete missing consumption-unit (UC_TI) values within each participant.
# Data are sorted by identifier, then missing values are filled both downward
# and upward within each participant group.
sgsdata_TI <- sgsdata_TI %>%
  arrange(Identifiant) %>% group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>% ungroup()

# Helper function: replace all NA values and empty strings with 0.
fill_zero <- function(df) { df %>% mutate(across(everything(), ~ ifelse(is.na(.) | . == "", 0, .))) }

# Apply zero-filling to all three cohort datasets.
sgsdata_TI      <- fill_zero(sgsdata_TI)
sgsdata_CSGA    <- fill_zero(sgsdata_CSGA)
sgsdata_nudges  <- fill_zero(sgsdata_nudges)

# Split the CSGA dataset into FFQ rows and booklet (supply diary) rows.
# FFQ rows: all rows where Mesure is NOT "Carnet".
# Booklet rows: rows where Mesure == "Carnet".
sgsdata_FFQ_CSGA     <- sgsdata_CSGA %>% filter(Mesure != "Carnet")
sgsdata_Booklet_CSGA <- sgsdata_CSGA %>% filter(Mesure == "Carnet")



# 3. CSGA — HARMONIZATION AND AGGREGATION----

# Harmonisation notes:
# - Cooked ham ("jambon blanc") is folded into the processed meat category for CSGA.
# - Added fats, vegetarian prepared meals, sauces, and dairy desserts are excluded
#   because these categories do not exist in the Nudges dataset.

# --- CSGA FFQ ---
sgsdata_FFQ_CSGA <- sgsdata_FFQ_CSGA %>%
  # Keep only participants who also have booklet data (matched pairs only).
  semi_join(sgsdata_Booklet_CSGA, by = "Identifiant") %>%
  # Drop all-zero numeric columns, then remove the "_FFQ" suffix from column names
  # so that FFQ and booklet columns share identical names.
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

# Build aggregated food-group variables for the CSGA FFQ dataset.
sgsdata_FFQ_CSGA$FV_Poids                    <- sgsdata_FFQ_CSGA$FRUITS_Poids + sgsdata_FFQ_CSGA$FRUITS_SECS_Poids + sgsdata_FFQ_CSGA$NOIX_Poids + sgsdata_FFQ_CSGA$LEGUMES_Poids  # Fruits & vegetables (incl. dried fruits and nuts)
sgsdata_FFQ_CSGA$FEC_Poids                   <- sgsdata_FFQ_CSGA$FEC_NON_RAF_Poids + sgsdata_FFQ_CSGA$FEC_RAF_Poids                                                                  # Starchy foods: refined + unrefined
sgsdata_FFQ_CSGA$PDTS_LAITIERS_Poids         <- sgsdata_FFQ_CSGA$LAIT_Poids + sgsdata_FFQ_CSGA$LAITAGES_Poids + sgsdata_FFQ_CSGA$FROMAGES_Poids                                     # Dairy: milk + dairy products + cheeses
sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Poids    <- sgsdata_FFQ_CSGA$CHARCUTERIE_HORS_JB_Poids                                                                                           # Cold cuts (excl. white ham — ham is in CHARCUTERIE for CSGA)
sgsdata_FFQ_CSGA$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_FFQ_CSGA$SNACKS_AUTRES_Poids + sgsdata_FFQ_CSGA$CEREALES_PD_Poids + sgsdata_FFQ_CSGA$PDTS_SUCRES_Poids                      # Discretionary foods: snacks + breakfast cereals + sweet products
sgsdata_FFQ_CSGA$POULET_OEUFS_Poids          <- sgsdata_FFQ_CSGA$POULET_Poids + sgsdata_FFQ_CSGA$OEUFS_Poids                                                                         # Poultry and eggs
sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Poids     <- sgsdata_FFQ_CSGA$VIANDE_ROUGE_Poids + sgsdata_FFQ_CSGA$PORC_Poids                                                                   # Red meat and pork
sgsdata_FFQ_CSGA$SSB_Poids                   <- sgsdata_FFQ_CSGA$SODAS_SUCRES_Poids + sgsdata_FFQ_CSGA$SODAS_LIGHT_Poids + sgsdata_FFQ_CSGA$FRUITS_JUS_Poids                        # Sugar-sweetened + diet beverages + fruit juices
sgsdata_FFQ_CSGA$VIANDES_Poids               <- sgsdata_FFQ_CSGA$POULET_OEUFS_Poids + sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Poids + sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Poids         # Total meats (all animal products excl. fish and dairy)

# Harmonise total summary indicators under the "_Poids" naming convention.
sgsdata_FFQ_CSGA$SOMME_KCAL_Poids         <- sgsdata_FFQ_CSGA$KCAL_TOTAL_Kcal
sgsdata_FFQ_CSGA$SOMME_HORS_BOISSON_Poids <- sgsdata_FFQ_CSGA$POIDS_HORS_BOISSON_Poids
sgsdata_FFQ_CSGA$SOMME_POIDS_Poids        <- sgsdata_FFQ_CSGA$POIDS_TOTAL_Poids

# --- CSGA Booklet ---
sgsdata_Booklet_CSGA <- sgsdata_Booklet_CSGA %>%
  # Keep only participants who also have FFQ data.
  semi_join(sgsdata_FFQ_CSGA, by = "Identifiant") %>%
  # Drop all-zero columns and remove the "_CARNET" suffix from column names.
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))

# Build the same aggregated food-group variables for the CSGA booklet dataset.
sgsdata_Booklet_CSGA$FV_Poids                    <- sgsdata_Booklet_CSGA$FRUITS_Poids + sgsdata_Booklet_CSGA$FRUITS_SECS_Poids + sgsdata_Booklet_CSGA$NOIX_Poids + sgsdata_Booklet_CSGA$LEGUMES_Poids
sgsdata_Booklet_CSGA$FEC_Poids                   <- sgsdata_Booklet_CSGA$FEC_NON_RAF_Poids + sgsdata_Booklet_CSGA$FEC_RAF_Poids
sgsdata_Booklet_CSGA$PDTS_LAITIERS_Poids         <- sgsdata_Booklet_CSGA$LAIT_Poids + sgsdata_Booklet_CSGA$LAITAGES_Poids + sgsdata_Booklet_CSGA$FROMAGES_Poids
sgsdata_Booklet_CSGA$POULET_OEUFS_Poids          <- sgsdata_Booklet_CSGA$POULET_Poids + sgsdata_Booklet_CSGA$OEUFS_Poids
sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Poids    <- sgsdata_Booklet_CSGA$CHARCUTERIE_HORS_JB_Poids
sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Poids     <- sgsdata_Booklet_CSGA$VIANDE_ROUGE_Poids + sgsdata_Booklet_CSGA$PORC_Poids
sgsdata_Booklet_CSGA$MG_Poids                    <- sgsdata_Booklet_CSGA$MGA_Poids
sgsdata_Booklet_CSGA$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_Booklet_CSGA$SNACKS_AUTRES_Poids + sgsdata_Booklet_CSGA$CEREALES_PD_Poids + sgsdata_Booklet_CSGA$PDTS_SUCRES_Poids
sgsdata_Booklet_CSGA$SSB_Poids                   <- sgsdata_Booklet_CSGA$SODAS_SUCRES_Poids + sgsdata_Booklet_CSGA$SODAS_LIGHT_Poids + sgsdata_Booklet_CSGA$FRUITS_JUS_Poids
sgsdata_Booklet_CSGA$SOMME_KCAL_Poids            <- sgsdata_Booklet_CSGA$KCAL_TOTAL_Kcal
sgsdata_Booklet_CSGA$SOMME_HORS_BOISSON_Poids    <- sgsdata_Booklet_CSGA$POIDS_HORS_BOISSON_Poids
sgsdata_Booklet_CSGA$SOMME_POIDS_Poids           <- sgsdata_Booklet_CSGA$POIDS_TOTAL_Poids
sgsdata_Booklet_CSGA$VIANDES_Poids               <- sgsdata_Booklet_CSGA$POULET_OEUFS_Poids + sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Poids + sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Poids



# 4. TI (IT) — HARMONIZATION AND AGGREGATION----

# Keep only single-person households (UC_TI == 1).
# FFQ rows: Mesure != "Carnet"; booklet rows: Mesure == "Carnet".
sgsdata_FFQ_IT     <- sgsdata_TI %>% filter(Mesure != "Carnet", UC_TI == 1)
sgsdata_Booklet_IT <- sgsdata_TI %>% filter(Mesure == "Carnet",  UC_TI == 1)

# Remove booklet rows where every CARNET column is zero or missing
# (i.e., no valid quantity was recorded).
sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  filter(
    rowSums(
      across(contains("CARNET"), ~ !is.na(.) & . != 0),
      na.rm = TRUE
    ) > 0
  )

# --- TI FFQ ---
sgsdata_FFQ_IT <- sgsdata_FFQ_IT %>%
  # Keep only participants who also have booklet data.
  semi_join(sgsdata_Booklet_IT, by = "Identifiant") %>%
  # Drop all-zero columns and strip the "_FFQ" suffix.
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

# Build aggregated food-group variables for the TI FFQ dataset.
# Note: for TI, white ham (JAMBON_BLANC) is a separate variable added to cold cuts.
sgsdata_FFQ_IT$FV_Poids                    <- sgsdata_FFQ_IT$FRUITS_Poids + sgsdata_FFQ_IT$FRUITS_SECS_Poids + sgsdata_FFQ_IT$NOIX_Poids + sgsdata_FFQ_IT$LEGUMES_Poids
sgsdata_FFQ_IT$FEC_Poids                   <- sgsdata_FFQ_IT$FEC_NON_RAF_Poids + sgsdata_FFQ_IT$FEC_RAF_Poids
sgsdata_FFQ_IT$PDTS_LAITIERS_Poids         <- sgsdata_FFQ_IT$LAIT_Poids + sgsdata_FFQ_IT$LAITAGES_Poids + sgsdata_FFQ_IT$FROMAGES_Poids
sgsdata_FFQ_IT$POULET_OEUFS_Poids          <- sgsdata_FFQ_IT$POULET_Poids + sgsdata_FFQ_IT$OEUFS_Poids
sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Poids    <- sgsdata_FFQ_IT$CHARCUTERIE_HORS_JB_Poids + sgsdata_FFQ_IT$JAMBON_BLANC_Poids  # Cold cuts including white ham
sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Poids     <- sgsdata_FFQ_IT$VIANDE_ROUGE_Poids + sgsdata_FFQ_IT$PORC_Poids
sgsdata_FFQ_IT$MG_Poids                    <- sgsdata_FFQ_IT$MGA_Poids
sgsdata_FFQ_IT$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_FFQ_IT$SNACKS_AUTRES_Poids + sgsdata_FFQ_IT$CEREALES_PD_Poids + sgsdata_FFQ_IT$PDTS_SUCRES_Poids
sgsdata_FFQ_IT$SSB_Poids                   <- sgsdata_FFQ_IT$SODAS_SUCRES_Poids + sgsdata_FFQ_IT$SODAS_LIGHT_Poids + sgsdata_FFQ_IT$FRUITS_JUS_Poids
sgsdata_FFQ_IT$SOMME_KCAL_Poids            <- sgsdata_FFQ_IT$KCAL_TOTAL_Kcal
sgsdata_FFQ_IT$SOMME_HORS_BOISSON_Poids    <- sgsdata_FFQ_IT$POIDS_HORS_BOISSON_Poids
sgsdata_FFQ_IT$SOMME_POIDS_Poids           <- sgsdata_FFQ_IT$POIDS_TOTAL_Poids
sgsdata_FFQ_IT$VIANDES_Poids               <- sgsdata_FFQ_IT$POULET_OEUFS_Poids + sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Poids + sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Poids

# --- TI Booklet ---
sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  semi_join(sgsdata_FFQ_IT, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))

# Build aggregated food-group variables for the TI booklet dataset.
sgsdata_Booklet_IT$FV_Poids                    <- sgsdata_Booklet_IT$FRUITS_Poids + sgsdata_Booklet_IT$FRUITS_SECS_Poids + sgsdata_Booklet_IT$NOIX_Poids + sgsdata_Booklet_IT$LEGUMES_Poids
sgsdata_Booklet_IT$FEC_Poids                   <- sgsdata_Booklet_IT$FEC_NON_RAF_Poids + sgsdata_Booklet_IT$FEC_RAF_Poids
sgsdata_Booklet_IT$PDTS_LAITIERS_Poids         <- sgsdata_Booklet_IT$LAIT_Poids + sgsdata_Booklet_IT$LAITAGES_Poids + sgsdata_Booklet_IT$FROMAGES_Poids
sgsdata_Booklet_IT$POULET_OEUFS_Poids          <- sgsdata_Booklet_IT$POULET_Poids + sgsdata_Booklet_IT$OEUFS_Poids
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Poids    <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_Poids
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Poids     <- sgsdata_Booklet_IT$VIANDE_ROUGE_Poids + sgsdata_Booklet_IT$PORC_Poids
sgsdata_Booklet_IT$MG_Poids                    <- sgsdata_Booklet_IT$MGA_Poids
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_Booklet_IT$SNACKS_AUTRES_Poids + sgsdata_Booklet_IT$CEREALES_PD_Poids + sgsdata_Booklet_IT$PDTS_SUCRES_Poids
sgsdata_Booklet_IT$SSB_Poids                   <- sgsdata_Booklet_IT$SODAS_SUCRES_Poids + sgsdata_Booklet_IT$SODAS_LIGHT_Poids + sgsdata_Booklet_IT$FRUITS_JUS_Poids
sgsdata_Booklet_IT$SOMME_KCAL_Poids            <- sgsdata_Booklet_IT$KCAL_TOTAL_Kcal
sgsdata_Booklet_IT$SOMME_HORS_BOISSON_Poids    <- sgsdata_Booklet_IT$POIDS_HORS_BOISSON_Poids
sgsdata_Booklet_IT$SOMME_POIDS_Poids           <- sgsdata_Booklet_IT$POIDS_TOTAL_Poids
sgsdata_Booklet_IT$VIANDES_Poids               <- sgsdata_Booklet_IT$POULET_OEUFS_Poids + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Poids + sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Poids



# 5. NUDGES — HARMONIZATION AND AGGREGATION----

# UC_TI.x is used here because this variable comes from the merged nudges dataset.
sgsdata_FFQ_nudges     <- sgsdata_nudges %>% filter(Mesure != "Carnet", UC_TI == 1)
sgsdata_Booklet_nudges <- sgsdata_nudges %>% filter(Mesure == "Carnet",  UC_TI.x == 1)

# Remove booklet rows where every CARNET column is zero or missing.
sgsdata_Booklet_nudges <- sgsdata_Booklet_nudges %>%
  filter(
    rowSums(
      across(contains("CARNET"), ~ !is.na(.) & . != 0),
      na.rm = TRUE
    ) > 0
  )

# --- Nudges FFQ ---
sgsdata_FFQ_nudges <- sgsdata_FFQ_nudges %>%
  semi_join(sgsdata_Booklet_nudges, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

# Build aggregated food-group variables for the Nudges FFQ dataset.
sgsdata_FFQ_nudges$FV_Poids                    <- sgsdata_FFQ_nudges$FRUITS_Poids + sgsdata_FFQ_nudges$FRUITS_SECS_Poids + sgsdata_FFQ_nudges$NOIX_Poids + sgsdata_FFQ_nudges$LEGUMES_Poids
sgsdata_FFQ_nudges$FEC_Poids                   <- sgsdata_FFQ_nudges$FEC_NON_RAF_Poids + sgsdata_FFQ_nudges$FEC_RAF_Poids
sgsdata_FFQ_nudges$PDTS_LAITIERS_Poids         <- sgsdata_FFQ_nudges$LAIT_Poids + sgsdata_FFQ_nudges$LAITAGES_Poids + sgsdata_FFQ_nudges$FROMAGES_Poids
sgsdata_FFQ_nudges$POULET_OEUFS_Poids          <- sgsdata_FFQ_nudges$POULET_Poids + sgsdata_FFQ_nudges$OEUFS_Poids
sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Poids    <- sgsdata_FFQ_nudges$CHARCUTERIE_HORS_JB_Poids + sgsdata_FFQ_nudges$JAMBON_BLANC_Poids
sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Poids     <- sgsdata_FFQ_nudges$VIANDE_ROUGE_Poids + sgsdata_FFQ_nudges$PORC_Poids
sgsdata_FFQ_nudges$MG_Poids                    <- sgsdata_FFQ_nudges$MGA_Poids
sgsdata_FFQ_nudges$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_FFQ_nudges$SNACKS_AUTRES_Poids + sgsdata_FFQ_nudges$CEREALES_PD_Poids + sgsdata_FFQ_nudges$PDTS_SUCRES_Poids
sgsdata_FFQ_nudges$SSB_Poids                   <- sgsdata_FFQ_nudges$SODAS_SUCRES_Poids + sgsdata_FFQ_nudges$SODAS_LIGHT_Poids + sgsdata_FFQ_nudges$FRUITS_JUS_Poids
sgsdata_FFQ_nudges$SOMME_KCAL_Poids            <- sgsdata_FFQ_nudges$SOMME_KCAL
sgsdata_FFQ_nudges$SOMME_HORS_BOISSON_Poids    <- sgsdata_FFQ_nudges$SOMME_HORS_BOISSON
sgsdata_FFQ_nudges$SOMME_POIDS_Poids           <- sgsdata_FFQ_nudges$SOMME_POIDS
sgsdata_FFQ_nudges$VIANDES_Poids               <- sgsdata_FFQ_nudges$POULET_OEUFS_Poids + sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Poids + sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Poids

# --- Nudges Booklet ---
sgsdata_Booklet_nudges <- sgsdata_Booklet_nudges %>%
  semi_join(sgsdata_FFQ_nudges, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))

# Build aggregated food-group variables for the Nudges booklet dataset.
sgsdata_Booklet_nudges$FV_Poids                    <- sgsdata_Booklet_nudges$FRUITS_Poids + sgsdata_Booklet_nudges$FRUITS_SECS_Poids + sgsdata_Booklet_nudges$NOIX_Poids + sgsdata_Booklet_nudges$LEGUMES_Poids
sgsdata_Booklet_nudges$FEC_Poids                   <- sgsdata_Booklet_nudges$FEC_NON_RAF_Poids + sgsdata_Booklet_nudges$FEC_RAF_Poids
sgsdata_Booklet_nudges$PDTS_LAITIERS_Poids         <- sgsdata_Booklet_nudges$LAIT_Poids + sgsdata_Booklet_nudges$LAITAGES_Poids + sgsdata_Booklet_nudges$FROMAGES_Poids
sgsdata_Booklet_nudges$POULET_OEUFS_Poids          <- sgsdata_Booklet_nudges$POULET_Poids + sgsdata_Booklet_nudges$OEUFS_Poids
sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Poids    <- sgsdata_Booklet_nudges$CHARCUTERIE_HORS_JB_Poids
sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Poids     <- sgsdata_Booklet_nudges$VIANDE_ROUGE_Poids + sgsdata_Booklet_nudges$PORC_Poids
sgsdata_Booklet_nudges$MG_Poids                    <- sgsdata_Booklet_nudges$MGA_Poids
sgsdata_Booklet_nudges$PDTS_DISCRETIONNAIRES_Poids <- sgsdata_Booklet_nudges$SNACKS_AUTRES_Poids + sgsdata_Booklet_nudges$CEREALES_PD_Poids + sgsdata_Booklet_nudges$PDTS_SUCRES_Poids
sgsdata_Booklet_nudges$SSB_Poids                   <- sgsdata_Booklet_nudges$SODAS_SUCRES_Poids + sgsdata_Booklet_nudges$SODAS_LIGHT_Poids + sgsdata_Booklet_nudges$FRUITS_JUS_Poids
sgsdata_Booklet_nudges$SOMME_KCAL_Poids            <- sgsdata_Booklet_nudges$SOMME_KCAL
sgsdata_Booklet_nudges$SOMME_HORS_BOISSON_Poids    <- sgsdata_Booklet_nudges$SOMME_HORS_BOISSON.x  # .x suffix because of merge
sgsdata_Booklet_nudges$SOMME_POIDS_Poids           <- sgsdata_Booklet_nudges$SOMME_POIDS
sgsdata_Booklet_nudges$VIANDES_Poids               <- sgsdata_Booklet_nudges$POULET_OEUFS_Poids + sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Poids + sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Poids



# 6. SPLIT BY CAMPAIGN AND PERIOD----

# TI — Campaign 1 (participants with identifiers NOT containing "PS" or "LE")
sgsdata_Booklet_IT11 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode == 0)  # Baseline
sgsdata_Booklet_IT21 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode == 1)  # Follow-up
sgsdata_FFQ_IT11     <- sgsdata_FFQ_IT     %>% filter(Campagne == 1, Periode == 0)
sgsdata_FFQ_IT21     <- sgsdata_FFQ_IT     %>% filter(Campagne == 1, Periode == 1)

# TI — Campaign 2 (participants with "PS" or "LE" in their identifier)
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode == 0)
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode == 1)
sgsdata_FFQ_IT12     <- sgsdata_FFQ_IT     %>% filter(Campagne == 2, Periode == 0)
sgsdata_FFQ_IT22     <- sgsdata_FFQ_IT     %>% filter(Campagne == 2, Periode == 1)

# Nudges — split by period only (no campaign distinction)
sgsdata_Booklet_nudges1 <- sgsdata_Booklet_nudges %>% filter(Periode == 0)
sgsdata_Booklet_nudges2 <- sgsdata_Booklet_nudges %>% filter(Periode == 1)
sgsdata_FFQ_nudges1     <- sgsdata_FFQ_nudges     %>% filter(Periode == 0)
sgsdata_FFQ_nudges2     <- sgsdata_FFQ_nudges     %>% filter(Periode == 1)


# 7. STACK ALL SUB-DATASETS INTO COMBINED FFQ AND BOOKLET FRAMES----


# Collect all FFQ sub-datasets in a named list.
dfs <- list(
  nudges1 = sgsdata_FFQ_nudges1,
  nudges2 = sgsdata_FFQ_nudges2,
  IT11    = sgsdata_FFQ_IT11,
  IT12    = sgsdata_FFQ_IT12,
  IT21    = sgsdata_FFQ_IT21,
  IT22    = sgsdata_FFQ_IT22
)

# Find the columns present in ALL sub-datasets, then stack them into one frame.
# The "source" column records which sub-dataset each row came from.
common_cols    <- Reduce(intersect, lapply(dfs, names))
sgsdata_FFQ_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")

# Same operation for the booklet sub-datasets.
dfs <- list(
  nudges1 = sgsdata_Booklet_nudges1,
  nudges2 = sgsdata_Booklet_nudges2,
  IT11    = sgsdata_Booklet_IT11,
  IT12    = sgsdata_Booklet_IT12,
  IT21    = sgsdata_Booklet_IT21,
  IT22    = sgsdata_Booklet_IT22
)

common_cols         <- Reduce(intersect, lapply(dfs, names))
sgsdata_Booklet_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")



# 8. ID HARMONIZATION----


# Helper function: keep only participants present in BOTH data frames of a pair.
# Returns a list with two filtered data frames ($df1 and $df2).
harmoniser_ids <- function(df1, df2, id_col = "Identifiant") {
  ids_communs <- intersect(df1[[id_col]], df2[[id_col]])
  df1_filtre  <- df1 %>% filter(.data[[id_col]] %in% ids_communs)
  df2_filtre  <- df2 %>% filter(.data[[id_col]] %in% ids_communs)
  list(df1 = df1_filtre, df2 = df2_filtre)
}

# Apply ID harmonization to every FFQ–booklet pair.
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

# Also harmonize the combined all-sources frames using source + ID + period as keys.
by_keys <- c("source", "Identifiant", "Periode")

ffq_comm <- sgsdata_FFQ_all %>%
  semi_join(sgsdata_Booklet_all, by = by_keys) %>%
  arrange(across(all_of(by_keys)))

booklet_comm <- sgsdata_Booklet_all %>%
  semi_join(sgsdata_FFQ_all, by = by_keys) %>%
  arrange(across(all_of(by_keys)))

pair_all              <- harmoniser_ids(booklet_comm, ffq_comm)
sgsdata_Booklet_comm  <- pair_all$df1
sgsdata_ffq_comm      <- pair_all$df2



# 9. CORE ANALYSIS FUNCTION: analyser_moyennes()----

# For each food variable ending in one of the specified suffixes, this function
# computes — on matched FFQ–booklet pairs:
#   - means and 95% CIs for both instruments
#   - percentage bias: (FFQ - Booklet) / Booklet * 100
#   - paired t-test p-value
#   - Pearson and Spearman correlations
#   - quintile agreement (same / adjacent / opposite quintile)

analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Poids"),
                              multiplier = 1000,
                              conf_level = 0.95) {
  
  library(dplyr)
  library(tidyr)
  
  # --- Step 0: Align rows by participant ID ---
  # Restrict both datasets to the intersection of IDs and sort identically.
  if ("Identifiant" %in% names(ffq_data) && "Identifiant" %in% names(booklet_data)) {
    commun_ids <- intersect(ffq_data$Identifiant, booklet_data$Identifiant)
    ffq_data     <- ffq_data     %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
    booklet_data <- booklet_data %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
  }
  
  # Safety check: both datasets must have the same number of rows after alignment.
  if (nrow(ffq_data) != nrow(booklet_data)) {
    stop("ffq_data and booklet_data do not have the same number of rows after alignment.")
  }
  
  # --- Step 1: Identify target columns by suffix ---
  motif        <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  vars_ffq     <- grep(motif, names(ffq_data),     value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  
  if (length(vars_ffq)     == 0) stop("No FFQ column ending with ",     paste(suffixes, collapse = ", "))
  if (length(vars_booklet) == 0) stop("No Booklet column ending with ", paste(suffixes, collapse = ", "))
  
  # --- Step 2: Keep only variables common to both datasets ---
  vars_communes <- intersect(vars_ffq, vars_booklet)
  if (length(vars_communes) == 0) stop("No common variable between FFQ and Booklet with the given suffixes.")
  
  tableau_base <- tibble(variable = vars_communes)
  
  # --- Step 3: Compute statistics for each variable on complete pairs ---
  stats <- lapply(tableau_base$variable, function(var) {
    
    x_full <- ffq_data[[var]]
    y_full <- booklet_data[[var]]
    
    # Keep only rows where both FFQ and Booklet values are non-missing.
    valid   <- !is.na(x_full) & !is.na(y_full)
    x       <- x_full[valid]  # FFQ values
    y       <- y_full[valid]  # Booklet values
    n_pairs <- length(x)
    
    # Return all NA if fewer than 2 valid pairs.
    if (n_pairs <= 1) {
      return(data.frame(
        variable = var, n_pairs = n_pairs,
        moyenne_Booklet = NA, ci95_Booklet = NA,
        moyenne_FFQ = NA, ci95_FFQ = NA,
        pct_bias = NA, p_value_diff = NA,
        pearson_correlation = NA, pearson_p_value = NA,
        spearman_correlation = NA, spearman_p_value = NA,
        pct_similar_quintile = NA, pct_adjacent_quintile = NA, pct_opposite_quintile = NA
      ))
    }
    
    # --- 3.1: Unit conversion kg → g for weight variables ---
    # SOMME_KCAL_Poids is in kcal and must NOT be multiplied by 1000.
    if (grepl("_Poids$", var) && var != "SOMME_KCAL_Poids") {
      x <- x * multiplier
      y <- y * multiplier
    }
    
    # --- 3.2: Means ---
    moyenne_FFQ     <- mean(x)
    moyenne_Booklet <- mean(y)
    
    # --- 3.3: Percentage bias (FFQ relative to booklet) ---
    pct_bias <- ifelse(
      moyenne_Booklet == 0, NA,
      (moyenne_FFQ - moyenne_Booklet) / moyenne_Booklet * 100
    )
    
    # --- 3.4: Paired t-test and individual 95% CIs ---
    t_diff <- t.test(x, y, paired = TRUE)   # Tests whether FFQ and booklet means differ
    t_ffq  <- t.test(x, conf.level = conf_level)
    t_book <- t.test(y, conf.level = conf_level)
    
    # --- 3.5: Pearson and Spearman correlations ---
    # Only computed if both variables have non-zero variance.
    if (sd(x) > 0 && sd(y) > 0) {
      pearson_tst  <- cor.test(x, y, method = "pearson",  exact = FALSE)
      spearman_tst <- cor.test(x, y, method = "spearman", exact = FALSE)
      pearson_est  <- unname(pearson_tst$estimate);  pearson_p  <- pearson_tst$p.value
      spearman_est <- unname(spearman_tst$estimate); spearman_p <- spearman_tst$p.value
    } else {
      pearson_est <- NA; pearson_p <- NA; spearman_est <- NA; spearman_p <- NA
    }
    
    # --- 3.6: Quintile agreement ---
    # Participants are ranked into quintiles separately in FFQ and booklet.
    # "Adjacent" is defined broadly as a 1- or 2-quintile gap.
    # "Opposite" means a gap of 3 or 4 quintiles.
    qx <- dplyr::ntile(x, 5)
    qy <- dplyr::ntile(y, 5)
    d  <- abs(qx - qy)
    pct_similar  <- mean(d == 0,           na.rm = TRUE) * 100
    pct_adjacent <- mean(d %in% c(1, 2),   na.rm = TRUE) * 100
    pct_opposite <- mean(d %in% c(3, 4),   na.rm = TRUE) * 100
    
    # --- 3.7: Assemble result row ---
    data.frame(
      variable    = var,
      n_pairs     = n_pairs,
      moyenne_Booklet = moyenne_Booklet,
      ci95_Booklet    = sprintf("[%.2f, %.2f]", t_book$conf.int[1], t_book$conf.int[2]),
      moyenne_FFQ     = moyenne_FFQ,
      ci95_FFQ        = sprintf("[%.2f, %.2f]", t_ffq$conf.int[1],  t_ffq$conf.int[2]),
      pct_bias        = pct_bias,
      p_value_diff    = t_diff$p.value,
      pearson_correlation  = pearson_est,  pearson_p_value  = pearson_p,
      spearman_correlation = spearman_est, spearman_p_value = spearman_p,
      pct_similar_quintile  = pct_similar,
      pct_adjacent_quintile = pct_adjacent,
      pct_opposite_quintile = pct_opposite
    )
  }) %>%
    bind_rows()
  
  # --- Step 4: Final table — round all numeric columns to 2 significant digits ---
  tableau_final <- stats %>%
    select(variable, n_pairs,
           moyenne_Booklet, ci95_Booklet,
           moyenne_FFQ, ci95_FFQ,
           pct_bias, p_value_diff,
           pearson_correlation, pearson_p_value,
           spearman_correlation, spearman_p_value,
           pct_similar_quintile, pct_adjacent_quintile, pct_opposite_quintile) %>%
    mutate(across(where(is.numeric), ~ signif(.x, 2)))
  
  return(tableau_final)
}



# 10. SAVE INTERMEDIATE DATASETS (snapshots before the analysis loop)----

# These "_bis" copies preserve the clean matched datasets so they can be reused
# later in the tertile and change analyses without being overwritten.
sgsdata_FFQ_IT11_bis      <- sgsdata_FFQ_IT11
sgsdata_Booklet_IT11_bis  <- sgsdata_Booklet_IT11
sgsdata_FFQ_IT12_bis      <- sgsdata_FFQ_IT12
sgsdata_Booklet_IT12_bis  <- sgsdata_Booklet_IT12
sgsdata_Booklet_IT21_bis  <- sgsdata_Booklet_IT21
sgsdata_FFQ_IT21_bis      <- sgsdata_FFQ_IT21
sgsdata_Booklet_IT22_bis  <- sgsdata_Booklet_IT22
sgsdata_FFQ_IT22_bis      <- sgsdata_FFQ_IT22
sgsdata_Booklet_nudges1_bis <- sgsdata_Booklet_nudges1
sgsdata_FFQ_nudges1_bis     <- sgsdata_FFQ_nudges1
sgsdata_Booklet_nudges2_bis <- sgsdata_Booklet_nudges2
sgsdata_FFQ_nudges2_bis     <- sgsdata_FFQ_nudges2
sgsdata_FFQ_CSGA_bis        <- sgsdata_FFQ_CSGA
sgsdata_Booklet_CSGA_bis    <- sgsdata_Booklet_CSGA
sgsdata_FFQ_com_bis         <- ffq_comm
sgsdata_Booklet_com_bis     <- booklet_comm



# 11. RUN analyser_moyennes() FOR ALL 7 CAMPAIGN × COHORT COMBINATIONS----

# tableau_final_1 → Nudges, period 0 (Nov 2021)
tableau_final_1 <- analyser_moyennes(sgsdata_FFQ_nudges1, sgsdata_Booklet_nudges1) %>% arrange(variable)
# tableau_final_2 → Nudges, period 1 (March 2022)
tableau_final_2 <- analyser_moyennes(sgsdata_FFQ_nudges2, sgsdata_Booklet_nudges2) %>% arrange(variable)
# tableau_final_3 → TI campaign 1, period 0 (Nov 2022)
tableau_final_3 <- analyser_moyennes(sgsdata_FFQ_IT11,    sgsdata_Booklet_IT11)    %>% arrange(variable)
# tableau_final_4 → TI campaign 1, period 1 (March 2023)
tableau_final_4 <- analyser_moyennes(sgsdata_FFQ_IT21,    sgsdata_Booklet_IT21)    %>% arrange(variable)
# tableau_final_5 → TI campaign 2, period 0 (Nov 2023)
tableau_final_5 <- analyser_moyennes(sgsdata_FFQ_IT12,    sgsdata_Booklet_IT12)    %>% arrange(variable)
# tableau_final_6 → TI campaign 2, period 1 (March 2024)
tableau_final_6 <- analyser_moyennes(sgsdata_FFQ_IT22,    sgsdata_Booklet_IT22)    %>% arrange(variable)
# tableau_final_7 → CSGA (monthly FFQ, Nov 2022)
tableau_final_7 <- analyser_moyennes(sgsdata_FFQ_CSGA,    sgsdata_Booklet_CSGA)    %>% arrange(variable)


# 12. REORDER VARIABLES AND ADD DELTA COLUMN----


# Fixed display order for food variables across all result tables.
variables <- c(
  "VIANDES_Poids", "CEREALES_PD_Poids", "FEC_NON_RAF_Poids", "FEC_RAF_Poids",
  "FROMAGES_Poids", "FRUITS_Poids", "FRUITS_SECS_Poids", "LAITAGES_Poids",
  "LEGUMES_Poids", "LEG_SECS_Poids", "NOIX_Poids", "OEUFS_Poids",
  "PDTS_SUCRES_Poids", "PORC_Poids", "POULET_Poids",
  "QUICHES_PIZZAS_TARTES_SALEES_Poids", "SNACKS_AUTRES_Poids",
  "VIANDE_ROUGE_Poids", "FRUITS_JUS_Poids", "LAIT_Poids",
  "SODAS_LIGHT_Poids", "SODAS_SUCRES_Poids", "MGA_Poids", "MGV_Poids",
  "FV_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids", "AUTRE_PDTS_ANIMAUX_Poids",
  "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids",
  "PLATS_PREP_VEGETARIENS_Poids", "PLATS_PREP_CARNES_Poids",
  "POISSONS_Poids", "ALCOOL_Poids", "LEG_SECS_Poids",
  "SAUCES_Poids", "DESSERTS_LACTES_Poids", "EAU_Poids", "CAFE_THE_Poids",
  "SOMME_HORS_BOISSON_Poids", "SOMME_POIDS_Poids", "SOMME_KCAL_Poids"
)

library(dplyr)
# Template tibble used as a left-join key to impose the variable order.
template <- tibble(variable = variables)

# Function: reorder rows according to the template and add Delta (FFQ mean - Booklet mean).
# Delta is placed as the 6th column.
reorder_and_delta <- function(df) {
  df_ord <- template %>%
    left_join(df, by = "variable") %>%
    mutate(Delta = moyenne_FFQ - moyenne_Booklet) %>%
    relocate(Delta, .after = 5)
  return(df_ord)
}

# Apply reordering and Delta calculation to all seven result tables.
tableau1_ord <- reorder_and_delta(tableau_final_1)
tableau2_ord <- reorder_and_delta(tableau_final_2)
tableau3_ord <- reorder_and_delta(tableau_final_3)
tableau4_ord <- reorder_and_delta(tableau_final_4)
tableau5_ord <- reorder_and_delta(tableau_final_5)
tableau6_ord <- reorder_and_delta(tableau_final_6)
tableau7_ord <- reorder_and_delta(tableau_final_7)


# 13. AGREEMENT CLASSIFICATION----

# Each food variable is classified into one of six agreement levels based on
# Pearson r, Spearman ρ, and the paired t-test p-value.
# The same case_when logic is applied to all seven result tables.
#
# Classification rules (evaluated in order):
#   "Intensely"    → both r ≥ 0.6 AND means not significantly different
#   "Strongly"     → one r ≥ 0.6 and the other ≥ 0.4, means not sig. different
#   "Substantially"→ both r ≥ 0.4 AND means not significantly different
#   "Moderately"   → at least one r ≥ 0.4 AND means not significantly different
#   "Poorly"       → at least one r ≥ 0.4 BUT means ARE significantly different
#   "Weakly"       → no r ≥ 0.4
#   "Missing"      → one of the statistics is NA

classify_agreement <- function(df) {
  df %>%
    mutate(
      classe = case_when(
        pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~
          "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
        ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) |
           (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~
          "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
        pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~
          "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
        (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~
          "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
        (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~
          "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
        spearman_correlation < 0.4 & pearson_correlation < 0.4 ~
          "Weakly. No correlation coefficient at least 0.4.",
        is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~
          "Missing"
      )
    )
}

# Note: the original script applies the same case_when inline to each table.
# The function above captures the logic; the calls below mirror the original exactly.
tableau1_ord <- tableau1_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau2_ord <- tableau2_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau3_ord <- tableau3_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau4_ord <- tableau4_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau5_ord <- tableau5_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau6_ord <- tableau6_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau7_ord <- tableau7_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05  ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 |pearson_correlation >= 0.4) &  p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4  ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))



# 14. PREPARE DATA FOR THE CORRELATION HEATMAP----

# Tag each result table with its measurement period label, then stack them.
df1 <- tableau1_ord %>% mutate(periode = "Nov21_TI")
df2 <- tableau2_ord %>% mutate(periode = "March22_TI")
df3 <- tableau3_ord %>% mutate(periode = "Nov22_TI")
df4 <- tableau4_ord %>% mutate(periode = "March23_TI")
df5 <- tableau5_ord %>% mutate(periode = "Nov23_TI")
df6 <- tableau6_ord %>% mutate(periode = "March24_TI")
df7 <- tableau7_ord %>% mutate(periode = "Nov22 (CSGA)")

# Combine all periods; drop any remaining kcal-only rows.
heat_df <- bind_rows(df1, df2, df3, df4, df5, df6, df7) %>%
  filter(!str_ends(variable, "_Kcal"), variable != "SOMME_KCALTOT")

# Ordered factor levels for the agreement classification (best → worst).
levels_classe <- c(
  "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  "Weakly. No correlation coefficient at least 0.4.",
  "Missing"
)

# Colour palette: green shades for strong agreement, yellow/red for weak, grey for missing.
palette_custom_named <- c(
  "Weakly. No correlation coefficient at least 0.4."                                                                                                    = "firebrick",
  "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different"                                  = "goldenrod",
  "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different"                           = "#C7E9C0",
  "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different"                                       = "#A1D97B",
  "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different"       = "#74C499",
  "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different"                                        = "#31A354",
  "Missing"                                                                                                                                              = "grey"
)

# Ordered x-axis period levels for the heatmap.
period_levels <- c(
  "Nov21_TI", "March22_TI",
  "Nov22_TI", "March23_TI",
  "Nov23_TI", "March24_TI",
  "Nov22 (CSGA)"
)

# Short x-axis tick labels (displayed instead of raw period codes).
labels_x <- setNames(
  c("Nov 21", "Mar 22", "Nov 22", "Mar 23", "Nov 23", "Mar 24", "Nov22"),
  period_levels
)

# English labels for food variable names (used for the y-axis).
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

# Build df_all: add facet category, wave group, and recode explicit NAs as "Missing".
df_all <- heat_df %>%
  mutate(
    # Row facet: group variables into three display categories.
    category = case_when(
      variable %in% c("SOMME_HORS_BOISSON_Poids", "SOMME_POIDS_Poids", "SOMME_KCAL_Poids") ~ "General\nindicator",
      variable %in% c("FV_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids",
                      "VIANDES_Poids", "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids")           ~ "General\nfood item",
      TRUE ~ "Specific\nfood item"
    ) %>% factor(levels = c("General\nindicator", "General\nfood item", "Specific\nfood item")),
    
    # Column facet: group measurement periods into FFQ waves.
    wave = case_when(
      periode %in% c("Nov21_TI",  "March22_TI") ~ "Weekly FFQ 1",
      periode %in% c("Nov22_TI",  "March23_TI") ~ "Weekly FFQ 2",
      periode %in% c("Nov23_TI",  "March24_TI") ~ "Weekly FFQ 3",
      periode == "Nov22 (CSGA)"                  ~ "Monthly FFQ 1"
    ) %>% factor(levels = c("Weekly FFQ 1", "Weekly FFQ 2", "Weekly FFQ 3", "Monthly FFQ 1", "All")),
    
    # Enforce period order on the x-axis.
    periode = factor(periode, levels = period_levels),
    
    # Replace NA in the classification with the explicit "Missing" level.
    classe  = fct_explicit_na(classe, na_level = "Missing")
  )

# Order variables from best to worst average agreement score
# (lower numeric level = better agreement).
df_all <- df_all %>%
  mutate(classe_num = as.integer(factor(classe, levels = levels_classe)))

var_ord <- df_all %>%
  group_by(variable) %>%
  summarise(score = mean(classe_num, na.rm = TRUE), .groups = "drop") %>%
  arrange(score) %>%
  pull(variable)

df_all <- df_all %>%
  mutate(variable = factor(variable, levels = var_ord))

# Sanity check: all classification levels must have a corresponding colour.
# This should print character(0) if the palette is complete.
print(setdiff(levels(df_all$classe), names(palette_custom_named)))


# 15. CORRELATION HEATMAP----

# Tile heatmap: x = measurement period, y = food variable, fill = agreement class.
# Facets: rows = food category; columns = FFQ wave.
p <- ggplot(df_all, aes(x = periode, y = variable, fill = classe)) +
  geom_tile(colour = "white") +
  facet_grid(
    category ~ wave,
    scales   = "free",
    space    = "free",
    switch   = "y",
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
    limits = levels_classe,   # Fix the legend order
    drop   = FALSE,
    labels = function(x) str_wrap(x, width = 30)
  ) +
  labs(title = "Correlation of Weight Variables", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin        = unit(c(1, 1, 1, 4), "lines"),
    axis.text.y        = element_text(angle = 0, hjust = 1, size = 11),
    panel.spacing.y    = unit(1, "lines"),
    strip.placement    = "outside",
    strip.text.y.left  = element_blank(),
    strip.background.y = element_blank(),
    strip.background.x = element_blank(),
    strip.text.x       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
    legend.text        = element_text(size = 12, lineheight = 1),
    panel.grid         = element_blank(),
    plot.title         = element_text(color = "black", face = "bold", hjust = 0.5, size = 12)
  )

print(p)


# 16. QUINTILE AGREEMENT ANALYSIS----


# Stack results from all 7 campaigns into one long data frame.
all_stats <- bind_rows(
  tableau_final_1 %>% mutate(periode = "Nov21_TI"),
  tableau_final_2 %>% mutate(periode = "March22_TI"),
  tableau_final_3 %>% mutate(periode = "Nov22_TI"),
  tableau_final_4 %>% mutate(periode = "March23_TI"),
  tableau_final_5 %>% mutate(periode = "Nov23_TI"),
  tableau_final_6 %>% mutate(periode = "March24_TI"),
  tableau_final_7 %>% mutate(periode = "Nov22_CSGA")
)

# Map raw period codes to short wave labels for plot facet headers.
lab_map <- c(
  Nov21_TI   = "Weekly\nFFQ 1",
  March22_TI = "Weekly\nFFQ 1",
  Nov22_TI   = "Weekly\nFFQ 2",
  March23_TI = "Weekly\nFFQ 2",
  Nov23_TI   = "Weekly\nFFQ 3",
  March24_TI = "Weekly\nFFQ 3",
  Nov22_CSGA = "Monthly\nFFQ 1"
)

# Reshape from wide to long: one row per variable × period × quintile classification.
# pct_similar_quintile, pct_adjacent_quintile, pct_opposite_quintile → single "percentage" column.
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
                            levels = c("Same quintile", "Adjacent quintiles", "Opposite quintiles")),
    periode = factor(periode,
                     levels = c("Nov21_TI", "Nov22_TI", "Nov23_TI", "Nov22_CSGA"))
  )

# English display labels for food variables (y-axis of the quintile plot).
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
  AUTRE_PDTS_ANIMAUX_Poids                   = "Cold cuts",
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

# Define variable groups for row facets in the quintile chart.
general_indicators <- c("SOMME_HORS_BOISSON_Poids", "SOMME_POIDS_Poids", "SOMME_KCAL_Poids")
general_food_items <- c("FV_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids",
                        "VIANDES_Poids", "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids")
# All remaining variables fall under "Specific items".


# Function: plot_quintile_by_campaign()----
# Produces a 100%-stacked horizontal bar chart showing the proportion of
# participants classified as same / adjacent / opposite quintile, faceted
# by food category (rows) and measurement period (columns).
# Arguments:
#   df          — long data frame from plot_df_vars
#   keep_vars   — optional vector of variable names to include
#   remove_vars — optional vector of variable names to exclude
#   label_map   — named vector mapping raw variable names to English labels
#   n_cols      — number of facet columns (unused here but kept for compatibility)
#   label_min   — minimum percentage to display as a text label inside bars
#   label_dec   — decimal places for text labels

plot_quintile_by_campaign <- function(df,
                                      keep_vars   = NULL,
                                      remove_vars = NULL,
                                      label_map   = NULL,
                                      n_cols      = 7,
                                      label_min   = 3,
                                      label_dec   = 0) {
  general_indicators <- c("SOMME_HORS_BOISSON_Poids", "SOMME_POIDS_Poids", "SOMME_KCAL_Poids")
  general_food_items <- c("FV_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids",
                          "VIANDES_Poids", "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids")
  
  df2 <- df
  if (!is.null(keep_vars))   df2 <- dplyr::filter(df2, variable %in% keep_vars)
  if (!is.null(remove_vars)) df2 <- dplyr::filter(df2, !variable %in% remove_vars)
  
  df2 <- df2 %>%
    dplyr::mutate(
      # Assign each variable to its display group.
      group = dplyr::case_when(
        variable %in% general_indicators ~ "General\nindicator",
        variable %in% general_food_items ~ "General\nfood items",
        TRUE                             ~ "Specific\nitems"
      ),
      group = factor(group, levels = c("General\nindicator", "General\nfood items", "Specific\nitems")),
      # Translate variable names to English labels; fall back to raw name if not found.
      var_en = ifelse(!is.null(label_map) & variable %in% names(label_map),
                      label_map[variable], variable),
      var_en = factor(var_en, levels = unique(var_en)),
      # Enforce a consistent ordering of periods on the x-axis.
      periode = {
        desired <- c("Nov21_TI", "March22_TI", "Nov22_TI", "March23_TI",
                     "Nov23_TI", "March24_TI", "Nov22_CSGA")
        lvls <- desired[desired %in% unique(periode)]
        factor(periode, levels = lvls)
      }
    ) %>%
    dplyr::filter(!is.na(periode), !is.na(var_en), !is.na(classification), !is.na(percentage)) %>%
    droplevels()
  
  ggplot(df2, aes(x = var_en, y = percentage, fill = classification)) +
    geom_col(position = "fill", width = 0.8) +
    # Show percentage labels inside bars only if the segment is large enough.
    geom_text(
      aes(label = ifelse(percentage >= label_min,
                         scales::number(percentage, accuracy = 1), "")),
      position = position_fill(vjust = 0.5),
      size = 2.5, color = "black", fontface = "bold"
    ) +
    coord_flip() +
    facet_grid(
      group ~ periode,
      scales   = "free_y",
      space    = "free_y",
      labeller = labeller(
        periode = as_labeller(lab_map),
        group   = function(x) rep("", length(x))  # Suppress row facet labels
      )
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(
      name   = "Classification",
      values = c("Same quintile" = "#31A354", "Adjacent quintiles" = "#C7E9C0", "Opposite quintiles" = "yellow")
    ) +
    labs(x = NULL, y = "% Individuals", title = "Quintile classification agreement by campaign") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y        = element_text(size = 8),
      strip.text.y.left  = element_blank(),
      strip.text.x       = element_text(face = "bold"),
      panel.spacing.y    = unit(0.5, "lines"),
      panel.spacing.x    = unit(0.2, "lines"),
      legend.position    = "bottom",
      plot.title         = element_text(hjust = 0.5),
      legend.title       = element_text(hjust = 0.5),
      legend.title.align = 0.5
    )
}

# Produce the quintile chart for a selected subset of food variables.
p_by_campaign <- plot_quintile_by_campaign(
  df        = plot_df_vars,
  keep_vars = c(
    "SOMME_HORS_BOISSON_Poids", "SOMME_POIDS_Poids", "SOMME_KCAL_Poids",
    "FV_Poids", "CAFE_THE_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids",
    "VIANDES_Poids", "AUTRE_PDTS_ANIMAUX_Poids", "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids",
    "CEREALES_PD_Poids", "FEC_NON_RAF_Poids", "FEC_RAF_Poids", "FROMAGES_Poids",
    "FRUITS_Poids", "FRUITS_SECS_Poids", "LAITAGES_Poids", "LEGUMES_Poids",
    "LEG_SECS_Poids", "NOIX_Poids", "OEUFS_Poids", "PDTS_SUCRES_Poids",
    "PORC_Poids", "POULET_Poids", "QUICHES_PIZZAS_TARTES_SALEES_Poids",
    "SNACKS_AUTRES_Poids", "VIANDE_ROUGE_Poids", "FRUITS_JUS_Poids",
    "LAIT_Poids", "SODAS_LIGHT_Poids", "SODAS_SUCRES_Poids",
    "MGA_Poids", "MGV_Poids", "PLATS_PREP_VEGETARIENS_Poids",
    "PLATS_PREP_CARNES_Poids", "POISSONS_Poids", "ALCOOL_Poids",
    "SAUCES_Poids", "DESSERTS_LACTES_Poids", "EAU_Poids"
  ),
  label_map = labels_EN,
  n_cols    = 4
)
print(p_by_campaign)


# 17. TERTILE ANALYSIS----

# Purpose: examine whether FFQ bias differs by level of true consumption.
# Participants are split into three groups based on their BOOKLET quantities:
#   T1 — bottom 20%  (lowest consumers)
#   T2 — middle 60%
#   T3 — top 20%    (highest consumers)
# The FFQ data are then masked to match the same participants per tertile,
# and analyser_moyennes() is re-run for each subgroup.


# Helper function: add_tertiles_id()
# For each _Poids variable, splits participants into T1/T2/T3 based on
# their observed quantity. Adds three new columns per variable:
#   var_T1, var_T2, var_T3 — contains the participant ID if in that group, NA otherwise.
# Ties are broken deterministically using the participant identifier.

add_tertiles_id <- function(df, id_col = "Identifiant") {
  
  poids_vars <- grep("(?i)_Poids$", names(df), value = TRUE, perl = TRUE)
  
  for (var in poids_vars) {
    
    num <- suppressWarnings(as.numeric(as.character(df[[var]])))
    
    # Initialise T1/T2/T3 columns for this variable as NA.
    df[[paste0(var, "_T1")]] <- NA_character_
    df[[paste0(var, "_T2")]] <- NA_character_
    df[[paste0(var, "_T3")]] <- NA_character_
    
    # Indices of rows with a non-missing value.
    valid <- which(!is.na(num))
    n_tot <- length(valid)
    if (n_tot == 0) next   # Skip if no valid observations
    
    # Compute cut-off indices for the 20% / 60% / 20% split.
    n1 <- floor(0.2 * n_tot)
    n2 <- floor(0.8 * n_tot)
    
    # Sort by quantity ascending, with identifier as a tiebreaker.
    ord  <- valid[order(num[valid], df[[id_col]][valid])]
    
    idx1 <- if (n1 > 0)       ord[seq_len(n1)]          else integer(0)
    idx2 <- if (n2 > n1)      ord[(n1 + 1):n2]          else integer(0)
    idx3 <- if (n_tot > n2)   ord[(n2 + 1):n_tot]       else integer(0)
    
    # Write participant IDs into the corresponding tertile column.
    df[[paste0(var, "_T1")]][idx1] <- df[[id_col]][idx1]
    df[[paste0(var, "_T2")]][idx2] <- df[[id_col]][idx2]
    df[[paste0(var, "_T3")]][idx3] <- df[[id_col]][idx3]
  }
  
  df
}


# Simplified version of analyser_moyennes() used in the tertile section.
# Returns only means, n_pairs, Delta, and % bias (no CIs or correlations).

analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Poids"),
                              multiplier = 1000) {
  
  motif        <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  vars_ffq     <- grep(motif, names(ffq_data),     value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  vars_communes <- intersect(vars_ffq, vars_booklet)
  
  if (!length(vars_communes)) stop("No common variable between FFQ and Booklet.")
  
  stats <- lapply(vars_communes, function(var) {
    x <- suppressWarnings(as.numeric(ffq_data[[var]]))
    y <- suppressWarnings(as.numeric(booklet_data[[var]]))
    
    # Convert kg → g except for kcal.
    if (grepl("_Poids$", var) && var != "SOMME_KCAL_Poids") {
      x <- x * multiplier
      y <- y * multiplier
    }
    
    valid <- !is.na(x) & !is.na(y)
    data.frame(
      variable        = var,
      n_pairs         = sum(valid),
      moyenne_Booklet = mean(y[valid], na.rm = TRUE),
      moyenne_FFQ     = mean(x[valid], na.rm = TRUE)
    )
  }) %>%
    bind_rows() %>%
    mutate(
      Delta    = moyenne_FFQ - moyenne_Booklet,
      pct_bias = ifelse(moyenne_Booklet == 0, NA_real_, Delta / moyenne_Booklet * 100),
      across(where(is.numeric), ~ signif(.x, 3))
    )
  
  return(stats)
}


# --- Step 2: Prepare tertile-filtered booklet and FFQ datasets ---

# Collect the "_bis" snapshot booklet datasets for all cohorts.
dfs_booklet <- list(
  nudges1 = sgsdata_Booklet_nudges1_bis,
  nudges2 = sgsdata_Booklet_nudges2_bis,
  IT11    = sgsdata_Booklet_IT11_bis,
  IT12    = sgsdata_Booklet_IT12_bis,
  IT21    = sgsdata_Booklet_IT21_bis,
  IT22    = sgsdata_Booklet_IT22_bis,
  CSGA    = sgsdata_Booklet_CSGA_bis,
  com     = sgsdata_Booklet_com_bis
)

# Add _T1/_T2/_T3 indicator columns to each booklet dataset.
dfs_booklet_tertiles <- imap(dfs_booklet, ~ add_tertiles_id(.x))

# For each cohort and each tertile (k = 1, 2, 3):
# Create a booklet data frame where _Poids values are kept only for participants
# in that tertile (all others set to NA).
for (base in names(dfs_booklet_tertiles)) {
  full_df    <- dfs_booklet_tertiles[[base]]
  poids_vars <- grep("(?i)_poids$", names(full_df), value = TRUE, perl = TRUE)
  
  for (k in 1:3) {
    suffix  <- paste0("_T", k)
    # Extract the T-k mask: one column per _Poids variable, containing ID or NA.
    mask_df <- full_df %>%
      select(ends_with(suffix)) %>%
      rename_with(~ sub(paste0(suffix, "$"), "", .x), ends_with(suffix))
    
    new_df <- full_df %>% select(Identifiant, all_of(poids_vars))
    
    # Set a variable's value to NA for participants NOT in this tertile.
    for (var in poids_vars) {
      if (!var %in% names(mask_df))
        stop("Mask missing column '", var, "' for ", base, "_T", k)
      new_df[[var]] <- ifelse(!is.na(mask_df[[var]]), new_df[[var]], NA)
    }
    
    assign(paste0("sgsdata_Booklet_", base, "_poids_T", k), new_df, envir = .GlobalEnv)
  }
}

# Create matching FFQ datasets restricted to the same T-k participants.
bases_ffq <- c("nudges1", "nudges2", "IT11", "IT12", "IT21", "IT22", "CSGA")
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
    
    # Null out FFQ values for participants whose booklet value is missing in this tertile.
    for (var in common_vars) {
      df_join[[var]] <- ifelse(!is.na(df_join[[paste0(var, ".mask")]]), df_join[[var]], NA_real_)
    }
    
    df_sel <- df_join %>% select(Identifiant, all_of(common_vars))
    assign(paste0("sgsdata_FFQ_", base, "_poids_T", k), df_sel, envir = .GlobalEnv)
  }
}


# --- Step 3: Run analyser_moyennes() for each tertile and each cohort ---

# T1/T2/T3 comparisons.
tab_tertiles <- list()
for (base in bases_ffq) {
  for (k in 1:3) {
    df_b <- get(paste0("sgsdata_Booklet_", base, "_poids_T", k))
    df_f <- get(paste0("sgsdata_FFQ_",     base, "_poids_T", k))
    nm   <- paste0("tableau_final_", base, "_T", k, "_ord")
    tab_tertiles[[nm]] <- analyser_moyennes(df_f, df_b) %>% arrange(variable)
  }
}

# "All" comparison (no tertile split) — used as the reference panel.
bases_all <- bases_ffq
tab_all   <- list()
for (base in bases_all) {
  df_b_full  <- get(paste0("sgsdata_Booklet_", base, "_bis"))
  df_f_full  <- get(paste0("sgsdata_FFQ_",     base, "_bis"))
  poids_b    <- grep("(?i)_poids$", names(df_b_full), value = TRUE, perl = TRUE)
  poids_f    <- grep("(?i)_poids$", names(df_f_full), value = TRUE, perl = TRUE)
  df_b       <- df_b_full %>% select(Identifiant, all_of(poids_b))
  df_f       <- df_f_full %>% select(Identifiant, all_of(poids_f))
  nm         <- paste0("tableau_final_", base, "_Tall_ord")
  tab_all[[nm]] <- analyser_moyennes(df_f, df_b) %>% arrange(variable)
}

# Combine T1/T2/T3 and "All" results into a single tidy data frame.
tbl_list      <- c(tab_tertiles, tab_all)
tableau_final <- bind_rows(tbl_list, .id = "source") %>%
  mutate(source = str_remove(source, "^tableau_final_")) %>%
  tidyr::separate(
    col   = source,
    into  = c("base", "tertile"),
    sep   = "_(?=T)",
    extra = "merge"
  ) %>%
  filter(grepl("_ord$", tertile)) %>%
  filter(!str_ends(variable, "_Kcal"), variable != "SOMME_KCALTOT")


# --- Step 4: Prepare the tertile arrow plot ---

# Focus on the six aggregated food categories.
cibles <- c("FV_Poids", "FEC_Poids", "PDTS_LAITIERS_Poids",
            "PDTS_DISCRETIONNAIRES_Poids", "SSB_Poids", "VIANDES_Poids")

df_plot_all <- tableau_final %>%
  filter(variable %in% cibles,
         !is.na(moyenne_Booklet), !is.na(moyenne_FFQ), !is.na(tertile)) %>%
  mutate(
    # Human-readable wave labels for the legend.
    TertileLabel = fct_recode(
      base,
      "Weekly FFQ 1 (Nov 21)"   = "nudges1",
      "Weekly FFQ 1 (March 22)" = "nudges2",
      "Weekly FFQ 2 (Nov 22)"   = "IT11",
      "Weekly FFQ 2 (March 23)" = "IT12",
      "Weekly FFQ 3 (Nov 23)"   = "IT21",
      "Weekly FFQ 3 (March 24)" = "IT22",
      "Monthly FFQ"                       = "CSGA"
    ),
    # English variable labels for the x-axis.
    variable_en = case_when(
      variable == "FV_Poids"                    ~ "Fruits & Vegetables",
      variable == "FEC_Poids"                   ~ "Starchy Foods",
      variable == "PDTS_LAITIERS_Poids"         ~ "Dairy Products",
      variable == "SSB_Poids"                   ~ "Sugary Sweet Beverages",
      variable == "PDTS_DISCRETIONNAIRES_Poids" ~ "Discretionary Foods",
      variable == "VIANDES_Poids"               ~ "Meats",
      TRUE                                      ~ variable
    ) %>% fct_relevel("Fruits & Vegetables", "Starchy Foods", "Dairy Products",
                      "Sugary Sweet Beverages", "Discretionary Foods", "Meats"),
    # Facet label for each tertile / "all" panel.
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

# Desired wave order for the colour legend (chronological).
wave_levels <- c(
  "Weekly FFQ 1 (Nov 21)",
  "Weekly FFQ 1 (March 22)",
  "Weekly FFQ 2 (Nov 22)",
  "Weekly FFQ 2 (March 23)",
  "Weekly FFQ 3 (Nov 23)",
  "Weekly FFQ 3 (March 24)",
  "Monthly FFQ"
)

# --- Step 5: Build the arrow plot ---
# Each arrow starts at the booklet mean and ends at the FFQ mean.
# Arrows are only drawn when |delta| >= 1 g/day/CU.

gap <- 2.2   # Spacing between food categories on the x-axis
off <- 0.25  # Horizontal offset between waves within one category

df_v <- df_plot_all %>%
  mutate(
    Bin        = fct_relevel(Campaign, "All (Supply data distribution)",
                             "Top 20% (Supply data distribution)",
                             "Middle 60% (Supply data distribution)",
                             "Bottom 20% (Supply data distribution)"),
    Wave       = factor(TertileLabel, levels = wave_levels),
    var_fac    = factor(variable_en),
    var_ord    = as.numeric(var_fac),
    W          = n_distinct(Wave),
    x_base     = var_ord * gap,
    x_pos      = x_base + (as.numeric(Wave) - (W + 1) / 2) * off,
    delta      = moyenne_FFQ - moyenne_Booklet,
    has_change = !is.na(delta) & abs(delta) >= 1  # Only show arrow if difference >= 1 g
  )

# X-axis break positions and labels.
x_breaks         <- df_v %>% distinct(var_ord, x_base) %>% arrange(var_ord) %>% pull(x_base)
x_labels         <- levels(df_v$var_fac) %>% str_wrap(width = 16)
x_labels_spaced  <- str_replace_all(x_labels, " ", "  ")  # Non-breaking spaces for alignment
seps             <- tibble(x = (x_breaks[-1] + x_breaks[-length(x_breaks)]) / 2)  # Vertical separators

# Colour palette (Okabe-Ito, accessible to colour-blind readers).
okabe_ito <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pal       <- setNames(okabe_ito[seq_along(wave_levels)], wave_levels)

ggplot(df_v, aes(color = Wave, group = Wave)) +
  # Vertical dashed lines between food categories.
  geom_vline(data = seps, aes(xintercept = x),
             linetype = "twodash", linewidth = 0.4, color = "#7F7F7F", alpha = 0.7) +
  # Arrows: tail at booklet mean, head at FFQ mean.
  geom_segment(
    data = dplyr::filter(df_v, has_change),
    aes(x = x_pos, xend = x_pos, y = moyenne_Booklet, yend = moyenne_FFQ),
    linewidth = 0.9, lineend = "round", alpha = 0.9,
    arrow = arrow(ends = "last", type = "closed", angle = 14, length = unit(2.4, "mm"))
  ) +
  facet_wrap(~ Bin, ncol = 1, scales = "fixed", strip.position = "top") +
  scale_color_manual(values = pal, breaks = wave_levels, name = "Wave") +
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



# 18. WITHIN-PARTICIPANT CHANGE ANALYSIS----

# Goal: compare the change in food consumption between baseline (Periode 0) and
# follow-up (Periode 1) as captured by the FFQ vs. the booklet.
# Uses the FULL TI and Nudges datasets (all participants, not just UC_TI == 1),
# restricted to households with groupe == 0 (no valid monthly food budget reported).
# UPDATE the paths below to match your local file system.


sgsdata_complet        <- read.xlsx((paste("Fichiers nettoyés/Fichiers traités/sgsdata_IT.xlsx", sep = "")))
sgsdata_nudges_complet <- read.xlsx((paste("sgsdata_nudges.xlsx", sep = "")))

# Re-assign campaign and fill UC_TI for the full dataset.
sgsdata_complet <- sgsdata_complet %>%
  mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"), 2, 1))
sgsdata_complet <- sgsdata_complet %>%
  arrange(Identifiant) %>% group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>% ungroup()
sgsdata_nudges_complet <- sgsdata_nudges_complet %>%
  arrange(Identifiant) %>% group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>% ungroup()

fill_zero <- function(df) { df %>% mutate(across(everything(), ~ ifelse(is.na(.) | . == "", 0, .))) }
sgsdata_complet        <- fill_zero(sgsdata_complet)
sgsdata_nudges_complet <- fill_zero(sgsdata_nudges_complet)

# Inspect the possible values of the monthly budget indicator.
unique(sgsdata_complet$Montant.mensuel.total)

# Encode the budget group:
#   groupe == 1 → valid budget reported ("Ok")
#   groupe == 0 → no valid budget (target population for this analysis)
# Keep only groupe == 0.
sgsdata_complet <- sgsdata_complet %>%
  mutate(
    groupe = case_when(
      Montant.mensuel.total == "Ok" ~ 1,
      Montant.mensuel.total == "0"  ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(groupe == 0)

# 19. Build the booklet dataset from the full TI data ----
# Select booklet weight columns (ending in _CARNET_POIDS), then compute aggregates.
sgsdata_Booklet_IT <- sgsdata_complet %>%
  select(Identifiant, UC_TI, Mesure, Campagne, Periode, groupe, ends_with("_CARNET_POIDS"))

# Aggregated food groups (booklet).
sgsdata_Booklet_IT$FV_CARNET_Poids                    <- sgsdata_Booklet_IT$FRUITS_CARNET_Poids + sgsdata_Booklet_IT$FRUITS_SECS_CARNET_Poids + sgsdata_Booklet_IT$NOIX_CARNET_Poids + sgsdata_Booklet_IT$LEGUMES_CARNET_Poids
sgsdata_Booklet_IT$FEC_CARNET_Poids                   <- sgsdata_Booklet_IT$FEC_NON_RAF_CARNET_Poids + sgsdata_Booklet_IT$FEC_RAF_CARNET_Poids
sgsdata_Booklet_IT$PDTS_LAITIERS_CARNET_Poids         <- sgsdata_Booklet_IT$LAIT_CARNET_Poids + sgsdata_Booklet_IT$LAITAGES_CARNET_Poids + sgsdata_Booklet_IT$FROMAGES_CARNET_Poids
sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Poids          <- sgsdata_Booklet_IT$POULET_CARNET_Poids + sgsdata_Booklet_IT$OEUFS_CARNET_Poids
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Poids    <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_CARNET_Poids + sgsdata_Booklet_IT$JAMBON_BLANC_CARNET_Poids
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_CARNET_Poids     <- sgsdata_Booklet_IT$VIANDE_ROUGE_CARNET_Poids + sgsdata_Booklet_IT$PORC_CARNET_Poids
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_CARNET_Poids <- sgsdata_Booklet_IT$SNACKS_AUTRES_CARNET_Poids + sgsdata_Booklet_IT$CEREALES_PD_CARNET_Poids + sgsdata_Booklet_IT$PDTS_SUCRES_CARNET_Poids
sgsdata_Booklet_IT$SSB_CARNET_Poids                   <- sgsdata_Booklet_IT$SODAS_SUCRES_CARNET_Poids + sgsdata_Booklet_IT$SODAS_LIGHT_CARNET_Poids + sgsdata_Booklet_IT$FRUITS_JUS_CARNET_Poids
# Total weight without beverages (booklet).
sgsdata_Booklet_IT$SOMME_HB_CARNET_Poids <- sgsdata_Booklet_IT$FV_CARNET_Poids + sgsdata_Booklet_IT$FEC_CARNET_Poids +
  sgsdata_Booklet_IT$PDTS_LAITIERS_CARNET_Poids + sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Poids + sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Poids +
  sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_CARNET_Poids + sgsdata_Booklet_IT$DESSERTS_LACTES_CARNET_Poids +
  sgsdata_Booklet_IT$QUICHES_PIZZAS_TARTES_SALEES_CARNET_Poids + sgsdata_Booklet_IT$MGA_CARNET_Poids + sgsdata_Booklet_IT$MGV_CARNET_Poids +
  sgsdata_Booklet_IT$POISSONS_CARNET_Poids + sgsdata_Booklet_IT$LEG_SECS_CARNET_Poids + sgsdata_Booklet_IT$PLATS_PREP_CARNES_CARNET_Poids +
  sgsdata_Booklet_IT$PLATS_PREP_VEGETARIENS_CARNET_Poids + sgsdata_Booklet_IT$SAUCES_CARNET_Poids
sgsdata_Booklet_IT$VIANDES_CARNET_Poids <- sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_CARNET_Poids + sgsdata_Booklet_IT$POULET_OEUFS_CARNET_Poids + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_CARNET_Poids

# Strip "_CARNET" from column names; drop rows where all _Poids columns are zero.
sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  rename_with(~ str_remove_all(.x, "_CARNET"), .cols = everything()) %>%
  filter(if_any(ends_with("_Poids"), ~ . != 0))

# 20. Build the FFQ dataset from the full TI data ----
sgsdata_complet_FFQ <- sgsdata_complet %>%
  select(Identifiant, UC_TI, Mesure, Campagne, Periode, groupe, ends_with("_FFQ_Poids"))

# Aggregated food groups (FFQ).
sgsdata_complet_FFQ$FV_FFQ_Poids                    <- sgsdata_complet_FFQ$FRUITS_FFQ_Poids + sgsdata_complet_FFQ$FRUITS_SECS_FFQ_Poids + sgsdata_complet_FFQ$NOIX_FFQ_Poids + sgsdata_complet_FFQ$LEGUMES_FFQ_Poids
sgsdata_complet_FFQ$FEC_FFQ_Poids                   <- sgsdata_complet_FFQ$FEC_NON_RAF_FFQ_Poids + sgsdata_complet_FFQ$FEC_RAF_FFQ_Poids
sgsdata_complet_FFQ$PDTS_LAITIERS_FFQ_Poids         <- sgsdata_complet_FFQ$LAIT_FFQ_Poids + sgsdata_complet_FFQ$LAITAGES_FFQ_Poids + sgsdata_complet_FFQ$FROMAGES_FFQ_Poids
sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Poids          <- sgsdata_complet_FFQ$POULET_FFQ_Poids + sgsdata_complet_FFQ$OEUFS_FFQ_Poids
sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids    <- sgsdata_complet_FFQ$CHARCUTERIE_HORS_JB_FFQ_Poids + sgsdata_complet_FFQ$JAMBON_BLANC_FFQ_Poids
sgsdata_complet_FFQ$VIANDE_ROUGE_PORC_FFQ_Poids     <- sgsdata_complet_FFQ$VIANDE_ROUGE_FFQ_Poids + sgsdata_complet_FFQ$PORC_FFQ_Poids
sgsdata_complet_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Poids <- sgsdata_complet_FFQ$SNACKS_AUTRES_FFQ_Poids + sgsdata_complet_FFQ$CEREALES_PD_FFQ_Poids + sgsdata_complet_FFQ$PDTS_SUCRES_FFQ_Poids
sgsdata_complet_FFQ$SSB_FFQ_Poids                   <- sgsdata_complet_FFQ$SODAS_SUCRES_FFQ_Poids + sgsdata_complet_FFQ$SODAS_LIGHT_FFQ_Poids + sgsdata_complet_FFQ$FRUITS_JUS_FFQ_Poids
sgsdata_complet_FFQ$SOMME_HB_FFQ_Poids <- sgsdata_complet_FFQ$FV_FFQ_Poids + sgsdata_complet_FFQ$FEC_FFQ_Poids +
  sgsdata_complet_FFQ$PDTS_LAITIERS_FFQ_Poids + sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Poids + sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids +
  sgsdata_complet_FFQ$PDTS_DISCRETIONNAIRES_FFQ_Poids + sgsdata_complet_FFQ$DESSERTS_LACTES_FFQ_Poids +
  sgsdata_complet_FFQ$QUICHES_PIZZAS_TARTES_SALEES_FFQ_Poids + sgsdata_complet_FFQ$MGA_FFQ_Poids + sgsdata_complet_FFQ$MGV_FFQ_Poids +
  sgsdata_complet_FFQ$POISSONS_FFQ_Poids + sgsdata_complet_FFQ$LEG_SECS_FFQ_Poids + sgsdata_complet_FFQ$PLATS_PREP_CARNES_FFQ_Poids +
  sgsdata_complet_FFQ$PLATS_PREP_VEGETARIENS_FFQ_Poids + sgsdata_complet_FFQ$SAUCES_FFQ_Poids
sgsdata_complet_FFQ$VIANDES_FFQ_Poids <- sgsdata_complet_FFQ$AUTRE_PDTS_ANIMAUX_FFQ_Poids + sgsdata_complet_FFQ$POULET_OEUFS_FFQ_Poids + sgsdata_complet_FFQ$VIANDE_ROUGE_PORC_FFQ_Poids

# Strip "_FFQ" from column names; drop rows where all _Poids columns are zero.
sgsdata_complet_FFQ <- sgsdata_complet_FFQ %>%
  rename_with(~ str_remove_all(.x, "_FFQ"), .cols = everything()) %>%
  filter(if_any(ends_with("_Poids"), ~ . != 0))


# 21. Split by campaign and period; join baseline (.x) with follow-up (.y) ----

# Campaign 1 — booklet: keep only single-person, groupe 0, booklet rows.
sgsdata_Booklet_IT11 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode == 0, groupe == 0, Mesure == "Carnet", UC_TI == 1)
sgsdata_Booklet_IT21 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode == 1, groupe == 0, Mesure == "Carnet", UC_TI == 1)
# Wide join: each row = one participant with baseline (.x) and follow-up (.y) side by side.
sgsdata_Booklet_IT1  <- left_join(sgsdata_Booklet_IT11, sgsdata_Booklet_IT21, by = "Identifiant") %>%
  select(matches("Poids|Identifiant")) %>%
  filter(if_any(matches("_Poids\\.(x|y)$"), ~ . != 0))

# Campaign 1 — FFQ: same logic for questionnaire rows.
sgsdata_FFQ_IT11 <- sgsdata_complet_FFQ %>% filter(Campagne == 1, Periode == 0, groupe == 0, Mesure != "Carnet", UC_TI == 1)
sgsdata_FFQ_IT21 <- sgsdata_complet_FFQ %>% filter(Campagne == 1, Periode == 1, groupe == 0, Mesure != "Carnet", UC_TI == 1)
sgsdata_FFQ_IT1  <- left_join(sgsdata_FFQ_IT11, sgsdata_FFQ_IT21, by = "Identifiant") %>%
  select(matches("Poids|Identifiant"))

# Keep only participants who have both baseline AND follow-up, and appear in both instruments.
sgsdata_FFQ_IT1     <- sgsdata_FFQ_IT1     %>% filter(if_all(ends_with(".y"), ~ !is.na(.))) %>% semi_join(sgsdata_Booklet_IT1, by = "Identifiant")
sgsdata_Booklet_IT1 <- sgsdata_Booklet_IT1 %>% filter(if_all(ends_with(".y"), ~ !is.na(.))) %>% semi_join(sgsdata_FFQ_IT1,     by = "Identifiant")

# Campaign 2 — same procedure.
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode == 0, groupe == 0, Mesure == "Carnet", UC_TI == 1)
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode == 1, groupe == 0, Mesure == "Carnet", UC_TI == 1)
sgsdata_Booklet_IT2  <- left_join(sgsdata_Booklet_IT12, sgsdata_Booklet_IT22, by = "Identifiant") %>%
  select(matches("Poids|Identifiant")) %>%
  filter(if_any(matches("_Poids\\.(x|y)$"), ~ . != 0))

sgsdata_FFQ_IT12 <- sgsdata_complet_FFQ %>% filter(Campagne == 2, Periode == 0, groupe == 0, Mesure != "Carnet", UC_TI == 1)
sgsdata_FFQ_IT22 <- sgsdata_complet_FFQ %>% filter(Campagne == 2, Periode == 1, groupe == 0, Mesure != "Carnet", UC_TI == 1)
sgsdata_FFQ_IT2  <- left_join(sgsdata_FFQ_IT12, sgsdata_FFQ_IT22, by = "Identifiant") %>%
  select(matches("Poids|Identifiant"))

sgsdata_FFQ_IT2     <- sgsdata_FFQ_IT2     %>% filter(if_all(ends_with(".y"), ~ !is.na(.))) %>% semi_join(sgsdata_Booklet_IT2, by = "Identifiant")
sgsdata_Booklet_IT2 <- sgsdata_Booklet_IT2 %>% filter(if_all(ends_with(".y"), ~ !is.na(.))) %>% semi_join(sgsdata_FFQ_IT2,     by = "Identifiant")

# Final mutual ID alignment.
sgsdata_FFQ_IT1 <- sgsdata_FFQ_IT1 %>% semi_join(sgsdata_Booklet_IT1, by = "Identifiant")
sgsdata_FFQ_IT2 <- sgsdata_FFQ_IT2 %>% semi_join(sgsdata_Booklet_IT2, by = "Identifiant")



# Function: process_data()
# Summarises a wide (baseline + follow-up) data frame into a single-row
# summary containing, for each selected food group:
#   - .x  = mean baseline quantity (g/day/CU)
#   - .y  = mean follow-up quantity
#   - _diff = follow-up minus baseline (change)
# Columns are renamed to English and converted from kg to g.
# Uncommented food groups shown in the final plot are kept; others are dropped.

process_data <- function(df) {
  
  # Lookup table: French variable name prefix → English display name.
  traductions <- c(
    VIANDES_Poids = "Meats",
    CAFE_THE_Poids = "Coffee / Tea",
    CEREALES_PD_Poids                 = "Breakfast cereals",
    CHARCUTERIE_HORS_JB_Poids         = "Cold cuts excluding white ham",
    DESSERTS_LACTES_Poids             = "Dairy Desserts",
    FEC_NON_RAF_Poids                 = "Unrefined Starches",
    FEC_RAF_Poids                     = "Refined Starches",
    FROMAGES_Poids                    = "Cheeses",
    FRUITS_Poids                      = "Fruits",
    FRUITS_SECS_Poids                 = "Dried Fruits",
    JAMBON_BLANC_Poids                = "White Ham",
    LAITAGES_Poids                    = "Dairy Products",
    LEGUMES_Poids                     = "Vegetables",
    LEG_SECS_Poids                    = "Legumes",
    MGA_Poids                         = "Animal Fats",
    MGV_Poids                         = "Vegetable Fats",
    NOIX_Poids                        = "Nuts",
    OEUFS_Poids                       = "Eggs",
    PDTS_SUCRES_Poids                 = "Sweet products",
    PLATS_PREP_CARNES_Poids           = "Meat Based Prepared Dishes",
    PLATS_PREP_VEGETARIENS_Poids      = "Vegetarian Prepared Dishes",
    POISSONS_Poids                    = "Fish",
    PORC_Poids                        = "Pork",
    POULET_Poids                      = "Chicken",
    QUICHES_PIZZAS_TARTES_SALEES_Poids= "Quiches/ Pizzas/ Savoury Pies",
    SAUCES_Poids                      = "Sauces",
    SNACKS_AUTRES_Poids               = "Other Snacks",
    VIANDE_ROUGE_Poids                = "Red Meat",
    ALCOOL_Poids                      = "Alcohol",
    EAU_Poids                         = "Water",
    FRUITS_JUS_Poids                  = "Fruit Juices",
    LAIT_Poids                        = "Milk",
    SODAS_LIGHT_Poids                 = "Diet Sodas",
    SODAS_SUCRES_Poids                = "Sugary Sodas",
    FV_Poids                          = "Fruits/vegetables",
    FEC_Poids                         = "Starchy foods",
    PDTS_LAITIERS_Poids               = "Dairy products",
    POULET_OEUFS_Poids                = "Eggs / chicken",
    AUTRE_PDTS_ANIMAUX_Poids          = "Cold cuts",
    PLATS_PREP_Poids                  = "Prepared dishes",
    VIANDE_ROUGE_PORC_Poids           = "Red meat/Pork",
    MG_Poids                          = "Added fats",
    PDTS_DISCRETIONNAIRES_Poids       = "Discretionary foods",
    SSB_Poids                         = "Sugary sweet beverages",
    SOMME_HB                          = "Total without beverages"
  )
  
  df %>%
    # 1) Keep only weight and identifier columns.
    select(matches("Poids|Identifiant|KCAL")) %>%
    # 2) Compute column means (averaging over participants).
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    # 3) For each food group present in both .x (baseline) and .y (follow-up),
    #    compute _diff = follow-up - baseline.
    {
      bases_x <- sub("\\.x$", "", grep("\\.x$", names(.), value = TRUE))
      bases_y <- sub("\\.y$", "", grep("\\.y$", names(.), value = TRUE))
      bases   <- intersect(bases_x, bases_y)
      for (b in bases) {
        .[[paste0(b, "_diff")]] <- .[[paste0(b, ".y")]] - .[[paste0(b, ".x")]]
      }
      .
    } %>%
    # 4) Drop follow-up (.y) columns and variables not shown in the final plot.
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
      -starts_with("MG_Poids"),
      -starts_with("AUTRE_PDTS_ANIMAUX_Poids"),
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
    # 5) Rename columns using the French → English lookup.
    rename_with(
      .cols = everything(),
      .fn = function(x) {
        vapply(x, function(col) {
          key <- names(traductions)[vapply(names(traductions), function(k) startsWith(col, k), logical(1))]
          if (length(key)) sub(paste0("^", key), traductions[key], col) else col
        }, character(1))
      }
    ) %>%
    # 6) Convert kg → g and round to 2 significant digits.
    mutate(across(everything(), ~ signif(.x * 1000, 2)))
}

# Apply process_data() to compute mean baseline, follow-up, and change for each dataset.
sgsdata_Booklet_IT1 <- process_data(sgsdata_Booklet_IT1)
sgsdata_FFQ_IT1     <- process_data(sgsdata_FFQ_IT1)
sgsdata_Booklet_IT2 <- process_data(sgsdata_Booklet_IT2)
sgsdata_FFQ_IT2     <- process_data(sgsdata_FFQ_IT2)



# Function: make_df_plot()
# Reshapes the single-row process_data() output into a tidy data frame
# with one row per food group, containing:
#   base       — English food group name
#   x          — baseline mean (g/d/CU)
#   diff       — change (follow-up − baseline) in g/d/CU
#   signe      — direction of change ("Rise" / "Decrease")
#   diff_label — formatted change string (e.g. "+12.00")
# Food groups are ordered by absolute change (largest first).

make_df_plot <- function(df_summary) {
  library(dplyr); library(stringr); library(tibble)
  
  # Extract food group names from ".x" columns (baseline).
  bases <- names(df_summary) %>%
    str_subset("\\.x$") %>%
    str_remove("\\.x$")
  
  if (length(bases) == 0) {
    stop("No '.x' column found. The '.x' columns may have been dropped or renamed in process_data().")
  }
  
  tibble(
    base_raw = bases,
    x        = unlist(df_summary[paste0(bases, ".x")],    use.names = FALSE),
    diff     = unlist(df_summary[paste0(bases, "_diff")], use.names = FALSE)
  ) %>%
    mutate(
      # Clean up the name: remove suffix, replace underscores with spaces, sentence-case.
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
      # Order food groups by absolute change (largest change at the top of the chart).
      ordered_levels <- if (nrow(.) > 1) .$base_clean[order(-abs(.$diff))] else .$base_clean
      mutate(., base = factor(base_clean, levels = unique(ordered_levels)))
    } %>%
    select(base, x, diff, signe, diff_label, x_label_pos)
}

# Reshape the summarised datasets into plot-ready format.
sgsdata_Booklet_IT1 <- make_df_plot(sgsdata_Booklet_IT1)
sgsdata_FFQ_IT1     <- make_df_plot(sgsdata_FFQ_IT1)
sgsdata_Booklet_IT2 <- make_df_plot(sgsdata_Booklet_IT2)
sgsdata_FFQ_IT2     <- make_df_plot(sgsdata_FFQ_IT2)

# Named list of all plot data frames (kept for reference).
list_df_plot <- list(
  Booklet_Winter_23 = sgsdata_Booklet_IT1,
  FFQ_Winter_23     = sgsdata_FFQ_IT1,
  Booklet_Winter_24 = sgsdata_Booklet_IT2,
  FFQ_Winter_24     = sgsdata_FFQ_IT2
)

# Tag each dataset with its campaign label, then stack booklet and FFQ separately.
df_booklet_23  <- sgsdata_Booklet_IT1 %>% mutate(campaign = "Booklet Winter 23")
df_booklet_24  <- sgsdata_Booklet_IT2 %>% mutate(campaign = "Booklet Winter 24")
df_booklet_all <- bind_rows(df_booklet_23, df_booklet_24)

df_ffq_23  <- sgsdata_FFQ_IT1 %>% mutate(campaign = "FFQ Winter 23")
df_ffq_24  <- sgsdata_FFQ_IT2 %>% mutate(campaign = "FFQ Winter 24")
df_ffq_all <- bind_rows(df_ffq_23, df_ffq_24)

# Remove summary rows (total weight, beverages) from the change plots.
df_booklet_all <- df_booklet_all %>%
  filter(!str_detect(as.character(base), regex("total|hors boisson|without beverages", ignore_case = TRUE)))
df_ffq_all <- df_ffq_all %>%
  filter(!str_detect(as.character(base), regex("total|hors boisson|without beverages", ignore_case = TRUE)))



# Function: make_plot_multi()
# Produces a horizontal arrow chart comparing baseline vs. follow-up
# consumption across food groups and campaigns.
# Each arrow: tail = baseline mean, head = baseline + change.
# Labels show the percentage change relative to baseline.
# Arguments:
#   df               — stacked plot data frame (from make_df_plot)
#   titre            — plot title
#   campaign_colors  — named colour vector for campaigns
#   offset           — vertical spacing between arrow tip and label
#   show_origin_point— whether to draw a dot at the baseline value
#   show_zero_line   — whether to draw a dashed line at zero

make_plot_multi <- function(
    df, titre, campaign_colors, offset = 5,
    arrow_len_mm = 4, arrow_angle = 12, arrow_size = 1.4,
    show_origin_point = TRUE,
    show_zero_line = FALSE
) {
  base_levels <- unique(df$base)
  
  df2 <- df %>%
    mutate(
      base       = factor(base, levels = base_levels),
      bar_end    = x + diff,                     # Position of the arrow tip
      label_y    = if_else(diff < 0,
                           pmax(bar_end - offset, -offset * 0.5),
                           bar_end + offset),   # Label position
      hjust_lb   = if_else(diff < 0, 1, 0),
      # Compute percentage change relative to baseline.
      diff_ratio = if_else(x != 0, diff / x, NA_real_),
      diff_label = case_when(
        is.na(diff_ratio) ~ "",
        diff_ratio >= 0   ~ paste0("+", scales::percent(diff_ratio, accuracy = 1, decimal.mark = ",")),
        TRUE              ~ scales::percent(diff_ratio, accuracy = 1, decimal.mark = ",")
      )
    )
  
  dodge <- position_dodge(width = 0.8)
  
  p <- ggplot(df2, aes(x = base, y = x, group = campaign, color = campaign)) +
    { if (show_origin_point) geom_point(position = dodge, size = 2.2, stroke = 0.7) else NULL } +
    # Arrow from baseline to follow-up.
    geom_segment(
      aes(y = x, yend = bar_end),
      position  = dodge,
      linewidth = arrow_size, lineend = "round",
      arrow = arrow(ends = "last", type = "closed",
                    angle = arrow_angle, length = unit(arrow_len_mm, "mm"))
    ) +
    # Connector line from arrow tip to label.
    geom_segment(aes(y = bar_end, yend = label_y), position = dodge, linewidth = 0.6) +
    # Percentage change label.
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

# 22. Produce booklet change plot ----
campaign_cols_booklet <- c(
  "Booklet Winter 23" = "#d95f02",
  "Booklet Winter 24" = "#7570b3"
)

p_booklet <- make_plot_multi(
  df              = df_booklet_all,
  titre           = "Booklet Comparisons",
  campaign_colors = campaign_cols_booklet,
  offset          = 10
) +
  theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y.right = element_blank(),
    plot.margin        = margin(t = 5, r = 50, b = 5, l = 5)
  )

print(p_booklet)

# 23. Produce FFQ change plot ----
campaign_cols_ffq <- c(
  "FFQ Winter 23" = "#d95f02",
  "FFQ Winter 24" = "#7570b3"
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

print(p_ffq)


# 24. Compute percentage change table for all four datasets ----
tbls_plot <- list(
  Booklet_IT1 = sgsdata_Booklet_IT1,
  FFQ_IT1     = sgsdata_FFQ_IT1,
  Booklet_IT2 = sgsdata_Booklet_IT2,
  FFQ_IT2     = sgsdata_FFQ_IT2
)

library(dplyr)
library(purrr)

# For each data frame, compute pct = diff / x * 100 (percentage change from baseline).
wide_pct <- imap_dfr(
  tbls_plot,
  ~ .x %>%
    mutate(
      table_type = .y,
      pct        = diff / x * 100
    )
)



# 25. EXPORT RESULTS TO EXCEL----

# All result tables are written to a single workbook, one sheet per table.
# UPDATE the save path at the bottom to match your local file system.


wb <- createWorkbook()

# df1–df7: classified result tables for each campaign wave (with agreement class).
addWorksheet(wb, "df1"); writeData(wb, sheet = "df1", df1)
addWorksheet(wb, "df2"); writeData(wb, sheet = "df2", df2)
addWorksheet(wb, "df3"); writeData(wb, sheet = "df3", df3)
addWorksheet(wb, "df4"); writeData(wb, sheet = "df4", df4)
addWorksheet(wb, "df5"); writeData(wb, sheet = "df5", df5)
addWorksheet(wb, "df6"); writeData(wb, sheet = "df6", df6)
addWorksheet(wb, "df7"); writeData(wb, sheet = "df7", df7)

# tableau_final: all tertile comparisons stacked (T1/T2/T3/Tall × all cohorts).
addWorksheet(wb, "tableau_final"); writeData(wb, sheet = "tableau_final", tableau_final)

# wide_pct: percentage-change table for the within-participant change analysis.
addWorksheet(wb, "wide_pct"); writeData(wb, sheet = "wide_pct", wide_pct)

# Save the workbook — UPDATE THIS PATH before running.
saveWorkbook(wb, "Comparaison_vf.xlsx")
