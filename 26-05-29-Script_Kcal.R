
# SCRIPT: FFQ vs. Food Supply Booklet — Kilocalorie Validation
# PURPOSE: Companion script to Script_Weight.R.
#          Same structure and logic, but all food variables are expressed in
#          kilocalories (_Kcal suffix) instead of grams (_Poids suffix).
#          No unit conversion is applied (multiplier = 1).




# 1. PACKAGE LOADING----

rm(list = ls())  # Clear the environment before starting

#install.packages("modelsummary")  # Uncomment to install if needed

library(haven); library(readxl); library(tidyverse); library(openxlsx); library(car)
library(readxl); library(dplyr); library(broom); library(scales); library(modelsummary)
library(ggplot2); library(effsize); library(lfe); library(ggpubr); library(vtable)
library("openxlsx"); library("dplyr"); library("tidyr"); library(ggplot2)
library("gridExtra"); library("RColorBrewer"); library(reshape2); library(Metrics)
library("poLCA"); library("webshot"); library(nlme); library(fixest); library(plm); library(lmtest)
library("htmltools"); library(clubSandwich); library(Matrix); library(lme4)
library(cobalt); library(knitr); library(tableone); library(purrr); library("plotly")
library("htmlwidgets")
library(ggrepel)


# 2. IMPORT RAW DATASETS----

# UPDATE these paths to match your local file system.

# Commented-out path reference (kept for traceability):


sgsdata_CSGA   <- read.xlsx((paste("sgsdata.xlsx", sep = "")))
sgsdata_TI     <- read.xlsx((paste("sgsdata_IT.xlsx", sep = "")))
sgsdata_nudges <- read.xlsx((paste("sgsdata_nudges.xlsx", sep = "")))
FFQ_NOV_23     <- read.xlsx((paste("23-11_FFQ.xlsx", sep = "")))

sgsdata_TI <- sgsdata_TI  # No transformation — kept for clarity

# Assign campaign based on participant identifier:
# IDs containing "PS" or "LE" → campaign 2; all others → campaign 1.
sgsdata_TI <- sgsdata_TI %>% mutate(Campagne = if_else(str_detect(Identifiant, "PS|LE"), 2, 1))

# Fill missing UC_TI values within each participant (downward then upward).
sgsdata_TI <- sgsdata_TI %>%
  arrange(Identifiant) %>% group_by(Identifiant) %>%
  fill(UC_TI, .direction = "downup") %>% ungroup()

# Replace all NA values and empty strings with 0.
fill_zero <- function(df) { df %>% mutate(across(everything(), ~ ifelse(is.na(.) | . == "", 0, .))) }

sgsdata_TI      <- fill_zero(sgsdata_TI)
sgsdata_CSGA    <- fill_zero(sgsdata_CSGA)
sgsdata_nudges  <- fill_zero(sgsdata_nudges)

# Split the CSGA dataset into FFQ rows and booklet rows.
sgsdata_FFQ_CSGA     <- sgsdata_CSGA %>% filter(Mesure != "Carnet")
sgsdata_Booklet_CSGA <- sgsdata_CSGA %>% filter(Mesure == "Carnet")



# 3. CSGA — HARMONIZATION AND AGGREGATION (kilocalories)----

# Harmonisation notes:
# - White ham ("jambon blanc") is included in the cold cuts category for CSGA.
# - Vegetable fats (MGV), vegetarian prepared meals, sauces, and dairy desserts
#   are excluded because these categories do not exist in the Nudges dataset.

# --- CSGA FFQ ---
sgsdata_FFQ_CSGA <- sgsdata_FFQ_CSGA %>%
  # Keep only participants who also have booklet data.
  semi_join(sgsdata_Booklet_CSGA, by = "Identifiant") %>%
  # Drop all-zero numeric columns, then remove the "_FFQ" suffix.
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

# Build aggregated food-group energy variables for the CSGA FFQ dataset.
sgsdata_FFQ_CSGA$FV_Kcal                    <- sgsdata_FFQ_CSGA$FRUITS_Kcal + sgsdata_FFQ_CSGA$FRUITS_SECS_Kcal + sgsdata_FFQ_CSGA$NOIX_Kcal + sgsdata_FFQ_CSGA$LEGUMES_Kcal        # Fruits & vegetables
sgsdata_FFQ_CSGA$FEC_Kcal                   <- sgsdata_FFQ_CSGA$FEC_NON_RAF_Kcal + sgsdata_FFQ_CSGA$FEC_RAF_Kcal                                                                      # Starchy foods
sgsdata_FFQ_CSGA$PDTS_LAITIERS_Kcal         <- sgsdata_FFQ_CSGA$LAIT_Kcal + sgsdata_FFQ_CSGA$LAITAGES_Kcal + sgsdata_FFQ_CSGA$FROMAGES_Kcal                                          # Dairy products
sgsdata_FFQ_CSGA$POULET_OEUFS_Kcal          <- sgsdata_FFQ_CSGA$POULET_Kcal + sgsdata_FFQ_CSGA$OEUFS_Kcal                                                                             # Poultry and eggs
sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Kcal    <- sgsdata_FFQ_CSGA$CHARCUTERIE_HORS_JB_Kcal                                                                                              # Cold cuts (excl. white ham — included in CHARCUTERIE for CSGA)
sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Kcal     <- sgsdata_FFQ_CSGA$VIANDE_ROUGE_Kcal + sgsdata_FFQ_CSGA$PORC_Kcal                                                                       # Red meat and pork
sgsdata_FFQ_CSGA$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_CSGA$SNACKS_AUTRES_Kcal + sgsdata_FFQ_CSGA$CEREALES_PD_Kcal + sgsdata_FFQ_CSGA$PDTS_SUCRES_Kcal                           # Discretionary foods
sgsdata_FFQ_CSGA$SSB_Kcal                   <- sgsdata_FFQ_CSGA$SODAS_SUCRES_Kcal + sgsdata_FFQ_CSGA$SODAS_LIGHT_Kcal + sgsdata_FFQ_CSGA$FRUITS_JUS_Kcal                             # Sugar-sweetened and diet beverages + fruit juices
sgsdata_FFQ_CSGA$SOMME_KCAL_Kcal            <- sgsdata_FFQ_CSGA$KCAL_TOTAL_Kcal                                                                                                       # Total energy intake
sgsdata_FFQ_CSGA$VIANDES_Kcal               <- sgsdata_FFQ_CSGA$POULET_OEUFS_Kcal + sgsdata_FFQ_CSGA$VIANDE_ROUGE_PORC_Kcal + sgsdata_FFQ_CSGA$AUTRE_PDTS_ANIMAUX_Kcal              # Total meats

# --- CSGA Booklet ---
sgsdata_Booklet_CSGA <- sgsdata_Booklet_CSGA %>%
  # Keep only participants who also have FFQ data.
  semi_join(sgsdata_FFQ_CSGA, by = "Identifiant") %>%
  # Drop all-zero columns and remove the "_CARNET" suffix.
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))

# Build the same aggregated food-group energy variables for the CSGA booklet dataset.
sgsdata_Booklet_CSGA$FV_Kcal                    <- sgsdata_Booklet_CSGA$FRUITS_Kcal + sgsdata_Booklet_CSGA$FRUITS_SECS_Kcal + sgsdata_Booklet_CSGA$NOIX_Kcal + sgsdata_Booklet_CSGA$LEGUMES_Kcal
sgsdata_Booklet_CSGA$FEC_Kcal                   <- sgsdata_Booklet_CSGA$FEC_NON_RAF_Kcal + sgsdata_Booklet_CSGA$FEC_RAF_Kcal
sgsdata_Booklet_CSGA$PDTS_LAITIERS_Kcal         <- sgsdata_Booklet_CSGA$LAIT_Kcal + sgsdata_Booklet_CSGA$LAITAGES_Kcal + sgsdata_Booklet_CSGA$FROMAGES_Kcal
sgsdata_Booklet_CSGA$POULET_OEUFS_Kcal          <- sgsdata_Booklet_CSGA$POULET_Kcal + sgsdata_Booklet_CSGA$OEUFS_Kcal
sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Kcal    <- sgsdata_Booklet_CSGA$CHARCUTERIE_HORS_JB_Kcal
sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Kcal     <- sgsdata_Booklet_CSGA$VIANDE_ROUGE_Kcal + sgsdata_Booklet_CSGA$PORC_Kcal
sgsdata_Booklet_CSGA$MG_Kcal                    <- sgsdata_Booklet_CSGA$MGA_Kcal                                                                                                       # Animal fats
sgsdata_Booklet_CSGA$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_Booklet_CSGA$SNACKS_AUTRES_Kcal + sgsdata_Booklet_CSGA$CEREALES_PD_Kcal + sgsdata_Booklet_CSGA$PDTS_SUCRES_Kcal
sgsdata_Booklet_CSGA$SSB_Kcal                   <- sgsdata_Booklet_CSGA$SODAS_SUCRES_Kcal + sgsdata_Booklet_CSGA$SODAS_LIGHT_Kcal + sgsdata_Booklet_CSGA$FRUITS_JUS_Kcal
sgsdata_Booklet_CSGA$SOMME_KCAL_Kcal            <- sgsdata_Booklet_CSGA$KCAL_TOTAL_Kcal
sgsdata_Booklet_CSGA$VIANDES_Kcal               <- sgsdata_Booklet_CSGA$POULET_OEUFS_Kcal + sgsdata_Booklet_CSGA$VIANDE_ROUGE_PORC_Kcal + sgsdata_Booklet_CSGA$AUTRE_PDTS_ANIMAUX_Kcal



# 4. TI (IT) — HARMONIZATION AND AGGREGATION (kilocalories)----

# Keep only single-person households (UC_TI == 1).
sgsdata_FFQ_IT     <- sgsdata_TI %>% filter(Mesure != "Carnet", UC_TI == 1)
sgsdata_Booklet_IT <- sgsdata_TI %>% filter(Mesure == "Carnet",  UC_TI == 1)

# Remove booklet rows where every CARNET column is zero or missing.
sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  filter(
    rowSums(
      across(contains("CARNET"), ~ !is.na(.) & . != 0),
      na.rm = TRUE
    ) > 0
  )

# --- TI FFQ ---
sgsdata_FFQ_IT <- sgsdata_FFQ_IT %>%
  semi_join(sgsdata_Booklet_IT, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

# Build aggregated food-group energy variables for the TI FFQ dataset.
# Note: for TI, white ham (JAMBON_BLANC) is a separate variable added to cold cuts.
sgsdata_FFQ_IT$FV_Kcal                    <- sgsdata_FFQ_IT$FRUITS_Kcal + sgsdata_FFQ_IT$FRUITS_SECS_Kcal + sgsdata_FFQ_IT$NOIX_Kcal + sgsdata_FFQ_IT$LEGUMES_Kcal
sgsdata_FFQ_IT$FEC_Kcal                   <- sgsdata_FFQ_IT$FEC_NON_RAF_Kcal + sgsdata_FFQ_IT$FEC_RAF_Kcal
sgsdata_FFQ_IT$PDTS_LAITIERS_Kcal         <- sgsdata_FFQ_IT$LAIT_Kcal + sgsdata_FFQ_IT$LAITAGES_Kcal + sgsdata_FFQ_IT$FROMAGES_Kcal
sgsdata_FFQ_IT$POULET_OEUFS_Kcal          <- sgsdata_FFQ_IT$POULET_Kcal + sgsdata_FFQ_IT$OEUFS_Kcal
sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Kcal    <- sgsdata_FFQ_IT$CHARCUTERIE_HORS_JB_Kcal + sgsdata_FFQ_IT$JAMBON_BLANC_Kcal  # Cold cuts including white ham
sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Kcal     <- sgsdata_FFQ_IT$VIANDE_ROUGE_Kcal + sgsdata_FFQ_IT$PORC_Kcal
sgsdata_FFQ_IT$MG_Kcal                    <- sgsdata_FFQ_IT$MGA_Kcal
sgsdata_FFQ_IT$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_IT$SNACKS_AUTRES_Kcal + sgsdata_FFQ_IT$CEREALES_PD_Kcal + sgsdata_FFQ_IT$PDTS_SUCRES_Kcal
sgsdata_FFQ_IT$SSB_Kcal                   <- sgsdata_FFQ_IT$SODAS_SUCRES_Kcal + sgsdata_FFQ_IT$SODAS_LIGHT_Kcal + sgsdata_FFQ_IT$FRUITS_JUS_Kcal
sgsdata_FFQ_IT$SOMME_KCAL_Kcal            <- sgsdata_FFQ_IT$KCAL_TOTAL_Kcal
sgsdata_FFQ_IT$VIANDES_Kcal               <- sgsdata_FFQ_IT$POULET_OEUFS_Kcal + sgsdata_FFQ_IT$VIANDE_ROUGE_PORC_Kcal + sgsdata_FFQ_IT$AUTRE_PDTS_ANIMAUX_Kcal

# --- TI Booklet ---
sgsdata_Booklet_IT <- sgsdata_Booklet_IT %>%
  semi_join(sgsdata_FFQ_IT, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))

# Build aggregated food-group energy variables for the TI booklet dataset.
sgsdata_Booklet_IT$FV_Kcal                    <- sgsdata_Booklet_IT$FRUITS_Kcal + sgsdata_Booklet_IT$FRUITS_SECS_Kcal + sgsdata_Booklet_IT$NOIX_Kcal + sgsdata_Booklet_IT$LEGUMES_Kcal
sgsdata_Booklet_IT$FEC_Kcal                   <- sgsdata_Booklet_IT$FEC_NON_RAF_Kcal + sgsdata_Booklet_IT$FEC_RAF_Kcal
sgsdata_Booklet_IT$PDTS_LAITIERS_Kcal         <- sgsdata_Booklet_IT$LAIT_Kcal + sgsdata_Booklet_IT$LAITAGES_Kcal + sgsdata_Booklet_IT$FROMAGES_Kcal
sgsdata_Booklet_IT$POULET_OEUFS_Kcal          <- sgsdata_Booklet_IT$POULET_Kcal + sgsdata_Booklet_IT$OEUFS_Kcal
sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Kcal    <- sgsdata_Booklet_IT$CHARCUTERIE_HORS_JB_Kcal
sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Kcal     <- sgsdata_Booklet_IT$VIANDE_ROUGE_Kcal + sgsdata_Booklet_IT$PORC_Kcal
sgsdata_Booklet_IT$MG_Kcal                    <- sgsdata_Booklet_IT$MGA_Kcal
sgsdata_Booklet_IT$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_Booklet_IT$SNACKS_AUTRES_Kcal + sgsdata_Booklet_IT$CEREALES_PD_Kcal + sgsdata_Booklet_IT$PDTS_SUCRES_Kcal
sgsdata_Booklet_IT$SSB_Kcal                   <- sgsdata_Booklet_IT$SODAS_SUCRES_Kcal + sgsdata_Booklet_IT$SODAS_LIGHT_Kcal + sgsdata_Booklet_IT$FRUITS_JUS_Kcal
sgsdata_Booklet_IT$SOMME_KCAL_Kcal            <- sgsdata_Booklet_IT$KCAL_TOTAL_Kcal
sgsdata_Booklet_IT$VIANDES_Kcal               <- sgsdata_Booklet_IT$POULET_OEUFS_Kcal + sgsdata_Booklet_IT$VIANDE_ROUGE_PORC_Kcal + sgsdata_Booklet_IT$AUTRE_PDTS_ANIMAUX_Kcal



# 5. NUDGES — HARMONIZATION AND AGGREGATION (kilocalories)----

# UC_TI.x is used here because this variable comes from the merged nudges dataset.
sgsdata_FFQ_nudges     <- sgsdata_nudges %>% filter(Mesure != "Carnet", UC_TI == 1)
sgsdata_Booklet_nudges <- sgsdata_nudges %>% filter(Mesure == "Carnet",  UC_TI.x == 1)

# --- Nudges FFQ ---
sgsdata_FFQ_nudges <- sgsdata_FFQ_nudges %>%
  semi_join(sgsdata_Booklet_nudges, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_FFQ", ""))

# Build aggregated food-group energy variables for the Nudges FFQ dataset.
sgsdata_FFQ_nudges$FV_Kcal                    <- sgsdata_FFQ_nudges$FRUITS_Kcal + sgsdata_FFQ_nudges$FRUITS_SECS_Kcal + sgsdata_FFQ_nudges$NOIX_Kcal + sgsdata_FFQ_nudges$LEGUMES_Kcal
sgsdata_FFQ_nudges$FEC_Kcal                   <- sgsdata_FFQ_nudges$FEC_NON_RAF_Kcal + sgsdata_FFQ_nudges$FEC_RAF_Kcal
sgsdata_FFQ_nudges$PDTS_LAITIERS_Kcal         <- sgsdata_FFQ_nudges$LAIT_Kcal + sgsdata_FFQ_nudges$LAITAGES_Kcal + sgsdata_FFQ_nudges$FROMAGES_Kcal
sgsdata_FFQ_nudges$POULET_OEUFS_Kcal          <- sgsdata_FFQ_nudges$POULET_Kcal + sgsdata_FFQ_nudges$OEUFS_Kcal
sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Kcal    <- sgsdata_FFQ_nudges$CHARCUTERIE_HORS_JB_Kcal + sgsdata_FFQ_nudges$JAMBON_BLANC_Kcal
sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Kcal     <- sgsdata_FFQ_nudges$VIANDE_ROUGE_Kcal + sgsdata_FFQ_nudges$PORC_Kcal
sgsdata_FFQ_nudges$MG_Kcal                    <- sgsdata_FFQ_nudges$MGA_Kcal
sgsdata_FFQ_nudges$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_FFQ_nudges$SNACKS_AUTRES_Kcal + sgsdata_FFQ_nudges$CEREALES_PD_Kcal + sgsdata_FFQ_nudges$PDTS_SUCRES_Kcal
sgsdata_FFQ_nudges$SSB_Kcal                   <- sgsdata_FFQ_nudges$SODAS_SUCRES_Kcal + sgsdata_FFQ_nudges$SODAS_LIGHT_Kcal + sgsdata_FFQ_nudges$FRUITS_JUS_Kcal
sgsdata_FFQ_nudges$SOMME_KCAL_Kcal            <- sgsdata_FFQ_nudges$SOMME_KCAL  # Nudges uses SOMME_KCAL directly (not KCAL_TOTAL_Kcal)
sgsdata_FFQ_nudges$VIANDES_Kcal               <- sgsdata_FFQ_nudges$POULET_OEUFS_Kcal + sgsdata_FFQ_nudges$VIANDE_ROUGE_PORC_Kcal + sgsdata_FFQ_nudges$AUTRE_PDTS_ANIMAUX_Kcal

# --- Nudges Booklet ---
sgsdata_Booklet_nudges <- sgsdata_Booklet_nudges %>%
  semi_join(sgsdata_FFQ_nudges, by = "Identifiant") %>%
  select(Identifiant, where(~ !(is.numeric(.) && all(. == 0)))) %>%
  rename_with(~ str_replace_all(.x, "_CARNET", ""))
#rename_with(~ paste0(.x, "_Kcal"), .cols = 28:61)  # Alternative renaming — currently unused

# Build aggregated food-group energy variables for the Nudges booklet dataset.
sgsdata_Booklet_nudges$FV_Kcal                    <- sgsdata_Booklet_nudges$FRUITS_Kcal + sgsdata_Booklet_nudges$FRUITS_SECS_Kcal + sgsdata_Booklet_nudges$NOIX_Kcal + sgsdata_Booklet_nudges$LEGUMES_Kcal
sgsdata_Booklet_nudges$FEC_Kcal                   <- sgsdata_Booklet_nudges$FEC_NON_RAF_Kcal + sgsdata_Booklet_nudges$FEC_RAF_Kcal
sgsdata_Booklet_nudges$PDTS_LAITIERS_Kcal         <- sgsdata_Booklet_nudges$LAIT_Kcal + sgsdata_Booklet_nudges$LAITAGES_Kcal + sgsdata_Booklet_nudges$FROMAGES_Kcal
sgsdata_Booklet_nudges$POULET_OEUFS_Kcal          <- sgsdata_Booklet_nudges$POULET_Kcal + sgsdata_Booklet_nudges$OEUFS_Kcal
sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Kcal    <- sgsdata_Booklet_nudges$CHARCUTERIE_HORS_JB_Kcal
sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Kcal     <- sgsdata_Booklet_nudges$VIANDE_ROUGE_Kcal + sgsdata_Booklet_nudges$PORC_Kcal
sgsdata_Booklet_nudges$MG_Kcal                    <- sgsdata_Booklet_nudges$MGA_Kcal
sgsdata_Booklet_nudges$PDTS_DISCRETIONNAIRES_Kcal <- sgsdata_Booklet_nudges$SNACKS_AUTRES_Kcal + sgsdata_Booklet_nudges$CEREALES_PD_Kcal + sgsdata_Booklet_nudges$PDTS_SUCRES_Kcal
sgsdata_Booklet_nudges$SSB_Kcal                   <- sgsdata_Booklet_nudges$SODAS_SUCRES_Kcal + sgsdata_Booklet_nudges$SODAS_LIGHT_Kcal + sgsdata_Booklet_nudges$FRUITS_JUS_Kcal
sgsdata_Booklet_nudges$SOMME_KCAL_Kcal            <- sgsdata_Booklet_nudges$SOMME_KCAL
sgsdata_Booklet_nudges$VIANDES_Kcal               <- sgsdata_Booklet_nudges$POULET_OEUFS_Kcal + sgsdata_Booklet_nudges$VIANDE_ROUGE_PORC_Kcal + sgsdata_Booklet_nudges$AUTRE_PDTS_ANIMAUX_Kcal


# 6. SPLIT BY CAMPAIGN AND PERIOD----


# TI — Campaign 1
sgsdata_Booklet_IT11 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode == 0)  # Baseline
sgsdata_Booklet_IT21 <- sgsdata_Booklet_IT %>% filter(Campagne == 1, Periode == 1)  # Follow-up
sgsdata_FFQ_IT11     <- sgsdata_FFQ_IT     %>% filter(Campagne == 1, Periode == 0)
sgsdata_FFQ_IT21     <- sgsdata_FFQ_IT     %>% filter(Campagne == 1, Periode == 1)

# TI — Campaign 2
sgsdata_Booklet_IT12 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode == 0)
sgsdata_Booklet_IT22 <- sgsdata_Booklet_IT %>% filter(Campagne == 2, Periode == 1)
sgsdata_FFQ_IT12     <- sgsdata_FFQ_IT     %>% filter(Campagne == 2, Periode == 0)
sgsdata_FFQ_IT22     <- sgsdata_FFQ_IT     %>% filter(Campagne == 2, Periode == 1)

# Nudges — split by period only
sgsdata_Booklet_nudges1 <- sgsdata_Booklet_nudges %>% filter(Periode == 0)
sgsdata_Booklet_nudges2 <- sgsdata_Booklet_nudges %>% filter(Periode == 1)
sgsdata_FFQ_nudges1     <- sgsdata_FFQ_nudges     %>% filter(Periode == 0)
sgsdata_FFQ_nudges2     <- sgsdata_FFQ_nudges     %>% filter(Periode == 1)



# 7. STACK ALL SUB-DATASETS INTO COMBINED FFQ AND BOOKLET FRAMES----


# Collect all FFQ sub-datasets; keep only columns common to all sources.
dfs <- list(
  nudges1 = sgsdata_FFQ_nudges1,
  nudges2 = sgsdata_FFQ_nudges2,
  IT11    = sgsdata_FFQ_IT11,
  IT12    = sgsdata_FFQ_IT12,
  IT21    = sgsdata_FFQ_IT21,
  IT22    = sgsdata_FFQ_IT22
)

common_cols     <- Reduce(intersect, lapply(dfs, names))  # Columns present in ALL sub-datasets
sgsdata_FFQ_all <- dfs %>%
  map(~ select(.x, all_of(common_cols))) %>%
  bind_rows(.id = "source")  # "source" column records the origin sub-dataset

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
harmoniser_ids <- function(df1, df2, id_col = "Identifiant") {
  ids_communs <- intersect(df1[[id_col]], df2[[id_col]])
  df1_filtre  <- df1 %>% filter(.data[[id_col]] %in% ids_communs)
  df2_filtre  <- df2 %>% filter(.data[[id_col]] %in% ids_communs)
  list(df1 = df1_filtre, df2 = df2_filtre)
}

# Apply to every FFQ–booklet pair.
pair_IT11 <- harmoniser_ids(sgsdata_Booklet_IT11, sgsdata_FFQ_IT11)
sgsdata_Booklet_IT11 <- pair_IT11$df1; sgsdata_FFQ_IT11 <- pair_IT11$df2

pair_IT12 <- harmoniser_ids(sgsdata_Booklet_IT12, sgsdata_FFQ_IT12)
sgsdata_Booklet_IT12 <- pair_IT12$df1; sgsdata_FFQ_IT12 <- pair_IT12$df2

pair_IT21 <- harmoniser_ids(sgsdata_Booklet_IT21, sgsdata_FFQ_IT21)
sgsdata_Booklet_IT21 <- pair_IT21$df1; sgsdata_FFQ_IT21 <- pair_IT21$df2

pair_IT22 <- harmoniser_ids(sgsdata_Booklet_IT22, sgsdata_FFQ_IT22)
sgsdata_Booklet_IT22 <- pair_IT22$df1; sgsdata_FFQ_IT22 <- pair_IT22$df2

pair_nudges1 <- harmoniser_ids(sgsdata_Booklet_nudges1, sgsdata_FFQ_nudges1)
sgsdata_Booklet_nudges1 <- pair_nudges1$df1; sgsdata_FFQ_nudges1 <- pair_nudges1$df2

pair_nudges2 <- harmoniser_ids(sgsdata_Booklet_nudges2, sgsdata_FFQ_nudges2)
sgsdata_Booklet_nudges2 <- pair_nudges2$df1; sgsdata_FFQ_nudges2 <- pair_nudges2$df2


# 9. CORE ANALYSIS FUNCTION: analyser_moyennes()----

# Identical to Script_Weight.R except:
#   - suffixes = "_Kcal"  (energy variables instead of weight variables)
#   - multiplier = 1      (no unit conversion needed — values are already in kcal)
#
# For each _Kcal variable, computes per matched FFQ–booklet pair:
#   means, 95% CIs, % bias, paired t-test, Pearson r, Spearman ρ,
#   and quintile agreement (same / adjacent / opposite).

analyser_moyennes <- function(ffq_data, booklet_data,
                              suffixes   = c("_Kcal"),
                              multiplier = 1,
                              conf_level = 0.95) {
  library(dplyr)
  library(tidyr)
  
  # --- Step 0: Align rows by participant ID ---
  if ("Identifiant" %in% names(ffq_data) && "Identifiant" %in% names(booklet_data)) {
    commun_ids   <- intersect(ffq_data$Identifiant, booklet_data$Identifiant)
    ffq_data     <- ffq_data     %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
    booklet_data <- booklet_data %>% filter(Identifiant %in% commun_ids) %>% arrange(Identifiant)
  }
  if (nrow(ffq_data) != nrow(booklet_data)) {
    stop("ffq_data and booklet_data do not have the same number of rows after alignment.")
  }
  
  # --- Step 1: Identify columns ending with the target suffix ---
  motif        <- paste0("(", paste(suffixes, collapse = "|"), ")$")
  vars_ffq     <- grep(motif, names(ffq_data),     value = TRUE)
  vars_booklet <- grep(motif, names(booklet_data), value = TRUE)
  
  if (length(vars_ffq)     == 0) stop("No FFQ column ending with ",     paste(suffixes, collapse = ", "))
  if (length(vars_booklet) == 0) stop("No Booklet column ending with ", paste(suffixes, collapse = ", "))
  
  # --- Step 2: List _Kcal columns (no transformation needed; multiplier = 1) ---
  kcal_ffq     <- grep("_Kcal$", vars_ffq,     value = TRUE)
  kcal_booklet <- grep("_Kcal$", vars_booklet, value = TRUE)
  
  # --- Step 3: Compute FFQ column means (no unit conversion applied) ---
  moy_ffq <- ffq_data %>%
    select(all_of(vars_ffq)) %>%
    { if (length(kcal_ffq) > 0) mutate(., across(all_of(kcal_ffq), ~ .x)) else . } %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_FFQ")
  
  # --- Step 4: Compute Booklet column means ---
  moy_booklet <- booklet_data %>%
    select(all_of(vars_booklet)) %>%
    { if (length(kcal_booklet) > 0) mutate(., across(all_of(kcal_booklet), ~ .x)) else . } %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "moyenne_Booklet")
  
  # --- Step 5: Join means into a base table ---
  tableau_base <- left_join(moy_booklet, moy_ffq, by = "variable")
  
  # --- Step 6: Per-variable statistics on complete pairs ---
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
    
    # Unit adjustment: only applied to _Poids variables; _Kcal variables are unchanged.
    # (multiplier = 1, so this block has no effect for this script)
    if (grepl("_Poids$", var) && var != "SOMME_KCAL_Poids") {
      x <- x * multiplier
      y <- y * multiplier
    }
    
    # Means.
    moyenne_FFQ     <- mean(x)
    moyenne_Booklet <- mean(y)
    
    # Percentage bias: (FFQ - Booklet) / Booklet * 100.
    pct_bias <- ifelse(
      moyenne_Booklet == 0, NA,
      (moyenne_FFQ - moyenne_Booklet) / moyenne_Booklet * 100
    )
    
    # Paired t-test (both measures from the same participant) and individual 95% CIs.
    t_diff <- t.test(x, y, paired = TRUE)
    t_ffq  <- t.test(x, conf.level = conf_level)
    t_book <- t.test(y, conf.level = conf_level)
    
    # Pearson and Spearman correlations (only if both have non-zero variance).
    if (sd(x) > 0 && sd(y) > 0) {
      pearson_tst  <- cor.test(x, y, method = "pearson",  exact = FALSE)
      spearman_tst <- cor.test(x, y, method = "spearman", exact = FALSE)
      pearson_est  <- unname(pearson_tst$estimate);  pearson_p  <- pearson_tst$p.value
      spearman_est <- unname(spearman_tst$estimate); spearman_p <- spearman_tst$p.value
    } else {
      pearson_est <- NA; pearson_p <- NA; spearman_est <- NA; spearman_p <- NA
    }
    
    # Quintile agreement:
    # "Adjacent" = gap of 1 or 2 quintiles (broad definition).
    # "Opposite" = gap of 3 or 4 quintiles.
    qx <- dplyr::ntile(x, 5)
    qy <- dplyr::ntile(y, 5)
    d  <- abs(qx - qy)
    pct_similar  <- mean(d == 0,         na.rm = TRUE) * 100
    pct_adjacent <- mean(d %in% c(1, 2), na.rm = TRUE) * 100
    pct_opposite <- mean(d %in% c(3, 4), na.rm = TRUE) * 100
    
    # Assemble result row for this variable.
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
  
  # --- Step 7: Final table — round all numeric columns to 2 significant digits ---
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

sgsdata_FFQ_IT11_bis        <- sgsdata_FFQ_IT11
sgsdata_Booklet_IT11_bis    <- sgsdata_Booklet_IT11
sgsdata_FFQ_IT12_bis        <- sgsdata_FFQ_IT12
sgsdata_Booklet_IT12_bis    <- sgsdata_Booklet_IT12
sgsdata_Booklet_IT21_bis    <- sgsdata_Booklet_IT21
sgsdata_FFQ_IT21_bis        <- sgsdata_FFQ_IT21
sgsdata_Booklet_IT22_bis    <- sgsdata_Booklet_IT22
sgsdata_FFQ_IT22_bis        <- sgsdata_FFQ_IT22
sgsdata_Booklet_nudges1_bis <- sgsdata_Booklet_nudges1
sgsdata_FFQ_nudges1_bis     <- sgsdata_FFQ_nudges1
sgsdata_Booklet_nudges2_bis <- sgsdata_Booklet_nudges2
sgsdata_FFQ_nudges2_bis     <- sgsdata_FFQ_nudges2
sgsdata_FFQ_CSGA_bis        <- sgsdata_FFQ_CSGA
sgsdata_Booklet_CSGA_bis    <- sgsdata_Booklet_CSGA



# 11. RUN analyser_moyennes() FOR ALL 7 CAMPAIGN × COHORT COMBINATIONS----

# tableau_final_1 → Nudges, period 0 (Nov 2021)
tableau_final_1 <- analyser_moyennes(sgsdata_FFQ_nudges1, sgsdata_Booklet_nudges1) %>% arrange(variable)
# tableau_final_2 → Nudges, period 1 (March 2022)
tableau_final_2 <- analyser_moyennes(sgsdata_FFQ_nudges2, sgsdata_Booklet_nudges2) %>% arrange(variable)
# tableau_final_3 → TI campaign 1, period 0 (Nov 2022)
tableau_final_3 <- analyser_moyennes(sgsdata_FFQ_IT11, sgsdata_Booklet_IT11) %>% arrange(variable)
# tableau_final_4 → TI campaign 1, period 1 (March 2023)
tableau_final_4 <- analyser_moyennes(sgsdata_FFQ_IT21, sgsdata_Booklet_IT21) %>% arrange(variable)
# tableau_final_5 → TI campaign 2, period 0 (Nov 2023)
tableau_final_5 <- analyser_moyennes(sgsdata_FFQ_IT12, sgsdata_Booklet_IT12) %>% arrange(variable)
# tableau_final_6 → TI campaign 2, period 1 (March 2024)
tableau_final_6 <- analyser_moyennes(sgsdata_FFQ_IT22, sgsdata_Booklet_IT22) %>% arrange(variable)
# tableau_final_7 → CSGA (monthly FFQ, Nov 2022)
tableau_final_7 <- analyser_moyennes(sgsdata_FFQ_CSGA, sgsdata_Booklet_CSGA) %>% arrange(variable)



# 12. REORDER VARIABLES AND ADD DELTA COLUMN----


# Fixed display order for energy variables across all result tables.
# Variables commented out are excluded from the heatmap.
variables <- c(
  "VIANDES_Kcal",
  "CEREALES_PD_Kcal",
  #"CHARCUTERIE_HORS_JB_Kcal",   # Excluded (absorbed into AUTRE_PDTS_ANIMAUX)
  "FEC_NON_RAF_Kcal",
  "FEC_RAF_Kcal",
  "FROMAGES_Kcal",
  "FRUITS_Kcal",
  "FRUITS_SECS_Kcal",
  #"JAMBON_BLANC_Kcal",           # Excluded (absorbed into AUTRE_PDTS_ANIMAUX for TI/Nudges)
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
  #"POULET_OEUFS_Kcal",           # Excluded (disaggregated into POULET and OEUFS)
  "AUTRE_PDTS_ANIMAUX_Kcal",
  "VIANDE_ROUGE_PORC_Kcal",
  "PDTS_DISCRETIONNAIRES_Kcal",
  "SSB_Kcal",
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

library(dplyr)
# Template used as a left-join key to impose the variable order in all result tables.
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

# Apply to all seven result tables.
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
# Identical logic to Script_Weight.R — applied to all seven tables.
#
# Levels (evaluated in order):
#   "Intensely"     → both r ≥ 0.6  AND  means not significantly different
#   "Strongly"      → one r ≥ 0.6 and the other ≥ 0.4, means not sig. diff.
#   "Substantially" → both r ≥ 0.4  AND  means not significantly different
#   "Moderately"    → at least one r ≥ 0.4  AND  means not significantly different
#   "Poorly"        → at least one r ≥ 0.4  BUT  means ARE significantly different
#   "Weakly"        → no r ≥ 0.4
#   "Missing"       → one of the statistics is NA

tableau1_ord <- tableau1_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau2_ord <- tableau2_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau3_ord <- tableau3_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau4_ord <- tableau4_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau5_ord <- tableau5_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau6_ord <- tableau6_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))

tableau7_ord <- tableau7_ord %>% mutate(classe = case_when(
  pearson_correlation >= 0.6 & spearman_correlation >= 0.6 & p_value_diff >= 0.05 ~ "Intensely. The two correlation coefficients are at least 0.6 and the averages are not significantly different",
  ((pearson_correlation >= 0.6 & spearman_correlation >= 0.4) | (spearman_correlation >= 0.6 & pearson_correlation >= 0.4)) & p_value_diff >= 0.05 ~ "Strongly. One of the two correlation coefficients is at least 0.6 and the other at least 0.4, and the averages are not significantly different",
  pearson_correlation >= 0.4 & spearman_correlation >= 0.4 & p_value_diff >= 0.05 ~ "Substantially. The two correlation coefficients are at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff >= 0.05 ~ "Moderately. At least one of the two correlation coefficients is at least 0.4 and the means are not significantly different",
  (spearman_correlation >= 0.4 | pearson_correlation >= 0.4) & p_value_diff < 0.05 ~ "Poorly. At least one of the two correlation coefficients is at least 0.4, but the means are statistically different",
  spearman_correlation < 0.4 & pearson_correlation < 0.4 ~ "Weakly. No correlation coefficient at least 0.4.",
  is.na(spearman_p_value) | is.na(pearson_p_value) | is.na(p_value_diff) ~ "Missing"
))



# 14. PREPARE DATA FOR THE CORRELATION HEATMAP----


# Tag each result table with its measurement period, then stack.
df1 <- tableau1_ord %>% mutate(periode = "Nov21_TI")
df2 <- tableau2_ord %>% mutate(periode = "March22_TI")
df3 <- tableau3_ord %>% mutate(periode = "Nov22_TI")
df4 <- tableau4_ord %>% mutate(periode = "March23_TI")
df5 <- tableau5_ord %>% mutate(periode = "Nov23_TI")
df6 <- tableau6_ord %>% mutate(periode = "March24_TI")
df7 <- tableau7_ord %>% mutate(periode = "Nov22 (CSGA)")

# Combine all periods; keep only _Kcal rows (drop any residual _Poids rows).
heat_df <- bind_rows(df1, df2, df3, df4, df5, df6, df7) %>%
  filter(!str_ends(variable, "_Poids"))

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

# Ordered x-axis period levels.
period_levels <- c(
  "Nov21_TI", "March22_TI",
  "Nov22_TI", "March23_TI",
  "Nov23_TI", "March24_TI",
  "Nov22 (CSGA)"
)

# Short x-axis tick labels.
labels_x <- setNames(
  c("Nov 21", "Mar 22", "Nov 22", "Mar 23", "Nov 23", "Mar 24", "Nov22"),
  period_levels
)

# English labels for food energy variables (y-axis of the heatmap).
labels_EN <- c(
  CEREALES_PD_Kcal                    = "Breakfast cereals",
  CAFE_THE_Kcal                       = "Coffee and tea",
  DESSERTS_LACTES_Kcal                = "Dairy Desserts",
  FEC_NON_RAF_Kcal                    = "Unrefined Starches",
  FEC_RAF_Kcal                        = "Refined Starches",
  FROMAGES_Kcal                       = "Cheeses",
  FRUITS_Kcal                         = "Fruits",
  FRUITS_SECS_Kcal                    = "Dried Fruits",
  LAITAGES_Kcal                       = "Dairy Products",
  LEGUMES_Kcal                        = "Vegetables",
  LEG_SECS_Kcal                       = "Legumes",
  MGA_Kcal                            = "Animal Fats",
  MGV_Kcal                            = "Vegetable Fats",
  NOIX_Kcal                           = "Nuts",
  OEUFS_Kcal                          = "Eggs",
  PDTS_SUCRES_Kcal                    = "Sweet products",
  PLATS_PREP_CARNES_Kcal              = "Meat Based Prepared Dishes",
  PLATS_PREP_VEGETARIENS_Kcal         = "Vegetarian Prepared Dishes",
  POISSONS_Kcal                       = "Fish",
  PORC_Kcal                           = "Pork",
  POULET_Kcal                         = "Chicken",
  QUICHES_PIZZAS_TARTES_SALEES_Kcal   = "Quiches, Pizzas & Savoury Pies",
  SAUCES_Kcal                         = "Sauces",
  SNACKS_AUTRES_Kcal                  = "Other Snacks",
  VIANDE_ROUGE_Kcal                   = "Red Meat",
  ALCOOL_Kcal                         = "Alcohol",
  EAU_Kcal                            = "Water",
  FRUITS_JUS_Kcal                     = "Fruit Juices",
  LAIT_Kcal                           = "Milk",
  SODAS_LIGHT_Kcal                    = "Diet Sodas",
  SODAS_SUCRES_Kcal                   = "Sugary Sodas",
  FV_Kcal                             = "Fruits and vegetables",
  FEC_Kcal                            = "Starchy foods",
  PDTS_LAITIERS_Kcal                  = "Dairy products",
  #POULET_OEUFS_Kcal                  = "Eggs / chicken",  # Excluded
  AUTRE_PDTS_ANIMAUX_Kcal             = "Cold cuts",
  VIANDE_ROUGE_PORC_Kcal              = "Red meat/Pork",
  PDTS_DISCRETIONNAIRES_Kcal          = "Discretionary foods",
  SSB_Kcal                            = "Sugary sweet beverages",
  SOMME_KCAL_Kcal                     = "Total kilocalories",
  VIANDES_Kcal                        = "Meats"
)

# Build df_all: add facet categories, wave groups, and recode explicit NAs as "Missing".
df_all <- heat_df %>%
  mutate(
    # Row facet: one general indicator (total kcal), six aggregated food groups, rest are specific.
    category = case_when(
      variable %in% c("SOMME_KCAL_Kcal") ~ "General\nindicator",
      variable %in% c(
        "FV_Kcal", "FEC_Kcal", "PDTS_LAITIERS_Kcal",
        "VIANDES_Kcal", "PDTS_DISCRETIONNAIRES_Kcal", "SSB_Kcal"
      ) ~ "General\nfood item",
      TRUE ~ "Specific\nfood item"
    ) %>% factor(levels = c("General\nindicator", "General\nfood item", "Specific\nfood item")),
    
    # Column facet: group periods into FFQ waves.
    wave = case_when(
      periode %in% c("Nov21_TI", "March22_TI") ~ "Weekly FFQ 1",
      periode %in% c("Nov22_TI", "March23_TI") ~ "Weekly FFQ 2",
      periode %in% c("Nov23_TI", "March24_TI") ~ "Weekly FFQ 3",
      periode == "Nov22 (CSGA)"                 ~ "Monthly FFQ 1"
    ) %>% factor(levels = c("Weekly FFQ 1", "Weekly FFQ 2", "Weekly FFQ 3", "Monthly FFQ 1", "All")),
    
    periode = factor(periode, levels = period_levels),
    classe  = fct_explicit_na(classe, na_level = "Missing")
  )

# Order variables from best to worst average agreement score.
df_all <- df_all %>%
  mutate(classe_num = as.integer(factor(classe, levels = levels_classe)))

var_ord <- df_all %>%
  group_by(variable) %>%
  summarise(score = mean(classe_num, na.rm = TRUE), .groups = "drop") %>%
  arrange(score) %>%
  pull(variable)

df_all <- df_all %>%
  mutate(variable = factor(variable, levels = var_ord))

# Sanity check: all classification levels must have a colour. Should print character(0).
print(setdiff(levels(df_all$classe), names(palette_custom_named)))



# 15. CORRELATION HEATMAP (energy intake)----

# Identical layout to Script_Weight.R; title changed to "Correlation of energy intake Variables".
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
    limits = levels_classe,
    drop   = FALSE,
    labels = function(x) str_wrap(x, width = 30)
  ) +
  labs(title = "Correlation of energy intake Variables", x = NULL, y = NULL) +
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



# 16. EXPORT RESULTS TO EXCEL----

# Each result table is written to a separate sheet in a single workbook.
# UPDATE the save path below to match your local file system.


wb <- createWorkbook()

# df1–df7: classified result tables for each campaign wave.
addWorksheet(wb, "df1"); writeData(wb, sheet = "df1", df1)
addWorksheet(wb, "df2"); writeData(wb, sheet = "df2", df2)
addWorksheet(wb, "df3"); writeData(wb, sheet = "df3", df3)
addWorksheet(wb, "df4"); writeData(wb, sheet = "df4", df4)
addWorksheet(wb, "df5"); writeData(wb, sheet = "df5", df5)
addWorksheet(wb, "df6"); writeData(wb, sheet = "df6", df6)
addWorksheet(wb, "df7"); writeData(wb, sheet = "df7", df7)

# Save the workbook — UPDATE THIS PATH before running.
saveWorkbook(wb, paste0("Comparaison_Kcal.xlsx"))
