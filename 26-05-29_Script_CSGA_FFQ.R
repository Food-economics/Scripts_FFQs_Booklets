
# SCRIPT: CSGA FFQ — Frequency-to-Weight and Energy Conversion
# PURPOSE: Transform raw CSGA FFQ frequency data into daily food intake
#          estimates (weight in kg/day and energy in kcal/day) per participant,
#          by multiplying consumption frequencies by portion sizes.
#          The same food groups are also computed from a separate CSGA
#          pre-calculated quantity dataset (CSGA_POIDS) for cross-checking.
# OUTPUT: FFQ_CSGA.xlsx — indicator table, corrected frequencies,
#         and corrected weight tables.



# 1. Package loading ----
# WORKING ENVIRONMENT SETUP
## Package import
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(readxl);library(dplyr);library(broom);library(scales)
library(modelsummary);library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");
library("dplyr");library("tidyr");library("ggplot2");library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library(questionr)


# 2. Import raw data ----
## Data import
setwd <- paste0("")


### Import CSGA data
# Raw FFQ response data — one row per participant, one column per food item.
#raw data
CSGA_base <- read_excel(
  path  = "Données_CSGA.xlsx",
  sheet = "ffq_donnees-brutes"
)

# Remove stray quotation marks and parentheses from all cells.
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\"","",.))
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\\(", "",.))
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\\)", "",.))

# Transformed FFQ data (SUVIMAX-style transformation applied upstream).
#CSGA TRANSFORMED DATA
CSGA_FREQ <- read_excel(
  path  = "Données_CSGA.xlsx",
  sheet = "ffq_donnees-transform-suvimax"
)

# Remove stray quotation marks and parentheses from all cells.
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\"","",.))
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\\(", "",.))
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\\)", "",.))

# Extract the "Quantite" columns from the transformed dataset as a numeric data frame.
# This gives the pre-calculated daily quantity (g or ml) per food item.
#WEIGHT
quant_cols <- grep("Quantite", names(CSGA_FREQ), value = TRUE)

CSGA_POIDS <- setNames(
  data.frame(
    Code = CSGA_FREQ$Code,
    lapply(
      CSGA_FREQ[grep("Quantite", names(CSGA_FREQ))],
      function(x) as.numeric(as.character(x))
    )
  ),
  c("Code", grep("Quantite", names(CSGA_FREQ), value = TRUE))
)

# Free memory — CSGA_FREQ no longer needed after extracting the quantity columns.
CSGA_FREQ <- NULL

### Import reference tables
# CALNUT: nutritional composition table — used to compute kcal from food weights.
# Encodage: mapping table between CSGA FFQ column names and standardized food names.
# Taille_Portion: portion-size reference table (size codes → grams).
# resultats_pondérés: weighted average nutritional values by TI food group.
# Poids_unitaires_manquants: unit weights for foods with no portion size photo.
CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_FFQ.xlsx")
Encodage <- read_xlsx(paste("Freq_FFQ.xlsx"))
Taille_Portion <- read_xlsx(paste("Taille portion.xlsx"))


# 3. Frequency recoding — portion size labels ----
# Translate portion size codes (0–5) into standardized size labels
# (e.g. "A", "B", "C", "Plus petit que A", "Plus grand que C").
# Applied to food groups that have a photo-based portion size question.
#HARMONIZE PORTION SIZE NAMES
#Translation of portion frequencies to TI-equivalent labels
# Vectorize the column names to recode
cols_to_recode <- c(
  "QTLEGUMES_1_3", "QTCRUDITES_1_3", "QTFRITES_1_3", "QTSTEAK_1_3",
  "QTVIANDE_1_3", "QTPOULET_1_3", "QTCHARCUT_1_3", "QTPOISSON_1_3",
  "QTPIZZA_1_3", "QTPATES_1_3", "QTFROM_1_3", "QTFROMRAP_1_3",
  "QTTARTE_1_3", "QTBAGUET_1_3", "QTPAINTR_1_3"
)

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ dplyr::recode(as.character(.),
                    `0` = "B", #As we will multiply by frequencies we can directly impute the average value
                    `1` = "Plus petit que A",
                    `2` = "A",
                    `3` = "B",
                    `4` = "C",
                    `5` = "Plus grand que C",
                    .default = NA_character_)
  ))


# 4. Frequency recoding — beverages ----
# Recode beverage consumption frequency codes (1–8) into
# numeric daily frequency values (times/day).

#Translation of beverage consumption frequencies
# Vectorize the column names to recode
cols_to_recode <- c( "boi_1_3" , "boi_2_3",  "boi_3_3" , "boi_4_3", "boi_5_3" , "boi_6_3",
                     "alc_1_3" , "alc_2_3", "alc_3_3" , "alc_4_3")

library(dplyr)

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 0.066,
      . == 3 ~ 0.142,
      . == 4 ~ 0.498,
      . == 5 ~ 1,
      . == 6 ~ 3,
      . == 7 ~ 6,
      . == 8 ~ 9,
      TRUE    ~ NA_real_
    )
  ))


# 5. Frequency recoding — dairy beverages ----
# Same recoding logic applied to milk and dairy drink frequency columns.

#Translation of dairy beverage consumption frequencies
# Vectorize the column names to recode
cols_to_recode <- c(
  "lai_1_3" , "lait_2_3", "lai_3_3" , "lai_4_3","lai_5_3" , "lai_6_3")

library(dplyr)

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 0.066,
      . == 3 ~ 0.142,
      . == 4 ~ 0.498,
      . == 5 ~ 1,
      . == 6 ~ 3,
      . == 7 ~ 6,
      . == 8 ~ 9,
      TRUE    ~ NA_real_
    )
  ))


# 6. Frequency recoding — all other food groups ----
# Recode consumption frequency codes (1–6) into numeric daily frequency values
# for all remaining food categories (bread, vegetables, starches, meats, fish,
# prepared dishes, cheeses, fruits, snacks, sauces, etc.).

#Translation of beverage consumption frequencies
# Vectorize the column names to recode
cols_to_recode <- c(
  "pai_1_3",  "pai_2_3",  "pai_3_3",  "pai_4_3", "cru_1_3",  "cru_2_3", "cru_3_3",  "cru_4_3",
  "leg_1_3",  "leg_2_3","leg_3_3",  "leg_4_3","leg_5_3",  "leg_6_3","leg_7_3",  "leg_8_3","leg_9_3",  "leg_10_3",
  "leg_11_3", "leg_12_3", "leg_13_3", "fec_1_3", "fec_2_3",  "fec_3_3", "fec_4_3",  "fec_5_3", "fec_6_3",  "fec_7_3",
  "fec_8_3",  "fec_9_3","fec_10_3","via_1_3",  "via_2_3","via_3_3",  "via_4_3","via_5_3",  "via_6_3",
  "aba_1_3",  "aba_2_3","aba_3_3",  "aba_4_3",  "aba_5_3",  "aba_6_3","aba_7_3",  "aba_8_3","aba_9_3",  "poi_2_3","poi_3_3",  "poi_4_3",
  "poi_5_3",  "poi_6_3","poi_7_3",  "poi_8_3","poi_9_3",  "pla_1_3","pla_2_3",  "pla_3_3","pla_4_3",  "pla_5_3",
  "pla_6_3",  "pla_8_3", "pla_9_3",  "pla_10_3", "pla_11_3",  "pla_12_3", "pla_13_3",  "fro_1_3", "fro_2_3",  "fro_3_3",
  "fro_4_3",  "fro_5_3","fro_6_3",  "fro_7_3","fro_8_3",  "fro_9_3","fro_10_3",  "fro_11_3","fru_1_3",  "fru_2_3",
  "fru_3_3",  "fru_4_3","fru_5_3",  "fru_6_3","fru_7_3",  "fru_8_3","fru_9_3",  "fru_10_3","fru_11_3",  "bis_1_3","bis_2_3",  "bis_3_3",
  "bis_4_3",  "bis_5_3","bis_6_3",  "bis_7_3","bis_8_3",  "bis_9_3","bis_10_3",  "bis_11_3","bis_12_3",  "bis_13_3","bis_14_3",  "sau_1_3",
  "sau_2_3",  "sau_3_3","sau_4_3",  "sau_5_3","sau_6_3",  "sau_7_3")

CSGA_base <- CSGA_base %>%
  mutate(across(
    all_of(cols_to_recode),
    ~ dplyr::recode(
      as.numeric(.),
      `1` = 0,
      `2` = 0.066,
      `3` = 0.142,
      `4` = 0.498,
      `5` = 1,
      `6` = 2,
      .default = NA_real_
    )
  ))


# 7. Build a unified frequency table aligned to the TI format ----
# Rename CSGA raw columns to match the standardized food names from the
# Encodage reference table, then extract only the renamed columns into Frame.

# CREATE A UNIFORM TABLE ACROSS ALL CAMPAIGNS
data <- CSGA_base   #left_join(CSGA_base, CSGA_FREQ, by="Code")
data$Biscottes.consommees.hors.petit.dejeuner.portion <- data$QTPAINTR_1_3
# Save original column names to identify which ones were renamed later.
#Save original names to later remove unused columns
orig_names <- names(data)

# Extract the encoding column name.
# Extract encoding column name
column_name <- "CSGA_FREQ"

# Retrieve old vs. new name vectors from the Encodage reference table.
# Retrieve old vs. new name vectors
anciens  <- Encodage[[column_name]]
nouveaux <- Encodage$Aliment

# Loop over name pairs and rename matching columns.
# Loop to rename
for (i in seq_along(anciens)) {
  pattern <- anciens[i]
  new_nm  <- make.names(nouveaux[i])
  # Which current columns contain this pattern?
  cols_to_rename <- grep(pattern, names(data), value = TRUE)
  # Rename each match
  if (length(cols_to_rename) > 0) {
    names(data)[ names(data) %in% cols_to_rename ] <- new_nm
  }
}

# Identify columns that were renamed (i.e. newly standardized columns).
# Identify renamed columns
renamed_cols <- setdiff(names(data), orig_names)
# Keep only those columns in Frame.
#keep only renamed ones
Frame <- data[, renamed_cols, drop = FALSE]


# Verification report: check which original patterns matched how many columns.
# Verification
report <- tibble( ancien   = anciens, nouveau  = nouveaux) %>%
  mutate(
    # Which original columns match the pattern?
    matched = map(ancien, ~ grep(.x, orig_names, value = TRUE)),
    n_match = map_int(matched, length),
    # Build a readable text field
    matched = map_chr(matched, ~ paste(.x, collapse = ", ")),
    new_name = make.names(nouveau))


## Build a table grouping all variables across campaigns
# Use the first column of Encodage as the column header template.
colonne_chaligne <- Encodage[, 1]
Frame <- data.frame(t(colonne_chaligne))
new_column_names <- Frame[1,]
names(Frame) <- new_column_names
Frame <- Frame[-1, ]


## Populate Frame with values from data
# Trim Frame to the same number of rows as data if needed.
if (nrow(Frame) != nrow(data)) { Frame <- Frame[1:nrow(data), ]}
colonnes_communes <- intersect(names(data), names(Frame))

Frame[colonnes_communes] <- data[colonnes_communes]
Frame$Biscottes.consommees.hors.petit.dejeuner.portion


# 8. Recode beverage portion type columns ----
# Convert portion type codes (0–6) into standardized size labels
# (Type A–F, or NA for code 0) for beverage items.
cols <- c(
  "THE.portion","CAFE.portion","EAU_BOUTEILLE.portion","EAU_ROB.portion",
  "JUS.portion","SIROP.portion","NON_LIGHT.portion","LIGHT.portion",
  "LAIT_ENT.portion","LAIT_DEMI.portion","LAIT_ECR.portion",
  "CACAO.portion","LAIT_VEGE.portion"
)

# Loading + recoding
Frame <- Frame %>%
  mutate(across(
    all_of(cols),
    ~ dplyr::recode(as.character(.x),
                    `0` = NA_character_,
                    `1` = "Type A",
                    `2` = "Type B",
                    `3` = "Type C",
                    `4` = "Type D",
                    `5` = "Type E",
                    `6` = "Type F", #Note: CSGA FFQ has one response option with no associated photo for milk
                    .default = NA_character_)
  ))


# 9. Participant exclusions ----
# Remove entirely empty rows, then exclude participants living
# with parents, in shared housing, or listed as invalid identifiers.

#Remove participants living with their parents
Frame <- Frame[rowSums(is.na(Frame)) < ncol(Frame), ]
#Remove people living with parents or in shared accommodation
Frame <- subset(Frame, !lieuVie %in% c(1,2,4,5,9,10))
Frame <- subset(Frame, !Numero.d.identifiant %in% c("accakf","ajtblk","azajdj","aibbga"))


# 10. Utility functions for frequency and weight extraction ----
# COMPUTE CORRECTION COEFFICIENTS TO ADJUST FREQUENCIES

## Helper function to replace NA with zero
replace_na_with_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
## Function to extract numeric frequency data from a given column of the Frame table
FREQ_intake <- function(data, x) {
  result <- as.numeric(data[[x]])
  result[is.na(result)] <- 0
  return(result)}


# 11. Portion size assignment ----
# ASSIGN PORTION SIZES
## Define the remplacer_poids function to replace size codes with weights using a debug message
# The function remplacer_poids takes two arguments (taille = size label, aliment = food name):
# - Displays a processing message.
# - Looks up the weight in Taille_Portion_long for the given food and size.
# - Returns NA if no match is found; otherwise returns the matched weight.
remplacer_poids <- function(taille, aliment) {message("Processing food: ", aliment, " and size: ", taille)
  poids <- Taille_Portion_long %>%
    filter(Aliment == aliment, Taille == taille) %>%
    pull(Poids)
  if (length(poids) == 0) {message("No match found for ", aliment, " with size ", taille)
    return(NA)}
  return(poids)}


## Build the portion weight table
### Filter portion columns
# Some food groups have a portion size (vegetables, crudités, fish, steaks,
# savoury pies, sweet pies...).
# Isolate these columns and build a new data frame "data_duplicated"
# that contains only those values.
colonnes_portion <- grep("portion$", names(Frame), value = TRUE)
Frame <- Frame %>%
  rename(Identifiant = Numero.d.identifiant)

Frame_duplicated <- subset(Frame, select = c("Identifiant", colonnes_portion))

### Duplicate portion columns once per specific food item within a general category
# The for loop iterates over each unique category in Taille_Portion$Catégorie:
# - Skips NA categories.
# - Selects Frame columns whose names start with the category name.
# - Counts how many times the category appears in Taille_Portion.
# - Duplicates the selected columns that many times and appends them to Frame_duplicated.
for (categorie in unique(Taille_Portion$Catégorie)) {
  if (is.na(categorie)) next
  # Find columns whose name starts with the category value
  column_names <- grep(paste0("^", categorie), names(Frame), value = TRUE)
  
  # Select the matching columns
  columns <- Frame[, column_names, drop = FALSE]
  
  nb_repeats <- sum(Taille_Portion$Catégorie == categorie, na.rm = TRUE)
  for (i in 1:nb_repeats) {
    Frame_duplicated <- cbind(Frame_duplicated, columns)
  }
}


# This step harmonizes the structure of the data frame.
# Each row is a participant, and categories have been copied
# once per specific food item they contain.
# Initialize Poids from Frame_duplicated
# Print the data frame with duplicated columns
Poids <- print(Frame_duplicated)

# Separate the first column (Identifiant) from the rest.
debut <- Poids[, 1]
fin <- Poids[, -c(1)]

# Sort remaining columns alphabetically.
fin_trie <- fin[, order(names(fin))]

# Merge both parts back together.
Poids <- cbind(debut, fin_trie)

# Keep only columns whose name ends with a digit (food-specific portion columns).
colonnes_a_garder <- grep("\\d$", names(Poids), value = TRUE)

# Select only columns ending with a digit.
Poids <- Poids[, colonnes_a_garder]

# Identify column names and count occurrences.
colnames_data <- names(Poids)

# Count occurrences of each column name.
occurrences <- table(colnames_data)


# Identify duplicate columns that do NOT end with a digit — to be removed.
colonnes_a_supprimer <- names(occurrences[occurrences > 1])
colonnes_a_supprimer <- colonnes_a_supprimer[!grepl("\\d$", colonnes_a_supprimer)]
# Remove duplicate columns not ending with a digit.
data_filtre <- Poids[, !names(Poids) %in% colonnes_a_supprimer]
# Print the filtered data frame.
print("Filtered DataFrame:")
Poids <- print(data_filtre)

### Rename copied columns with the specific food item name
# Rename portion columns using the food-specific names from Taille_Portion.
new_column_names <- Taille_Portion$Aliment[match(names(Poids), Taille_Portion$Catégorie2)]
names(Poids) <- new_column_names

# Ensure all size columns in Taille_Portion are character type before pivoting.
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

### Apply the function to replace size labels with weights.
# Convert Taille_Portion to long format:
# - pivot_longer stacks all size columns into two new columns: Taille (size label) and Poids (weight).
# - na.omit removes rows with missing values.
# - filter excludes the "Catégorie" and "Catégorie2" rows (not size labels).
Taille_Portion_long <- Taille_Portion %>%
  pivot_longer(cols = -Aliment, names_to = "Taille", values_to = "Poids") %>%
  na.omit() %>%
  filter(Taille != "Catégorie" & Taille != "Catégorie2")

### Replace size labels with weights by applying remplacer_poids to each cell
Poids_modifie <- Poids
for (col in names(Poids_modifie)) {
  Poids_modifie[[col]] <- sapply(Poids_modifie[[col]], function(taille) remplacer_poids(taille, col))
}

colnames(Poids_modifie)

### Add unit weights for foods that have a fixed (non-variable) portion size
# filtered_df1 = subset of Taille_Portion_long where Taille == "Poids_unitaire".
# For each row, assign the fixed weight value to the corresponding food column in Poids_modifie.
filtered_df1 <- subset(Taille_Portion_long, Taille == "Poids_unitaire")
for (i in 1:nrow(filtered_df1)) {
  aliment <- filtered_df1$Aliment[i]
  valeur <- filtered_df1$Poids[i]
  Poids_modifie[[aliment]] <- valeur
}

# Finalize the weight table: add Identifiant, convert all columns to numeric.
# Finalize the table
Poids_modifie <- cbind(Frame$Identifiant, Poids_modifie)
names(Poids_modifie)[1] <- "Identifiant"
Poids_modifie[,-1] <- lapply(Poids_modifie[,-1], as.numeric)

# Remove beverage portion columns — beverages are handled separately via frequency recoding.
#REMOVE BEVERAGE COLUMNS
cols_to_remove <- c(
  "du.cacao.ou.chocolat.en.poudre",
  "de.cafe.y.compris.decafeine",
  "d.eau.du.robinet.verre",
  "d.eau.en.bouteille.ou.bonbonne.verre",
  "de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre",
  "de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre",
  "de.sirop.verre",
  "de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre",
  "de.lait.demi.ecreme",
  "de.lait.ecreme",
  "de.lait.entier.",
  "de.lait.vegetal.sojarizavoine.",
  "de.the"
)

Poids_modifie <- Poids_modifie %>%
  dplyr::select(-dplyr::any_of(cols_to_remove))

# Replace remaining NAs in Poids_modifie with 0, then remove rows with missing/zero ID.
Poids_modifie[is.na(Poids_modifie)] <- 0

Poids_modifie <- Poids_modifie[Poids_modifie$Identifiant != 0, ]
Frame <- Frame[!is.na(Frame$Identifiant), ]


# 12. Compute food weight consumed per food group ----
# CALCULATE WEIGHT OF CONSUMED FOODS
# Multiply consumption frequencies by portion sizes for each food item.
# Results are stored both in a grouped summary (FFQ_POIDS) and a detailed
# item-level table (FFQ_POIDS_Int).
FFQ_POIDS <-data.frame(Frame$Identifiant)
names(FFQ_POIDS)[1] = "Identifiant"

FFQ_POIDS_Int <-data.frame(Frame$Identifiant)
names(FFQ_POIDS_Int )[1] = "Identifiant"

##ALCOOL (alcohol) ----
FFQ_POIDS$ALCOOL_FFQ <- rep(0, nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")

# Initialize a vector to store recognized columns.
recognized_columns <- list()

# Loop over categories to check which columns are recognized.
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "No column recognized"
  }
}

terms <- numeric(nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$ALCOOL_FFQ <- terms

# CSGA pre-calculated alcohol quantity (ml → kg).
CSGA_POIDS$ALCOOL_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cidrebiere + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.vin.blancroserouge +
                            CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.alcools.forts + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.aperitifs)/1000

##CAFE_THE (coffee and tea) ----
FFQ_POIDS$CAFE_THE_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.cafe.y.compris.decafeine", colnames(Frame))
ncol2 <- grep("de.the",colnames(Frame))
term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$CAFE.portion))
term2 <- replace_na_with_zero(FREQ_intake(Frame, ncol2)*replace_na_with_zero(Poids_modifie$THE.portion))
FFQ_POIDS_Int$de.cafe.y.compris.decafeine <-term1
FFQ_POIDS_Int$de.the <- term2
FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2

# CSGA pre-calculated coffee and tea quantity (ml → kg).
CSGA_POIDS$CAFE_THE_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.the + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cafe)/1000


##CEREALES_PD (breakfast cereals) ----
FFQ_POIDS$CEREALES_PD_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli",colnames(Frame))
FFQ_POIDS$CEREALES_PD_FFQ  <- FREQ_intake(Frame,ncol1)*Poids_modifie$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli
FFQ_POIDS_Int$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli <- FFQ_POIDS$CEREALES_PD_FFQ

# CSGA pre-calculated breakfast cereal quantity (g → kg).
CSGA_POIDS$CEREALES_PD_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cereales.petit.dejeuner/1000


##CHARCUTERIE_HORS_JB (cold cuts excluding white ham) ----
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- rep(0,nrow(Frame))
categories <- c("du.saucisson.sec.ou.salamiy.compris.a.l.aperitif", "du.cervelas.ou.de.la.mortadelle",
                "du.pate.ou.des.rillettes", "du.jambon.crubacon","du.jambon.blanc" , "des.saucisses.fraiches.ou.fumees.y.compris.merguez")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "No column recognized"
  }
}

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- terms

# CSGA pre-calculated cold cuts quantity (g → kg).
CSGA_POIDS$CHARCUTERIE_HORS_JB_FFQ  <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.saucisson.sec +
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cervelasmortadelle +
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.paterillettes +
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.saucisses.fraichesfumees +
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.jambon)/1000


##DESSERTS_LACTES (dairy desserts) ----
FFQ_POIDS$DESSERTS_LACTES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.glace", "des.entremets.cremes.desserts.de.type.Danetteliegeoismoussesflans.",
                "des.entremets.au.soja.ou.yaourts.au.soja.ou.autres.yaourts.aux.laits.vegetaux")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "No column recognized"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$DESSERTS_LACTES_FFQ <- terms

# CSGA pre-calculated dairy dessert quantity (g → kg).
CSGA_POIDS$DESSERTS_LACTES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.glace +
                                     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.entremets +
                                     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.entremets.au.sojayaourts.soja )/1000


##EAU (water) ----
# Only bottled water is counted here — tap water is commented out.
FFQ_POIDS$EAU_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("d.eau.en.bouteille.ou.bonbonne.verre", colnames(Frame))
#ncol2 <- grep("d.eau.du.robinet.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$EAU_BOUTEILLE.portion)
#term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$EAU_ROB.portion)
FFQ_POIDS$EAU_FFQ <- term1 # + term2
FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1
#FFQ_POIDS_Int$d.eau.du.robinet.verre <- term2

#CSGA_POIDS$EAU_FFQ <- ( CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.eau.en.bouteillebonbonne)/1000

##FEC_NON_RAF (unrefined starchy foods) ----
FFQ_POIDS$FEC_NON_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("du.painspeciaux.hors.petit.dejeuner.","du.pain.complet.et.autres.pains.speciaux.au.petit.dejeuner",
                "du.mais.ou.de.la.polenta","des.pommes.de.terre.a.l.eau.ou.au.four","des.pommes.de.terre.rissolees.ou.sautees",
                "de.la.puree.de.pomme.de.terre","d.autres.feculents.quinoamaniocbanane.plantainigname.",
                "des.pates.completes.ou.semi.completes","du.riz.complet.ou.semi.complet","du.mais1")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "No column recognized"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}

# Check for entirely NA columns among matched categories.
cols_entierement_na <- names(which(sapply(unlist(lapply(categories, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))


FFQ_POIDS$FEC_NON_RAF_FFQ <- terms
# CSGA pre-calculated unrefined starchy food quantity (g → kg).
CSGA_POIDS$FEC_NON_RAF_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pain.completpains.speciaux +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mais +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommes.de.terre.a.l.eau +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommes.de.terre.rissolees +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.puree.de.pommes.de.terre +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.feculents)/1000


##FEC_RAF (refined starchy foods) ----
FFQ_POIDS$FEC_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.semouledu.ble.tabouleen.accompagnement.autre.que.dans.un.couscousEbly",
                "du.riz.blanc", "des.pates.macaronisspaghettiscoquillettes", "du.pain.blancde.mie.hors.petit.dejeuner.",
                "des.biscottesdes.craquottesdes.pains.grilles.au.petit.dejeuner","des.biscottesdes.craquottesdes.pains.grilles.type.suedois.hors.petit.dejeuner",
                "du.pain.blanc.au.petit.dejeuner")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "No column recognized"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FEC_RAF_FFQ <- terms

# CSGA pre-calculated refined starchy food quantity (g → kg).
CSGA_POIDS$FEC_RAF_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.semouleble +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.riz +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pates +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pain +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.biscottescracottepains.grilles )/1000

##FROMAGES (cheeses) ----
FFQ_POIDS$FROMAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.en.morceaux","du.Roquefortdu.Bleu.quelle.qu.en.soit.l.origine",
                "du.fromage.de.chevre","autres.types.de.fromages.camembertbrie.","de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.rape.sur.les.plats.patesriz.",
                "du.fromage.a.pate.molle.camembertcoulommiersbrie.","du.fromage.a.tartiner.cancoillotteSaint.MoretVache.qui.rit.","de.la.mozzarella")
terms <- numeric(nrow(Frame))
non_reconnues <- c()

for (cat in categories) {
  ncol <- grep(cat, colnames(Frame))
  
  if (length(ncol) == 0) {
    # Add to the list of unrecognized categories.
    non_reconnues <- c(non_reconnues, cat)
  } else {
    # Apply the formula if the column is found.
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[cat]])
    terms <- terms + term
    FFQ_POIDS_Int[[cat]] <- term
  }
}

FFQ_POIDS$FROMAGES_FFQ <- terms

# CSGA pre-calculated cheese quantity (g → kg).
CSGA_POIDS$FROMAGES_FFQ <-  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.emmentalgruyerecomtebeaufort.en.morceaux +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.emmentalgruyerecomtebeaufort.rape +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.roquefortbleu +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fromage.de.chevre +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.types.de.fromages)/1000


##FRUITS ----
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
cols_entierement_na <- names(which(sapply(unlist(lapply(categories, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))
cols_entierement_na <- names(which(sapply(unlist(lapply(Poids_modifie, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))

# CSGA pre-calculated fruit quantity (g → kg).
CSGA_POIDS$FRUITS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.abricotpecheprunecerise +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fraiseframboise + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.raisin +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.melonpasteque + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.banane +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.kiwi + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.agrumes +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommepoire + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.exotiques )/1000

##FRUITS_JUS (fruit juices) ----
FFQ_POIDS$FRUITS_JUS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre", colnames(Frame))
FFQ_POIDS$FRUITS_JUS_FFQ<-  replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$JUS.portion))
FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ

# CSGA pre-calculated fruit juice quantity (ml → kg).
CSGA_POIDS$FRUITS_JUS_FFQ <- CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.jus.d.orangepamplemousseananaspommeraisin / 1000

##FRUITS_SECS (dried fruits) ----
FFQ_POIDS$FRUITS_SECS_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.seches.abricotsdattesfiguespruneaux.",colnames(Frame))
FFQ_POIDS$FRUITS_SECS_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.seches.abricotsdattesfiguespruneaux.)
FFQ_POIDS_Int$des.fruits.seches.abricotsdattesfiguespruneaux. <- FFQ_POIDS$FRUITS_SECS_FFQ

# CSGA pre-calculated dried fruit quantity (g → kg).
CSGA_POIDS$FRUITS_SECS_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.seches / 1000

Frame$de.lait.demi.ecreme
##LAIT (milk) ----
# Three milk types: whole, semi-skimmed, and skimmed.
# Cacao/chocolate milk (term4) is commented out.
FFQ_POIDS$LAIT_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.lait.entier.", colnames(Frame))
ncol2 <- grep("de.lait.demi.ecreme",colnames(Frame))
ncol3 <- grep("de.lait.ecreme",colnames(Frame))
#ncol4 <- grep("du.cacao.ou.chocolat.en.poudre",colnames(Frame))
term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) *  Poids_modifie$LAIT_ENT.portion )
term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$LAIT_DEMI.portion  )
term3 <-replace_na_with_zero(FREQ_intake(Frame,ncol3)*Poids_modifie$LAIT_ECR.portion )
#term4 <-replace_na_with_zero(FREQ_intake(Frame,ncol4)*Poids_modifie$CACAO.portion)
FFQ_POIDS$LAIT_FFQ <- term1 + term2 + term3 # + term4
FFQ_POIDS_Int$de.lait.entier. <- term1
FFQ_POIDS_Int$de.lait.demi.ecreme <- term2
FFQ_POIDS_Int$de.lait.ecreme <- term3
#FFQ_POIDS_Int$du.cacao.ou.chocolat.en.poudre <- term4

# CSGA pre-calculated milk quantity (ml → kg).
CSGA_POIDS$LAIT_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.demi.ecreme +
                          CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.ecreme +
                          CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.entier ) / 1000

##LAITAGES (dairy products — yogurts, fromage blanc) ----
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

# CSGA pre-calculated dairy product quantity (g → kg).
CSGA_POIDS$LAITAGES_FFQ <- (CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.fromage.blancpetits.suissesyaourts.20%30%40%MG`+ CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.fromage.blancyaourt.0%MG`)/1000

##LEG_SECS (legumes) ----
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
# CSGA pre-calculated legume quantity (g → kg).
CSGA_POIDS$LEG_SECS_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.legumes.secs/1000


##LEGUMES (vegetables) ----
FFQ_POIDS$LEGUMES_FFQ <- rep(0,nrow(Frame))
categories <- c("des.haricots.verts",
                "des.endivesdes.epinardsdu.cresson",
                "des.poireaux",
                "du.chou.vertchou.fleurBruxellesbrocolis",
                "des.carottes.cuites",
                "des.courgettesdes.auberginesdes.poivronsdes.tomates.cuites.ratatouille.",
                "des.petits.pois",
                "des.artichautsdu.fenouildes.aspergesdu.celeri",
                "des.champignons",
                "du.potirondes.patates.douces",
                "de.la.soupe.de.legumes",
                "de.la.salade.vertede.la.machede.la.roquettedes.epinardsdu.cresson",
                "des.carottes.rapees",
                "de.l.avocat.au.moins.un.demi.avocat",
                "d.autres.crudites")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LEGUMES_FFQ <- terms
Poids_modifie$d.autres.crudites.tomatesbetteraveschouconcombreradis.

# CSGA pre-calculated vegetable quantity (g → kg).
CSGA_POIDS$LEGUMES_FFQ  <-
  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.endivesepinardcresson +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.haricots.verts+
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poireaux +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chou +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.carottes.cuites +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.courgetteauberginespoivronstomates.cuites +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.petits.pois +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.artichautfenouilaspergeceleri +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.champignon +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.potironpatate.douce +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.salade.vertemacheroquetteepinardcresson +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.carottes.rapees +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.avocat +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.crudites +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.soupe.de.legumes )/1000

##MGA (animal fats — butter and crème fraîche) ----
FFQ_POIDS$MGA_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates.",colnames(Frame))
ncol2 <- grep("de.la.creme.fraiche",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. )
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.la.creme.fraiche)
FFQ_POIDS$MGA_FFQ <- term1 + term2
FFQ_POIDS_Int$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. <-term1
FFQ_POIDS_Int$de.la.creme.fraiche <-term2

# CSGA pre-calculated animal fat quantity (g → kg).
CSGA_POIDS$MGA_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.creme.fraiche + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.beurre)/1000

##MGV (vegetable fats) ----
# Set to zero — not available in CSGA FFQ.
FFQ_POIDS$MGV_FFQ <- rep(0,nrow(Frame))
CSGA_POIDS$MGV_FFQ <- rep(0,nrow(CSGA_POIDS))


##NOIX (nuts) ----
FFQ_POIDS$NOIX_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.a.coque.noixnoisettesamandes.",colnames(Frame))
FFQ_POIDS$NOIX_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)

# CSGA pre-calculated nut quantity (g → kg).
CSGA_POIDS$NOIX_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.a.coque)/1000
FFQ_POIDS_Int$des.fruits.a.coque.noixnoisettesamandes. <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)

##OEUFS (eggs) ----
# Two preparation types: poached/boiled (weight = 83g/unit) and fried/omelette.
FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.oeufspochesdurs.ou.a.la.coque.2",colnames(Frame))
ncol2 <- grep("des.oeufssur.le.plat.en.omelette1",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*0.08333 ) # Poids_modifie$des.oeufspochesdurs.ou.a.la.coque.1)
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$des.oeufssur.le.plat.en.omelette1)
FFQ_POIDS$OEUFS_FFQ <- term1 + term2

FFQ_POIDS_Int$des.oeufspochesdurs.ou.a.la.coque.2 <-term1
FFQ_POIDS_Int$des.oeufssur.le.plat.en.omelette1 <-term2

# CSGA pre-calculated egg quantity (g → kg).
CSGA_POIDS$OEUFS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.oeufs.pochesdursa.la.coque +
                           CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.oeufs.sur.le.platomelette)/1000

##PDTS_SUCRES (sweet products) ----
FFQ_POIDS$PDTS_SUCRES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.tarte.aux.fruitsau.flan.","de.la.briochedu.cakedu.quatre.quarts",
                "des.biscuitspur.beurresecsa.la.confiturefourresau.chocolat.","Des.gateaux.patissiers.tout.faits.browniecrepepain.d.epice.","des.gateux.patissiers.au.chocolata.la.creme.",
                "de.la.pate.a.tartiner.au.chocolat.type.Nutella","des.barres.chocolatees.MarsBounty.","des.barres.de.cereales.Granny.","des.bonbons","des.viennoiseries.croissantspains.au.chocolat.",
                "du.chocolat.noirau.laitaux.noisettes.","du.mielde.la.confitureou.marmelade","du.sorbet")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PDTS_SUCRES_FFQ <- terms

# CSGA pre-calculated sweet product quantity (g → kg).
CSGA_POIDS$PDTS_SUCRES_FFQ <-
  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.tarte.sucree +
     CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.briochecakequatre-quart`+
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.biscuits +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.gateaux.patissiers +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pate.a.tartiner.au.chocolat +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.barres.chocolatees +
     CSGA_POIDS$ Quantite.g.journaliere.de.consommation.de.barres.de.cereales +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.bonbons +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viennoiseries +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chocolat +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mielconfituremarmelade +
     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sorbet)/1000

##PLATS_PREP_CARNES (meat-based prepared dishes) ----
FFQ_POIDS$PLATS_PREP_CARNES_FFQ  <- rep(0,nrow(Frame))
categories <- c("des.raviolislasagnespates.fourrees","du.cassoulet","du.couscous","des.salades.composees.toutes.faites.avec.feculents.et.viande",
                "de.la.paella","de.la.choucroute.avec.de.la.charcuterie", "du.chili.con.carne", "des.plats.cuisines.alleges", "des.plats.cuisines.a.base.de.poisson")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]])
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PLATS_PREP_CARNES_FFQ <- terms

# CSGA pre-calculated meat-based prepared dish quantity (g → kg).
CSGA_POIDS$PLATS_PREP_CARNES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.raviolislasagnespates.fourrees +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cassoulet +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.couscous +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.paella  +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.choucroute +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chili.con.carne +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.plats.cuisines.alleges+
                                       + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.plats.cuisines.a.base.de.poisson)/1000

##POISSONS (fish and seafood) ----
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

# CSGA pre-calculated fish quantity (g → kg).
CSGA_POIDS$POISSONS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.frais.ou.congele +
                              CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.a.l.huile + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.pane +
                              CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.saleen.saumure +
                              CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.coquillages)/1000

##PORC (pork, excluding charcuterie) ----
FFQ_POIDS$PORC_FFQ<- rep(0,nrow(Frame))
ncol1 <- grep("de.la.viande.de.porc.sauf.charcuterie",colnames(Frame))
FFQ_POIDS$PORC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.viande.de.porc.sauf.charcuterie)
FFQ_POIDS_Int$de.la.viande.de.porc.sauf.charcuterie <- FFQ_POIDS$PORC_FFQ

# CSGA pre-calculated pork quantity (g → kg).
CSGA_POIDS$PORC_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.porc.sf.charcuterie/1000

##POULET (poultry — chicken, turkey, rabbit) ----
FFQ_POIDS$POULET_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.la.volaille.pouletdinde.du.lapin",colnames(Frame))
FFQ_POIDS$POULET_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.volaille.pouletdinde.du.lapin)
FFQ_POIDS_Int$de.la.volaille.pouletdinde.du.lapin <- FFQ_POIDS$POULET_FFQ

# CSGA pre-calculated poultry quantity (g → kg).
CSGA_POIDS$POULET_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.volaillelapin/1000

##QUICHES_PIZZAS_TARTES_SALEES (savoury pies, pizzas, quiches) ----
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

# CSGA pre-calculated savoury pie/pizza quantity (g → kg).
CSGA_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pizza + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.tartes.salees)/1000

##SAUCES ----
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
# CSGA pre-calculated sauce quantity (g → kg).
CSGA_POIDS$SAUCES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mayonnaise +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sauce.vinaigrette +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sauce.soja +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.ketchup)/1000

##SNACKS_AUTRES (other snacks — fast food, crisps, nuts, sandwiches) ----
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
# CSGA pre-calculated snack quantity (g → kg).
CSGA_POIDS$SNACKS_AUTRES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cacahuetes +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.gateaux.aperitifs.sales +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chips +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.friands +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sandwichs +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.hamburgers)/1000

##SODAS_LIGHT (diet sodas + plant-based milk drinks) ----
FFQ_POIDS$SODAS_LIGHT_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.lait.vegetal.sojarizavoine.", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$LAIT_VEGE.portion)
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$LIGHT.portion)
FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2

# CSGA pre-calculated diet soda / plant milk quantity (ml → kg).
CSGA_POIDS$SODAS_LIGHT_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.boisson.au.soja + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.colalimonadesoda.light)/1000

##SODAS_SUCRES (sugary sodas + syrups) ----
FFQ_POIDS$SODAS_SUCRES_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.sirop.verre", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$SIROP.portion)
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$NON_LIGHT.portion)
FFQ_POIDS_Int$de.sirop.verre <- term1
FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2

FFQ_POIDS$SODAS_SUCRES_FFQ <- term1 + term2
# CSGA pre-calculated sugary soda quantity (ml → kg).
CSGA_POIDS$SODAS_SUCRES_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.siropeau.aromatisee + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.colalimonadesoda.non.light)/1000

##VIANDE_ROUGE (red meat — beef, veal, lamb, offal) ----
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

# CSGA pre-calculated red meat quantity (g → kg).
CSGA_POIDS$VIANDE_ROUGE_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.boeuf +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.steaks.haches + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.veau +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.d.agneaumouton + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.languetripesboudinandouilletteris.de.veaurognons +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.foie)/1000


# 13. Remove incomparable categories from CSGA_POIDS ----
# Drop spices, vegetable fats, and margarine — not present in other cohorts.
#REMOVE spices / CAFE_THE / MGV
CSGA_POIDS$EPICES_CONDIMENTS_FFQ <- NULL
#CSGA_POIDS$CAFE_THE_FFQ <- NULL
CSGA_POIDS$MGV_FFQ <- NULL
#CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cafe<- NULL
#CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.the<- NULL
CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.margarine <- NULL
#FFQ_POIDS$CAFE_THE_FFQ <- NULL


# 14. Compute total weight indicators ----
# Total food weight per participant/day (sum of all food group columns).
#Total sum — TI PROCESSING
FFQ_POIDS$POIDS_TOTAL_FFQ <- rowSums(FFQ_POIDS[2:32])

# Total weight excluding beverages (subtract all liquid categories).
FFQ_POIDS$POIDS_HORS_BOISSON_FFQ <- replace_na_with_zero(FFQ_POIDS$POIDS_TOTAL_FFQ -
                                                           FFQ_POIDS$ALCOOL_FFQ -
                                                           FFQ_POIDS$FRUITS_JUS_FFQ -
                                                           FFQ_POIDS$LAIT_FFQ -
                                                           FFQ_POIDS$EAU_FFQ -
                                                           FFQ_POIDS$SODAS_LIGHT_FFQ -
                                                           FFQ_POIDS$SODAS_SUCRES_FFQ -
                                                           FFQ_POIDS$CAFE_THE_FFQ)


# Add the _Poids suffix to all _FFQ columns to harmonize naming with other datasets.
#Add _Poids suffix
FFQ_POIDS <- FFQ_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_FFQ$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )


# 15. Compute energy intake (kcal) per food group ----
# Reshape the item-level weight table to long format, join with CALNUT
# to get kcal per 100g, then compute kcal per item per participant.

# Reshape FFQ_POIDS_Int to long format (one row per participant × food item).
df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids")

# Remove rows with missing weight values.
df_long <- df_long %>%
  filter(!is.na(Poids))

#df_long <- inner_join(df_long, CALNUT, by= "FFQ_CSGA", relationship = "many-to-many")
# Join with the CALNUT nutritional reference table on the food item name.
df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")

# CALCULATE KILOCALORIES PER FOOD ITEM
# nrj_kcal is in kcal/100g, Poids is in kg → multiply by 10 to get kcal.
df_long$nrj_kcal_alim <- df_long$nrj_kcal*df_long$Poids*10

# Aggregate kcal by participant and TI food group.
FFQ_KCAL <- aggregate(nrj_kcal_alim ~  Identifiant + groupe_TI_TdC   , df_long, FUN = sum)
# Pivot to wide format: one column per food group.
FFQ_KCAL<- pivot_wider(
  FFQ_KCAL,
  id_cols = Identifiant,
  names_from = groupe_TI_TdC,
  values_from = nrj_kcal_alim
)


# Compute total daily energy intake (sum of all food group columns).
# Calculate the sum of columns for each row
FFQ_KCAL$KCAL_TOTAL <- rowSums(FFQ_KCAL[, 2:ncol(FFQ_KCAL)], na.rm = TRUE)


# Compute total energy intake excluding beverages.
# Calculate the sum of columns excluding beverages
FFQ_KCAL$KCAL_HORS_BOISSON <- with(FFQ_KCAL, KCAL_TOTAL  -
                                     ALCOOL -
                                     FRUITS_JUS -
                                     CAFE_THE -
                                     LAIT -
                                     EAU -
                                     SODAS_LIGHT -
                                     SODAS_SUCRES)


# Add the _FFQ_Kcal suffix to all food group columns.
# Add _FFQ_KCAL suffix to each category
FFQ_KCAL <- FFQ_KCAL %>%
  rename_with(
    ~ paste0(.x, "_FFQ_Kcal"),
    .cols = -Identifiant
  )


FFQ_KCAL$UC_TI <- NULL


# 16. Build the final indicator table ----
# Join FFQ_POIDS (weights) and FFQ_KCAL (energy) on participant ID,
# then tag all rows as coming from the FFQ instrument.
#Build final table
FFQ_id <- FFQ_POIDS
FFQ_id<- inner_join(FFQ_id, FFQ_KCAL, by="Identifiant")
FFQ_id$Mesure <- "FFQ"


# 17. Export to Excel ----
# DOWNLOAD

# Create a new workbook object
wb <- createWorkbook()

# Add each data frame to a separate sheet.
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", FFQ_id)

addWorksheet(wb, "Frequences_corrigées")
writeData(wb, sheet = "Frequences_corrigées", Frame)

addWorksheet(wb, "Poids_corrigée_TI")
writeData(wb, sheet = "Poids_corrigée_TI", FFQ_POIDS)

addWorksheet(wb, "Poids_corrigée_CSGA")
writeData(wb, sheet = "Poids_corrigée_CSGA", FFQ_POIDS)

# UPDATE this path before running.
saveWorkbook(wb,("FFQ_CSGA.xlsx"))
