
# SCRIPT: Data Harmonization — TI, Nudges (commented out), and CSGA cohorts
# PURPOSE: Load pre-processed FFQ and booklet files per measurement wave,
#          harmonize participant IDs, filter implausible energy intakes,
#          and export one analysis-ready Excel file per cohort.


# PART 1 — TI COHORT----

# 1. Package loading ----
#SCRIPT CHEQUES
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")

# 2. Import raw data — four TI measurement waves ----
# Each wave: one booklet file (Carnet) + one FFQ file.
# Duplicate participant rows in FFQ files are removed immediately after import.
# UPDATE all paths to match your local file system.

#Nov 22
Carnet_nov_22<- read.xlsx((paste("Carnets_Tableaux_nov_22.xlsx", sep="")))
FFQ_nov_22<- read.xlsx((paste("FFQ_Tableaux_nov_22.xlsx", sep="")))
FFQ_nov_22 <- FFQ_nov_22[!duplicated(FFQ_nov_22$Identifiant), ]

#Mars23
Carnet_mars_23<- read.xlsx((paste("Carnets_Tableaux_mars_23.xlsx", sep="")))
FFQ_mars_23<- read.xlsx((paste("FFQ_Tableaux_mars_23.xlsx", sep="")))
FFQ_mars_23 <- FFQ_mars_23[!duplicated(FFQ_mars_23$Identifiant), ]

#Nov23
Carnet_nov_23<- read.xlsx((paste("Carnets_Tableaux_nov_23.xlsx", sep="")))
FFQ_nov_23<- read.xlsx((paste("FFQ_Tableaux_nov_23.xlsx", sep="")))
FFQ_nov_23 <- FFQ_nov_23[!duplicated(FFQ_nov_23$Identifiant), ]

#Mars24
Carnet_mars_24<- read.xlsx((paste("Carnets_Tableaux_mars_24.xlsx", sep="")))
FFQ_mars_24<- read.xlsx((paste("FFQ_Tableaux_mars_24.xlsx", sep="")))
FFQ_mars_24 <- FFQ_mars_24[!duplicated(FFQ_mars_24$Identifiant), ]



# 3. FFQ merging and harmonization ----

# Stack the two November FFQ waves (Nov 22 + Nov 23).
#Fusion des tableaux de novembre
FFQ_NOV <- rbind(FFQ_nov_22, FFQ_nov_23)

# The March 2024 FFQ is missing the education-level variable.
# It is imputed from the pooled November FFQ via a left join on participant ID.
##Imputation of education level in the March table
temp  <- FFQ_NOV[, c("Identifiant", "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.")]
FFQ_mars_24  <- left_join(FFQ_mars_24, temp, by="Identifiant")

# Rename the newly joined column (remove the trailing dot added by the join).
names(FFQ_mars_24)[ncol(FFQ_mars_24)] <- "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu"
# Remove the duplicate ".x" version of the education column already in mars_24.
FFQ_mars_24 <- FFQ_mars_24[, -c(which(names(FFQ_mars_24) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu..x"))] #duplicate empty column, also .n
# Remove the education column from mars_23 (it will be supplied at stacking if needed).
FFQ_mars_23 <- FFQ_mars_23[, -c(which(names(FFQ_mars_23) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."))] #duplicate empty column, also .n

# Stack the two March FFQ waves (Mar 23 + Mar 24).
##Merging March tables
FFQ_MARS <- rbind(FFQ_mars_23, FFQ_mars_24)


# Stack November booklets (Nov 22 + Nov 23).
Carnet_NOV <- bind_rows(Carnet_nov_22, Carnet_nov_23)

# Stack March booklets (Mar 23 + Mar 24).
Carnet_MARS <- bind_rows(Carnet_mars_23, Carnet_mars_24)


# 4. Cross-wave ID harmonization — booklets ----
# Keep only participants who have a booklet record in BOTH November AND March.

# Remove March booklet rows for participants absent from November booklets.
##Remove from MARCH all rows not present in NOV
###Remove March booklet IDs not found in November booklets
Carnet_MARS <- Carnet_MARS %>%
  semi_join(Carnet_NOV, by = "Identifiant")

#print(unique(Carnet_MARS$Identifiant))

# Remove November booklet rows for participants absent from March booklets.
###Remove November booklet IDs not found in March booklets
Carnet_NOV <- Carnet_NOV %>%
  semi_join(Carnet_MARS, by = "Identifiant")


# 5. FFQ outlier exclusion ----
# Participants with total energy intake outside [500, 4500] kcal/day are excluded.
# Removed IDs are stored in removed_ids for traceability.

FFQ_NOV$KCAL_TOTAL_FFQ_Kcal  # Verify the energy column exists before filtering
# Filters to exclude implausible FFQ values
initial_ids <- FFQ_NOV$Identifiant
FFQ_NOV<- FFQ_NOV %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ_NOV <- FFQ_NOV %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)

initial_ids <- FFQ_MARS$Identifiant
FFQ_MARS<- FFQ_MARS %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ_MARS <- FFQ_MARS %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)


# 6. Cross-wave ID harmonization — FFQ ----
# Enforce the same participant set across November and March FFQ datasets.

# Remove March FFQ rows for participants absent from November FFQ.
#Remove March FFQ IDs not found in November FFQ
initial_ids <- FFQ_MARS$Identifiant

FFQ_MARS <- FFQ_MARS %>%
  semi_join(FFQ_NOV, by = "Identifiant")
removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)

# Remove November FFQ rows for participants absent from March FFQ.
###Remove November FFQ IDs not found in March FFQ
FFQ_NOV <- FFQ_NOV %>%
  semi_join(FFQ_MARS, by = "Identifiant")

# Print the final list of participant IDs retained in both booklet waves.
print(unique(Carnet_NOV$Identifiant))

# Drop the temporary outlier-boundary columns (no longer needed after filtering).
FFQ_NOV$borne_inf  <- NULL
FFQ_MARS$borne_inf <- NULL
FFQ_NOV$borne_sup  <- NULL
FFQ_MARS$borne_sup <- NULL


# 7. Stack booklets into the final combined booklet dataset ----
sgsdata_Carnets <- bind_rows(Carnet_NOV, Carnet_MARS)

# 8. Identify columns shared by both FFQ datasets.
# Find shared columns
cols_communes <- intersect(names(FFQ_NOV), names(FFQ_MARS))

# Among shared columns, find those whose R class differs between the two waves.
# Identify columns with differing class
diff_cols <- cols_communes[sapply(cols_communes, function(col) {
  !identical(class(FFQ_NOV[[col]]), class(FFQ_MARS[[col]]))
})]

# Convert only these columns to character (note: non-numeric values will become NA downstream).
# Force only these columns to character (caution: non-numeric values → NA)
FFQ_NOV <- FFQ_NOV %>%
  mutate(across(all_of(diff_cols), ~ as.character(.)))

FFQ_MARS <- FFQ_MARS %>%
  mutate(across(all_of(diff_cols), ~ as.character(.)))

# Stack November and March FFQ — type conflict resolved.
# Now we can bind without errors
sgsdata_FFQ <- bind_rows(FFQ_NOV, FFQ_MARS)

# 9. Export TI cohort to Excel ----
# UPDATE the save path before running.
#Exporting final tables
# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("sgsdata_IT.xlsx")))




# PART 2 — NUDGES COHORT (commented out — same logic as TI)----

# Mirrors Part 1 but uses Nudges data (Nov 2021 / March 2022).
# Key differences vs. TI:
#   - Column renaming (.x → _POIDS, .y → _KCAL) is active in this block.
#   - Outlier filter uses SOMME_FFQ_KCAL instead of KCAL_TOTAL_FFQ_Kcal.
#   - Output: sgsdata_nudges.xlsx.
# To run: uncomment the entire block and execute independently.

####NUDGES HARMONIZATION
#
##Package loading
#rm(list = ls())
#

##Nov 21
#Carnet_NOV <- read.xlsx((paste("Carnets_Tableaux_nov_21.xlsx", sep="")))
#FFQ_NOV  <- read.xlsx((paste("FFQ_Tableaux_nov_21.xlsx", sep="")))
#FFQ_NOV <- FFQ_NOV[!duplicated(FFQ_NOV$Identifiant), ]
#
##mars22
#Carnet_MARS <- read.xlsx((paste("Carnets_Tableaux_mars_22.xlsx", sep="")))
#FFQ_MARS <- read.xlsx((paste("FFQ_Tableaux_mars_22.xlsx", sep="")))
#FFQ_MARS <- FFQ_MARS[!duplicated(FFQ_MARS$Identifiant), ]
#
#
#
## Replace .x suffix with _POIDS first
#names(Carnet_NOV ) <- gsub("\\.x$", "_POIDS", names(Carnet_NOV))
## Then replace .y suffix with _KCAL
#names(Carnet_NOV ) <- gsub("\\.y$", "_KCAL", names(Carnet_NOV))
#
## Replace .x suffix with _POIDS first
#names(Carnet_MARS) <- gsub("\\.x$", "_POIDS", names(Carnet_MARS))
## Then replace .y suffix with _KCAL
#names(Carnet_MARS) <- gsub("\\.y$", "_KCAL", names(Carnet_MARS))
#
#
#
#
#
###Remove from MARCH all rows not present in NOV
####Remove March booklet IDs not found in November booklets
#initial_ids <- Carnet_MARS$Identifiant
#Carnet_MARS <- Carnet_MARS %>%
#  semi_join(Carnet_NOV, by = "Identifiant")
#removed_ids <- setdiff(initial_ids, Carnet_MARS$Identifiant)
#
##print(unique(Carnet_MARS$Identifiant))
#
####Remove November booklet IDs not found in March booklets
#initial_ids <- Carnet_NOV$Identifiant
#Carnet_NOV <- Carnet_NOV %>%
#  semi_join(Carnet_MARS, by = "Identifiant")
#removed_ids <- setdiff(initial_ids, Carnet_NOV$Identifiant)
#
#
## Filters to exclude implausible FFQ values
## Note: energy variable is SOMME_FFQ_KCAL for Nudges (vs KCAL_TOTAL_FFQ_Kcal for TI).
#initial_ids <- FFQ_NOV$Identifiant
#FFQ_NOV<- FFQ_NOV %>%
#  mutate(borne_inf = 500,
#         borne_sup =  4500)
#FFQ_NOV <- FFQ_NOV %>%
#  filter(SOMME_FFQ_KCAL >= borne_inf & SOMME_FFQ_KCAL <= borne_sup)
#removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)
#
#initial_ids <- FFQ_MARS$Identifiant
#FFQ_MARS<- FFQ_MARS %>%
#  mutate(borne_inf = 500,
#         borne_sup =  4500)
#FFQ_MARS <- FFQ_MARS %>%
#  filter(SOMME_FFQ_KCAL >= borne_inf & SOMME_FFQ_KCAL <= borne_sup)
#removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)
#
##Remove March FFQ IDs not found in November FFQ
#initial_ids <- FFQ_MARS$Identifiant
#
#FFQ_MARS <- FFQ_MARS %>%
#  semi_join(FFQ_NOV, by = "Identifiant")
#removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)
#
####Remove November FFQ IDs not found in March FFQ
#FFQ_NOV <- FFQ_NOV %>%
#  semi_join(FFQ_MARS, by = "Identifiant")
#
#print(unique(Carnet_NOV$Identifiant))
#
#FFQ_NOV$borne_inf  <- NULL
#FFQ_MARS$borne_inf <- NULL
#FFQ_NOV$borne_sup  <- NULL
#FFQ_MARS$borne_sup <- NULL
#
#
#sgsdata_Carnets <- bind_rows(Carnet_NOV, Carnet_MARS)
#
#
## 1. Find shared columns
#cols_communes <- intersect(names(FFQ_NOV), names(FFQ_MARS))
#
## 2. Identify columns with differing class
#diff_cols <- cols_communes[sapply(cols_communes, function(col) {
#  !identical(class(FFQ_NOV[[col]]), class(FFQ_MARS[[col]]))
#})]
#
## 3. Force only these columns to character (caution: non-numeric values → NA)
#FFQ_NOV <- FFQ_NOV %>%
#  mutate(across(all_of(diff_cols), ~ as.character(.)))
#
#FFQ_MARS <- FFQ_MARS %>%
#  mutate(across(all_of(diff_cols), ~ as.character(.)))
#
## 4. Now we can bind without errors
#sgsdata_FFQ <- bind_rows(FFQ_NOV, FFQ_MARS)
#
#
##COLUMN NAME CORRECTIONS
#names(sgsdata_Carnets) <- sub("_POIDS$",  "_Poids",  names(sgsdata_Carnets))
#names(sgsdata_Carnets) <- sub("_KCAL$",  "_Kcal",  names(sgsdata_Carnets))
#names(sgsdata_FFQ) <- sub("_FFQ$",  "_FFQ_Poids",  names(sgsdata_FFQ))
#
#
#
#
#
#sgsdata <- bind_rows(sgsdata_Carnets, sgsdata_FFQ)
#
#sgsdata <- sgsdata %>%
#  # Sort by identifier before filling downward/upward
#  arrange(Identifiant) %>%
#  group_by(Identifiant) %>%
#  # Fill NA values downward first, then upward
#  fill(groupe, .direction = "downup") %>%
#  ungroup()
#
##Exporting final tables
## Create a new workbook object
#wb <- createWorkbook()
#
#addWorksheet(wb, "sgsdata")
#writeData(wb, sheet = "sgsdata", sgsdata  )
#
#saveWorkbook(wb,(paste0(sgsdata_nudges.xlsx")))
#
#
#
#
#



# PART 3 — CSGA COHORT----

# Unlike TI and Nudges, CSGA has a single measurement wave — no Nov/March split.
# Booklet and FFQ data come from single files.
# ID harmonization uses a mutual semi_join; the same outlier filter then applies.
# Output: sgsdata.xlsx.

# 1. Package loading and environment reset for CSGA ----
###CSGA HARMONIZATION
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")

# 2. Import CSGA booklet and FFQ files ----
# UPDATE these paths to match your local file system.
Carnets <- read.xlsx((paste("Carnets_CSGA.xlsx", sep="")))
FFQ  <- read.xlsx((paste("FFQ_CSGA.xlsx", sep="")))

# 3. Cross-instrument ID harmonization ----
# Keep only participants present in BOTH booklet and FFQ (mutual semi_join).
# Sort by identifier to ensure consistent row order.
Carnets <- Carnets %>%
  semi_join(FFQ, by = "Identifiant") %>%  # keep those also in FFQ
  arrange(Identifiant)

FFQ <- FFQ %>%
  semi_join(Carnets, by = "Identifiant") %>%  # keep those also in Carnets
  arrange(Identifiant)


# 4. FFQ outlier exclusion ----
# Exclude participants with total energy intake outside [500, 4500] kcal/day.
# Removed IDs are stored in removed_ids for traceability.
initial_ids <- FFQ$Identifiant
FFQ<- FFQ %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ <- FFQ %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ_NOV$Identifiant)


# 15. Stack booklets and FFQ into the final CSGA dataset ----
sgsdata <- bind_rows(Carnets, FFQ)


# 16. Export CSGA cohort to Excel ----
# UPDATE the save path before running.
#Exporting final tables
# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("sgsdata.xlsx")))
