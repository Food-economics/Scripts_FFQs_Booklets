# LOADING THE WORKING ENVIRONMENT  --------------
## Importing packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(readxl);library(dplyr);library(broom);library(scales)
library(modelsummary);library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");
library("dplyr");library("tidyr");library("ggplot2");library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library(questionr)


### Importing data  ------------------
#raw data
CSGA_base <- read_excel(
  path  = "Données_CSGA.xlsx",
  sheet = "ffq_donnees-brutes"
)

CSGA_base <- CSGA_base %>% mutate_all(~gsub("\"","",.))
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\\(", "",.))
CSGA_base <- CSGA_base %>% mutate_all(~gsub("\\)", "",.))

#CSGA TRANSFORM
CSGA_FREQ <- read_excel(
  path  = "Données_CSGA.xlsx",
  sheet = "ffq_donnees-transform-suvimax"
)

CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\"","",.))
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\\(", "",.))
CSGA_FREQ <- CSGA_FREQ %>% mutate_all(~gsub("\\)", "",.))

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

CSGA_FREQ <- NULL

### Importing the appendix tables data ----------------

CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_FFQ.xlsx")
Encodage <- read_xlsx(paste("Freq_FFQ.xlsx"))
Taille_Portion <- read_xlsx(paste("Taille portion.xlsx"))


#HARMONIZE PORTION NAMES
#Translating portion frequencies into TI equivalent
# Vectorize the names of the columns to recode
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
                    `0` = "B", #Since we will multiply / frequencies, we can directly impute the average value
                    `1` = "Plus petit que A",
                    `2` = "A",
                    `3` = "B",
                    `4` = "C",
                    `5` = "Plus grand que C",
                    .default = NA_character_)
  ))



#Translating the drink consumption frequencies
# Vectorize the names of the columns to recode
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




#Translating the drink consumption frequencies
# Vectorize the names of the columns to recode
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



#Translating the drink consumption frequencies
# Vectorize the names of the columns to recode
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



# CREATING A UNIFORM TABLE FOR ALL CAMPAIGNS  ------
data <- CSGA_base   #left_join(CSGA_base, CSGA_FREQ, by="Code")
data$Biscottes.consommees.hors.petit.dejeuner.portion <- data$QTPAINTR_1_3
#Keeping the original names in memory to later remove the columns I don't use
orig_names <- names(data)

# Extracting the name of the encoding column
column_name <- "CSGA_FREQ"  

# Getting the old vs. new name vectors
anciens  <- Encodage[[column_name]]
nouveaux <- Encodage$Aliment

# Looping over these vectors to rename
for (i in seq_along(anciens)) {
  pattern <- anciens[i]
  new_nm  <- make.names(nouveaux[i])
  # which of the current columns contain this pattern?
  cols_to_rename <- grep(pattern, names(data), value = TRUE)
  # replace each one
  if (length(cols_to_rename) > 0) {
    names(data)[ names(data) %in% cols_to_rename ] <- new_nm
  }
}

# identify the columns whose name has changed
renamed_cols <- setdiff(names(data), orig_names)
#keep only these
Frame <- data[, renamed_cols, drop = FALSE]


# Check
report <- tibble( ancien   = anciens, nouveau  = nouveaux) %>%
  mutate(
    # which initial columns match the pattern?
    matched = map(ancien, ~ grep(.x, orig_names, value = TRUE)),
    n_match = map_int(matched, length),
    # prepare a readable text field
    matched = map_chr(matched, ~ paste(.x, collapse = ", ")),
    new_name = make.names(nouveau))


## Creating a table that groups together all the variables from the different campaigns ---------------------
colonne_chaligne <- Encodage[, 1]
Frame <- data.frame(t(colonne_chaligne))
new_column_names <- Frame[1,]
names(Frame) <- new_column_names
Frame <- Frame[-1, ]


## Imputing the values of data into the new Frame table ------------
if (nrow(Frame) != nrow(data)) { Frame <- Frame[1:nrow(data), ]}
colonnes_communes <- intersect(names(data), names(Frame))

Frame[colonnes_communes] <- data[colonnes_communes]
Frame$Biscottes.consommees.hors.petit.dejeuner.portion


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
                    `6` = "Type F", #Note for milk, the CSGA FFQ has a response option with no associated photo
                    .default = NA_character_)
  ))


#Removing individuals who live with their parents
Frame <- Frame[rowSums(is.na(Frame)) < ncol(Frame), ]
#Removing people who live with their parents or in shared housing
Frame <- subset(Frame, !lieuVie %in% c(1,2,4,5,9,10))
Frame <- subset(Frame, !Numero.d.identifiant %in% c("accakf","ajtblk","azajdj","aibbga"))


# CALCULATING CORRECTION COEFFICIENTS TO CORRECT THE FREQUENCIES -----------------------------------------




## Function to replace NA with zero ---------
replace_na_with_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
## Creating a function to extract the numeric data in the table---------------
FREQ_intake <- function(data, x) {
  result <- as.numeric(data[[x]])
  result[is.na(result)] <- 0
  return(result)}

# ASSIGNING PORTION SIZES -------------------------------
## Defining the function to replace codes with weights, with debugging messages -------------
#The remplacer_poids function takes two arguments, taille and aliment, and does the following:
#Displaying a message: shows a message indicating the food and size passed as arguments are being processed.
#Filtering and extracting the weight: uses the Taille_Portion_long data frame and applies the following steps:
#Filters the rows where the Aliment column matches the given food and the Taille column matches the given size.
#Extracts the values of the Poids column from the filtered rows.
#Handling cases with no match:
#Checks whether the length of the poids vector is zero (meaning no match was found).
#If no match is found, displays a message indicating there is no match for the given food and size, then returns NA.
#If a match is found, the function returns the extracted weight.
remplacer_poids <- function(taille, aliment) {message("Traitement de l'aliment: ", aliment, " et de la taille: ", taille)
  poids <- Taille_Portion_long %>%
    filter(Aliment == aliment, Taille == taille) %>%
    pull(Poids)
  if (length(poids) == 0) {message("Pas de correspondance trouvée pour ", aliment, " avec la taille ", taille)
    return(NA)}
  return(poids)}


##Building the portion weight table---------------
### Filtering the portion columns -------------------------------------
#Some food groups have a portion size (vegetables, raw vegetables, fish,
#steak, sweet tarts, savory tarts...)
#We start by isolating these columns and sizing a new dataframe "data_duplicated"
#that only takes these values into account
colonnes_portion <- grep("portion$", names(Frame), value = TRUE)
Frame <- Frame %>%
  rename(Identifiant = Numero.d.identifiant)

Frame_duplicated <- subset(Frame, select = c("Identifiant", colonnes_portion))

###Duplicating the portion columns as many times as a food belongs to a general category: vegetable, raw vegetable... ---------
#The for loop examines each unique category in the Catégorie column of the Taille_Portion data frame.
#Here is what this loop does in detail:
#Iterating over categories: for each unique category in Taille_Portion$Catégorie:
#Checking for NA values: if the category is NA, it moves on to the next iteration without running the rest of the code:
#Selecting columns: selects the columns of Frame whose names start with the category name
#Calculating the number of repetitions: calculates how many times the category appears in Taille_Portion$Catégorie
#Duplicating columns: for each occurrence of the category, adds the renamed columns to Frame_duplicated:
for (categorie in unique(Taille_Portion$Catégorie)) {
  if (is.na(categorie)) next 
  # Find the columns whose name starts with the category value
  column_names <- grep(paste0("^", categorie), names(Frame), value = TRUE)
  
  # Select the corresponding columns
  columns <- Frame[, column_names, drop = FALSE]
  
  nb_repeats <- sum(Taille_Portion$Catégorie == categorie, na.rm = TRUE)
  for (i in 1:nb_repeats) {
    Frame_duplicated <- cbind(Frame_duplicated, columns)
  }
}


#This step aims to harmonize the structure of the dataframe
#Each row corresponds to an identifier, and the goal is for the categories to have been
#copied as many times as there are specific foods.
# Initializing Poids with the data from Frame_duplicated
# Displaying the DataFrame with the duplicated columns
Poids <- print(Frame_duplicated)

# Separating the first two columns from the others
debut <- Poids[, 1]
fin <- Poids[, -c(1)]

# Sorting the remaining columns alphabetically
fin_trie <- fin[, order(names(fin))]

# Merging the two parts
Poids <- cbind(debut, fin_trie)

# Identifying the columns ending with a digit
colonnes_a_garder <- grep("\\d$", names(Poids), value = TRUE)

# Selecting only the columns ending with a digit
Poids <- Poids[, colonnes_a_garder]

# Identifying the column names
colnames_data <- names(Poids)

# Counting the occurrences of each column name
occurrences <- table(colnames_data)


# Identifying the duplicated columns that do not end with a digit
colonnes_a_supprimer <- names(occurrences[occurrences > 1])
colonnes_a_supprimer <- colonnes_a_supprimer[!grepl("\\d$", colonnes_a_supprimer)]
# Removing the duplicated columns that do not end with a digit
data_filtre <- Poids[, !names(Poids) %in% colonnes_a_supprimer]
# Displaying the filtered DataFrame
print("DataFrame filtré:")
Poids <- print(data_filtre)
### Renaming the copied columns with the specific food name ----------
#This step renames the copied portions in the weight table with the specific portion-size foods
new_column_names <- Taille_Portion$Aliment[match(names(Poids), Taille_Portion$Catégorie2)]
names(Poids) <- new_column_names

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
### Applying the function to replace portion sizes with the
# Transforming the Taille_Portion table into long format
#The pivot_longer function converts the specified columns (all columns except Aliment) into a long format. This means the columns will be "stacked" into two new columns named Taille (for the old column names) and Poids (for the old column values).
#The na.omit function removes rows that contain missing values (NA).
#The filter function excludes rows where the Taille column equals "Catégorie" or "Catégorie2".
Taille_Portion_long <- Taille_Portion %>%
  pivot_longer(cols = -Aliment, names_to = "Taille", values_to = "Poids") %>%
  na.omit() %>%
  filter(Taille != "Catégorie" & Taille != "Catégorie2")

### Replacing sizes with weights by applying the remplacer_poids function---------------------
Poids_modifie <- Poids
for (col in names(Poids_modifie)) {
  Poids_modifie[[col]] <- sapply(Poids_modifie[[col]], function(taille) remplacer_poids(taille, col))
}

colnames(Poids_modifie)
###This step adds the unit weight of foods whose portion does not vary---------------
#Filtering the data: subset(Taille_Portion_long, Taille == "Poids_unitaire"): selects only the rows of Taille_Portion_long where the Taille column equals "Poids_unitaire". The result is stored in filtered_df1.
#For loop to update Poids_modifie: for each row of filtered_df1, extracts the values of the Aliment and Poids columns.
#Creates an entry in the Poids_modifie list where the food name is the key and the weight value is the associated value.

filtered_df1 <- subset(Taille_Portion_long, Taille == "Poids_unitaire")
for (i in 1:nrow(filtered_df1)) {
  aliment <- filtered_df1$Aliment[i]
  valeur <- filtered_df1$Poids[i]
  Poids_modifie[[aliment]] <- valeur
}

# Finalizing the table
Poids_modifie <- cbind(Frame$Identifiant, Poids_modifie)
names(Poids_modifie)[1] <- "Identifiant"
Poids_modifie[,-1] <- lapply(Poids_modifie[,-1], as.numeric)

#REMOVING THE DRINK COLUMNS
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

Poids_modifie[is.na(Poids_modifie)] <- 0

Poids_modifie <- Poids_modifie[Poids_modifie$Identifiant != 0, ]
Frame <- Frame[!is.na(Frame$Identifiant), ]

# CALCULATING THE WEIGHT OF FOODS CONSUMED    -------------------------------
#In this step, the weight of foods is calculated by multiplying the frequencies by the portion sizes for all foods
FFQ_POIDS <-data.frame(Frame$Identifiant)
names(FFQ_POIDS)[1] = "Identifiant"

FFQ_POIDS_Int <-data.frame(Frame$Identifiant)
names(FFQ_POIDS_Int )[1] = "Identifiant"

##ALCOHOL_FFQ ---------------
FFQ_POIDS$ALCOOL_FFQ <- rep(0, nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")

# Initializing a vector to store the recognized columns
recognized_columns <- list()

# Looping over the categories to check which columns are recognized
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
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


CSGA_POIDS$ALCOOL_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cidrebiere + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.vin.blancroserouge +
                            CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.alcools.forts + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.aperitifs)/1000

#  ##COFFEE_TEA_FFQ--------------
FFQ_POIDS$CAFE_THE_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.cafe.y.compris.decafeine", colnames(Frame))
ncol2 <- grep("de.the",colnames(Frame))
term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$CAFE.portion))  
term2 <- replace_na_with_zero(FREQ_intake(Frame, ncol2)*replace_na_with_zero(Poids_modifie$THE.portion))
FFQ_POIDS_Int$de.cafe.y.compris.decafeine <-term1 
FFQ_POIDS_Int$de.the <- term2 
FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2

CSGA_POIDS$CAFE_THE_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.the + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cafe)/1000


##CEREALS_PD_FFQ---------------
FFQ_POIDS$CEREALES_PD_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli",colnames(Frame))
FFQ_POIDS$CEREALES_PD_FFQ  <- FREQ_intake(Frame,ncol1)*Poids_modifie$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli
FFQ_POIDS_Int$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli <- FFQ_POIDS$CEREALES_PD_FFQ

CSGA_POIDS$CEREALES_PD_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cereales.petit.dejeuner/1000


#DELI_MEATS_EXCL_HAM_FFQ
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- rep(0,nrow(Frame))
categories <- c("du.saucisson.sec.ou.salamiy.compris.a.l.aperitif", "du.cervelas.ou.de.la.mortadelle",
                "du.pate.ou.des.rillettes", "du.jambon.crubacon","du.jambon.blanc" , "des.saucisses.fraiches.ou.fumees.y.compris.merguez")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
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

CSGA_POIDS$CHARCUTERIE_HORS_JB_FFQ  <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.saucisson.sec +
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cervelasmortadelle + 
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.paterillettes + 
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.saucisses.fraichesfumees +
                                          CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.jambon)/1000



##DAIRY_DESSERTS_FFQ----------------
FFQ_POIDS$DESSERTS_LACTES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.glace", "des.entremets.cremes.desserts.de.type.Danetteliegeoismoussesflans.",
                "des.entremets.au.soja.ou.yaourts.au.soja.ou.autres.yaourts.aux.laits.vegetaux")
for (category in categories) {
  col_indices <- grep(category, colnames(Frame))
  if (length(col_indices) > 0) {
    recognized_columns[[category]] <- colnames(Frame)[col_indices]
  } else {
    recognized_columns[[category]] <- "Aucune colonne reconnue"
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

CSGA_POIDS$DESSERTS_LACTES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.glace +
                                     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.entremets +
                                     CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.entremets.au.sojayaourts.soja )/1000



##WATER_FFQ--------------
FFQ_POIDS$EAU_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("d.eau.en.bouteille.ou.bonbonne.verre", colnames(Frame))
#ncol2 <- grep("d.eau.du.robinet.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$EAU_BOUTEILLE.portion) 
#term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$EAU_ROB.portion)
FFQ_POIDS$EAU_FFQ <- term1 # + term2
FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1 
#FFQ_POIDS_Int$d.eau.du.robinet.verre <- term2 

#CSGA_POIDS$EAU_FFQ <- ( CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.eau.en.bouteillebonbonne)/1000

##STARCHES_NON_REFINED_FFQ----------------------
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
    recognized_columns[[category]] <- "Aucune colonne reconnue"
  }
}
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}

cols_entierement_na <- names(which(sapply(unlist(lapply(categories, function(cat) grep(cat, names(Frame), value=TRUE))), function(col) all(is.na(Frame[[col]])))))


FFQ_POIDS$FEC_NON_RAF_FFQ <- terms
CSGA_POIDS$FEC_NON_RAF_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pain.completpains.speciaux +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mais + 
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommes.de.terre.a.l.eau +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommes.de.terre.rissolees + 
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.puree.de.pommes.de.terre +
                                 CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.feculents)/1000


##STARCHES_REFINED_FFQ---------------------
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
    recognized_columns[[category]] <- "Aucune colonne reconnue"
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


CSGA_POIDS$FEC_RAF_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.semouleble +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.riz + 
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pates +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pain +
                             CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.biscottescracottepains.grilles )/1000

##CHEESES_FFQ----------------------------
FFQ_POIDS$FROMAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.en.morceaux","du.Roquefortdu.Bleu.quelle.qu.en.soit.l.origine",
                "du.fromage.de.chevre","autres.types.de.fromages.camembertbrie.","de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.rape.sur.les.plats.patesriz.",
                "du.fromage.a.pate.molle.camembertcoulommiersbrie.","du.fromage.a.tartiner.cancoillotteSaint.MoretVache.qui.rit.","de.la.mozzarella")
terms <- numeric(nrow(Frame))
non_reconnues <- c()

for (cat in categories) {
  ncol <- grep(cat, colnames(Frame))
  
  if (length(ncol) == 0) {
    # Add to the list of unrecognized categories
    non_reconnues <- c(non_reconnues, cat)
  } else {
    # Apply the formula if the column is found
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[cat]])
    terms <- terms + term
    FFQ_POIDS_Int[[cat]] <- term
  }
}

FFQ_POIDS$FROMAGES_FFQ <- terms

CSGA_POIDS$FROMAGES_FFQ <-  (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.emmentalgruyerecomtebeaufort.en.morceaux +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.emmentalgruyerecomtebeaufort.rape +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.roquefortbleu +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fromage.de.chevre +
                               CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.autres.types.de.fromages)/1000


##FRUITS_FFQ -----------------------
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

CSGA_POIDS$FRUITS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.abricotpecheprunecerise +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fraiseframboise + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.raisin +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.melonpasteque + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.banane +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.kiwi + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.agrumes +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pommepoire + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.exotiques )/1000

##FRUIT_JUICE_FFQ---------------
FFQ_POIDS$FRUITS_JUS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre", colnames(Frame))
FFQ_POIDS$FRUITS_JUS_FFQ<-  replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$JUS.portion)) 
FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ

CSGA_POIDS$FRUITS_JUS_FFQ <- CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.jus.d.orangepamplemousseananaspommeraisin / 1000

##DRIED_FRUITS_FFQ ------------------
FFQ_POIDS$FRUITS_SECS_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.seches.abricotsdattesfiguespruneaux.",colnames(Frame))
FFQ_POIDS$FRUITS_SECS_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.seches.abricotsdattesfiguespruneaux.)
FFQ_POIDS_Int$des.fruits.seches.abricotsdattesfiguespruneaux. <- FFQ_POIDS$FRUITS_SECS_FFQ


CSGA_POIDS$FRUITS_SECS_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.seches / 1000

Frame$de.lait.demi.ecreme
##MILK_FFQ---------------------
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

CSGA_POIDS$LAIT_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.demi.ecreme + 
                          CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.ecreme +
                          CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.lait.entier ) / 1000

##DAIRY_PRODUCTS_FFQ------------------
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

CSGA_POIDS$LAITAGES_FFQ <- (CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.fromage.blancpetits.suissesyaourts.20%30%40%MG`+ CSGA_POIDS$`Quantite.g.journaliere.de.consommation.de.fromage.blancyaourt.0%MG`)/1000
##DRIED_LEGUMES_FFQ-------------------
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
CSGA_POIDS$LEG_SECS_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.legumes.secs/1000


##VEGETABLES_FFQ-------------------
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

##ANIMAL_FAT_FFQ -----------------------
FFQ_POIDS$MGA_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates.",colnames(Frame))
ncol2 <- grep("de.la.creme.fraiche",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. )
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.la.creme.fraiche)
FFQ_POIDS$MGA_FFQ <- term1 + term2 
FFQ_POIDS_Int$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. <-term1 
FFQ_POIDS_Int$de.la.creme.fraiche <-term2 


CSGA_POIDS$MGA_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.creme.fraiche + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.beurre)/1000

##VEGETABLE_FAT_FFQ------------------
FFQ_POIDS$MGV_FFQ <- rep(0,nrow(Frame))
CSGA_POIDS$MGV_FFQ <- rep(0,nrow(CSGA_POIDS))


##NUTS_FFQ----------------
FFQ_POIDS$NOIX_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.a.coque.noixnoisettesamandes.",colnames(Frame))
FFQ_POIDS$NOIX_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)

CSGA_POIDS$NOIX_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.fruits.a.coque)/1000
FFQ_POIDS_Int$des.fruits.a.coque.noixnoisettesamandes. <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.) 

##EGGS_FFQ-----------------
FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.oeufspochesdurs.ou.a.la.coque.2",colnames(Frame))
ncol2 <- grep("des.oeufssur.le.plat.en.omelette1",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*0.08333 ) # Poids_modifie$des.oeufspochesdurs.ou.a.la.coque.1)
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$des.oeufssur.le.plat.en.omelette1)
FFQ_POIDS$OEUFS_FFQ <- term1 + term2 

FFQ_POIDS_Int$des.oeufspochesdurs.ou.a.la.coque.2 <-term1 
FFQ_POIDS_Int$des.oeufssur.le.plat.en.omelette1 <-term2 


CSGA_POIDS$OEUFS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.oeufs.pochesdursa.la.coque +
                           CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.oeufs.sur.le.platomelette)/1000

##SWEET_PRODUCTS_FFQ ---------------------
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

##READY_MEALS_FFQ------------------
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

CSGA_POIDS$PLATS_PREP_CARNES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.raviolislasagnespates.fourrees +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cassoulet +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.couscous +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.paella  +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.choucroute +
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chili.con.carne + 
                                       CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.plats.cuisines.alleges+
                                       + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.plats.cuisines.a.base.de.poisson)/1000

##FISH_FFQ--------------------
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

CSGA_POIDS$POISSONS_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.frais.ou.congele +
                              CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.a.l.huile + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.pane +
                              CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.poisson.saleen.saumure +
                              CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.coquillages)/1000

##PORK_FFQ-----------------
FFQ_POIDS$PORC_FFQ<- rep(0,nrow(Frame))
ncol1 <- grep("de.la.viande.de.porc.sauf.charcuterie",colnames(Frame))
FFQ_POIDS$PORC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.viande.de.porc.sauf.charcuterie)
FFQ_POIDS_Int$de.la.viande.de.porc.sauf.charcuterie <- FFQ_POIDS$PORC_FFQ

CSGA_POIDS$PORC_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.porc.sf.charcuterie/1000

##CHICKEN_FFQ------------------
FFQ_POIDS$POULET_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.la.volaille.pouletdinde.du.lapin",colnames(Frame))
FFQ_POIDS$POULET_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.volaille.pouletdinde.du.lapin)
FFQ_POIDS_Int$de.la.volaille.pouletdinde.du.lapin <- FFQ_POIDS$POULET_FFQ

CSGA_POIDS$POULET_FFQ <- CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.volaillelapin/1000

##QUICHES_PIZZAS_SAVORY_TARTS-----------------
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


CSGA_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.pizza + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.tartes.salees)/1000

##SAUCES_FFQ-----------------
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
CSGA_POIDS$SAUCES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.mayonnaise +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sauce.vinaigrette +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sauce.soja +
                            CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.ketchup)/1000

##SNACKS_OTHER_FFQ---------------
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
CSGA_POIDS$SNACKS_AUTRES_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.cacahuetes +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.gateaux.aperitifs.sales +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.chips +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.friands +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.sandwichs +
                                   CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.hamburgers)/1000

##LIGHT_SODAS_FFQ-------------------
FFQ_POIDS$SODAS_LIGHT_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.lait.vegetal.sojarizavoine.", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$LAIT_VEGE.portion) 
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$LIGHT.portion)
FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2

CSGA_POIDS$SODAS_LIGHT_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.boisson.au.soja + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.colalimonadesoda.light)/1000

##SUGARY_SODAS_FFQ------------------
FFQ_POIDS$SODAS_SUCRES_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.sirop.verre", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre", colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$SIROP.portion) 
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$NON_LIGHT.portion)
FFQ_POIDS_Int$de.sirop.verre <- term1
FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2

FFQ_POIDS$SODAS_SUCRES_FFQ <- term1 + term2
CSGA_POIDS$SODAS_SUCRES_FFQ <- (CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.siropeau.aromatisee + CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.colalimonadesoda.non.light)/1000

##RED_MEAT_FFQ--------------
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

CSGA_POIDS$VIANDE_ROUGE_FFQ <- (CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.boeuf +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.steaks.haches + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.de.veau +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.viande.d.agneaumouton + CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.languetripesboudinandouilletteris.de.veaurognons +
                                  CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.foie)/1000


#I REMOVE spices / CAFE_THE/ MGV
CSGA_POIDS$EPICES_CONDIMENTS_FFQ <- NULL
#CSGA_POIDS$CAFE_THE_FFQ <- NULL
CSGA_POIDS$MGV_FFQ <- NULL
#CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.cafe<- NULL
#CSGA_POIDS$Quantite.ml.journaliere.de.consommation.de.the<- NULL
CSGA_POIDS$Quantite.g.journaliere.de.consommation.de.margarine <- NULL
#FFQ_POIDS$CAFE_THE_FFQ <- NULL


#Sum - TI PROCESSING
FFQ_POIDS$POIDS_TOTAL_FFQ <- rowSums(FFQ_POIDS[2:32])


FFQ_POIDS$POIDS_HORS_BOISSON_FFQ <- replace_na_with_zero(FFQ_POIDS$POIDS_TOTAL_FFQ - 
                                                           FFQ_POIDS$ALCOOL_FFQ - 
                                                           FFQ_POIDS$FRUITS_JUS_FFQ - 
                                                           FFQ_POIDS$LAIT_FFQ -
                                                           FFQ_POIDS$EAU_FFQ -
                                                           FFQ_POIDS$SODAS_LIGHT_FFQ -
                                                           FFQ_POIDS$SODAS_SUCRES_FFQ - 
                                                           FFQ_POIDS$CAFE_THE_FFQ)



#Adding the _Poids suffix
FFQ_POIDS <- FFQ_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_FFQ$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )


df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

#df_long <- inner_join(df_long, CALNUT, by= "FFQ_CSGA", relationship = "many-to-many")
df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")

#CALCULATING KILOCALORIES PER FOOD TI
df_long$nrj_kcal_alim <- df_long$nrj_kcal*df_long$Poids*10
FFQ_KCAL <- aggregate(nrj_kcal_alim ~  Identifiant + groupe_TI_TdC   , df_long, FUN = sum)
FFQ_KCAL<- pivot_wider(
  FFQ_KCAL,
  id_cols = Identifiant,
  names_from = groupe_TI_TdC,
  values_from = nrj_kcal_alim
)


# Calculating the sum of the columns for each row
FFQ_KCAL$KCAL_TOTAL <- rowSums(FFQ_KCAL[, 2:ncol(FFQ_KCAL)], na.rm = TRUE)


# Calculating the sum of the columns excluding beverages
FFQ_KCAL$KCAL_HORS_BOISSON <- with(FFQ_KCAL, KCAL_TOTAL  - 
                                     ALCOOL - 
                                     FRUITS_JUS - 
                                     CAFE_THE - 
                                     LAIT - 
                                     EAU - 
                                     SODAS_LIGHT - 
                                     SODAS_SUCRES)


# Adding the _FFQ_KCAL suffix to each category
FFQ_KCAL <- FFQ_KCAL %>%
  rename_with(
    ~ paste0(.x, "_FFQ_Kcal"),
    .cols = -Identifiant
  )




FFQ_KCAL$UC_TI <- NULL


#Building the final table ------------------------------------------------
FFQ_id <- FFQ_POIDS
FFQ_id<- inner_join(FFQ_id, FFQ_KCAL, by="Identifiant")
FFQ_id$Mesure <- "FFQ"



# DOWNLOAD ----------------------------

# Create a new workbook object
wb <- createWorkbook()

# Add each dataframe to a different sheet
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", FFQ_id)

addWorksheet(wb, "Frequences_corrigées")
writeData(wb, sheet = "Frequences_corrigées", Frame)

addWorksheet(wb, "Poids_corrigée_TI")
writeData(wb, sheet = "Poids_corrigée_TI", FFQ_POIDS)

addWorksheet(wb, "Poids_corrigée_CSGA")
writeData(wb, sheet = "Poids_corrigée_CSGA", FFQ_POIDS)

saveWorkbook(wb,("FFQ_CSGA.xlsx"))