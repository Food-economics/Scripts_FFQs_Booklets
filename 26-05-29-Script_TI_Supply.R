# 1. LOADING THE WORK ENVIRONMENT ----
## Package imports ----
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx); library(readxl);library(dplyr);
library(broom);library(scales);library(modelsummary);library(ggplot2);library(effsize);library(lfe);
library(ggpubr);library(vtable);library;library("openxlsx");
library("dplyr"); library("tidyr");library("ggplot2");library("gridExtra");library(lubridate);
library("RColorBrewer");library(reshape2);library(Metrics);library(questionr);library(zoo)

## Data import ----
# Set working directory based on the researcher's username

### Enter the campaign date ----
# Define the campaign period and the number of recording days
campaign<-"24-03" #"22-11" #23-02 #"23-11" #"24-03" 
Nj <- 28 #"Number of recording days: 28 if 23-11/24-03, 29 otherwise

### Import Nov_2022 data ----
Carnet_nov_22 <- read.xlsx("22-11_Carnets.xlsx")
Metadata_nov_22 <- read_excel("22-11_Carnets.xlsx", sheet = "Metadata")

### Import March_2023 data ----
Carnet_mars_23 <- read_excel("23-02_Carnets.xlsx")
Metadata_mars_23 <- read_excel("FFQ_Tableaux_mars_23.xlsx", sheet = "Metadata")
# Standardise the CCAS label by removing the parenthetical note
Metadata_mars_23 <- Metadata_mars_23 %>%
  mutate(Identifiant = gsub("-CCAS \\(inclus Pôle emploi et SPF\\)", "-CCAS", Identifiant))

### Import Nov_2023 data ----
Carnet_nov_23 <- read_excel("23-11_Carnets.xlsx")
Metadata_nov_23 <- read_excel("FFQ_Tableaux_nov_23.xlsx", sheet = "Metadata")

### Import March_2024 data ----
# The March 2024 data is split into two files (a and b); they are merged by common columns
Saisie_a <- read_excel("24-03_Carnets_a.xlsx")
Saisie_b <- read_excel("24-03_Carnets_b.xlsx")
Carnet_mars_24 <- full_join(Saisie_a,Saisie_b,by=c("Code","TicketCode","Lieu","Date","CodeCIQUAL","LibelleCIQUAL","Categorie1","Categorie2",
                                                   "Nb","Unite","Prix","Appreciation","Labels","Menu","PrixMenu","LibelleCustom","MontantChequeAlimentaire",
                                                   "DateSaisie","DateMAJ","Photo"))
Metadata_mars_24 <- read_excel("FFQ_Tableaux_mars_24.xlsx", sheet = "Metadata")

### Import ancillary reference tables ----
# These tables contain nutritional references, store classifications, RHD portions, and manual corrections
CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_CARNET.xlsx")
magasins <- read_excel("Reclassement_magasins.xlsx")
RHD_COL <- read_excel("RHD_COL.xlsx")
RHD_COM <- read_excel("RHD_COM.xlsx") 
Correction_lieu_nov_22 <- read_excel("22_11_correction_lieu.xlsx")
Correction_date_nov_22 <- read_excel("22-11_Correction_date.xlsx")
Correction_lieu_mars_23 <- read_excel("23_02_correction_lieu.xlsx")
Correction_date_mars_23 <- read_excel("23-02_Correction_date.xlsx")
Correction_lieu_nov_23 <- read_excel("23_11_correction_lieu.xlsx")
Correction_date_nov_23 <- read_excel("23-11_Correction-date.xlsx")
Correction_date_mars_24 <- read_excel("24-03_Correction-date.xlsx")
Correction_lieu_mars_24 <- read_excel("24_03_correction_lieu.xlsx")
Reclassement_Libelle_Custom <- read_excel("Reclassement_Libelle_Custom.xlsx")
Reclassement_Groupe_TI <- read_excel("Reclassement_groupe_TI.xlsx")
resultats_pondérés <- read_excel("moyennes_pondérées.xlsx")
Poids_unitaires_manquants <- read_excel("poids_unitaire_manquants.xlsx")
Recap_envoi_cheques <- read_excel("Recap_envoi_cheque.xlsx")
resultats_pondérés <- read_excel("resultats_pondérés.xlsx")

# 2. CLEANING THE COD_ACHATS FILE: LOCATIONS / DATES / LIBELLE_CUSTOM / LIBELLE_CIQUAL ----
# Select the dataset and assign it to resultats_codachats and metadata based on the value of the campaign variable
if (campaign == "22-11") { 
  resultats_codachats <- Carnet_nov_22
  metadata <- Metadata_nov_22 } else { 
    if (campaign == "23-02") { 
      resultats_codachats <- Carnet_mars_23
      metadata <- Metadata_mars_23 
    } else { 
      if (campaign == "23-11") { 
        resultats_codachats <- Carnet_nov_23
        metadata <- Metadata_nov_23 
      } else {  
        if (campaign == "24-03")
          resultats_codachats <- Carnet_mars_24
        metadata <- Metadata_mars_24 }}}


## Identifier corrections ----
# Rename the first column as "Identifiant" and fix known typos in participant IDs
names(resultats_codachats)[1] = "Identifiant"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="8447-CCAS") ] = "8747-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PE19-CCAS") ] = "PE019-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="1564-Epimut") ] = "1654-Epimut"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="P E013-CCAS") ] = "PE013-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP032-CCAS") ] = "SP040-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP-052-CCAS") ] = "SP052-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP-017-CCAS") ] = "SP017-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="pe003-CCAS") ] = "PE003-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PS004") ] = "LE255"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PS197") ] = "PS161"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE148") ] = "PS284"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE195") ] = "PS285"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE088") ] = "PS286"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE207") ] = "PS287"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE093") ] = "PS288"

print(unique(resultats_codachats$Identifiant))

## Extracting household composition and UC_TI count from Nov_2022 metadata into March_2023 ----
# For the 24-03 campaign, household composition questions were not re-asked, so UC_TI is not included
# in metadata_mars_24. The information is therefore extracted from the Nov_2023 campaign and merged.
if (campaign == "24-03") {
  Ajout <- Metadata_nov_23[, c("Identifiant", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer")]
  metadata <- metadata[, !names(metadata) %in% c("UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer" )]
  metadata <- inner_join(Ajout, metadata,  by = "Identifiant")}

#if (campaign == "23-02") {
#  Ajout <- Metadata_nov_22[, c("Identifiant", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer")]
#  metadata <- metadata[, !names(metadata) %in% c("UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer" )]
#  metadata <- inner_join(Ajout, metadata,  by = "Identifiant")}

# Join the voucher amounts sent to participants into metadata
metadata <- left_join (metadata, Recap_envoi_cheques, by="Identifiant")

## Merging UC and number of household members into resultats_codachats ----
# Extract the identifier, entry date, number of household members, UC_TI, and food budget from metadata,
# then join these variables into resultats_codachats
temp <- metadata[, c("Identifiant", "Date.de.saisie", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer", "Budget.mensuel.alimentation.", "Budget.hebdomadaire.alimentation.")]
# Compute a unified monthly food budget: use the monthly value if available, otherwise multiply the weekly value by 4
temp$budget_alim <- ifelse(is.na(temp$Budget.mensuel.alimentation.), (4*as.numeric(temp$Budget.hebdomadaire.alimentation.)), (temp$Budget.mensuel.alimentation.))
lignes_vide <- temp[is.na(temp $Date.de.saisie), ]


# Empty rows are not considered here: these correspond to participants for whom a response was expected
# but no data was recorded (e.g. November 2023 / March 2024).
# Verification to check that the join on identifiers is performed correctly
#resultats_codachats <- resultats_codachats %>%
#  anti_join(temp,resultats_codachats , by = c("Identifiant"))
# Find identifiers present in metadata but not in resultats_codachats
#identifiants_seulement_metadata <- setdiff(metadata$Identifiant, resultats_codachats$Identifiant)
#print("Identifiers present only in metadata:")
#print(identifiants_seulement_metadata)
# Find identifiers present in resultats_codachats but not in metadata
#identifiants_seulement_resultats_codachats <- setdiff(resultats_codachats$Identifiant, metadata$Identifiant)
#print("Identifiers present only in resultats_codachats:")
#print(identifiants_seulement_resultats_codachats)
#print(unique(valeurs_non_jointes$Identifiant))
resultats_codachats <- left_join(resultats_codachats,temp, by = "Identifiant") 
print(unique(resultats_codachats$Identifiant))

lignes_vide <- resultats_codachats[is.na(resultats_codachats$Date.de.saisie), ]
unique(lignes_vide$Identifiant)

# 3. LOCATION CORRECTIONS ----
### Join resultats_codachats with corrected supply locations from the manually verified tables ----
# This step performs a join between the location correction tables (verified by Pascal)
# and the supply locations recorded in resultats_codachats.
# The code modifies and merges resultats_codachats with various correction sources
# depending on the campaign, while standardising the date format and assigning values to a new column Lieu_vf.
if (campaign == "22-11") {
  resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
  resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")
  # There are two date formats in the correction file. One date is given as a character string.
  # During conversion to Date, R reverses month and day. The following steps correct this before the join.
  Correction_lieu_nov_22$Date1 <-  as.Date(as.numeric(Correction_lieu_nov_22$Date), origin = "1899-12-30")
  # Convert dates to character strings
  dates_char <- format(Correction_lieu_nov_22$Date1, "%Y-%d-%m")
  # Split character strings into components
  date_parts <- strsplit(dates_char, "-")
  # Reassemble components in the correct order
  corrected_dates_char <- sapply(date_parts, function(x) {
    paste(x[1], x[2], x[3], sep = "-")
  })
  # Convert the corrected character strings back to Date objects
  Correction_lieu_nov_22$Date1 <- as.Date(corrected_dates_char, format = "%Y-%m-%d")
  Correction_lieu_nov_22$Date2 <- as.Date(Correction_lieu_nov_22$Date, format = "%m/%d/%Y")
  Correction_lieu_nov_22$Date <-  coalesce(Correction_lieu_nov_22$Date1, Correction_lieu_nov_22$Date2)
  Correction_lieu_nov_22 <- Correction_lieu_nov_22[, !names(Correction_lieu_nov_22) %in% c("Date1", "Date2")]
  # Remove trailing whitespace
  resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
  Correction_lieu_nov_22$Lieu <- trimws(Correction_lieu_nov_22$Lieu)
  # Standardise case
  resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
  Correction_lieu_nov_22$Lieu <- tolower(Correction_lieu_nov_22$Lieu)
  # Remove invisible characters
  resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
  Correction_lieu_nov_22$Lieu <- iconv(Correction_lieu_nov_22$Lieu, to = "ASCII//TRANSLIT")
  # Check that correction values are applied to the table
  valeurs_non_jointes <- Correction_lieu_nov_22 %>%
    anti_join(resultats_codachats, by = c("Lieu", "Date","Identifiant"))
  print(valeurs_non_jointes)
  resultats_codachats <- left_join(resultats_codachats, Correction_lieu_nov_22, by=c("Lieu", "Date","Identifiant"))
  resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
  # Verify that correction values appear in Lieu_cor: if so, the difference should be NA
  temp<- setdiff(Correction_lieu_nov_22$Lieu_cor, resultats_codachats$Lieu_vf)
  print(unique((resultats_codachats$Lieu_cor)))
}else{ 
  if (campaign == "23-02") {
    resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
    resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")
    Correction_lieu_mars_23$Date1 <-  as.Date(as.numeric(Correction_lieu_mars_23$Date), origin = "1899-12-30")
    dates_char <- format(Correction_lieu_mars_23$Date1, "%Y-%d-%m")
    date_parts <- strsplit(dates_char, "-")
    corrected_dates_char <- sapply(date_parts, function(x) {
      paste(x[1], x[2], x[3], sep = "-")
    })
    Correction_lieu_mars_23$Date1 <- as.Date(corrected_dates_char, format = "%Y-%m-%d")
    Correction_lieu_mars_23$Date2 <- as.Date(Correction_lieu_mars_23$Date, format = "%m/%d/%Y")
    Correction_lieu_mars_23$Date <-  coalesce(Correction_lieu_mars_23$Date1, Correction_lieu_mars_23$Date2)
    Correction_lieu_mars_23 <- Correction_lieu_mars_23[, !names(Correction_lieu_mars_23) %in% c("Date1", "Date2")]
    # Remove trailing whitespace
    resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
    Correction_lieu_mars_23$Lieu <- trimws(Correction_lieu_mars_23$Lieu)
    # Standardise case
    resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
    Correction_lieu_mars_23$Lieu <- tolower(Correction_lieu_mars_23$Lieu)
    # Remove invisible characters
    resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
    Correction_lieu_mars_23$Lieu <- iconv(Correction_lieu_mars_23$Lieu, to = "ASCII//TRANSLIT")
    # Check that correction values are applied to the table
    valeurs_non_jointes <- Correction_lieu_mars_23 %>%
      anti_join(resultats_codachats, by = c("Lieu", "Date","Identifiant"))
    print(valeurs_non_jointes)
    resultats_codachats <- left_join(resultats_codachats, Correction_lieu_mars_23, by=c("Lieu", "Date","Identifiant"))
    resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
    # Verify that correction values appear in Lieu_cor: if so, the difference should be NA
    temp<- setdiff(Correction_lieu_mars_23$Lieu_cor, resultats_codachats$Lieu_vf)
    print(temp)
    print(unique((resultats_codachats$Lieu_cor)))
  } else {
    if (campaign == "23-11") {
      resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
      resultats_codachats$Date <- as.Date(resultats_codachats$Date , format="%Y-%m-%d")
      Correction_lieu_nov_23$Date <- as.Date(Correction_lieu_nov_23$Date, format="%Y-%m-%d")
      # Remove trailing whitespace
      resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
      Correction_lieu_nov_23$Lieu <- trimws(Correction_lieu_nov_23$Lieu)
      # Standardise case
      resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
      Correction_lieu_nov_23$Lieu <- tolower(Correction_lieu_nov_23$Lieu)
      # Remove invisible characters
      resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
      Correction_lieu_nov_23$Lieu <- iconv(Correction_lieu_nov_23$Lieu, to = "ASCII//TRANSLIT")
      # Check that correction values are applied to the table
      valeurs_non_jointes <- Correction_lieu_nov_23 %>%
        anti_join(resultats_codachats,  Correction_lieu_nov_23,by = c("Lieu", "Date","Identifiant"))
      print(valeurs_non_jointes)
      resultats_codachats <- left_join(resultats_codachats, Correction_lieu_nov_23, by=c("Lieu", "Date","Identifiant"))
      resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
      # Verify that correction values appear in Lieu_cor: if so, the difference should be NA
      temp<- setdiff(Correction_lieu_nov_23$Lieu_cor, resultats_codachats$Lieu_vf)
      print(temp)
      print(unique((resultats_codachats$Lieu_cor)))
    } else {
      if (campaign == "24-03") {
        resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
        resultats_codachats$Date <- as.Date(resultats_codachats$Date , format="%Y-%m-%d")
        Correction_lieu_mars_24$Date <- as.Date(Correction_lieu_mars_24$Date, format="%Y-%m-%d")
        # Remove trailing whitespace
        resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
        Correction_lieu_mars_24$Lieu <- trimws(Correction_lieu_mars_24$Lieu)
        # Standardise case
        resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
        Correction_lieu_mars_24$Lieu <- tolower(Correction_lieu_mars_24$Lieu)
        # Remove invisible characters
        resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
        Correction_lieu_mars_24$Lieu <- iconv(Correction_lieu_mars_24$Lieu, to = "ASCII//TRANSLIT")
        # Check that correction values are applied to the table
        valeurs_non_jointes <- Correction_lieu_mars_24 %>%
          anti_join(resultats_codachats,  Correction_lieu_mars_24,by = c("Lieu", "Date","Identifiant"))
        print(valeurs_non_jointes)
        resultats_codachats <- left_join(resultats_codachats, Correction_lieu_mars_24, by=c("Lieu", "Date","Identifiant"))
        resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
        # Verify that correction values appear in Lieu_cor: if so, the difference should be NA
        temp<- setdiff(Correction_lieu_mars_24$Lieu_cor, resultats_codachats$Lieu_vf)
        print(temp)
        print(unique((resultats_codachats$Lieu_cor)))
      }}}}

#print(unique(resultats_codachats$Identifiant))

### Join supply locations with the store-type classification ("Lieu1" / "Lieu2") ----
# Normalise the Lieu_vf column in both tables before joining (whitespace, case, invisible characters)
resultats_codachats$Lieu_vf <- trimws(resultats_codachats$Lieu_vf)
magasins$Lieu_vf <- trimws(magasins$Lieu_vf)
resultats_codachats$Lieu_vf <- tolower(resultats_codachats$Lieu_vf)
magasins$Lieu_vf <- tolower(magasins$Lieu_vf)
resultats_codachats$Lieu_vf<- iconv(resultats_codachats$Lieu_vf, to = "ASCII//TRANSLIT")
magasins$Lieu_vf<- iconv(magasins$Lieu_vf, to = "ASCII//TRANSLIT")
#valeurs_non_jointes <- resultats_codachats  %>%
#  anti_join(magasins, by = c("Lieu_vf"))

resultats_codachats <- inner_join(resultats_codachats, magasins, by=c("Lieu_vf")) 
describe(is.na(resultats_codachats$Lieu2))

# Remove unnecessary columns
if (campaign == "22-11" |campaign == "23-02"|campaign == "23-11") {
  resultats_codachats <- subset(resultats_codachats, select = -c(Photo, Nom_photo, Lieu_cor, Observation))}

# 4. DATE CORRECTIONS ----
### Initialise the recording start and end date boundaries ----
# The code adjusts start and end dates in resultats_codachats based on the campaign,
# after converting the entry date to Date format. Periods differ slightly by campaign.
# For campaigns 22-23: participants started recording on the day they received the booklet.
# For campaigns 23-24: participants started recording the day after completing the FFQ.
# Recording lasted 29 days in 22-23 and 28 days in 23-24.

# Convert dates to the correct format, handling multiple input formats
formats <- c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y")
resultats_codachats$Date <- parse_date_time(resultats_codachats$Date, orders = formats)
resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")

# Define start dates
resultats_codachats$date_starting <- as.Date(resultats_codachats$Date.de.saisie, format = "%d/%m/%Y")
if (campaign == "22-11" |campaign == "23-02") {
  # 29 recording days in campaigns 22-23
  resultats_codachats$Date_début <- resultats_codachats$date_starting 
  resultats_codachats$Date_fin <- resultats_codachats$date_starting +28
} else if (campaign == "23-11" | campaign == "24-03") {
  # 28 recording days in campaigns 23-24
  resultats_codachats$Date_début <- resultats_codachats$date_starting+1
  resultats_codachats$Date_fin <- resultats_codachats$date_starting +28}
lignes_vide <- resultats_codachats[is.na(resultats_codachats$Date_début), ]


### Remove data entered in the wrong link for November 2022 and November 2023 ----
if (campaign == "22-11") {
  start_date <- as.Date('2023-09-12')
  end_date <- as.Date('2023-10-25')
  donnees_a_supprimer <- resultats_codachats %>%
    filter(DateSaisie >= start_date & DateSaisie <= end_date)
  # 3580 observations to remove: 24399 observations should remain
  resultats_codachats <- resultats_codachats %>%
    filter(is.na(DateSaisie) | !(DateSaisie >= start_date & DateSaisie <= end_date))}

if (campaign == "23-11") {
  start_date <- as.Date('2024-03-01')
  end_date <- as.Date('2024-03-31')
  # Remove dates in March: 31190 observations should remain
  dates_mars <- resultats_codachats %>%
    filter(Date >= start_date & Date < end_date)
  resultats_codachats <- resultats_codachats %>%
    filter(is.na(Date) | Date < start_date | Date > end_date) } 

### Extend the end-of-recording boundary for participants who went on holiday ----
resultats_codachats <- resultats_codachats %>%
  mutate(Date_fin = case_when(
    campaign == "24-03" & Identifiant %in% c("LE041", "LE043", "PS192", "PS194", "PS208", "PS229", "PS244") ~ date_starting + 36, # 29 days + 7 extra days
    campaign == "24-03" & Identifiant == "PS267" ~ date_starting + 43, # Add two weeks: 29 days + 14 extra days
    TRUE ~ Date_fin
  ))

# Check on a few identifiers to verify that the filter works correctly
temp <- resultats_codachats %>% filter (resultats_codachats$Identifiant == "PS267") 

### Assign a random date within the recording interval for each ticket entered more than 56 days before or after the recording period ----
#### Function to generate a random date between Date_début and Date_fin ----
# Date_début and Date_fin are converted to Date objects to ensure valid date operations
generate_random_date <- function(Date_début, Date_fin) { 
  Date_début <- as.Date(Date_début)
  Date_fin <- as.Date(Date_fin)
  # If either date is NA, return NA to avoid computation errors on undefined values
  if (is.na(Date_début) || is.na(Date_fin)) { return(NA) }
  # Compute the difference in days between Date_fin and Date_début using difftime, then convert to numeric
  diff_days <- as.numeric(difftime(Date_fin, Date_début, units = "days"))
  # Generate a random number of days between 0 and diff_days using sample
  random_days <- sample(0:diff_days, 1)
  # Add the random number of days to Date_début to obtain the random date
  Date_aléatoire <- Date_début + random_days
  return(Date_aléatoire)
}

#### Apply the function to each row of the dataframe ----

resultats_codachats <- resultats_codachats %>%
  rowwise() %>%
  mutate(Date_aléatoire = generate_random_date(Date_début, Date_fin))

#### Replace dates outside the 56-day window ----
# For each row in resultats_codachats, this transformation checks whether Date falls outside the range
# between 56 days before Date_début and 56 days after Date_fin.
# If so, Date is replaced by Date_aléatoire; otherwise Date retains its original value.
resultats_codachats <- resultats_codachats %>%
  mutate(Date = case_when(
    !is.na(Date) & (Date < Date_début - 56 | Date > Date_fin + 56) ~ Date_aléatoire,
    TRUE ~ Date
  ))

resultats_codachats$Date <- as.Date(resultats_codachats$Date, origin = "1970-01-01")

### Check whether any tickets still fall outside the boundaries ----
#A_verifier <-resultats_codachats %>% filter(Date < Date_début| Date > Date_fin)
#tickets_a_verifier <- A_verifier %>%
#  select(Identifiant, Lieu, Date) %>%
#  distinct()
# Define the path to the photo directory
#repertoire_photos <- "E:/TdC_novembre_2023"

# List all files in the directory
#photos <- list.files(path = repertoire_photos)
# Convert the list of filenames to a data frame
#df_photos <- data.frame(Noms_de_Photos = photos)
# Extract the identifier and date from each filename
#df_photos$Identifiant <- sub("^(.*)_.*$", "\\1", df_photos$Noms_de_Photos)
#df_photos$Date <- sub("^.*_(\\d{8}).*$", "\\1", df_photos$Noms_de_Photos)
# Convert the date to "YYYY-MM-DD" format
#df_photos$Date <- as.Date(df_photos$Date, format = "%Y%m%d")
# Remove the Noms_de_Photos column and duplicate rows
#df_photos <- df_photos %>%
#  select(-Noms_de_Photos) %>%
#  distinct()
#df_photos$Message <- "La_photo_existe" 

# Perform a join to check for matches
#result <- left_join(tickets_a_verifier, df_photos, by = c("Identifiant", "Date"))
#write.xlsx(result,(paste0(campaign,"_carnet_appro/verif2.xlsx")))
#write.xlsx(tickets_a_verifier,(paste0(campaign,"_carnet_appro/verif.xlsx")))

### Apply manual corrections: all out-of-boundary tickets have been verified and dates corrected, deleted, or left pending if the ticket is illegible ----
# Date column conversion to Date format
# For all values outside the boundaries recorded up to 56 days before and after the entry date,
# the date was verified if the photo was available.
#resultats_codachats$Date <- as.Date(resultats_codachats$Date, format="%Y-%m-%d")

# Select the correction table based on the campaign
if (campaign == "22-11") {
  Correction_date <- Correction_date_nov_22
} else if (campaign == "23-02") {
  Correction_date <- Correction_date_mars_23
} else if (campaign == "23-11") {
  Correction_date <- Correction_date_nov_23
} else if (campaign == "24-03") {
  Correction_date <- Correction_date_mars_24
}

# Convert the Date_corrigée column to Date format if it exists
Correction_date$Date_corrigée <- as.Date(Correction_date$Date_corrigée, format="%Y-%m-%d")
Correction_date$Date <- as.Date(Correction_date$Date, format="%Y-%m-%d")
Correction_date$Lieu <- trimws(Correction_date$Lieu)
# Standardise case
Correction_date$Lieu <- tolower(Correction_date$Lieu)
# Remove invisible characters
Correction_date$Lieu<- iconv(Correction_date$Lieu, to = "ASCII//TRANSLIT")
# Left join between resultats_codachats and Correction_date
valeurs_non_jointes <- Correction_date %>%
  anti_join(resultats_codachats, by = c("Lieu", "Date", "Identifiant"))

resultats_codachats <- left_join(resultats_codachats, Correction_date, by=c("Lieu", "Date", "Identifiant"))
dup_rows <- Correction_date[duplicated(Correction_date[, c("Identifiant", "Date", "Lieu")]), ]
print(dup_rows)
print(unique(resultats_codachats$Date_corrigée))
print(unique(resultats_codachats$Identifiant))

# Use coalesce to create a final date column Date_vf: use corrected date if available, otherwise keep original
resultats_codachats$Date_vf <- coalesce(resultats_codachats$Date_corrigée, resultats_codachats$Date)

# Convert Date_vf to Date format
resultats_codachats$Date_vf <- as.Date(resultats_codachats$Date_vf, format="%Y-%m-%d")

# Filter out rows flagged for deletion
resultats_codachats <- resultats_codachats %>%
  filter(is.na(`A supprimer`)) 
print(unique(resultats_codachats$`A supprimer`))

### Check whether any tickets still fall outside the boundaries ----
#A_verifier <-resultats_codachats %>% filter(Date_vf < Date_début| Date_vf > Date_fin)
#tickets_a_verifier <- A_verifier %>%
#  select(Identifiant, Lieu, Date) %>%
#  distinct()

### For missing or illegible tickets: compare the date with the entry date and apply the following corrections ----
# If the ticket year differs from the entry year: apply the entry year
# Same logic for the month
# If neither month nor year match: remove the date
# The first if checks whether the campaign is "23-11" or "24-03"
if (campaign == "23-11" | campaign == "24-03") {
  if (all(resultats_codachats$Date_vf < resultats_codachats$Date_début | resultats_codachats$Date_vf > resultats_codachats$Date_fin)) {
    
    resultats_codachats <- resultats_codachats %>%
      mutate(
        Date_finale = case_when(
          (Date_vf > Date_fin | Date_vf < Date_début) ~
            case_when(
              year(DateSaisie) != year(Date_vf) ~ make_date(year(DateSaisie), month(Date_vf), day(Date_vf)),
              month(DateSaisie) != month(Date_vf) ~ make_date(year(Date_vf), month(DateSaisie), day(Date_vf)),
              (year(DateSaisie) != year(Date_vf) & month(DateSaisie) != month(Date_vf)) ~ as.Date(DateSaisie),
              TRUE ~ Date_vf
            ),
          TRUE ~ Date_vf
        )
      )
    
    resultats_codachats$Date_vf <- resultats_codachats$Date_finale
  }
}

# List of identifiers to filter
#identifiants_a_filtrer <- c("1730-Epimut", "A1-Epimut", "6222-Episourire", "6273-Episourire", "SP041-CCAS")
#Frame_filtre <- resultats_codachats[resultats_codachats$Identifiant %in% identifiants_a_filtrer, ]
#print(unique(Frame_filtre$Identifiant))
#print(unique(resultats_codachats$Identifiant))
#temp <- resultats_codachats[is.na(resultats_codachats$Date_début) | as.Date(resultats_codachats$Date_vf) > as.Date(resultats_codachats$Date_début), ]
#temp <-resultats_codachats[is.na(resultats_codachats$Date_début) | as.Date(resultats_codachats$Date_vf) < as.Date(resultats_codachats$Date_fin), ]
#ids_in_resultats_not_in_temp <- setdiff(resultats_codachats$Identifiant, temp$Identifiant)
# Finding IDs present in temp but not in resultats_codachats
#ids_in_temp_not_in_resultats <- setdiff(temp$Identifiant, resultats_codachats$Identifiant)
# Output the differences
#cat("IDs in resultats_codachats but not in temp:\n")
#print(ids_in_resultats_not_in_temp)
#cat("\nIDs in temp but not in resultats_codachats:\n")
#print(ids_in_temp_not_in_resultats)
#print(unique(resultats_codachats$Identifiant))

# Fix a store name typo
resultats_codachats$Lieu_vf [resultats_codachats$Lieu_vf== "epi'sourire - dijon(place jeacques prevert)" ] <- "epi'sourire - dijon (place jeacques prevert)"

# Remove opticourses data
# Store rows before filtering
#resultats_avant_filtre <- resultats_codachats
# Remove rows with no entry date: these correspond to opticourses participants
#resultats_codachats <- resultats_codachats %>%
#  filter(!is.na(Date_début))
# Find removed rows by comparing before and after filtering
#lignes_supprimees <- anti_join(resultats_avant_filtre, resultats_codachats)
# Display removed rows
#lignes_supprimees
#unique(lignes_supprimees$Identifiant)


### Remove data outside the recording boundaries ----
# Apply filtering using case_when
resultats_avant_filtre <- resultats_codachats
resultats_codachats <- resultats_codachats %>%
  filter(
    case_when(
      is.na(Date_vf) ~ TRUE, # Keep rows where Date_vf is NA
      as.Date(Date_vf) >= as.Date(Date_début) & as.Date(Date_vf) <= as.Date(Date_fin) ~ TRUE, 
      TRUE ~ FALSE # Exclude all other rows
    )
  )

# Find removed rows by comparing before and after filtering
lignes_supprimees <- anti_join(resultats_avant_filtre, resultats_codachats)
# Display removed rows
lignes_supprimees


# Compute relative week number
resultats_codachats$semaine_num <- floor(as.numeric(resultats_codachats$Date - resultats_codachats$date_starting) / 7) + 1
# Cap at week 4
resultats_codachats$semaine_num <- pmin(resultats_codachats$semaine_num, 4)
# Keep only rows where semaine_num >= 1
resultats_codachats <- resultats_codachats[
  resultats_codachats$semaine_num >= 1,
]

# 5. JOINING COD-ACHATS WITH CALNUT ----
# Merge the CALNUT nutritional reference table with the resultats_codachats recording file.
# Rename "CodeCIQUAL" to "CODACHATS_alim_code" and "Categorie1" to "groupe_TI_TdC".
# resultats_codachats and CALNUT are joined by CODACHATS_alim_code.
colnames(resultats_codachats)[colnames(resultats_codachats) == 'CodeCIQUAL'] <- 'CODACHATS_alim_code'
resultats_codachats <- left_join(resultats_codachats, CALNUT, by=c("CODACHATS_alim_code"))

#Rename "Category1" with groupe_TI_TdC
colnames(resultats_codachats)[colnames(resultats_codachats) == 'Categorie1'] <- 'groupe_TI_TdC1'

### Assign foods recorded under Libelle_Custom to a TI group ----
resultats_codachats<- left_join(resultats_codachats, Reclassement_Libelle_Custom, by=c("LibelleCustom"), relationship = "many-to-many")

#diff_df1_df2 <- setdiff(resultats_codachats_test$LibelleCustom, resultats_codachats$LibelleCustom)
#diff_df2_df1 <- setdiff(resultats_codachats$LibelleCustom, resultats_codachats_test$LibelleCustom)

# Standardise Categorie2 to uppercase
resultats_codachats$Categorie2 <- toupper(resultats_codachats$Categorie2 )

# If groupe_TI_TdC1 is missing, use the value from Reclassement_TI; otherwise keep the original
temp <- ifelse(( is.na(resultats_codachats$groupe_TI_TdC1)),(resultats_codachats$Reclassement_TI),(resultats_codachats$groupe_TI_TdC1))
resultats_codachats$groupe_TI_TdC1 <- temp
# Prioritise the value from groupe_TI_TdC (from CALNUT) over groupe_TI_TdC1 where available
resultats_codachats$groupe_TI_TdC1 <- ifelse((is.na(resultats_codachats$groupe_TI_TdC)),(resultats_codachats$groupe_TI_TdC1),(resultats_codachats$groupe_TI_TdC))

### Correct TI_TDC group classification errors ----
resultats_codachats<- left_join(resultats_codachats, Reclassement_Groupe_TI, by=c("groupe_TI_TdC1"), relationship = "many-to-many")
temp <- ifelse(( is.na(resultats_codachats$New)),(resultats_codachats$groupe_TI_TdC1),(resultats_codachats$New))
resultats_codachats$groupe_TI_TdC1 <- temp
describe(!is.na(resultats_codachats$Categorie2))
print(unique(resultats_codachats$Identifiant))


### Remove duplicate rows: eliminate duplicate voucher amounts ----
temp <- resultats_codachats %>%
  # Separate rows with non-empty MontantChequeAlimentaire and apply distinct()
  filter(!is.na(MontantChequeAlimentaire) & MontantChequeAlimentaire != "") %>%
  distinct() %>%
  # Add back rows where MontantChequeAlimentaire is empty or NA
  bind_rows(resultats_codachats %>% filter(is.na(MontantChequeAlimentaire) | MontantChequeAlimentaire == ""))
resultats_codachats  <- temp 


#22-11 MEAN : 18  MEDIAN 13  --> Epiceries MEAN 15 / MEDIAN  12
#23-02 : MEAN : 21 / MEDIAN 16 --> Epiceries MEAN 19 / MEDIAN  15
#23-11 :  MEAN 31 /  MEDIAN : 22 --> LE : MEAN 29 / MEDIAN 18.5
#24/03 : MEAN 40 / MEDIAN 36 --> LE : MEAN : 39 / MEDIAN 36

# 6. CLEANING COD_ACHATS: WEIGHTS / PRICES / UNITS ----

# If the quantity exceeds 30 units and the food is not an egg or coffee/tea, assign grams
resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = ifelse(
      # Original condition...
      Nb > 30 &
        Unite == "unités" &
        # ...and the food is NOT a compote
        LibelleCIQUAL != "Compote de fruits, allégée en sucres" &
        LibelleCIQUAL != "Compote de pomme" &
        LibelleCIQUAL != "Compote de fruits" &
        LibelleCIQUAL != "Compote de fruits, allégée en sucres" & 
        LibelleCIQUAL != "Sushi ou maki aux produits de la mer" &
        groupe_TI_TdC1!="OEUFS" & 
        groupe_TI_TdC1!="CAFE_THE", 
      "grammes",
      Unite
    )
  )


# If quantity, price, PrixMenu or appreciation is negative or zero
# (CODAPPRO assigns the value "-1" for "don't know"), replace with NA
resultats_codachats$Prix[resultats_codachats$Prix < 0 ] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu < 0 ] <- NA
resultats_codachats$Nb[resultats_codachats$Nb <= 0 ] <- NA
resultats_codachats$Appreciation[resultats_codachats$Appreciation < 0 ] <- NA
# If price or PrixMenu equals 0 and the location is not recorded as a "donation", assign NA
resultats_codachats$Prix[resultats_codachats$Prix == 0 & resultats_codachats$Lieu1 != "dons" ] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu == 0 & resultats_codachats$Lieu1 != "dons" ] <- NA
# If quantity equals 0, assign NA
resultats_codachats$Nb[resultats_codachats$Nb == 0 ] <- NA

## Correct main unit conversion errors ----
# If quantity is below 10 g and the food is not a spice, multiply by 1000 (weight entered in kilos)
# If price exceeds 100€ and the food is not alcohol, assign NA
resultats_codachats$Nb <- ifelse((resultats_codachats$Unite == "grammes" & resultats_codachats$Nb < 10 & resultats_codachats$groupe_TI_TdC1 != "EPICES_CONDIMENTS" & resultats_codachats$Lieu2 != "RHD"), (resultats_codachats$Nb*1000 ), (resultats_codachats$Nb))
resultats_codachats$Prix <- ifelse((resultats_codachats$Prix > 100 & resultats_codachats$groupe_TI_TdC1 != "ALCOOL"), (NA), (resultats_codachats$Prix))

# If weight exceeds 50 kg, assign grams
# If quantity is between 10 g and 20.5 g, assign units
# If quantity is below 1 cl, assign litres
# If quantity exceeds 1000 kg, assign grams
resultats_codachats$Unite <- with(resultats_codachats,
                                  ifelse( Nb > 50 & Unite == "kilos", "grammes",
                                          ifelse(Nb > 10 & Nb < 20.5 & Unite == "grammes", "unités",
                                                 ifelse( Nb < 1 & Unite == "centilitres", "litres",
                                                         ifelse(Nb > 1000 & Unite == "kilos", "grammes", Unite)))))


## Adjust quantity when price is low and quantity is high ----
resultats_codachats$Nb <- with(resultats_codachats,ifelse(Prix < 3 & Nb > 17 & Unite == "litres", Nb /10,Nb))
## Case-by-case corrections ----

# Add a price per kg column
resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb

### New condition to multiply Nb by 10 for specific foods if Nb <= 20 g ----
aliments_specifiques <- c(
  "Bonbon/ bouchée chocolat fourrage gaufrettes/ biscuit", "Bonbons, tout type", "Champignon, tout type, cru",
  "Champignons à la grecque, appertisés", "Barre chocolatée biscuitée", "Fromage à pâte molle et croûte fleurie double crème environ 30% MG",
  "Miel", "Sucre blanc", "Crème fraîche, 15 à 20% MG, UHT", "Rillettes de poulet", "Champignon noir, séché",
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
  mutate(Nb = ifelse(LibelleCIQUAL %in% aliments_specifiques & Nb <= 20 & Unite == "grammes" & Prix_kg > 0.05, Nb * 10, Nb))

### New condition to divide Prix by 10 if Nb >= 100 g and Prix >= 10€ for specific foods ----
aliments_specifiques <- c("Saumon fumé", "Barres chocolatées", "Pâte à tartiner chocolat et noisette", "Rosette ou Fuseau", "Pâtisserie (aliment moyen)",
                          "Pomme de terre de conservation, crue", "Chocolat, en tablette (aliment moyen)", "Mélange apéritif graine non salée fruit séché",
                          "Mozzarella au lait de vache", "Sandwich baguette, jambon emmental")
resultats_codachats$Prix <- ifelse(
  resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & 
    resultats_codachats$Nb >= 100 & 
    resultats_codachats$Prix >= 10 & resultats_codachats$Prix_kg > 0.05 & resultats_codachats$Unite == "grammes", 
  resultats_codachats$Prix / 10,
  resultats_codachats$Prix)

### New condition to convert Nb to centilitres if Nb < 100 g for specific foods ----
aliments_specifiques <- c( "Bière \"de spécialités\" ou d'abbaye, régionales ou d'une brasserie (degré d'alcool variable)",
                           "Boisson gazeuse, sans jus de fruit, sucrée","Jus de fruits (aliment moyen)", "Boisson préparée à partir de sirop à diluer type menthe, fraise, etc., sucré, dilué dans l'eau",
                           "Huile de pépins de raisin")
resultats_codachats <- within(resultats_codachats, {
  Unite <- ifelse(
    LibelleCIQUAL %in% aliments_specifiques & Nb < 100 & Unite == "grammes" & Prix_kg > 0.05 , 
    "centilitres", 
    Unite
  )})

### Case-by-case corrections on quantities ----
print(unique(resultats_codachats$Identifiant))

# Assign a unit weight of 63 grams per egg
resultats_codachats <- resultats_codachats %>%
  mutate(
    # Convert quantity
    Nb = if_else(
      groupe_TI_TdC1 == "OEUFS" & Unite == "unités",
      Nb * 0.063,
      Nb
    ),
    # Update unit
    Unite = if_else(
      groupe_TI_TdC1 == "OEUFS" & Unite == "unités" & !is.na(Nb),
      "kilos",
      Unite
    )
  )
oeufs <- resultats_codachats[resultats_codachats$groupe_TI_TdC1 == "OEUFS", ]

resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Aubergine, crue" & 
    resultats_codachats$Nb == 75 & 
    resultats_codachats$Prix == 3.98,
  750,
  resultats_codachats$Nb
)


# 1 pot of compote = 100 g
resultats_codachats <- resultats_codachats %>%
  mutate(
    # Convert quantity
    Nb = if_else(
      (LibelleCIQUAL == "Compote de fruits allégée en sucres rayon frais"  |
         LibelleCIQUAL == "Compote de pomme" |
         LibelleCIQUAL == "Compote de fruits" |
         LibelleCIQUAL == "Compote de fruits, allégée en sucres" |
         LibelleCIQUAL == "Compote (aliment moyen)" 
       
      )
      & Unite == "unités",
      Nb * 0.1,
      Nb
    ),
    # Update unit
    Unite = if_else(
      (LibelleCIQUAL == "Compote de fruits allégée en sucres rayon frais"  |
         LibelleCIQUAL == "Compote de pomme" |
         LibelleCIQUAL == "Compote de fruits" |
         LibelleCIQUAL == "Compote de fruits, allégée en sucres" |
         LibelleCIQUAL == "Compote (aliment moyen)" 
      ) & Unite == "unités" & !is.na(Nb),
      "kilos",
      Unite
    )
  )

resultats_codachats$Unite <- ifelse(
  (resultats_codachats$LibelleCIQUAL == "Amande, avec peau" & 
     resultats_codachats$Nb == 0.2 & 
     resultats_codachats$Unite == "centilitres"),
  "kilos",
  resultats_codachats$Unite
)

df_libelles <- unique(resultats_codachats$LibelleCIQUAL)

# Keep only those starting with "Su"
libelles_S <- df_libelles[grepl("^Su", df_libelles)]


# Unit weight of one sushi = 0.04 kg (40 g)
resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
    resultats_codachats$Unite      == "unités",
  resultats_codachats$Nb * 0.04,  # multiply by 0.04 (kg)
  resultats_codachats$Nb
)

# Update unit from "unités" to "kilos" for sushi
resultats_codachats$Unite <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
    resultats_codachats$Unite      == "unités",
  "kilos",
  resultats_codachats$Unite
)

# Select rows where LibelleCIQUAL matches exactly
subset_sushi <- resultats_codachats %>%
  filter(LibelleCIQUAL == "Sushi ou maki aux produits de la mer")

resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Oeuf, cru" & resultats_codachats$Nb==30 ), (300), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Nem ou Pâté impérial" & resultats_codachats$Unite=="unités"), (resultats_codachats$Nb*0.1), (resultats_codachats$Nb))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Nem ou Pâté impérial" & resultats_codachats$Unite=="unités"), ("kilos"), (resultats_codachats$Unite))

resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Champignon, morille, crue" & resultats_codachats$Nb == 30 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (300), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Champignon, lentin ou shiitaké, séché" & resultats_codachats$Nb == 20 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (200), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Yaourt à la grecque, nature" & resultats_codachats$Nb == 12 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05 ), (120), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Yaourt aromatisé, avec édulcorants, 0% MG" & resultats_codachats$Nb == 12 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05 ), (120), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Yaourt aux fruits, sucré" & resultats_codachats$Nb == 16 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (160), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Purée de tomate" & resultats_codachats$Nb == 15000 ), (15), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Fruits de mer (aliment moyen), cru" & resultats_codachats$Nb == 12 & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (120), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Pizza 4 fromages" & resultats_codachats$Nb == 16 & resultats_codachats$Unite=="unités"), ( 1), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Terrine de canard" & resultats_codachats$Nb == 130 & resultats_codachats$Prix==7.6  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Sandwich baguette, jambon emmental" & resultats_codachats$Prix<0.45  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05), (NA), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Toasts ou Canapés salés, garnitures diverses, préemballés" & resultats_codachats$Prix==8.05  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05),(resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Rillettes de poulet" & resultats_codachats$Nb==180 & resultats_codachats$Prix==8.8  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05),(resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse(
  resultats_codachats$LibelleCIQUAL == "Bœuf, fauxfilet, cru" &
    resultats_codachats$Nb == 240 &
    resultats_codachats$Prix == 12.48 &
    resultats_codachats$Unite == "grammes" &
    resultats_codachats$Prix_kg > 0.05,
  resultats_codachats$Nb * 10,
  resultats_codachats$Nb
)
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL == "Bœuf, fauxfilet, cru" & resultats_codachats$Nb==110 & resultats_codachats$Prix==7.5  & resultats_codachats$Unite=="grammes" & resultats_codachats$Prix_kg > 0.05),(resultats_codachats$Nb*10), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="Moutarde" & resultats_codachats$Nb < 200 & resultats_codachats$Unite=="grammes" & resultats_codachats$Nb < 0.05  & resultats_codachats$Prix_kg > 0.05 ), (resultats_codachats$Nb*10 ), (resultats_codachats$Nb))
resultats_codachats$Nb <- ifelse((resultats_codachats$LibelleCIQUAL =="banane, crue" & resultats_codachats$Nb > 1070 ), (1.07 ), (resultats_codachats$Nb))

### Case-by-case corrections on units ----
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Pomme, crue" & resultats_codachats$Nb == 1 & resultats_codachats$Unite=="centilitres"), ("kilos" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Tomate, bouillie/cuite à l'eau" & resultats_codachats$Nb == 1600 ), ("unités"), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Vin rouge" & resultats_codachats$Nb == 1 & resultats_codachats$Unite=="centilitres"), ("litres" ), (resultats_codachats$Unite))
aliments_specifiques <- c("Sauce pour nems à base de nuocmam dilué, préemballée", "Sauce soja, préemballée",
                          "Bière \"spéciale\" (56° alcool)", "Bière \"spéciale\" (5-6° alcool)")
resultats_codachats$Unite <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Nb <= 3 & resultats_codachats$Unite == "centilitres", "litres", resultats_codachats$Unite)
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Sauce soja, préemballée" & resultats_codachats$Nb == 2.7 & resultats_codachats$Unite=="centilitres"), ("litres" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Fromage (aliment moyen)" & resultats_codachats$Nb == 1600 & resultats_codachats$Unite=="kilos"), ("grammes" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Vinaigre" & resultats_codachats$Nb == 1 & resultats_codachats$Unite=="grammes"), ("litres" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Sel blanc, non iodé, non fluoré" & resultats_codachats$Nb == 0.75 & resultats_codachats$Unite=="grammes"), ("kilos" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Moutarde" & resultats_codachats$Nb < 200 & resultats_codachats$Unite=="centilitres"), ("grammes" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Cornichon, au vinaigre" & resultats_codachats$Unite=="litres"), ("kilos" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Cornichon, au vinaigre" & resultats_codachats$Unite=="centilitres"), ("grammes" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Nb == 1 & resultats_codachats$Unite == "grammes", "kilos", resultats_codachats$Unite)
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Oeuf, cru" & resultats_codachats$Unite=="grammes"& resultats_codachats$Prix==100), ("unités" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Croissant, sans précision" & resultats_codachats$Identifiant=="LE116"& resultats_codachats$Nb=="2"), ("unités" ), (resultats_codachats$Unite))
resultats_codachats$Unite <- ifelse((resultats_codachats$LibelleCIQUAL =="Crème de lait, 15 à 20% MG, légère, épaisse, rayon frais" & resultats_codachats$Identifiant=="LE126" & resultats_codachats$Nb == 8), ("unités"), (resultats_codachats$Unite))

print(unique(resultats_codachats$Identifiant)) 

### Case-by-case corrections on prices ----
# If prices equal 100, assign NA
resultats_codachats$Prix <- ifelse((resultats_codachats$LibelleCIQUAL == "Oeuf, à la coque" & resultats_codachats$Prix==100),(NA), (resultats_codachats$Prix))
aliments_specifiques <- c("Pâtes sèches standard, cuites, non salées","Pâtes sèches standard, crues")
resultats_codachats$Prix <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Prix==100, 1, resultats_codachats$Prix)
resultats_codachats$Prix <- ifelse((resultats_codachats$LibelleCIQUAL == "Ravioli chinois vapeur à la crevette" & resultats_codachats$Nb==105 & resultats_codachats$Prix==7.5),(NA), (resultats_codachats$Prix))
aliments_specifiques <- c("Abat, cru (aliment moyen)", "Abat, cuit (aliment moyen)",
                          "Thon, cru ", "Accra de poisson")
resultats_codachats$Prix <- ifelse(resultats_codachats$LibelleCIQUAL %in% aliments_specifiques & resultats_codachats$Prix == 100, 10, resultats_codachats$Prix)


# 7. IMPUTATION OF MISSING WEIGHTS AND PRICES ----
## Correction of remaining price data ----
# During the November 2023 campaign, some participants recorded in-store grocery purchases under "PrixMenu".
# These purchases should not be treated as out-of-home (RHD/donation) meals.
# PrixMenu is set to NA for these entries since only the total grocery spend is available;
# the standard price/weight imputation procedure will be applied to these foods.
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="commerce" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="Epicerie" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="commerce" | resultats_codachats$Lieu2 =="Epicerie"), (NA), (resultats_codachats$Menu))
# If the product comes from out-of-home catering (RHD): transfer the price to prixMenu (everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(NA),(resultats_codachats$Prix))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="RHD" ), ("Oui"), (resultats_codachats$Menu))
resultats_codachats$Nb <-ifelse(((resultats_codachats$Lieu2 =="RHD") & resultats_codachats$Nb <9 & resultats_codachats$Unite =="grammes"    ),(NA),(resultats_codachats$Nb ))
resultats_codachats$Unite <-ifelse(((resultats_codachats$Lieu2 =="RHD") & is.na(resultats_codachats$Nb) & resultats_codachats$Unite =="grammes" ),(NA),(resultats_codachats$Unite ))

# If the product is a donation: transfer the price to prixMenu.
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="dons")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="dons")),(NA),(resultats_codachats$Prix))
# If it is not a menu, assign NA to PrixMenu
resultats_codachats$PrixMenu <-ifelse((is.na(resultats_codachats$Menu)),(NA),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse((is.na(resultats_codachats$PrixMenu)),(resultats_codachats$Prix),(NA))

# Divide the menu price by the total number of foods purchased together
# Group purchases by date and supply location
resultats_codachats <- resultats_codachats %>% arrange(Date) 
grouped_data <- resultats_codachats %>% group_by(Date_vf, Lieu_vf)
# Copy the menu price into all rows of the same ticket
resultats_codachats <- grouped_data %>% mutate(PrixMenu = ifelse(row_number() == 1, PrixMenu / n(), PrixMenu))
# Compute the individual price for each food
resultats_codachats <- grouped_data %>% mutate(PrixMenu = ifelse(PrixMenu, PrixMenu[1] / n(), PrixMenu))
# If at this stage the prices for RHD foods are zero, assign NA
resultats_codachats$Menu[resultats_codachats$PrixMenu == 0 ] <- NA
resultats_codachats$Menu[resultats_codachats$Menu == 0 ] <- NA


## Create homogeneous Price (€) and Weight (kg) variables ----
# Two harmonised variables that consolidate all weight and price data under a single unit
resultats_codachats$Poids <- resultats_codachats$Unite    
resultats_codachats$Prix_all <- resultats_codachats$Prix
resultats_codachats$Prix_all <-ifelse((is.na(resultats_codachats$Prix_all)),(resultats_codachats$PrixMenu),(resultats_codachats$Prix_all))
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


#resultats_codachats<-  resultats_codachats %>%
#  left_join( Poids_unitaires_manquants, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$Unite =="unités"  & is.na(resultats_codachats$Poids)  & is.na(resultats_codachats$Prix) ), (resultats_codachats$Poids_uni*resultats_codachats$Nb), (resultats_codachats$Poids))

describe(is.na(resultats_codachats$Poids))

## Compute price per kg for all foods ----
resultats_codachats$Prix_kg <- resultats_codachats$Prix_all / resultats_codachats$Poids
print(unique(resultats_codachats$Prix_kg))

## Compute mean price per kg and mean weight for imputation: CIQUAL code × Lieu1 ----
# Compute the mean price per unit weight for each food
prix_moyen_par_poids1 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # exclude NA
    str_trim(LibelleCIQUAL) != "",   # exclude empty strings
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  summarise(
    prix_moyen_ciqual1     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual1 = n(),
    .groups = "drop"
  )

# Join mean price data to the main table
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids1, by = c("LibelleCIQUAL", "Lieu1")) 

# prix_calculé_ciqual1: computes an imputed price using Poids and prix_moyen_ciqual1.
# poids_calculé_ciqual1: computes an imputed weight using Prix_all and prix_moyen_ciqual1.
resultats_codachats$prix_calculé_ciqual1  <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual1
resultats_codachats$poids_calculé_ciqual1 <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual1

# Count the number of observations per group
resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  mutate( nombre_donnees_ciqual1 = n())

## Compute mean price per kg and mean weight for imputation: CIQUAL code × Lieu2 ----
# Compute the mean price per unit weight for each food
prix_moyen_par_poids2 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # exclude NA
    str_trim(LibelleCIQUAL) != "",   # exclude empty strings
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  summarise(
    prix_moyen_ciqual2     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual2 = n(),
    .groups = "drop"
  )

# Impute missing values using the na.aggregate function
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids2,
            by = c("LibelleCIQUAL", "Lieu2"))

resultats_codachats$prix_calculé_ciqual2 <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual2
resultats_codachats$poids_calculé_ciqual2  <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual2

resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  mutate(nombre_donnees_ciqual2 = n())

## Compute mean price per kg and mean weight for imputation: TI group × Lieu2 ----
# Compute the mean price per unit weight for each food except "Epices_condiments"
prix_moyen_par_poids3 <- resultats_codachats %>%
  filter(!is.na(Prix_all), !is.na(Poids),
         groupe_TI_TdC1 != "Epices_condiments") %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  summarise(
    prix_moyen_groupe_TI_TdC1     = mean(Prix_kg),
    nombre_donnees_groupe_TI_TdC1 = n(),
    .groups = "drop"
  )

# Join mean prices to the original dataset
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids3,
            by = c("groupe_TI_TdC1", "Lieu2"))

# Compute imputed price and weight for all groups except "Epices_condiments"
resultats_codachats <- resultats_codachats %>%
  mutate(prix_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Poids * prix_moyen_groupe_TI_TdC1, NA_real_),
         poids_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Prix_all / prix_moyen_groupe_TI_TdC1, NA_real_))

# Compute the number of observations per group and location
resultats_codachats <- resultats_codachats %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  mutate(nombre_donnees_groupe_TI_TdC1 = n()) %>%
  ungroup()


bis <- resultats_codachats %>%
  filter(Identifiant == "39-Epimut") %>%
  select(Date, LibelleCIQUAL, groupe_TI_TdC1 , Poids, Prix_all, prix_moyen_ciqual1, prix_moyen_ciqual2,prix_moyen_groupe_TI_TdC1
  ) %>%
  print(n = Inf)

## Final imputation ----
# Initialise the final columns from the original columns
resultats_codachats$Poids_vf <- resultats_codachats$Poids
resultats_codachats$Prix_vf  <- resultats_codachats$Prix_all

# Corrected function: uses the current state of Poids_vf / Prix_vf to avoid overwriting previous imputations
calculate_vf <- function(data, prix_calc_col, poids_calc_col, nombre_col) {
  # Start from already-imputed values
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

# Apply in three passes, without overwriting previous imputations
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


# Imputation for donations
# The only modification applied concerns the price. If NA, impute a value of zero.
resultats_codachats$Prix_vf <- ifelse((resultats_codachats$Lieu1 =="dons" & is.na(resultats_codachats$Prix_vf)), (0), (resultats_codachats$Prix_vf))
print(unique(resultats_codachats$Identifiant))
describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

### Imputation of weights for out-of-home (RHD) foods ----
#### RHD_COL 
resultats_codachats<-  resultats_codachats %>%
  left_join(RHD_COL, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COL" & resultats_codachats$Unite =="unités"), (resultats_codachats$Poids_RHD_COL), (resultats_codachats$Poids_vf))
resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COL" ), (resultats_codachats$Poids_RHD_COL), (resultats_codachats$Poids_vf))

#RHD_COM
resultats_codachats<-  resultats_codachats %>%
  left_join(RHD_COM, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COM" & resultats_codachats$Unite =="unités"), (resultats_codachats$Poids_RHD_COM), (resultats_codachats$Poids_vf))
resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COM" ), (resultats_codachats$Poids_RHD_COM), (resultats_codachats$Poids_vf))

print(unique(resultats_codachats$Identifiant))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

## Verification of price per kg after imputation ----
# Compute the price per kg after imputation
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # if price is zero -> 0
      Poids_vf == 0          ~ NA_real_,    # if weight is zero -> NA
      TRUE                    ~ Prix_vf / Poids_vf
    )
  )

describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

#Nov 22 : Poids vf : 1,1% Prix vf : 1,2%
#Mars 23 : Poids :1%, Prix vf: 1,6%
#Nov 23 : Poids vf :0;9%, Prix vf: 13%
#Mars 24 : Poids vf : 0,5 et  prix vf : 10%
resultats_codachats_CAFE_THE <- resultats_codachats %>%
  filter(groupe_TI_TdC1 == "CAFE_THE")

lignes_vide <- resultats_codachats[is.na(resultats_codachats$Prix_Kg_post_imput), ]
# Remove spices and condiments
resultats_codachats <- subset(
  resultats_codachats,
  !groupe_TI_TdC1 %in% c("EPICES_CONDIMENTS")
)


## Remove identifiers whose recorded expenditure is less than 25% of their declared food budget ----
# Compute the sum of Prix_vf per identifier
somme_prix_vf <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(somme_prix_vf = sum(Prix_vf, na.rm = TRUE),)

# Compute one quarter of the food budget
valeurs_uniques_budget <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(budget_unique = unique(budget_alim))

Comparaison <- inner_join(somme_prix_vf , valeurs_uniques_budget, by="Identifiant")
Comparaison$quart_budget <- as.numeric(Comparaison$budget_unique)/4
Comparaison$dix_budget <- as.numeric(Comparaison$budget_unique)*10

print(unique(resultats_codachats$Identifiant))
# Display an alert message for identifiers where the sum of prix_vf is below the declared food budget
Comparaison <- Comparaison %>%
  mutate(
    message_alerte = ifelse( quart_budget > somme_prix_vf,
                             paste("Alerte: La somme des prix_vf (", somme_prix_vf, ") est inférieure au quart du budget alimentaire (", budget_unique, ")"),
                             "Aucune alerte"))
identifiants_alerte <- Comparaison %>%
  filter(somme_prix_vf < quart_budget )%>%
  pull(Identifiant)

# Remove identifiers flagged with an alert from resultats_codachats
resultats_codachats<- resultats_codachats%>%
  filter(!Identifiant %in% identifiants_alerte )


# 8. IMPUTATION OF NUTRITIONAL AND ENVIRONMENTAL DATA ----
## Aggregate variables of interest by groupe_TI_TdC from the CALNUT dataframe ----
resultats_codachats$Unite <- ifelse(is.na(resultats_codachats$Unite),(resultats_codachats$Unite=="unités"), (resultats_codachats$Unite))
# Foods recorded only by TI category have no imputed values for nutrients, environmental indicators,
# consumption percentages, or yield factors.
# These values are therefore imputed as category-level means from the CALNUT table.
# filter(!is.na(Poids_vf) & Poids_vf != 0) keeps rows where Poids_vf is not NA and not zero.
# Data are then grouped by groupe_TI_TdC1 and a weighted mean is computed.
colonnes_a_transformer<- c("yield_factor","pct_conso", "retinol_mcg", "nrj_kcal", "proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
                           "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
                           "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                           "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                           "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "DQR", "EF", "climat", "couche_ozone", "ions",
                           "ozone", "partic", "acid", "eutro_terr", "eutro_eau", 
                           "eutro_mer", "sol", "toxi_eau", "ress_eau", "ress_ener", "ress_min")

# The idea is to assign average nutrient values based on the most representative foods per TI category
## Join the weighted means table to resultats_codachats ----
resultats_codachats <-left_join(resultats_codachats,resultats_pondérés,by="groupe_TI_TdC1")
# Replace missing values with the corresponding category mean
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer), 
                ~ ifelse(is.na(.x), get(paste0("mean_", cur_column())), .x)))

## Remove mean_ columns after replacement if necessary ----
resultats_codachats <- resultats_codachats[, setdiff(names(resultats_codachats), grep("^mean_", names(resultats_codachats), value = TRUE))]



# 9. INDICATOR CALCULATION ----
## Weight (kg/person/day) ----
# To link COD-Appro quantities with FFQ values, the weight of supplies must always be multiplied
# by yield_factor * pct_conso to obtain the consumed weight for the supply booklets.
resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD"),(resultats_codachats$Poids_vf*resultats_codachats$yield_factor*resultats_codachats$pct_conso),(resultats_codachats$Poids_vf))
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf


# Coffee/tea conversion
#https://www.femobook.com/blogs/coffee-knowledge/a-guide-to-the-golden-cup-standard?
# 55 g of coffee per 1 L of water
# ISO 3103 standard: 2 g of tea per 1 L of water
#resultats_codachats$nrj_kcal <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_vf))


resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$Poids_vf*resultats_codachats$Sec_Vol),(resultats_codachats$Poids_consomme_vf))
resultats_codachats_CAFE_THE <- resultats_codachats %>%
  filter(groupe_TI_TdC1 == "CAFE_THE")


resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # if price is zero -> 0
      Poids_consomme_vf == 0          ~ NA_real_,    # if weight is zero -> NA
      TRUE                    ~ Prix_vf / Poids_consomme_vf
    )
  )


## Verification of price per kg after imputation ----
# Compute price per kg

Nourriture_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$Poids_consomme_vf  )
names(Nourriture_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","Poids_vf_consommee")
#print(unique(Nourriture_consommee$groupe_TI_TdC))
# List of food categories
categories <- unique(Nourriture_consommee$groupe_TI_TdC)
# Loop to create the corresponding columns in Nourriture_consommee
for (categorie in categories) {
  Nourriture_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Nourriture_consommee$groupe_TI_TdC == categorie, Nourriture_consommee$Poids_vf_consommee, 0)}
# Remove the "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Nourriture_consommee <- subset(Nourriture_consommee, select = -c(groupe_TI_TdC, Poids_vf_consommee, NA_CARNET))


# Aggregate into the Carnet_POIDS dataframe
Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
resultats_codachats$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(resultats_codachats$Combien.de.personnes.vivent.dans.votre.foyer)
# List of column names to aggregate
colonnes <- names(Nourriture_consommee)[-1] # Exclude the "Identifiant" column
# Loop to aggregate data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Nourriture_consommee, FUN = sum)
  Carnet_POIDS <- left_join(Carnet_POIDS, Temp, by = "Identifiant")
}

Carnet_POIDS$AUTRE_CARNET <- NULL

# Divide columns by UC multiplied by the number of recording days
Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$UC_TI*Nj)
#Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$Combien.de.personnes.vivent.dans.votre.foyer*Nj)
# Compute row sums across all columns
Carnet_POIDS$POIDS_TOTAL_CARNET <- rowSums(Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE)
# Compute row sums excluding beverages
Carnet_POIDS$POIDS_HORS_BOISSON_CARNET <- with(Carnet_POIDS, POIDS_TOTAL_CARNET - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET -
                                                 CAFE_THE_CARNET)


# Add _Poids suffix to all _CARNET columns
Carnet_POIDS <- Carnet_POIDS %>%
  rename_with(
    ~ ifelse(
      grepl("_CARNET$", .x),
      paste0(.x, "_Poids"),
      .x
    ),
    .cols = -any_of(c("Identifiant", "UC_TI"))
  )

## Kilocalories (Kcal/person/day) ----
# For each food, impute its nutritional value in kcal/kg based on the consumed weight.
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf

# Adjust kcal for coffee/tea by applying the dry/volume conversion factor
resultats_codachats$kcal_aliment_vf <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$groupe_TI_TdC1 == "CAFE_THE" ),(resultats_codachats$kcal_aliment_vf/ resultats_codachats$Sec_Vol),(resultats_codachats$kcal_aliment_vf))

# For each food consumed, impute its nutritional value in kcal/kg based on the consumed weight.
Kcal_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$kcal_aliment_vf  )
names(Kcal_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","kcal_aliment_vf")
# List of food categories
categories <- unique(Kcal_consommee$groupe_TI_TdC)
# Loop to create the corresponding columns in Kcal_consommee
for (categorie in categories) {
  Kcal_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Kcal_consommee$groupe_TI_TdC == categorie,
                                                           Kcal_consommee$kcal_aliment_vf, 0)}
# Remove the "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Kcal_consommee <- subset(Kcal_consommee, select = -c(groupe_TI_TdC, kcal_aliment_vf, NA_CARNET))

# Aggregate into the Carnet_KCAL dataframe

#Carnet_KCAL <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
Carnet_KCAL <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate
colonnes <- names(Kcal_consommee)[-1] # Exclude the "Identifiant" column
# Loop to aggregate data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Kcal_consommee, FUN = sum)
  Carnet_KCAL <- left_join(Carnet_KCAL, Temp, by = "Identifiant")}
# Divide columns by UC multiplied by Nj
#Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$Combien.de.personnes.vivent.dans.votre.foyer * Nj)
Carnet_KCAL$AUTRE_CARNET <- NULL
Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$UC_TI* Nj)
# Compute row sums across all columns
Carnet_KCAL$KCAL_TOTAL_CARNET <- rowSums(Carnet_KCAL[, 3:ncol(Carnet_KCAL)], na.rm = TRUE)
# Compute row sums excluding beverages
Carnet_KCAL$KCAL_HORS_BOISSON_CARNET <- with(Carnet_KCAL, KCAL_TOTAL_CARNET - 
                                               ALCOOL_CARNET - 
                                               FRUITS_JUS_CARNET - 
                                               LAIT_CARNET - 
                                               EAU_CARNET - 
                                               SODAS_LIGHT_CARNET - 
                                               SODAS_SUCRES_CARNET - 
                                               CAFE_THE_CARNET)


# Add _KCAL suffix to all _CARNET columns
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

# 10. BUILDING FINAL OUTPUT TABLES ----
Carnet_id <- metadata 
# Remove columns that are entirely NA or empty
Carnet_id <- Carnet_id %>%
  select_if(~ !all(is.na(.)) & !all(. == ""))

# Assign a period indicator based on the campaign (0 = pre, 1 = post)
if (campaign == "22-11" |campaign == "23-11") { Carnet_id$Periode <-0   }
if (campaign == "23-02" |campaign == "24-03") { Carnet_id$Periode <-1   }
Carnet_id$Mesure <- "Carnet"

#Carnet_POIDS <- Carnet_POIDS[, !grepl("^Combien.de.personnes.vivent.dans.votre.foyer", names(Carnet_POIDS))]
Carnet_POIDS <- Carnet_POIDS[, !grepl("UC_TI", names(Carnet_POIDS))]
Carnet_id <- left_join(Carnet_id, Carnet_POIDS, by='Identifiant')

Carnet_id <- left_join(Carnet_id, Carnet_KCAL, by='Identifiant')

# Build the cleaned raw data file
fichier_nettoyé <- 
  data.frame(
    Identifiant = resultats_codachats$Identifiant,
    Date = resultats_codachats$Date_vf,
    Lieu = resultats_codachats$Lieu_vf,
    Lieu1 = resultats_codachats$Lieu1,
    Lieu2 = resultats_codachats$Lieu2,
    LibelleCIQUAL = resultats_codachats$LibelleCIQUAL,
    groupe_TI_TdC = resultats_codachats$groupe_TI_TdC1,
    Poids_achat = resultats_codachats$Poids_vf,
    Poids_consomme = resultats_codachats$Poids_consomme_vf,
    Prix = resultats_codachats$Prix_vf, 
    Prix_Kg = resultats_codachats$Prix_Kg_post_imput,
    Montant_cheque = resultats_codachats$MontantChequeAlimentaire,
    Labels = resultats_codachats$Labels,
    Appeciation = resultats_codachats$Appreciation
  )

# 11. EXPORT ----

# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", Carnet_id  )

addWorksheet(wb, "Données_brutes_nettoyées")
writeData(wb, sheet = "Données_brutes_nettoyées", fichier_nettoyé )

# Save the workbook to the appropriate file path based on the campaign
if (campaign == "22-11") {
  saveWorkbook(wb,(paste0("Carnets_Tableaux_nov_22.xlsx")))
}else{ 
  if (campaign == "23-02") {
    saveWorkbook(wb,(paste0("Carnets_Tableaux_mars_23.xlsx"))) 
  } else {
    if (campaign == "23-11") {
      saveWorkbook(wb,(paste0("Carnets_Tableaux_nov_23.xlsx"))) 
    } else { 
      saveWorkbook(wb,(paste0("Carnets_Tableaux_mars_24.xlsx"))) 
    }}}
