#VOUCHERS SCRIPT
#Importing packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")

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


#Merging and Harmonizing the FFQs---------------------------------------------------
#Merging the November tables
FFQ_NOV <- rbind(FFQ_nov_22, FFQ_nov_23)

##Imputing the diploma to the March table ------------------------------
temp  <- FFQ_NOV[, c("Identifiant", "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.")]
FFQ_mars_24  <- left_join(FFQ_mars_24, temp, by="Identifiant")

names(FFQ_mars_24)[ncol(FFQ_mars_24)] <- "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu"
FFQ_mars_24 <- FFQ_mars_24[, -c(which(names(FFQ_mars_24) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu..x"))] #empty duplicate column and .n as well
FFQ_mars_23 <- FFQ_mars_23[, -c(which(names(FFQ_mars_23) == "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu."))] #empty duplicate column and .n as well

##Merging the March tables ----------------------------------
FFQ_MARS <- rbind(FFQ_mars_23, FFQ_mars_24)


Carnet_NOV <- bind_rows(Carnet_nov_22, Carnet_nov_23)
Carnet_MARS <- bind_rows(Carnet_mars_23, Carnet_mars_24)


##Removes from MARS all the rows that do not appear in NOV
###remove the March booklet ids that are not found in the November booklets------------
Carnet_MARS <- Carnet_MARS %>%
  semi_join(Carnet_NOV, by = "Identifiant")

###remove the November booklet ids that are not found in the March booklets-----------------
Carnet_NOV <- Carnet_NOV %>%
  semi_join(Carnet_MARS, by = "Identifiant")

FFQ_NOV$KCAL_TOTAL_FFQ_Kcal
# Filters to exclude outlier FFQs----------------------------
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

#remove the March FFQ ids that are not found in the November FFQs------------
initial_ids <- FFQ_MARS$Identifiant

FFQ_MARS <- FFQ_MARS %>%
  semi_join(FFQ_NOV, by = "Identifiant")
removed_ids <- setdiff(initial_ids, FFQ_MARS$Identifiant)

###remove the November booklet ids that are not found in the March booklets-----------------
FFQ_NOV <- FFQ_NOV %>%
  semi_join(FFQ_MARS, by = "Identifiant")

print(unique(Carnet_NOV$Identifiant))

FFQ_NOV$borne_inf  <- NULL
FFQ_MARS$borne_inf <- NULL
FFQ_NOV$borne_sup  <- NULL
FFQ_MARS$borne_sup <- NULL


sgsdata_Carnets <- bind_rows(Carnet_NOV, Carnet_MARS)


# 1. Identifying the shared columns
cols_communes <- intersect(names(FFQ_NOV), names(FFQ_MARS))

# 2. Identifying those whose class differs
diff_cols <- cols_communes[sapply(cols_communes, function(col) {
  !identical(class(FFQ_NOV[[col]]), class(FFQ_MARS[[col]]))
})]

# 3. Forcing only these columns to numeric (watch out for non-numeric values → NA)
FFQ_NOV <- FFQ_NOV %>%
  mutate(across(all_of(diff_cols), ~ as.character(.)))

FFQ_MARS <- FFQ_MARS %>%
  mutate(across(all_of(diff_cols), ~ as.character(.)))

# 4. We can now bind without error
sgsdata_FFQ <- bind_rows(FFQ_NOV, FFQ_MARS)


sgsdata <- bind_rows(sgsdata_Carnets, sgsdata_FFQ)
#Downloading the final tables -----------------------------------------
# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("sgsdata_IT.xlsx")))



###HARMONIZING CSGA ------------------------------------------------------
#Importing packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);
library(readxl);library(dplyr);library(broom);library(scales);library(modelsummary)
library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");library("dplyr");library("tidyr");library("ggplot2");
library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library("poLCA");library("webshot")



Carnets <- read.xlsx((paste("Carnets_CSGA.xlsx", sep="")))
FFQ  <- read.xlsx((paste("FFQ_CSGA.xlsx", sep="")))

Carnets <- Carnets %>%
  semi_join(FFQ, by = "Identifiant") %>%  # keep those that are also in FFQ
  arrange(Identifiant)

FFQ <- FFQ %>%
  semi_join(Carnets, by = "Identifiant") %>%  # keep those that are also in Carnets
  arrange(Identifiant)



initial_ids <- FFQ$Identifiant
FFQ<- FFQ %>%
  mutate(borne_inf = 500,
         borne_sup =  4500)
FFQ <- FFQ %>%
  filter(KCAL_TOTAL_FFQ_Kcal >= borne_inf & KCAL_TOTAL_FFQ_Kcal <= borne_sup)
removed_ids <- setdiff(initial_ids, FFQ$Identifiant)


sgsdata <- bind_rows(Carnets, FFQ)


#Downloading the final tables -----------------------------------------
# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "sgsdata")
writeData(wb, sheet = "sgsdata", sgsdata  )

saveWorkbook(wb,(paste0("sgsdata.xlsx")))