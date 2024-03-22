

library(haven)
cereales <- read_dta("C:/Users/lenovo/Desktop/COURS_Traitements_Stat_R/BASE_EHCVM/cereales.dta")
View(cereales)
## Renaming the variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")
View(cereales)

library(readxl)
Table_de_conversion_phase_2 <- read_excel("C:/Users/lenovo/Desktop/COURS_Traitements_Stat_R/BASE_EHCVM/Table de conversion phase 2.xlsx")
View(Table_de_conversion_phase_2)

data_merge1 <-left_join(cereales, Table_de_conversion_phase_2, by=c("cereales__id"="produitID","Unite_cons"="uniteID","Taille_cons"="tailleID"))
View(data_merge1)


#Calculer la quantité achetée en kg
#On numérise poids

data_merge1$poids <- as.numeric(data_merge1$poids)


data_merge1$qtty_achat_kg <- with(data_merge1, (data_merge1$poids)*(data_merge1$Qtty_achat) / 1000, na.rm = TRUE)

#Ou bien
library("data.table")
data_merge1<-data.table(data_merge1)
data_merge1<-data_merge1[, qtty_achat_kg := data_merge1$poids * data_merge1$Qtty_achat / 1000]

#Calcul de la quantité totale
View(data_merge1)
cat("Totale quantité consommée pour tout produit en kg =",
    sum(data_merge1$Qtty_achat[data_merge1$uniteNom=="Kg"], na.rm =TRUE))

library(tidyverse)
glimpse(data_merge1)

#Calculer le prix unitaire (par kg)
library("data.table")
data_merge1<-data.table(data_merge1)
data_merge1<-data_merge1[, prix_achat_unité := data_merge1$Value_achat / data_merge1$qtty_achat_kg]
#Calculer les dépenses de consommation
library("data.table")
data_merge1<-data.table(data_merge1)
data_merge1<-data_merge1[, dépense_cons := data_merge1$Qtty_cons * data_merge1$prix_achat_unité]

#Valeurs abrrerantes : corrections
############################################################
#Utilisation de la méthode de la gamme interquartile
#Selon cette méthode, une valeur V est dite abérante si elle elle vérifie:

# V< Q1-1,5*(Q3-Q1) ou V > Q3 + 1,5*(Q3-Q1)
#Au niveau de la variable 'Qtty_cons'

hist(data_merge1$Qtty_cons,main = "Histogramme avant supression des valeurs abérantes")

data_merge1$Qtty_cons <- ifelse(is.na(data_merge1$Qtty_cons), 0, data_merge1$Qtty_cons)
data_merge1<- as.data.frame(data_merge1)


data_merge1$Qtty_cons < quantile(data_merge1$Qtty_cons, 0.25) - 1.5*IQR(data_merge1$Qtty_cons)
data_merge1$Qtty_cons > quantile(data_merge1$Qtty_cons, 0.25) + 1.5*IQR(data_merge1$Qtty_cons)

anormal_Qtty_cons <- which(data_merge1$Qtty_cons < quantile(data_merge1$Qtty_cons, 0.25) - 1.5*IQR(data_merge1$Qtty_cons)
                           |data_merge1$Qtty_cons > quantile(data_merge1$Qtty_cons, 0.75) + 1.5*IQR(data_merge1$Qtty_cons))
#Suppression des valeurs aberantes

data_merge1<- data_merge1[-anormal_Qtty_cons, ]


hist(data_merge1$Qtty_cons,main = "Histogramme après supression des valeurs abérantes")

#$ STOP $# 