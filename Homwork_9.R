
## Suite MD
source("cereales_base")

# Renomer, créer, labeliser les variables,  Recoder ;
# changer de type ;

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

library(tidyverse)
glimpse(cereales)

cereales$unite_cons <- factor(cereales$Unite_cons,
                              levels = unname(attr(cereales$Unite_cons,
                                                   "labels")),
                              labels =names(attr(cereales$Unite_cons,
                                                 "labels")))

cereales$taille_cons <- factor(cereales$Taille_cons,
                               levels = unname(attr(cereales$Taille_cons,
                                                    "labels")),
                               labels =names(attr(cereales$Taille_cons,
                                                  "labels")))



#Importer la base céréales

library(readxl)
Table_de_conversion_phase_2 <- read_excel("C:/Users/lenovo/Desktop/COURS_Traitements_Stat_R/BASE_EHCVM/Table de conversion phase 2.xlsx")
View(Table_de_conversion_phase_2)

#Calculer la quantité achetée en kg
#On merge suivant les poids en fonction des quantités achetées

cereales1 <-merge(cereales, Table_de_conversion_phase_2, by.x=c("cereales__id","Unite_achat","Taille_achat"), by.y=c("produitID","uniteID","tailleID"), all.x = TRUE)
View(cereales1)


#On numérise poids
cereales1$poids <- as.numeric(cereales1$poids)
is.numeric(cereales1$poids)

cereales1$qtty_achat_kg <- with(cereales1, (cereales1$poids)*(cereales1$Qtty_achat) / 1000, na.rm = TRUE)


#Calcul de la quantité totale
View(cereales1)
cat("Totale quantité achetée pour tout produit est de =",
    sum(cereales1$Qtty_achat[cereales1$uniteNom=="Kg"], na.rm =TRUE),"Kg")

library(tidyverse)
glimpse(cereales1)






#Calculer le prix unitaire (par kg)
library("data.table")
cereales1<-data.table(cereales1)
cereales1<-cereales1[, prix_achat_unité := cereales1$Value_achat / cereales1$qtty_achat_kg]


#Calculer les dépenses de consommation
#Il faut ici faire un autre merge qui permet de voir réellement les quantités conommées

cereales2 <-merge(cereales, Table_de_conversion_phase_2, by.x=c("cereales__id","Unite_cons","Taille_cons"), by.y=c("produitID","uniteID","tailleID"), all.x = TRUE)
View(cereales2)

cereales2$poids <- as.numeric(cereales2$poids)
is.numeric(cereales2$poids)

#Ou bien
library("data.table")
cereales2<-data.table(cereales2)
cereales2<-cereales2[, qtty_cons_kg := cereales2$poids * cereales2$Qtty_cons / 1000]



library("data.table")
cereales1<-data.table(dcereales1)
cereales1<-cereales1[, dépense_cons := cereales1$Qtty_cons * cereales1$prix_achat_unité]

install.packages("dplyr")
library("dplyr")



glimpse(cereales1)

library("haven")
cereales1$cereales__id <- as.factor(cereales1$cereales__id)
cereales1$cereales__id <- as.double(cereales1$cereales__id)
cereales1$Unite_cons <-as.factor(cereales1$Unite_cons)
cereales1$Taille_cons<-as.factor(cereales1$Taille_cons)

cereales2$cereales__id <- as.factor(cereales2$cereales__id)
cereales2$cereales__id <- as.double(cereales2$cereales__id)
cereales2$Unite_cons <-as.factor(cereales2$Unite_cons)
cereales2$Taille_cons<-as.factor(cereales2$Taille_cons)

Base_extraite <- cereales1 %>% select(cereales__id, Unite_cons, Taille_cons, prix_achat_unité)

View(Base_extraite)
Base_dépense_cons <- merge(cereales2, Base_extraite, by.x=c("cereales__id","Unite_cons","Taille_cons"), by.y=c("cereales__id","Unite_cons","Taille_cons"), all.x =TRUE)
View(Base_dépense_cons)
#Maintenant dans la base 'Base_dépense_cons', on a les colonnes prix_unité et Qtty_cons dont le produit donne
#les dépenses de consommation

library("data.table")
Base_dépense_cons<-data.table(Base_dépense_cons)
Base_dépense_cons<-Base_dépense_cons[, dépense_cons_kg := Base_dépense_cons$qtty_cons_kg * Base_dépense_cons$prix_achat_unité]
