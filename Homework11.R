
########################### Fait par Papa Amadou NIANG ################################
########### Application de la méthode de traitement des valeurs #######################
################### aberrantes et manquantes sur R ####################################

# Imporation de la base Table de coversion et cereales

# First step traitement de la base céréales

library(haven)
cereales <-read_dta("cereales.dta")
str(cereales)

## Renommer les variables 

colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")
## Gestion des NA

## Suppression des ménages qui ne consomment pas de céréales

attach(cereales)
anyNA(Qtty_cons)

## Création de variables temporaires

cereales$t<-ifelse(is.na(Qtty_cons)==1,1,0) # Ménages qui ne consomment pas de céréales
table(cereales$t) 

cereales_na<-cereales[cereales$t==1,]

cereales<-cereales[cereales$t==0,]

# Supression des variables temporaires

cereales$t<- NULL


#Second step

library(readxl)
Table_de_conversion<- read_excel(
  "D:/Traitement Statistique avec R/Table de conversion phase 2.xlsx")

Table_de_conversion$...8 <- NULL
Table_de_conversion$...9 <- NULL

colnames(Table_de_conversion) [1:6] <- c("cereales__id","Nom_Prod",
                                         "Unite_cons","Nom_Unite",
                                         "Taille_cons","Nom_Taille")
cereales <- merge(cereales, Table_de_conversion, 
                  by = c("cereales__id", "Unite_cons", "Taille_cons"), all.x = TRUE)


# Traitement de la base de données

library(data.table)
cereales_fin <- data.table(cereales)
setnames(cereales_fin,"poids","poids_cons")

cereales_fin[,poids_cons:=as.numeric(poids_cons)] # Conversion de la variable poids_cons
cereales_fin[,qtty_cons_kg:= poids_cons*Qtty_cons/1000] ## Quantité consommée en kg
cereales_fin[,summary(qtty_cons_kg)] # Quelques paramètres de tendances centrales

#' calculer la quantite achete en kg; 

cereales_fin <- cereales_fin [, Unite_achat := as.double(Unite_achat)]
cereales_fin <- cereales_fin [, Taille_achat := as.double(Taille_achat)]
cereales_fin <- cereales_fin [, cereales__id := as.double(cereales__id)]

colnames(Table_de_conversion) [1:6] <- c("cereales__id","Nom_Prod",
                                         "Unite_achat","Nom_Unite",
                                         "Taille_achat","Nom_Taille")
cereales_fina <- merge(cereales_fin, Table_de_conversion, 
                       by = c("cereales__id", "Unite_achat", "Taille_achat"), all.x = TRUE)

cereales_fina <- data.table(cereales_fina)
setnames(cereales_fina, "poids", "poids_achat")
cereales_fina <- cereales_fina[,poids_achat:=as.numeric(poids_achat)]

summary(cereales_fina$poids_achat)

summary(cereales_fina[!is.na(cereales_fina$poids_achat), "poids_achat"])

table(cereales_fina$tailleNom.y)

cereales_fina [, qtty_achat_kg := Qtty_achat*poids_achat/1000]
boxplot(cereales_fina$qtty_achat_kg)

#' calculer le prix unitaire ;
## Un prix unitaire poiur chaque combinaison (produit,unite, taille)

cereales_fina$pu <- cereales_fina$Value_achat/cereales_fina$Qtty_achat
cereales_fina[Unite_achat==100, summary(pu)]
cereales_fina[cereales__id<5 & Unite_achat==100, summary(pu)]
cereales_fina[cereales__id<5 & Unite_achat==100 & pu <2000, summary(pu)]

### Extraire les Prix 


prixunitaire <- subset(cereales_fina, !is.na(pu), 
                       select =c("cereales__id", "Unite_achat", "Taille_achat", "pu") )

## Traitement des pu aberrants ; 

idc <- unique(cereales_fina$cereales__id)

library(dplyr)
cereales_fina <- cereales_fina %>%
  group_by(cereales__id) %>%
  mutate(pu = ifelse(!is.na(pu) & pu > quantile(pu, 0.75, na.rm = TRUE), quantile(pu, 0.75, na.rm = TRUE), pu))

# Calculer la moyenne et la médiane de 'pu' pour chaque combinaison (p,u,t)
library(dplyr)

resultats <- prixunitaire %>%
  group_by(pu, Unite_achat, Taille_achat) %>%
  summarise(
    mediane_pu = median(pu, na.rm = TRUE)
  )

library(dplyr)

# Calculer le prix 'p' pour chaque combinaison (p,u,t)
prixunitaire2 <- prixunitaire %>%
  group_by(pu, Unite_achat, Taille_achat) %>%
  summarise(
    p = mean(pu, na.rm = TRUE)
  )



#' Ramener cette sous-base dans la base cereales4 pour calculer 
#' les depenses de consommations ; 

library(dplyr)

# Joindre la sous-base 'prixunitaire' à la base 'cereales4'
cereales_fina <- cereales_fina %>%
  left_join(prixunitaire2, by = c("pu", "Unite_achat", "Taille_achat"))

# Calculer les dépenses de consommation
cereales_fina <- cereales_fina %>%
  mutate(depenses = p * Qtty_achat)  

#' 1:: evaluer le taux de matching : n(Pc,Uc,Tc) aynt un prix P sur le
#' le nombre total de combinaison n(Pc,Uc,Tc); 

library(dplyr)

# Calculer le nombre total de combinaisons (Pc,Uc,Tc)
total_combinaisons <- nrow(cereales_fina)

# Calculer le nombre de combinaisons (Pc,Uc,Tc) ayant un prix P
combinaisons_avec_prix <- cereales_fina %>%
  filter(!is.na(p)) %>%
  nrow()

# Calculer le taux de correspondance
taux_correspondance <- combinaisons_avec_prix / total_combinaisons

# Afficher le taux de correspondance
print(taux_correspondance)

#' 2:: Reflechir a comment valoriser ces quantites n'ayant de prix  

# Imputer les valeurs manquantes par la médiane
cereales_fina$pu[is.na(cereales_fina$pu)] <- median(cereales_fina$pu, na.rm = TRUE)

#' Valeurs aberrantes :: corrections ; 
Q1 <- quantile(cereales_fina$pu, 0.25, na.rm = TRUE)
Q3 <- quantile(cereales_fina$pu, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Remplacer les valeurs aberrantes par la médiane
cereales_fina$pu[cereales_fina$pu < (Q1 - 1.5 * IQR) | cereales_fina$pu > (Q3 + 1.5 * IQR)] <- median(cereales_fina$pu, na.rm = TRUE)
