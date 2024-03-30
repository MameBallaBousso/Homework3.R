
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

#Calculer la quantité achetée en kg
#On merge suivant les poids en fonction des quantités achetées

cereales1 <-merge(cereales, Table_de_conversion_phase_2, by.x=c("cereales__id","Unite_achat","Taille_achat"),
                  by.y=c("produitID","uniteID","tailleID"), all.x = TRUE)
View(cereales1)

#On numérise poids
cereales1$poids <- as.numeric(cereales1$poids)
is.numeric(cereales1$poids)

library("data.table")
cereales1 <- data.table(cereales1)
setnames(cereales1, "poids", "poids_achat")

summary(cereales1$poids_achat)

cereales1$qtty_achat_kg <- with(cereales1, (cereales1$poids)*(cereales1$Qtty_achat) / 1000, na.rm = TRUE)
summary(cereales1[!is.na(cereales1$poids_achat), "poids_achat"])
boxplot(cereales1$qtty_achat_kg)

cereales1$pu <- cereales1$Value_achat/cereales1$Qtty_achat
cereales1[Unite_achat==100, summary(pu)]
cereales1[cereales__id<5 & Unite_achat==100, summary(pu)]
cereales1[cereales__id<5 & Unite_achat==100 & pu <2000, summary(pu)]

prixunitaire <- subset(cereales1, !is.na(pu), 
                       select =c("cereales__id", "Unite_achat", "Taille_achat", "pu") )

View(prixunitaire)


## Traitement des pu aberrants ; 

idc <- unique(cereales1$cereales__id)

library(dplyr)
cereales1 <- cereales1 %>%
  group_by(cereales__id) %>%
  mutate(pu = ifelse(!is.na(pu) & pu > quantile(pu, 0.75, na.rm = TRUE), quantile(pu, 0.75, na.rm = TRUE), pu))

summary(cereales1$pu)

# Calculer la moyenne et la médiane de 'pu' pour chaque combinaison (p,u,t)
library(dplyr)
result <- prixunitaire %>%
  group_by(pu, Unite_achat, Taille_achat) %>%
  summarise(
    mediane_pu = median(pu, na.rm = TRUE)
  )
View(result)

# Calculer le prix 'p' pour chaque combinaison (p,u,t)

prixunitaire2 <- prixunitaire %>%
  group_by(pu, Unite_achat, Taille_achat) %>%
  summarise(
    p = mean(pu, na.rm = TRUE)
  )
View(prixunitaire2)


#' Ramener cette sous-base dans la base cereales1 pour calculer 
#' les depenses de consommations ; 

library(dplyr)

# Joindre la sous-base 'prixunitaire' à la base 'cereales4 (cereale1 pour moi)'
cereales1 <- cereales1 %>%
  left_join(prixunitaire2, by = c("pu", "Unite_achat", "Taille_achat"))



# Calculer les dépenses de consommation
cereales1 <- cereales1 %>%
  mutate(dépense_cons = p * Qtty_achat)  

#' 1:: evaluer le taux de matching : n(Pc,Uc,Tc) aynt un prix P sur le
#' le nombre total de combinaison n(Pc,Uc,Tc); 

library(dplyr)

# Calculer le nombre total de combinaisons (Pc,Uc,Tc)
total_combinaisons <- nrow(cereales1)
total_combinaisons

# Calculer le nombre de combinaisons (Pc,Uc,Tc) ayant un prix P
combinaisons_avec_p <- cereales1 %>%
  filter(!is.na(p)) %>%
  nrow()
combinaisons_avec_p

# Calculer le taux de correspondance
taux_correspondance <- combinaisons_avec_p / total_combinaisons
print(taux_correspondance)

#' 2:: Reflechir a comment valoriser ces quantites n'ayant pas de prix  

# Imputer les valeurs manquantes par la médiane

cereales1$pu[is.na(cereales1$pu)] <- median(cereales1$pu, na.rm = TRUE)

#' Valeurs aberrantes :: corrections ; 
Q1 <- quantile(cereales1$pu, 0.25, na.rm = TRUE)
Q3 <- quantile(cereales1$pu, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Remplacer les valeurs aberrantes par la médiane
cereales1$pu[cereales1$pu < (Q1 - 1.5 * IQR) | cereales1$pu > (Q3 + 1.5 * IQR)] <- median(cereales1$pu, na.rm = TRUE)

