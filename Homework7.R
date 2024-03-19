## Rev-QCM

## Suite MD
source("cereales_base")

# Renomer, créer, labeliser les variables,  Recoder ;
# changer de type ;
glimpse(cereales)
cereales$t<-NULL

labprod <- c("Riz local brisé"	,"Riz local entier"	,"Riz importé brisé"
             ,"Riz importé entier"	,"Riz importé 3"	,"Maïs en épi","Maïs en grain"	,"Mil"	,"Sorgho"	,"Blé"	,"Fonio"	,"Autres céréales"	,"Farine de maïs"	,"semoule de mais"	,"Farine/semoule de mil"	,"semoule de mil"	,"Farine de blé local ou importé"	,"semoule de blé "	,"Autres farines de céréales"	,"Autres semoules de céréales"	,"Pâtes alimentaires"	,"Pain moderne"	,"Pain moderne type 2"	,"Pain traditionnel"	,"Pains traditionnel type 2"	,"Céréales de petit déjeuner"	
             ,"Croissants"	,"Biscuits"	,"Gâteaux"	,"Beignets, galettes")

levprod <- unique(cereales$cereales__id)


edit(levprod)
levprodN <- names(attr(cereales$cereales__id,"labels"))
levprodL <- unname(attr(cereales$cereales__id,"labels"))

cereales$produit1 <- as.factor(cereales$cereales__id)

library(tidyverse)
glimpse(cereales)

table(cereales$produit1)

cereales$produit <- factor(cereales$produit1, 
                           levels = levprodL,
                           labels = levprodN )
table(cereales$produit)

glimpse(cereales)


library(haven)
edit(cereales$Unite_cons)

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

# 6 découpage en classe ;identifier une cereale et une unite standard;

cereales$classCereal <- cut(cereales$Qtty_cons, 
                            labels = c("Tres faible",
                                       "Faible",
                                       "Moyen",
                                       "Eleve"),
                            breaks = c(0,50,70,110,168))


table(cereales$classCereal)


cereales$classCereal_RizKg <- if_else(cereales$cereales__id==1 & cereales$Unite_cons==100,
                                     cut(cereales$Qtty_cons, labels = c("Tres faible","Faible", "Moyen" ,"Eleve"), 
                                         breaks = c(0,50,70,110,168)), NA)

table(cereales$classCereal_RizKg)

c0 <- unique(cereales[cereales$Unite_cons==100,"Taille_cons"])
c1 <- cereales[cereales$cereales__id<5 & cereales$unite_cons==100, ]
## essayer de merger la base cereale avec la table de conversion 

#******** On renomme d'abord les colonnes dans la deuxième bas (TC)

#colnames(Table_de_conversion_phase_2) <-c("ProduitD","produit","Unité_ID","unite_cons",
                                        #"TailleID","taille_cons","poids","X","Y")


data_merge1 <-left_join(cereales, Table_de_conversion_phase_2, by=c("cereales__id"="produitID","Unite_cons"="uniteID","Taille_cons"="tailleID"))
View(data_merge1)

#Avec les labels
data_merge<-left_join(cereales, Table_de_conversion_phase_2, by=c("produit"="produitNom","unite_cons"="uniteNom","taille_cons"="tailleNom"))
View(data_merge)

#Vérification des valeurs manquantes dans la colonne NA
sum(is.na(data_merge1$poids))



 

table(data_merge1$poids)

#$ STOP $# 