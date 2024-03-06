Nom<- c("BOUSSO","COULIBALY","DIAKHATE","DIAW","DIENG","FAYE","HILDEGARDE","SARAH-LAURE","NIANG","NIASS","KANE","NDONG","NGEMFUOU","DE LE FLECHE","Rayan", "SENE")
Prénom<-c("Mame Balla","Khadidiatou", "Khadidiatou","Awa","Samba","Ameth","Edima Biyenda","Fougwoug","Papa Ahmadou","Ahmadou","Alioune Abdou Salam","Tamsir","Célia","Jeanne","Ange","Malick")
Age<-c("23 ans","19 ans","19 ans","20 ans","19 ans","21 ans","20 ans","20 ans","19 ans","20 ans","19 ans","20 ans","20 ans","21 ans", "18 ans","19 ans")
Mention_ISEP<-c("Bien","Excellent","Tbien","Tbien","TBien","Tbien","Tbien","Tbien","Tbien","Tbien","Tbien","Tbien","Tbien","Tbien","Tbien","Tbien")
Taille<-c(185,163,195,180,175,180,175,160,185,163,195,180,175,180,175,184)
Poids<-c(62,58,59,58,59,75,62,57,65,58,59,80,58,75,57,63)

nombre_variables <- 15
nombre_observations <- 304

# Créer un data.frame 
ma_base_de_donnees <- data.frame(
  Nom<- rep(Nom,19),
  Prénom<- rep(Prénom,19),
  Age<- rep(Age,19),
  Mention<- rep(Mention_ISEP,19),
  Taille<- rep(Taille,19),
  Poids<- rep(Poids,19),
  Célibataire<- sample(c("Oui", "Non"), nombre_observations, replace = TRUE),  # Variable catégorielle
  Sexe<- sample(c("Masculin", "Féminim"), nombre_observations, replace = TRUE),  # Variable catégorielle
  Matricule1<- rpois(nombre_observations, lambda = 3),  # Variable numérique avec distribution de Poisson
  Chiffre_choisi<- factor(sample(letters[1:5], nombre_observations, replace = TRUE)),  # Variable factorielle
  Fiancé<- sample(c("Oui", "Non"), nombre_observations, replace = TRUE),  # Variable binaire
  Lettre_préféré<- sample(letters[10:15], nombre_observations, replace = TRUE),  # Variable catégorielle
  Matricule2<- rnorm(nombre_observations),
  Niveau_R<- sample(c("Très Bon", "Bon","Assez Bon","Médiocre"), nombre_observations, replace = TRUE),
  Logicel_préféré<- sample(c("Python", "Stata","SPSS","R","Excel"), nombre_observations, replace = TRUE)
)
View(ma_base_de_donnees)
#Renommer les lignes, colonnes
matrice_base_data<- as.matrix(ma_base_de_donnees)
rownames(matrice_base_data)<- c(rep("MBB",304))
colnames(matrice_base_data)<- c("Nom","Prénom","Age","Mention","Taille","Poids","Célibataire","Sexe","Matricule1","Chiffre_préféré","Fiancé","Lettre_préférée","Matricule2","Niveau_R","Logiciel_Préféré")
View(matrice_base_data)
#Stat desc
summary(ma_base_de_donnees)
#Histogramme de Nom en fonction de choix du poids
library(ggplot2)
ggplot(ma_base_de_donnees, aes(x = Nom, y = Poids, fill = Nom)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Histogramme du poids par Nom",
       x="Nom",
       y="Taille") +
  theme_minimal()
#Exporter la base

#format excel
writexl::write_xlsx(ma_base_de_donnees, "Base_data_allongée.xlsx") 
#Format stata 
install.packages("haven")
library(haven)
write_dta(ma_base_de_donnees, "Base_data_allongée.dta") 
#Format spss
write_sav(ma_base_de_donnees, "Base_data_allongée.sav") 
#Importation de la base de l'enquête du BSA
Enquête_BSA <- read_excel("C:/Users/lenovo/Desktop/Enquête_BSA.xlsx")
View(Enquête_BSA)



##Exercice N°2 :Test de kh-2
#On étudie l'indépendance linéaire entre le poids et la taille
#On pose les hypothèse :
#H0 les deux varibles sont indépendantes
#H1 les deux varibles ne sont pas indépendantes

##Croisement des variables poids et taille (avec un tableau CD)
install.packages("questionr")
library(questionr)

#Mettons les variables en catégories
poids_cat <- cut(Poids, breaks = c(55, 60, 70, 80), labels = c("Faible", "Moyen", "Fort"))
taille_cat <- cut(Taille, breaks = c(150, 170, 180, 200), labels = c("Petite", "Moyenne", "Grande"))

table(taille_cat,poids_cat)

Tab<-table(taille_cat,poids_cat)
lprop(Tab) 
cprop(Tab)
#Test de Khi-2
chisq.test(Tab)
chisq.residuals(Tab)
#Interprétation


#La p-value trouée étant proche de 0 ( inférieure à 2.2e-16), donc on retiens l'hypothése H1
#C-à-d les variables tailles et poids ne sont pas indépendantes
