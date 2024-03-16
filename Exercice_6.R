
# Importing dataframe------------------------------------------------------------------------------
cereales <-read_dta("cereales.dta")
str(cereales)

## Renaming the variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")


## Renommer, créer, labeliser, recoder les variables-------------------------------------

# On cherche les variables qui ont lieu d'être recodées : ce sont celles qui ont un système d'étiquettage
names(cereales)

to_rec <- list()
for (i in names(cereales)){
  if(is.null(attr(cereales[[i]], "labels"))== 0){ # On met deux crochet pour préciser que c'est la colonne que l'on veut
    to_rec <- c(to_rec, i)
  }
}

to_rec # To rec est donc la liste des variables à recoder


c_val <- names(attr(cereales[["Unite_achat"]], "labels"))
names(c_val) <- attr(cereales[["Unite_achat"]], "labels")
c_val

# Crétion des variables recodées
noms <- list()
for (i in to_rec ){
  
  
  # Inversion de label et d'étiquette : Liste où on a 100 = kg et non kg = 100__________
  c_val <- names(attr(cereales[[i]], "labels"))
  names(c_val) <- attr(cereales[[i]], "labels")
  
  
  nom <- paste0("Var_rec_", i) # nom_variable
  
  
  # Creation variable
  cereales <- mutate(cereales, 
                     A = unname(c_val[as.character(cereales[[i]])]))
  
  
  attr(cereales$A,"labels") <- c_val # codage...
  attr(cereales$A, "label") <- paste0(attr(cereales$Unite_achat, "label"),"_recoded") # label
  noms[[nom]] <- cereales$A # Ajout à la liste
}

# noms est une liste de vecteurs, nos nouvelles variables
noms

# On supprime A
cereales$A <- NULL

# Transformation en dataframe puis merging with cereales
noms <- as.data.frame(noms)
View(cer)
cereales <- cbind.data.frame(cereales, noms)




## Découpage en classe -----------------------------------------------------------

# On crée un histogramme avec la règle de Sturges pour trouver le nombre de classes
#(on aurait aussi pu calculer nous-mêmes...)

H <- hist(cereales$Value_achat, breaks="Sturges")
length(H$breaks) - 1 # 15 classes

# Découpons en classes pour le Kg, le riz importé brisé et les deux.

rice <- cereales$Unite_cons[cereales$cereales__id == 3]
Kgr <- cereales$Unite_cons[cereales$Unite_cons == 100]
rice_Kgr <- cereales$Unite_cons[cereales$cereales__id == 3 & cereales$Unite_cons == 100]

rice <- cut(rice, breaks = length(H$breaks) - 1)
Kgr <- cut(Kgr, breaks = length(H$breaks) - 1)
rice_Kgr <- cut(rice_Kgr, breaks = length(H$breaks) - 1)

freq(rice)
freq(Kgr)
freq(rice_Kgr)


# Repérer les valeurs manquantes

val_maq <- which(is.na(cereales), arr.ind = TRUE)
dim(val_maq)
val_maq

## Détecter les individus aberrants, we use slice_min et slice_max---------------------------------------

# Pour ceux qui ont acheté / consommé  du riz en kg...
#         Pour l'achat
View(slice_min(cereales[cereales$Unite_achat == 100 & cereales$cereales__id == 3,], Qtty_achat, n= 7)) #  7 valeurs aberrantes du haut
slice_min(cereales[cereales$Unite_achat == 100 & cereales$cereales__id == 3,], Qtty_achat , n= 7) #  7 valeurs aberrantes du bas

#         Pour la  consommation
slice_max(cereales[cereales$Unite_cons==100 & cereales$cereales__id == 3],cereales$Qtty_cons, n=7) 
slice_min(cereales[cereales$Unite_cons==100 & cereales$cereales__id == 3],cereales$Qtty_cons, n=7) 







