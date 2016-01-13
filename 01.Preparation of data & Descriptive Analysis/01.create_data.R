#######################################################################################
#                                                                                     #
#                       Lecture et preparation des donnees                            #
#                                                                                     #
#######################################################################################

# Lecture des donnees et transformation prealables des variables quantitatives. Retrait
# des variables non pertinentes pour l'analyse. Pour les deux methodes, on conservera
# les memes variables au depart. Cette phase de traitement des donnees est une etape importante
# dans les processus d'analyses statistiques.

# Importation des donnees
#setwd("~/projet_scoring/01.Preparation of data & Descriptive Analysis")
ozone <- read.table('http://dupuy.perso.math.cnrs.fr/enseignement/RLogistique/ozone.dat',h=T)

# Changement du type de la variable jour
ozone[,"JOUR"]=as.factor(ozone[,"JOUR"])

# Histogrammes des variables initiales (visualisation de la distribution):
par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"RMH2O"]);
hist(ozone[,"NO2"]);hist(ozone[,"NO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# Transformation des variables et trace des histogrammes apres transformation (on cherche
# a approcher une distribution normale: transformations racine carre/logarithme)
ozone[,"SRMH2O"]=sqrt(ozone[,"RMH2O"])
ozone[,"LNO2"]=log(ozone[,"NO2"])
ozone[,"LNO"]=log(ozone[,"NO"])


par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"SRMH2O"]);
hist(ozone[,"LNO2"]);hist(ozone[,"LNO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# Suppression des variables inutiles pour la suite et creation de la variable reponse 
# (variable binaire a expliquer): DepSeuil
ozone=ozone[,c(1:4,8:13)]
ozone[,"DepSeuil"]=as.factor(ozone[,"O3obs"]>150)
O3obs<-ozone[,2]
ozone=ozone[,-2]
# on supprime la variable O3obs (qui a servit a construire la variable a expliquer), puisqu'elle ne
# rentrera pas dans le modele (c'est ce qu'on cherche a prevoir !)

# Conlusion:
# On a maintenant une base d'étude "propre", constituee de 9 variables explicatives, et d'une variable
# a expliquer (sur la derniere colonne)
# > head(ozone)
# JOUR MOCAGE TEMPE STATION VentMOD  VentANG     SRMH2O      LNO2        LNO DepSeuil
# 1    1   93.2  21.5     Aix  9.5000 -0.64350 0.09203260 0.4712528 -0.8580218        0
# 2    1  104.6  20.2     Aix  8.0100 -0.04996 0.09386160 0.7518877 -0.6329933        0
# 3    0  103.6  17.4     Aix  9.3771 -0.12832 0.09751923 0.5050087 -0.7614260        0
# 4    0   94.8  18.8     Aix  9.4578 -0.34516 0.09246621 0.8544153 -0.3552474        0
# 5    0   99.0  23.7     Aix  7.8791 -0.41822 0.08549854 0.5025918 -0.7940731        0
# 6    0  114.3  23.6     Aix  6.3127  0.06341 0.10871982 1.6707211  0.2949059        0

# Quelques variables pour rendre plus comprehensible les appels
predic_quanti. <- c("MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
predic_quali. <- c("JOUR","STATION")
predic <- c("JOUR","STATION","MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
reponse <- "DepSeuil"

# Creation des echantillons apprentissage/test:
# On est donc ici dans une procédure classique, ou l'on a un echantillon d'apprentissage,
# qui sert a construire le modele, et un echantillon test, sur lequel on evalue les
# performances du modele.
# Pour creer le sous-echantillon d'apprentissage et le sous-échantillon de test, on
# utilise la fonction "createDataPartition" du package caret. Elle renvoie les
# indices de l'échantillon d'apprentissage. Il suffit ensuite de séparer le jeu
# de données avec ces indices.
set.seed(42)
n <- nrow(ozone)
testind <- sample(1:n,0.2*n)
apprind <- setdiff(1:n,testind)
train.ozone <- ozone[apprind,]
test.ozone <- ozone[testind,]
