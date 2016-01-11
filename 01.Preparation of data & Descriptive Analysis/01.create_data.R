#######################################################################################
#                                                                                     #
#                       Lecture et preparation des donnees                            #
#                                                                                     #
#######################################################################################
# Lecture des donnees et transformation prealables des variables quantitatives

# Lecture des donnees et transformation prealables des variables quantitatives. Retrait
# des variables non pertinentes pour l'analyse. Pour les deux techniques, on conservera
# les mêmes variables au départ.

# Importation des donnees
ozone <- read.table('ozone.dat',h=T)

# Changement du type de la variable jour
ozone[,"JOUR"]=as.factor(ozone[,"JOUR"])

# Histogrammes des variables initiales
par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"RMH2O"]);
hist(ozone[,"NO2"]);hist(ozone[,"NO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# Transformation des variables et trace des histogrammes apres transformation
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

# Passage en numerique de la variable reponse
ozone$DepSeuil = as.numeric(ozone$DepSeuil)-1
