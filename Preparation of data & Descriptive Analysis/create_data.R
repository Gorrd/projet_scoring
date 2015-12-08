#######################################################################################
#                                                                                     #
#                       Lecture et préparation des données                            #
#                                                                                     #
#######################################################################################

# Lecture des données et transformation préalables des variables quantitatives

# Importation des données
ozone <- read.table('ozone.dat',h=T)

# Changement du type de la variable jour
ozone[,"JOUR"]=as.factor(ozone[,"JOUR"])

# Histogrammes des variables initiales
par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"RMH2O"]);
hist(ozone[,"NO2"]);hist(ozone[,"NO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# Transformation des variables et tracé des histogrammes après transformation
ozone[,"SRMH2O"]=sqrt(ozone[,"RMH2O"])
ozone[,"LNO2"]=log(ozone[,"NO2"])
ozone[,"LNO"]=log(ozone[,"NO"])

par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"SRMH2O"]);
hist(ozone[,"LNO2"]);hist(ozone[,"LNO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# Suppression des variables inutiles pour la suite et création de la variable réponse 
# (variable binaire à expliquer): DepSeuil
ozone=ozone[,c(1:4,8:13)]
ozone[,"DepSeuil"]=as.factor(ozone[,"O3obs"]>150)
