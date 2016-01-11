#######################################################################################
#                                                                                     #
#                              Analyse descriptive                                    #
#                                                                                     #
#######################################################################################

# Description des variables et premières interprétations des variables

head(ozone)
summary(ozone)
str(ozone)

# Quelques variables pour rendre plus comprehensible les appels
predic_quanti. <- c("O3obs","MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
predic_quali. <- c("JOUR","STATION")
predic <- c("JOUR","STATION","O3obs","MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
reponse <- "DepSeuil"


prop.table(table(ozone$DepSeuil))
prop.table(table(ozone$O3obs>180))
# 17% des observations ont depassees le seuil d'ozone de 150 µg/m3. Concernant le seuil
# légal d'information de 180 µg/m3, il est dépassé par seulement 7,7% des observations.
# Cela confirme l'utilisation du seuil de 150 µg/m3 pour la variable réponse DepSeuil.

# Separation des donnees
par(mfrow=c(3,3))
for(i in predic_quanti.)
{
  plot(ozone[,i], col=ifelse(ozone[,reponse] == T, "red", "blue"),pch=19,cex=.5,ylab=i)
}
# Observations séparées pour la variable O3Obs

# Plot variable reponse en fonction des variables
par(mfrow=c(3,3))
for(i in predic_quanti.)
{
  plot(as.numeric(ozone[,reponse])-1~ozone[,i],col=ifelse(ozone[,reponse] == T, "red", "blue"),xlab=i,ylab="DepSeuil")
}

# Pour créer le sous-échantillon d'apprentissage et le sous-échantillon de test, on
# utilise la fonction "createDataPartition" du package caret. Elle renvoie les
# indices de l'échantillon d'apprentissage. Il suffit ensuite de séparer le jeu
# de données avec ces indices.
library(caret)
splitIndex <- createDataPartition(ozone[,reponse], p = .8, list = FALSE, times = 1)
train.ozone <- ozone[ splitIndex,]
test.ozone  <- ozone[-splitIndex,]
