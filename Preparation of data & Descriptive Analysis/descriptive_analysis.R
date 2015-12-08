#######################################################################################
#                                                                                     #
#                              Analyse descriptive                                    #
#                                                                                     #
#######################################################################################

head(ozone)
summary(ozone)
str(ozone)

# Passage en numérique de la variable réponse
ozone$DepSeuil = as.numeric(ozone$DepSeuil)-1

# Quelques variables pour rendre plus compréhensible les appels
predic_quanti. <- c("O3obs","MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
predic_quali. <- c("JOUR","STATION")
reponse <- "DepSeuil"


prop.table(table(ozone$DepSeuil))
# 17% des observations ont dépassées le seuil d'ozone

# Séparation des données
par(mfrow=c(3,3))
for(i in predic_quanti.)
{
  plot(ozone[,i], col=ifelse(ozone[,reponse] == T, "red", "blue"),pch=19,cex=.5)
}

# Plot variable réponse
par(mfrow=c(3,3))
for(i in predic_quanti.)
{
  plot(ozone[,reponse]~ozone[,i],col=ifelse(ozone[,reponse] == T, "red", "blue"),xlab=i)
}

# Apprentissage et test
library(caret)
splitIndex <- createDataPartition(ozone[,reponse], p = .8, list = FALSE, times = 1)
train.ozone <- ozone[ splitIndex,]
test.ozone  <- ozone[-splitIndex,]
