#######################################################################################
#                                                                                     #
#                              Analyse descriptive                                    #
#                                                                                     #
#######################################################################################

# Description des variables et premières interpretations 

summary(ozone)
# Quelques chiffres/interpretations:
# 1. Plus de jours non feries que feries (logique): cependant, une proportion non negligeable de
# jours feries a ete prise en compte (plus d'un quart), pour pouvoir evaluer une eventuelle influence.
# 2. Temperatures comprises entre 10.4 et 38 deg (avec une moyenne de 23.9): les observations se situent 
# plutot dans une periode estivale, en consideration des temperatures en France.
# 3. 5 stations meteo
str(ozone)

# Quelques variables pour rendre plus comprehensible les appels
predic_quanti. <- c("MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
predic_quali. <- c("JOUR","STATION")
reponse <- "DepSeuil"

# Quelques chiffres concernant le depassement de seuil:
prop.table(table(ozone$DepSeuil))
prop.table(table(O3obs>180))
# 17% des observations depassent le seuil d'ozone de 150 µg/m3. Concernant le seuil
# legal d'information de 180 µg/m3, il est depasse par seulement 7,7% des observations.
# Cela confirme l'utilisation du seuil de 150 µg/m3 pour la variable réponse DepSeuil (
# 7.7% représentant un taux un peu faible pour l'analyse)


# On veut ici analyser les performances sur prédicteur MOCAGE. En particulier, le role de tout l'etude
# est d'ameliorer localement les predictions obtenues par ce modele.
# Creation de la matrice de confusion:
moc_dep <- as.factor(ozone[,"MOCAGE"]>150)
table(obs=ozone$DepSeuil,pred=moc_dep)
# On obtient un taux d'erreur de 24%.

# Plot variable reponse
par(mfrow=c(3,3))
for(i in predic_quanti.)
{
  plot(ozone[,reponse]~ozone[,i],col=ifelse(ozone[,reponse] == T, "red", "blue"),xlab=i,ylab="DepSeuil")
}
# A premiere vue, il n'y a pas de variable qui semble completement separer les donnees

# Pour creer le sous-échantillon d'apprentissage et le sous-échantillon de test, on
# utilise la fonction "createDataPartition" du package caret. Elle renvoie les
# indices de l'échantillon d'apprentissage. Il suffit ensuite de séparer le jeu
# de données avec ces indices.
# install.packages("caret")
library(caret)
splitIndex <- createDataPartition(ozone[,reponse], p = .8, list = FALSE, times = 1)
train.ozone <- ozone[ splitIndex,]
test.ozone  <- ozone[-splitIndex,]
