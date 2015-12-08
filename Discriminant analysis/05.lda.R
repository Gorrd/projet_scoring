#######################################################################################
#                                                                                     #
#                           Analyse discriminante linéaire                            #
#                                                                                     #
#######################################################################################

library(MASS)

# Création modèle
ozone.lda <- lda(DepSeuil ~ .,data=train.ozone)
ozone.lda

# Matrice de confusion + risque
lda.pred <- predict(ozone.lda)
table(train.ozone[,reponse],lda.pred$class)

# Autre package + utiliser l'échantillon d'apprentissage

#Ceci est un test
