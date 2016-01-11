#######################################################################################
#                                                                                     #
#                           Analyse discriminante lineaire                            #
#                                                                                     #
#######################################################################################

library(MASS)

# Creation modele
ozone.lda <- lda(DepSeuil ~ .,data=train.ozone)
ozone.lda

# Matrice de confusion + risque
lda.pred <- predict(ozone.lda)
table(train.ozone[,reponse],lda.pred$class)

# Autre package + utiliser l'echantillon d'apprentissage

#Ceci est un test
