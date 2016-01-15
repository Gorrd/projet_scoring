#######################################################################################
#                                                                                     #
#                           Analyse discriminante lineaire                            #
#                                                                                     #
#######################################################################################

# On realise une analyse discriminante lineaire avec les variables choisies
# precedemment et STATION. On suppose que les variables ont la meme variance.

library(MASS)

# Modele lineaire
ozone.lda <- lda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone)
ozone.lda
# Prior probabilities of groups : proportions (estimees) de chacun des groupes dans
# le jeu de donnees. Resultats coherents
# Group means : Moyenne de chaque variable dans les deux groupes.

# Nous avons un modele sur les donnees d'apprentissage. Testons le modele sur la
# prediction en utilisant les donnees tests.
pred.test <- predict(object=ozone.lda,newdata=test.ozone)
matrice.confu <- table(pred.test$class, test.ozone$DepSeuil)
(matrice.confu[1,2]+matrice.confu[2,1])/sum(matrice.confu)
# erreur : 13.9%

# Nous utilisons une methode d'estimation de l'erreur basee sur la validation croisee.
ozone.lda2 <- lda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone,CV=T)
matrice.confu.cv <- table(train.ozone[,reponse],ozone.lda2$class)
(matrice.confu.cv[1,2]+matrice.confu.cv[2,1])/sum(matrice.confu.cv)
# erreur : 12,6% 

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],pred.test$posterior[,1], print.thres="best",col="blue",print.auc=TRUE)
# AUC de 0.8862

# Nous allons verifier l'hypothese d'homoscedasticite, c'est à dire si les matrices de variance
# covariances sont identiques pour les deux groupes. On realise pour cela un test M de Box, avec
# comme hypothèse nulle : sigma_1 = sigma_2.
library(biotools)
boxM(train.ozone[,predic_quanti.],train.ozone[,reponse])
# D'après la sortie du test, on ne peut pas assurer cette hypothèse.

# Assumer que les matrices de variance-covariance sont egales n'est pas tenable. De plus,
# le jeu de donnees d'apprentissage est large. Nous allons donc utiliser une Analyse
# Discriminante Quadratique.

