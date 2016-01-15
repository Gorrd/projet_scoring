#######################################################################################
#                                                                                     #
#                           Analyse discriminante lineaire                            #
#                                                                                     #
#######################################################################################

# On réalise une analyse discriminante linéaire avec les variables choisies
# précédemment et STATION. On suppose que les variables ont la même variance.

library(MASS)

# Modèle linéaire
ozone.lda <- lda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone)
ozone.lda
# Prior probabilities of groups : proportions (estimees) de chacun des groupes dans
# le jeu de données. Résultats cohérents
# Group means : Moyenne de chaque variable dans les deux groupes.

# Nous avons un modèle sur les données d'apprentissage. Testons le modèle sur la
# prédiction en utilisant les données tests.
pred.test <- predict(object=ozone.lda,newdata=test.ozone)
matrice.confu <- table(pred.test$class, test.ozone$DepSeuil)
(matrice.confu[1,2]+matrice.confu[2,1])/sum(matrice.confu)
# erreur : 13.9%

# Nous utilisons une méthode d'estimation de l'erreur basée sur la validation croisée.
ozone.lda2 <- lda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone,CV=T)
matrice.confu.cv <- table(train.ozone[,reponse],ozone.lda2$class)
(matrice.confu.cv[1,2]+matrice.confu.cv[2,1])/sum(matrice.confu.cv)
# erreur : 12,6% 

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],pred.test$posterior[,1], print.thres="best",col="blue",print.auc=TRUE)
# AUC de 0.8862

# Nous allons vérifier l'hypothèse d'homoscédasticité, c'est à dire si les matrices de variance
# covariances sont identiques pour les deux groupes. On réalise pour cela un test M de Box, avec
# comme hypothèse nulle : sigma_1 = sigma_2.
library(biotools)
boxM(train.ozone[,predic_quanti.],train.ozone[,reponse])
# D'après la sortie du test, on ne peut pas assurer cette hypothèse.

# Assumer que les matrices de variance-covariance sont égales n'est pas tenable. De plus,
# le jeu de données d'apprentissage est large. Nous allons donc utiliser une Analyse
# Discriminante Quadratique.

