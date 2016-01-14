#######################################################################################
#                                                                                     #
#                           Analyse discriminante lineaire                            #
#                                                                                     #
#######################################################################################

# On réalise une analyse discriminante linéaire avec toutes les variables. On suppose 
# que les variables ont la même variance.

library(MASS)

# Modèle linéaire
ozone.lda <- lda(DepSeuil ~ .,data=train.ozone)
ozone.lda
# Prior probabilities of groups : proportions (estimees) de chacun des groupes dans
# le jeu de données. Résultats cohérents
# Group means : Moyenne de chaque variable dans les deux groupes.

# Nous avons un modèle sur les données d'apprentissage. Testons le modèle sur la
# prédiction en utilisant les données tests.
pred.test <- predict(object=ozone.lda,newdata=test.ozone)
table(pred.test$class, test.ozone$DepSeuil)
# erreur : 12.9%

# Nous utilisons une méthode d'estimation de l'erreur basée sur la validation croisée.
ozone.lda2 <- lda(DepSeuil ~ .,data=train.ozone,CV=T)
table(train.ozone[,reponse],ozone.lda2$class)
# erreur : 12,6% 

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],pred.test$posterior[,1], print.thres="best",col="blue",print.auc=TRUE)
# AUC de 0.8947

# test M de Box
library(biotools)
boxM(train.ozone[,predic_quanti.],train.ozone[,reponse])
# La LDA assure que les observations de chaque classe proviennent d'une distribution
# gausienne multivariée avec une matrice de covariance commune. Le test M de Box teste
# l'hypothèse nulle d'égalité de matrice variance-covariance pour les deux classes.
# Or d'après la sortie du test, on ne peut pas assurer cette hypothèse.

# Assumer que les matrices de variance-covariance sont égales n'est pas tenable. De plus,
# le jeu de données d'apprentissage est large. Nous allons donc utiliser une Analyse
# Discriminante Quadratique.

