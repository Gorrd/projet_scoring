#######################################################################################
#                                                                                     #
#                           Analyse discriminante lineaire                            #
#                                                                                     #
#######################################################################################

library(MASS)

# Modèle linéaire
ozone.lda <- lda(DepSeuil ~ .,data=train.ozone)
ozone.lda

# Evaluation des performances
pred.test <- predict(ozone.lda,test.ozone)$class

# Test error
table(pred.test, test.ozone$DepSeuil)
1-mean(pred.test == test.ozone$DepSeuil)

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

