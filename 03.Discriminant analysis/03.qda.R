#######################################################################################
#                                                                                     #
#                           Analyse discriminante quadratique                         #
#                                                                                     #
#######################################################################################

# Nous avons vu que les hypothèses pour la LDA ne sont pas tenables. Nous nous tournons
# donc vers une analyse QDA. Le but est de mesurer l'efficacité du modèle à travers
# l'étude de la courbe ROC et un taux d'erreur de classification.

library(MASS)

# Modèle quadratique
ozone.qda <- qda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone)
ozone.qda
# Prior probabilities of groups : proportions (estimees) de chacun des groupes dans
# le jeu de données. Résultats cohérents
# Group means : Moyenne de chaque variable dans les deux groupes.

# On utilise la même approche que la LDA.

# Nous avons un modèle sur les données d'apprentissage. Testons le modèle sur la
# prédiction en utilisant les données tests.
pred.test <- predict(object=ozone.qda,newdata=test.ozone)
matrice.confu <- table(pred.test$class, test.ozone$DepSeuil)
(matrice.confu[1,2]+matrice.confu[2,1])/sum(matrice.confu)
# erreur : 11,0%

# Nous utilisons une méthode d'estimation de l'erreur basée sur la validation croisée.
ozone.qda2 <- qda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone,CV=T)
matrice.confu.cv <- table(train.ozone[,reponse],ozone.qda2$class)
(matrice.confu.cv[1,2]+matrice.confu.cv[2,1])/sum(matrice.confu.cv)
# erreur : 11,5% 

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],pred.test$posterior[,1], print.thres="best",col="blue",print.auc=TRUE)
# AUC de 0.8888