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
ozone.qda <- qda(DepSeuil ~ .,data=train.ozone)
ozone.qda
# Prior probabilities of groups : proportions (estimees) de chacun des groupes dans
# le jeu de données. Résultats cohérents
# Group means : Moyenne de chaque variable dans les deux groupes.

# Nous avons un modèle sur les données d'apprentissage. Testons le modèle sur la
# prédiction en utilisant les données tests.
pred.test <- predict(object=ozone.qda,newdata=test.ozone)
table(pred.test$class, test.ozone$DepSeuil)
# erreur : 12,0%

# Nous utilisons une méthode d'estimation de l'erreur basée sur la validation croisée.
ozone.qda2 <- qda(DepSeuil ~ .,data=train.ozone,CV=T)
table(train.ozone[,reponse],ozone.qda2$class)
# erreur : 12,9% 

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],pred.test$posterior[,1], print.thres="best",col="blue",print.auc=TRUE)
# AUC de 0.8658

# En conclusion, l'analyse QDA présente une moins bonne AUC mais prédit mieux le modèle que
# l'analyse LDA.