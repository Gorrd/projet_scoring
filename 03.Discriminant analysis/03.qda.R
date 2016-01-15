#######################################################################################
#                                                                                     #
#                           Analyse discriminante quadratique                         #
#                                                                                     #
#######################################################################################

# Nous avons vu que les hypotheses pour la LDA ne sont pas tenables. Nous nous tournons
# donc vers une analyse QDA. Le but est de mesurer l'efficacite du modele à travers
# l'etude de la courbe ROC et un taux d'erreur de classification.

library(MASS)

# Modele quadratique
ozone.qda <- qda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone)
ozone.qda
# Prior probabilities of groups : proportions (estimees) de chacun des groupes dans
# le jeu de donnees. Résultats coherents
# Group means : Moyenne de chaque variable dans les deux groupes.

# On utilise la meme approche que la LDA.

# Nous avons un modele sur les donnees d'apprentissage. Testons le modele sur la
# prediction en utilisant les donnees tests.
pred.test <- predict(object=ozone.qda,newdata=test.ozone)
matrice.confu <- table(pred.test$class, test.ozone$DepSeuil)
(matrice.confu[1,2]+matrice.confu[2,1])/sum(matrice.confu)
# erreur : 11,0%

# Nous utilisons une methode d'estimation de l'erreur basee sur la validation croisee.
ozone.qda2 <- qda(DepSeuil ~ MOCAGE+TEMPE+SRMH2O+STATION,data=train.ozone,CV=T)
matrice.confu.cv <- table(train.ozone[,reponse],ozone.qda2$class)
(matrice.confu.cv[1,2]+matrice.confu.cv[2,1])/sum(matrice.confu.cv)
# erreur : 11,5% 

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],pred.test$posterior[,1], print.thres="best",col="blue",print.auc=TRUE)
# AUC de 0.8888