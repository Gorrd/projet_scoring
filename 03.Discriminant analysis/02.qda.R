#######################################################################################
#                                                                                     #
#                           Analyse discriminante quadratique                         #
#                                                                                     #
#######################################################################################

library(MASS)

## Modèle quadratique
ozone.qda <- qda(DepSeuil ~ .,data=train.ozone)

# Evaluation des performances
pred.test <- predict(ozone.qda,test.ozone)$class

# Test error
table(pred.test, test.ozone$DepSeuil)
1-mean(pred.test == test.ozone$DepSeuil)

# Courbe ROC
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],predict(ozone.qda,test.ozone)$posterior, print.thres="best",col="blue",print.auc=TRUE)

# LOO error
ozone.qda.loo <- qda(DepSeuil ~ .,data=train.ozone, CV=T)
table(ozone.qda.loo$class , train.ozone$DepSeuil)
mean(ozone.qda.loo$class == train.ozone$DepSeuil)