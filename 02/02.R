#######################################################################################
#                                                                                     #
#                                 Validation du modèle                                #
#                                                                                     #
#######################################################################################

# Modèle final retenu
m.log <- glm(DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + SRMH2O,
              data = train.ozone, family=binomial)

cook <- cooks.distance(m.log)
plot(cook,type="s")

# Pearson residuals
plot(m.log)
plot(fitted(m.log) ~residuals(m.log,"pearson"))

# Courbe ROC
library(pROC)
p.est.test <- predict(m4.log,newdata=test.ozone,type="response")
plot.roc(test.ozone[,reponse],p.est.test, print.thres=TRUE)

# Interprétation coefficients + linéarité
library(effects)
plot(allEffects(m.log))
