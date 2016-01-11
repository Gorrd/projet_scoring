#######################################################################################
#                                                                                     #
#                                 Validation du modele                                #
#                                                                                     #
#######################################################################################

# Modele final retenu
m.log <- glm(DepSeuil ~ TEMPE + STATION + VentMOD + SRMH2O + LNO2 + LNO,
              data = train.ozone, family=binomial)

cook <- cooks.distance(m.log)
plot(cook,type="s")

# Pearson residuals
plot(m.log)
plot(fitted(m.log) ~residuals(m.log,"pearson"))

# Interpretation coefficients + linearite
library(effects)
plot(allEffects(m.log))
