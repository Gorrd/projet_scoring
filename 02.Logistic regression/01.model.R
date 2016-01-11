#######################################################################################
#                                                                                     #
#              Mise en place d'un modele de regression logistique                     #
#                                                                                     #
#######################################################################################

# La variable O3obs est problematique pour la modelisation avec un modele de regression logistique.
# En effet il est possible de séparer les données concernant la variable O3obs, vu que la variable
# réponse est séparée par le seuil de concentration 150µg/m3.
# On retire donc O3obs pour tous les modèles à suivre de régression logistique.
m1.log <- glm(DepSeuil ~ JOUR + MOCAGE + TEMPE + STATION + VentMOD + VentANG + SRMH2O + LNO2 + LNO,
             data = train.ozone, family=binomial)
summary(m1.log)


# Testons l'effet global des variables qualitatives avec l'aide d'un test de Wald. 
# On teste la nullité des modalités des variables qualitatives en même temps.
library(aod)

# Test de Wald pour JOUR
wald.test(b = coef(m1.log), Sigma = vcov(m1.log), Terms = 2)

# Test de Wald pour STATION
wald.test(b = coef(m1.log), Sigma = vcov(m1.log), Terms = 5:8)

# On enleve la variable jour
m2.log <- glm(DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + VentANG + SRMH2O + LNO2 + LNO,
              data = train.ozone, family=binomial)
summary(m2.log)

# Selection automatique de variables
m.ba <- step(m2.log,direction="both")
#DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + SRMH2O

m3.log <- glm(DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + SRMH2O,
             data = train.ozone, family=binomial)
summary(m3.log)

# Qualite predictive des trois modeles
library(boot)
cost <- function(r, pi) mean(abs(r-pi)>0.5)

cv.glm(train.ozone, m1.log, cost, K=10)$delta[1]
cv.glm(train.ozone, m2.log, cost, K=10)$delta[1]
cv.glm(train.ozone, m3.log, cost, K=10)$delta[1]

# Matrice de confusion
p.est.test <- predict(m3.log, newdata = test.ozone, type="response")
y.est.test <- as.numeric(p.est.test>0.5)
mat.confu <- table(obs=test.ozone[,reponse], pred=y.est.test)
diag(1/table(test.ozone[,reponse]))%*%table(obs=test.ozone[,reponse],pred=y.est.test)
(mat.confu[2,1]+mat.confu[1,2])/sum(mat.confu)
# Taux d'erreur : 9.1% : c'est cool

# Modele final : m3.log

