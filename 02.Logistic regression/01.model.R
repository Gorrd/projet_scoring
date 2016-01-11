#######################################################################################
#                                                                                     #
#              Mise en place d'un modele de regression logistique                     #
#                                                                                     #
#######################################################################################

## PREMIER MODELE : TOUTES LES VARIABLES

# La variable O3obs est problematique pour la modelisation avec un modele de regression logistique.
# En effet il est possible de séparer les données concernant la variable O3obs, vu que la variable
# réponse est séparée par le seuil de concentration 150µg/m3.
# On retire donc O3obs pour tous les modèles à suivre de régression logistique.
m1.log <- glm(DepSeuil ~ JOUR + MOCAGE + TEMPE + STATION + VentMOD + VentANG + SRMH2O + LNO2 + LNO,
             data = train.ozone, family=binomial(link=logit))

# Interprétation des résultats du premier modèle
summary(m1.log)
anova(m1.log, test="Chisq")
# L'anova montre la différence entre

## DEUXIEME MODELE : TESTS DE WALD

# Testons l'effet global des variables qualitatives avec l'aide d'un test de Wald. 
# On teste la nullité des modalités des variables qualitatives en même temps.
library(aod)

# Test de Wald pour JOUR
wald.test(b = coef(m1.log), Sigma = vcov(m1.log), Terms = 2)
# La variable JOUR semble peu utile

# Test de Wald pour STATION
wald.test(b = coef(m1.log), Sigma = vcov(m1.log), Terms = 5:8)

# On enleve la variable jour, et on obtient un second modèle.
m2.log <- glm(DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + VentANG + SRMH2O + LNO2 + LNO,
              data = train.ozone, family=binomial(link=logit))
summary(m2.log)
anova(m2.log, test="Chisq")

## TROISIEME MODELE : SELECTION AUTOMATIQUE

# Création d'un modèle avec une selection automatique de variables
m3.log <- step(m1.log,direction="both")
summary(m3.log)
anova(m3.log,m2.log,test="Chisq")

## SELECTION DU MODELE ##


# Qualite predictive des trois modeles : on estime l'erreur de prédiction en utlisant une
# validation croisée.
library(boot)
cost <- function(r, pi) mean(abs(r-pi)>0.5)
cv.glm(train.ozone, m1.log, cost, K=10)$delta[1]
cv.glm(train.ozone, m2.log, cost, K=10)$delta[1]
cv.glm(train.ozone, m3.log, cost, K=10)$delta[1]
# Plus petite erreur -> modèle 2

# Matrice de confusion
p.est.test <- predict(m3.log, newdata = test.ozone, type="response")
y.est.test <- as.numeric(p.est.test>0.5)
mat.confu <- table(obs=test.ozone[,reponse], pred=y.est.test)
diag(1/table(test.ozone[,reponse]))%*%table(obs=test.ozone[,reponse],pred=y.est.test)
(mat.confu[2,1]+mat.confu[1,2])/sum(mat.confu)
# 10% (mais change à chaque fois)

# Courbe ROC
library(pROC)
p.est.test <- predict(m3.log,newdata=test.ozone,type="response")
plot.roc(test.ozone[,reponse],p.est.test, print.thres=TRUE)


