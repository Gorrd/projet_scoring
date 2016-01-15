#######################################################################################
#                                                                                     #
#              Mise en place d'un modele de regression logistique                     #
#                                                                                     #
#######################################################################################

#######################################################################################
## PREMIER MODELE : TOUTES LES VARIABLES
#######################################################################################

m1.log <- glm(DepSeuil ~.,
              data = train.ozone, family="binomial")
# Remarque: traitement des variables qualitatives par traitement disjonctif complet.


# Interprétation des résultats du premier modèle
summary(m1.log)
# Quelques commentaires:
# 1. De manière générale, les variances des parametres estimes sont plutot faibles (ce qui est positif)
# 2. Pas de valeurs "aberrantes" des parametres (valeurs trop importantes) 
# 3. Test de significativite globale: Null deviance est la deviance du modele logit(P)=beta_0
# -> suit une loi du khi-deux a p degres de lib (ou p = nb de param, ici p=13 car traitement disjonctif
# des variables qualitatives dans le modele): clairement, les variables explicatives 
# ont simultanement une influence sur la  probabilite d’apparition de l’evenement etudie.
anova(m1.log,test="Chisq")
# teste les significative des coefficients: tests de deviance (que l'on retrouve dans le summary)
# A premiere vue, certains coefficients ne semblent pas significatifs


## On va maintenant construire plusieurs modeles, a partir du modele de base. Les modeles seront donc emboites
## Il nous faudra ensuite choisir parmis ces modeles le plus pertinent par le biais de certains criteres (courbes ROC 
# etc)

#######################################################################################
## DEUXIEME MODELE : TESTS DE WALD -> suppression de certaines variables ?
#######################################################################################

# On suit une procedure assez "classique" de suppression des variables. A partir d'un modele, on supprime la
# variable la moins significative, dans le sens du critère de Wald, puis on construit un nouveau modele 
# sans cette variable, et on recommence, jusqu'a obtenir uniquement des variables significatives.
# install.packages("aod")
# Remarque: on doit etre prudent lors des tests pour la variable station, traitee par traitement disjonctif, et 
# bien proceder a un test de nullite simultane
library(aod)

# 1.
summary(m1.log)
wald.test(b = coef(m1.log), Sigma = vcov(m1.log), Terms = 5:8) 
# la variable la moins significative au sens de Wald est VentANG, avec une p-valeur de 0.942407  

# 2. modele sans VentANG
ma.log <- glm(DepSeuil ~ JOUR + MOCAGE + TEMPE + STATION + VentMOD + SRMH2O + LNO2 + LNO,
              data = train.ozone, family="binomial")
summary(ma.log)
wald.test(b = coef(ma.log), Sigma = vcov(ma.log), Terms = 5:8) 
# la variable la moins significative au sens de Wald est JOUR1 (donc JOUR), avec une p-valeur de 0.562342 

# 3. modele sans VentANG et JOUR
mb.log <- glm(DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + SRMH2O + LNO2 + LNO,
              data = train.ozone, family=binomial(link=logit))
summary(mb.log)
wald.test(b = coef(mb.log), Sigma = vcov(mb.log), Terms = 4:7)
# la variable la moins significative au sens de Wald est la variable MOCAGE, avec une p-valeur de 0.108572
# On peut se poser la question de la suppression de cette variable: meme si statistiquement, la variable serait
# a supprimer, d'un point de vue pratique cela semble contradictoire. En effet, elle est censee fournir une estimation
# plutot "correcte" du niveau d'ozone (puisqu'utilisée par meteo france!), et devrait servir de base a notre modele
# (on cherche a "enrichir" cette donnee)

# 4. modele sans VentANG, JOUR et MOCAGE
mc.log <- glm(DepSeuil ~ TEMPE + STATION + VentMOD + SRMH2O + LNO2 + LNO,
              data = train.ozone, family=binomial(link=logit))
summary(mc.log)
wald.test(b = coef(mc.log), Sigma = vcov(mc.log), Terms = 3:6)
# Arrive ici, on a un modele dont les variables sont significatives au sens de Wald. On construit cependant 
# un modele qui prend en compte la variable MOCAGE (ce raisonnement est plutot lie a des considerations
# pratiques que statistiques): 
# On construit donc le  modele:

m2.log<-mb.log # avec MOCAGE

#######################################################################################
## TROISIEME MODELE : SELECTION AUTOMATIQUE
#######################################################################################

# On se propose ici d'utiliser une procedure de selection automatique des variables (mixte)
# (au sens du critere de l'AIC)

m3.log <- step(m1.log,direction="both")
summary(m3.log)
# On obtient par cette procédure le modele complet, sans les variables: VentANG et JOUR -> ceci rejoint les 
# premieres observations sur le niveau de significativite des coeffs. m3.log est donc m2.log

# Remarque: une procedure backward aboutit au meme resultat
# m4.log <- step(m1.log,direction="backward")


#######################################################################################
## Conclusion et selection de modele
#######################################################################################
# On a donc maintenant 2 modeles, m1.log (complet) et m2.log (sans JOUR et VentANG)
# On va chercher a obtenir le modele le plus pertinent parmis ces modèles

## SELECTION DU MODELE ##
# On realise ici les operations avec le jeu de donnees d'apprentissage, le jeu de tets nous sera utile plus tard

# Qualite predictive des trois modeles : on estime l'erreur de prédiction en utilisant une
# validation croisée: on choisit une valeur classique de K=10
library(boot)
cost <- function(r, pi) mean(abs(r-pi)>0.5)
cv.glm(train.ozone, m1.log, cost, K=10)$delta[1]
# [1] 0.120048
cv.glm(train.ozone, m2.log, cost, K=10)$delta[1]
# [1] 0.1188475

# Meilleur taux d'erreur pour le modele 2: (mais resultats sont très proches)

# LOOCV
n=nrow(train.ozone)
cv.glm(train.ozone, m1.log, cost, K=n)$delta[1]
# [1] 0.1128451
cv.glm(train.ozone, m2.log, cost, K=n)$delta[1]
# [1] 0.1164466
# Les resultats sont encore une fois tres proches...

# AIC: on peut comparer les modeles en terme d'AIC
# m1.log: AIC: 455.0789
# m2.log: AIC: 451.4182
# plus petit AIC pour le modele m2.log -> on aurait tendance a preferer ce modele

# Conclusion: On aurait tendance a chosir le modele 2 (par parcimonie par rapport au modele 1), 
# au vu des résultats precedents, meme si les conclusions ne peuvent pas etre tranchees.

# On realise un test d'hypothese (deviance) de modeles emboites, entre le modele complet et le modele 2,
# pour voir si l'on peut raisonnablement choisir la parcimonie.
d1 <- m1.log$deviance
d2 <- m2.log$deviance
delta_d <- d2-d1 # [1] 0.3393363
# H0: le modele le plus simple (m2.log) est approprie
# sous H0, la stat de test delta_d suit un khi-deux a 2 degre de lib (2 para en moins)
qchisq(0.95,df=2) # [1] 5.991465

# Conclusion: on retient le modele le plus parcimonieux m2.log
# Notre modele final contient donc les variables: MOCAGE, TEMPE, RMH2O, NO2, NO, STATION, VentMOD

modele_final <- m2.log

#######################################################################################
## taux d'erreur et courbe ROC
#######################################################################################
# On analyse ici les performances de notre modele retenu sur l'echantillon test, par le
# biais de la matrice de confusion, du taux d'erreur, de la courbe ROC

# Matrice de confusion et erreur de classif
p.est.test <- predict(modele_final, newdata = test.ozone, type="response") # prediction des probas
y.est.test <- as.numeric(p.est.test>0.5) # regle d'affectation "basique"
mat.confu <- table(obs=test.ozone[,reponse], pred=y.est.test)
mat.confu_freq <- diag(1/table(test.ozone[,reponse]))%*%table(obs=test.ozone[,reponse],pred=y.est.test) # en frequence
error_rate <- (mat.confu[2,1]+mat.confu[1,2])/sum(mat.confu)
# On obtient:
# [1] 0.125

# Courbe ROC
#install.packages("pROC")
library(pROC)
par(mfrow=c(1,1))
plot.roc(test.ozone[,reponse],p.est.test, print.thres="best",col="blue",print.auc=TRUE)
# Conclusion: tres bonne AUC du modele retenu 0.904

