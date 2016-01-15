#######################################################################################
#                                                                                     #
#                                 Validation du modele                                #
#                                                                                     #
#######################################################################################
# Une fois notre modele choisi, nous devons nous assurer que celui ci est bon 
# (ajustement des donnees, valeurs aberrantes/influentes, ... ): on valide ou non le modele

## Test d'ajustement des donnees: Hosmer-Lemeshow
# Test d'hypothese: le modele est-il approprie ?
# H0 := le modele est approprie
#install.packages("ResourceSelection")
library(ResourceSelection)
y=predict(modele_final,newdata=train.ozone,type="response") # sur les donnees d'apprentissage
hoslem.test(x=as.numeric(train.ozone[,reponse])-1,y=y,g=10)
# p-value = 0.5785
# Conclusion: on accepte l'hypothese nulle, le modele est approprie en terme d'ajustement

## Analyse des residus

# L'analyse des residus est importante: ils doivent generalement se situer entre -2 et 2. Ainsi,
# une donnee sera consideree comme aberrante si le residu correspondant est hors de ces limites.
# En regression logistique, on s'interesse plutot aux residus de deviance et de pearson
plot(resid(modele_final,type="deviance"), type = "p", cex = 0.5, ylab = "Résidus de deviance ", 
     col = "blue", ylim = c(-3, 3),main="Résidus de déviance et limites acceptables")
abline(h = c(-2, 2), col = "red")

plot(resid(modele_final,type="pearson"), type = "p", cex = 0.5, ylab = "Résidus de Pearson ", 
     col = "blue", ylim = c(-3, 3),main="Résidus de Pearson et limites acceptables")
abline(h = c(-2, 2), col = "red")

# On note que des points depassent les limites. Beaucoup de litterature concerne la maniere de
# "gerer" les valeurs aberrantes. On adopte ici une demarche un peu "brutale": on supprimera les
# individus dont les residus correspondant de pearson et de deviance depassent les limites.
bool_dev <- as.numeric(abs(resid(modele_final,type="deviance"))<2)
bool_pearson <- as.numeric(abs(resid(modele_final,type="pearson"))<2)
bool_keep <- bool_dev*bool_pearson # vecteur booleen des individus a garder pour construire le nouveau modele
ind_keep <- which(bool_keep==1) 
# 31 donnees ont ete supprimees pour etablir notre nouveau modele

# CONSTRUCTION DU MODELE FINAL AVEC LES DONNEES ABERRANTES SUPPRIMEES
data_final <- train.ozone[ind_keep,]
MODELE <- glm(DepSeuil ~ MOCAGE + TEMPE + STATION + VentMOD + SRMH2O + LNO2 + LNO,
              data = data_final, family=binomial(link=logit))
summary(MODELE)

# A titre de comparaison, on compare maintenant notre taux d'erreur et notre courbe ROC aux resultats
# obtenus sans la suppression des donnees aberrantes.

p_est_test <- predict(MODELE, newdata = test.ozone, type="response") # prediction des probas
y_est_test <- as.numeric(p_est_test>0.5) # regle d'affectation "basique"
mat_confu <- table(obs=test.ozone[,reponse], pred=y_est_test)
error_rate_MODELE <- (mat_confu[2,1]+mat_confu[1,2])/sum(mat_confu)
# On obtient: [1] 0.1201923
# Sans la suppression des donnees, on obtenait: 0.125
# Le taux d'erreur est donc plus interessant, parce que plus faible.

moc_dep_test <- as.factor(test.ozone[,"MOCAGE"]>150)
table(obs=test.ozone$DepSeuil,pred=moc_dep_test)
# on obtient un taux d'erreur de 0.2163462 pour le predicteur MOCAGE -> superieur
# a celui du modele

plot.roc(test.ozone[,reponse],p.est.test,col="blue",main="Comparaison des courbes ROC")
plot.roc(test.ozone[,reponse],p_est_test,col="red",add=TRUE)
plot.roc(test.ozone[,reponse],test.ozone[,"MOCAGE"],col="green",add=TRUE)
legend(x=0.7,y=0.5,legend=c("modèle final","modèle final traité","MOCAGE"),fill=c("blue","red","green"),)
# Comparaison peu significative, les classifieurs sont tres similaires.




# Interpretation coefficients + linearite
install.packages("effects")
library(effects)
plot(allEffects(modele_final))
