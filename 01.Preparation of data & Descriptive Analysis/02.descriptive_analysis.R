#######################################################################################
#                                                                                     #
#                              Analyse descriptive                                    #
#                                                                                     #
#######################################################################################

# Quelques statistiques basiques, et premieres interepretations des variables.

summary(ozone)
# Quelques chiffres/interpretations:
# 1. Plus de jours non feries que feries (logique): cependant, une proportion non negligeable de
# jours feries a ete prise en compte (plus d'un quart), pour pouvoir evaluer une eventuelle influence.
# 2. Temperatures comprises entre 10.4 et 38 deg (avec une moyenne de 23.9): les observations se situent 
# plutot dans une periode estivale, en consideration des temperatures en France.
# 3. 5 stations meteo: Aix-en-Provence, Cadarache, Plan de Cuques, Rambouillet, et Alès -> influence de la zone ?

# Quelques chiffres concernant le depassement de seuil:
prop.table(table(ozone$DepSeuil))
prop.table(table(O3obs>180))
# 17% des observations depassent le seuil d'ozone de 150 µg/m3. Concernant le seuil
# legal d'information de 180 µg/m3, il est depasse par seulement 7,7% des observations.
# Cela confirme l'utilisation du seuil de 150 µg/m3 pour la variable réponse DepSeuil (
# 7.7% représentant un taux un peu faible pour l'analyse)


# On veut ici analyser les performances du prédicteur MOCAGE (prédicteur déterministe). En particulier, 
# le role de tout l'etude est d'ameliorer localement les predictions obtenues par ce modele par l'ajout de
# variables explicatives.
# Creation de la matrice de confusion:
moc_dep <- as.factor(ozone[,"MOCAGE"]>150)
table(obs=ozone$DepSeuil,pred=moc_dep)
# On obtient un taux d'erreur de 24%.

# Plot variable reponse en fonction des variables
par(mfrow=c(3,3))
for(i in predic_quanti.)
{
  plot(as.numeric(ozone[,reponse])-1~ozone[,i],col=ifelse(ozone[,reponse] == T, "red", "blue"),xlab=i,ylab="DepSeuil")
}
# A premiere vue, il n'y a pas de variable qui semble completement separer les donnees

# Multicolinearite: un probleme classique des etudes statistiques est la colinearite entre les variables
# explicatives (-> mauvaise estimation des parametres)
# Une idée: matrice de correlation entre les variables quantitatives
mat_cor<-cor(ozone[,predic_quanti.])
# Les valeurs de correlation sont faibles, hormis pour les variables LN0/LN02 -> etre prudent dans la 
# suite...
