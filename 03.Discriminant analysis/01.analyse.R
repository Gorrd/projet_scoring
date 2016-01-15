#######################################################################################
#                                                                                     #
#           Variables les plus discriminantes & Hypothèses de normalités              #
#                                                                                     #
#######################################################################################

# Pour la création de modèles d'analyse discriminante, plusieurs hypothèses sont à
# satisfaire. Nous devons d'abord déterminer quelles sont les variables qui séparent au
# mieux les données concernant le seuil de dépassement d'ozone. Ainsi, il sera possible
# de ne garder que les variables les plus discriminantes. Ensuite, il est nécessaire
# de vérifier que la loi jointe des variables sélectionnées suit une loi gaussienne
# multidimensionnelle.

# Représentation des classes. On peut se faire une première impression sur quelles
# variables séparent les données en deux nuages distincts.
par(mar=c(0,0,0,0))
pan <- function(x,y)
{
  xy <- cbind.data.frame(x,y)
  s.class(xy,train.ozone$DepSeuil,include.ori=F,add.p=T,clab=1.5,
          col=c("red","blue",cpoi=2,csta=0.5))
}
pairs(train.ozone[,predic_quanti.],panel=pan)

# Nous allonster tester l'effet de la variable discrète DepSeuil sur les différentes
# variables continues. On utilise pour cela une ANOVA :
anova(aov(MOCAGE~DepSeuil,data=train.ozone)) # 169.09 (**)
anova(aov(TEMPE~DepSeuil,data=train.ozone)) # 263.42 (*)
anova(aov(VentMOD~DepSeuil,data=train.ozone)) # 5.3862
anova(aov(VentANG~DepSeuil,data=train.ozone)) # 10.325
anova(aov(SRMH2O~DepSeuil,data=train.ozone)) # 75.176 (***)
anova(aov(LNO2~DepSeuil,data=train.ozone)) # 9.5535
anova(aov(LNO~DepSeuil,data=train.ozone)) # 2.7351

# On regarde maintenant les distributions marginales des variables continues.
DY <- which(train.ozone$DepSeuil == T)
DN <- which(train.ozone$DepSeuil == F)
par(mfrow=c(3,3))
for(k in predic_quanti.)
{
  br <- seq(min(train.ozone[,k]),max(train.ozone[,k]),length.out=30)
  hist(train.ozone[DY,k],breaks=br,col="red",main=NULL,freq=F,xlab=k,ylab="Frequence")
  hist(train.ozone[DN,k],breaks=br,col="blue",main=NULL,freq=F,xlab=k,ylab="Frequence",add=T)
}

# Avec les ANOVA et les histogrammes, on peut déterminer les variables les plus discriminantes
# des données. On garde MOCAGE, TEMPE et SRMH20.

var <- c("MOCAGE","TEMPE","SRMH20")

# On visualise une estimation de la loi jointe de MOCAGE, TEMPE et SRMH20.
par(mfrow=c(1,2))
kde <- bkde2D(train.ozone[,var],bandwidth=3)
image(kde$x1,kde$x2,kde$fhat)
contour(kde$x1,kde$x2,kde$fhat,add=T)
points(train.ozone[,var],pch=21,bg=c("red","blue")[as.numeric(train.ozone$DepSeuil)])
persp(kde$fhat,phi=45,expand=.5,col="blue")

# loi jointe de MOCAGE et TEMPE avec superposition de la loi normale bivariée
# correspondante. Les temps de calculs sont assez conséquents !
library(mvtnorm)
library(KernSmooth)
var <- c("MOCAGE","TEMPE")
dev.new()
kde.vers <- bkde2D(train.ozone[DY,var],bandwidth=3)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
points(train.ozone[DY,var],pch=21,bg="blue")
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=T,lty=2)

t1=seq(min(train.ozone[,"MOCAGE"])-1,max(train.ozone[,"MOCAGE"])+1,by=0.01)
t2=seq(min(train.ozone[,"TEMPE"])-1,max(train.ozone[,"TEMPE"])+1,by=0.01)
T1=matrix(t1,nrow=length(t1),ncol=length(t2))
T2=t(matrix(t2,nrow=length(t2),ncol=length(t1)))
g.vers <- dmvnorm(cbind(c(T1),c(T2)),mean=c(mean(train.ozone[DY,"MOCAGE"]),mean(train.ozone[DY,"TEMPE"])),
                  sigma=as.matrix(cov(train.ozone[DY,var])))
contour(t1,t2,matrix(g.vers,nrow=length(t1),ncol=length(t2)),add=T)

