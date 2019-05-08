library(ggplot2)
library("gridExtra")

Database <- read.table("/amuhome/m17013118/Documents/INFO/4A/Analyse Donnée/DataAnalysis-master/data1TP1.txt", header = TRUE)
summary(Database)

# Question 1 :
A<-ggplot(Database, aes(x=A, y=Y)) + geom_point() +  geom_smooth(method=lm) # Courbe décroissante, relation faible, relation non linéaire négative
B<-ggplot(Database, aes(x=B, y=Y)) + geom_point() +  geom_smooth(method=lm) # Courbe croissante, relation faible, relation non linéaire positive
C<-ggplot(Database, aes(x=C, y=Y)) + geom_point() +  geom_smooth(method=lm) # Résultats confus
D<-ggplot(Database, aes(x=D, y=Y)) + geom_point() +  geom_smooth(method=lm) # Courbe exponentielle, relation faible, relation non linéaire positive
E<-ggplot(Database, aes(x=E, y=Y)) + geom_point() +  geom_smooth(method=lm) # Courbe en cloche, relation faible
grid.arrange(A, B, C, D, E)

Coef_Pearson<-function(X,Y){
  covariance<- cov(X,Y)
  ecart<-sd(X)*sd(Y)
  rslt<-covariance/ecart
  rslt
}

# Question 2 :
# La variable ayant la plus petite corrélation est celle de E, les points forment une parabole ce qui n'est pas propice à une régression linéaire
Coef_Pearson(Database$A,Database$Y)
Coef_Pearson(Database$B,Database$Y)
Coef_Pearson(Database$C,Database$Y)
Coef_Pearson(Database$D,Database$Y)
Coef_Pearson(Database$E,Database$Y)
#cor(Database$A,Database$Y)

Coef_Spearman<-function(X,Y){
  denominateur<-(15^3)-15
  sum<-0
  rgX<-rank(X)
  rgY<-rank(Y)
  for (i in (1:15)) {
    rang<-rgX[i]-rgY[i]
    sum<-sum+(rang^2)
  }
  numerateur<-6*sum
  rslt<-1-(numerateur/denominateur)
  rslt
}


Coef_Spearman(Database$A,Database$Y)
Coef_Spearman(Database$B,Database$Y)
Coef_Spearman(Database$C,Database$Y)
Coef_Spearman(Database$D,Database$Y)
Coef_Spearman(Database$E,Database$Y)
#cor(Database$A,Database$Y,method="spearman")

# Question 4 :
# Pour calculer la relation non linéaire et non monotone entre les variables E et Y, on pourrait réaliser une courbe de tendance de type polynomiale.
# On pourrait alors approcher la relation par un polynôme.

# Question 5 :
Student <- read.table("/amuhome/m17013118/Documents/INFO/4A/Analyse Donnée/DataAnalysis-master/data2TP1.txt", header = TRUE)

independance<-function(X){
  moy<-mean(X)
  ecart<-sd(X)
  rslt<-abs(moy-19)/(ecart/sqrt(length(X)))
  rslt
}

# Hypothèse nulle : L'inflation n'a pas affecté le coût théorique : 2.145
independance(Student$Marseille) # 2.177
# 2.145 < 2.177 donc on rejette

# Hypothèse nulle : Il y a une dépendance significative entre Marseille et Aix-en-Provence
# Valeur théorique : 2.048
independance2<-function(X,Y){
  moyX<-mean(X)
  moyY<-mean(Y)
  ecartX<-sd(X)
  ecartY<-sd(Y)
  rslt<-abs(moyX-moyY)/sqrt(((ecartX^2)/length(X))+((ecartY^2)/length(Y)))
  rslt
}

independance2(Student$Marseille, Student$Aix) # 2.321
# 2.048 < 2.321, on rejette donc pas de dépendance significative entre les deux villes
# 2.468 > 2.312, on accepte
# Avec une précision de 98%, la valeur obtenue lors du test est inclue dans la courbe en cloche alors qu'elle ne l'était pas avec une précision de 95%.
# C'est normal puisque le test est plus précis.

val1<-(9/16)*(1528+106+117+381)
val2<-(3/16)*(1528+106+117+381)
val3<-(3/16)*(1528+106+117+381)
val4<-(1/16)*(1528+106+117+381)
print(val1)
print(val2)
print(val3)
print(val4)

data<-cbind(c(1528,106,117,381), c(val1,val2,val3,val4))
khi2<-function(data){
  sum<-0
  for (i in 1:4) {
    sum<-sum+(((data[i,1]-data[i,2])^2)/data[i,2])
  }
  sum
}
khi2(data)
#Le résultat du khi 2 (966,61) est très supérieur à la valeur du tableau khi deux (7.81). On en conclut donc que l'hypothèse 
#H0 n'est pas validée donc le ratio n'est pas 9:3:3:1

#--Question 8--
vt1<-((29+5+46)*(29+40+18))/200
vt2<-((40+32+8)*(29+40+18))/200
vt3<-((18+22+0)*(29+40+18))/200
vt4<-((29+5+46)*(5+32+22))/200
vt5<-((40+32+8)*(5+32+22))/200
vt6<-((18+22+0)*(5+32+22))/200
vt7<-((29+5+46)*(46+8))/200
vt8<-((40+32+8)*(46+8))/200
vt9<-((18+22+0)*(46+8))/200

absent<-cbind(c(29,40,18), c(vt1,vt2,vt3))
atypique<-cbind(c(5,32,22), c(vt4,vt5,vt6))
typique<-cbind(c(46,8,0), c(vt7,vt8,vt9))
khi2<-function(data){
  sum<-0
  for (i in 1:3) {
    sum<-sum+(((data[i,1]-data[i,2])^2)/data[i,2])
  }
  sum
}
khi2(absent)
khi2(atypique)
khi2(typique)
khi2forme<-khi2(absent)+khi2(atypique)+khi2(typique)
print(khi2forme)

vt1<-((20+60)*(20+29+12))/200
vt2<-((29+51)*(20+29+12))/200
vt3<-((12+28)*(20+29+12))/200
vt4<-((20+60)*(60+51+28))/200
vt5<-((29+51)*(60+51+28))/200
vt6<-((12+28)*(60+51+28))/200

absent<-cbind(c(20,29,12), c(vt1,vt2,vt3))
present<-cbind(c(60,51,28), c(vt4,vt5,vt6))

khi2(absent)
khi2(present)
khi2couleur<-khi2(absent)+khi2(present)
print(khi2couleur)

# Question 9 :
# Les tests paramétriques se basent sur des distributions statistiques supposées dans les données.
# Par conséquent, certaines conditions de validité doivent être vérifiées pour que le résultat d’un test paramétrique soit fiable.
# Par exemple, le test t de Student pour échantillons indépendants n’est fiable que si les données associées à chaque échantillon
# suivent une distribution normale et si les variances des échantillons sont homogènes.

# Les tests non-paramétriques ne se basent pas sur des distributions statistiques.
# Ils peuvent donc être utilisés même si les conditions de validité des tests paramétriques ne sont pas vérifiées.
# Le test du Khi Deux est donc non paramétrique.

# Question 10 :
# Les données qualitatives sont des données auxquelles on ne peut pas attribuer une valeur ou une caractéristique.
# Exemples de propriétés physiques qualitatives : La couleur, la texture, le goût, l'odeur, l'état et la ductilité.
# Les coefficients de Pearson et de Spearman sont des coefficients de corrélation.
# Or, une corrélation permet de savoir s'il existe un lien entre deux variables quantitatives, si les valeurs des deux
# variables varient dans le même sens ou dans le sens contraire.
# Les coefficients de Pearson et de Spearman ne sont donc apllicables que sur des données quantitatives et en aucun cas sur des données qualitatives.