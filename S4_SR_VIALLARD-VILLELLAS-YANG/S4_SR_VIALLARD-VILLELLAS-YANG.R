#Projet 
getwd()    #Where we are working
#setwd("C:/Users/yannv/OneDrive - De Vinci/A2/S4/Satistiques sous R/Projet/S4_SR_VIALLARD-VILLELLAS-YANG")   #where we want to work

library(stringr)
library(plotrix)
library(car)

tableau=read.csv("C:/Users/yannv/OneDrive - De Vinci/A2/S4/Satistiques sous R/Projet/S4_SR_VIALLARD-VILLELLAS-YANG/healthy_lifestyle_city_2021.csv")# modifier le chemin pour ouvrir le fichier

#Problématique : Quelles sont les facteurs influents sur le bonheur d'une population au sein d'une ville ?


#On simplifie le nom des variables

sunshine<-tableau$Sunshine.hours.City.
life<-tableau$Life.expectancy.years...Country.
pollution<-tableau$Pollution.Index.score...City.
happiness<-tableau$Happiness.levels.Country.
takeOut<-tableau$Number.of.take.out.places.City.
continent<-tableau$Continent

#on va créer deux nouvelles variables grâce à la librairie stringr qui va permettre d'avoir des données numériques exploitables

obesity=as.numeric(str_replace_all(tableau$Obesity.levels.Country.,"%",""))
cost=as.numeric(str_replace_all(tableau$Cost.of.a.bottle.of.water.City.,"Â£",""))

#On visualise la répartition des continents (données qualitatives) avec des diagrammes ciculaires et un diagramme à bâtons 

barplotContinet<-barplot(table(continent),main="Continent")#ranger
pieContinent<-pie(table(continent),main="Continent")#ranger
pieContinent3D<-pie3D(table(continent),main="Continent 3D",labels = c("Africa","Asia","Europe","North America","Oceania","South America"))


#on visualise nos données quantitatives avec des histogrammes et la fonction summary()

histLife<-hist(life,col="red",main="Répartition de l'espérance de vie",xlab="Espérance de vie en année",ylab="Fréquence")
summary(life) 
histSunshine<-hist(sunshine,col="yellow",main="Répartition du temps d'ensoleillement",xlab="Heures par an",ylab = "Fréquence")
summary(sunshine) 
histHappiness<-hist(happiness,col="darkgreen",main="Répartition du niveau de bonheur",xlab = "Niveau de bonheur",ylab = "Fréquence")
summary(happiness)
histPollution<-hist(pollution,col="grey",main="Répartition de la pollution",xlab = "Pollution de l'air en %",ylab = "Fréquence")
summary(pollution)
histTakeOutPlaces<-hist(takeOut,col="lightgreen",main="Répartiton du nombre de restaurants à emporter",xlab="Nombre de restaurants à emporter ",ylab="Fréquence")
summary(takeOut)
histObesity<-hist(obesity,col="brown",main="Répartition du pourcentage d'obésité", xlab="Obésité de la population en %",ylab="Fréquence")
summary(obesity)
hiscost<-hist(cost,col="blue",main="Répartition du prix d'une bouteille d'eau",xlab="Prix en livres",ylab="Fréquence")
summary(cost) 

#on visualise avec des boîtes à moustaches

boxplot(life,pollution,obesity,names = c("Espérance de vie","Pollution de l'air en %","Pourcentage d'obésité"))
boxplot(happiness,cost,names=c("Niveau de bonheur","Prix d'une bouteille d'eau"))
boxplot(takeOut,sunshine,names = c("Nombre de restaurants à emporter","Temps d'ensoleillement")) 
 
#Avec des diagramme à bâtons et des boîtes à moustaches on visualise la répartition des 3 variables en fonction du continent 


df<-data.frame(continent,sunshine,cost,happiness)
boxplot(df$sunshine~df$continent,col="lightyellow",xlab="",ylab="Ensoleillement en heure par année",main="Temps d'ensoleillement par continent")
barplot(c(3333,2354.727,1850.889,2499.5,2564.444,2264),col="lightyellow",ylab="Ensoleillement en heure par année",names=c("Afrique","Asie","Europe","Océanie","Amérique du Nord","Amérique du Sud"),main="Temps d'ensoleillement par continent")
boxplot(df$happiness~df$continent,col="lightpink",ylab="Bonheur",main="Niveau de bonheur par continent")
barplot(c(4.48,5.49,6.91,7.22,6.95,6.17),col="lightpink",ylab="Bonheur",names=c("Afrique","Asie","Europe","Océanie","Amérique du Nord","Amérique du Sud"),main="Niveau de bonheur par continent")
boxplot(df$cost~df$continent,col="lightblue",ylab="Prix en £",main="Prix de la bouteille d'eau par continent")
barplot(c(0.375,0.564,1.639,1.525,1.233,0.505),col="lightblue",ylab="Prix en £",names=c("Afrique","Asie","Europe","Océanie","Amérique du Nord","Amérique du Sud"),main="Prix de la bouteille d'eau par continent")



#A refaire, bizarre le boxplot

#On fait un test de Shapiro pour voir si nos données sont normalement distribuées

shapiro.test(sunshine)#   p-value = 0.01931
shapiro.test(life)#p-value = 1.401e-06| Après modification de valeurs : p-value = 7.23e-05
shapiro.test(happiness)#p-value = 0.002118 | Après modification de valeurs : p-value = 0.005066
shapiro.test(obesity)#p-value = 0.001401 | Après modification de valeurs : p-value = 0.003214
shapiro.test(takeOut)#p-value = 2.245e-07 | Après modification de valeurs : p-value = 8.206e-05

shapiro.test(cost)#p-value = 0.07345| Après modification de valeurs : p-value = 0.08574
shapiro.test(pollution)#p-value = 0.1946



#Nous allons maitenant chercher des corélations linéaire à l'aide de la fonction scatterplot() de la librairie car ainsi que les coefficients grâce à lm() 
#La fonction cor.test() nous donnes le coefficient de corrélation LINEAIRE
#Dans certains cas on retire les valeurs extrêmes

scatterplot(cost,happiness,main="Niveau de bonheur en fonction du prix de la bouteille d'eau",xlab="Prix d'une bouteille d'eau",ylab = "Niveau de bonheur" ) #on prends ca
cor.test(cost,happiness)
lm(happiness~cost)$coefficients
cost[37]<-NA
happiness[26]<-NA
scatterplot(cost,happiness,main="Niveau de bonheur en fonction du prix de la bouteille d'eau",xlab="Prix d'une bouteille d'eau",ylab = "Niveau de bonheur" ) #on prends ca
cor.test(cost,happiness)
lm(happiness~cost)$coefficients

scatterplot(life,happiness,main="Niveau de bonheur en fonction de l'espérance de vie",xlab="Espérance de vie",ylab="Niveau de bonheur" ) #on prends ca
cor.test(life,happiness)
lm(happiness~life)$coefficients
life[39]<-NA
scatterplot(life,happiness,main="Niveau de bonheur en fonction de l'espérance de vie",xlab="Espérance de vie",ylab="Niveau de bonheur" ) #on prends ca
cor.test(life,happiness)
lm(happiness~life)$coefficients

scatterplot(takeOut,happiness,main="Niveau de bonheur en fonction du nombre de restaurant à emporter",xlab="Nombre de restaurant à emporter",ylab = "Niveau de bonheur" ) #on prends ca
cor.test(takeOut,happiness)
lm(takeOut~happiness)$coefficient 
takeOut[38]<-NA
takeOut[29]<-NA
takeOut[35]<-NA
scatterplot(takeOut,happiness,main="Niveau de bonheur en fonction du nombre de restaurant à emporter",xlab="Nombre de restaurant à emporter",ylab = "Niveau de bonheur" ) #on prends ca
cor.test(takeOut,happiness)
lm(takeOut~happiness)$coefficient 

scatterplot(sunshine,happiness,main="Niveau de bonheur en fonction du temps d'ensoleillement",xlab="Temps d'ensoleillement",ylab="Niveau de bonheur" ) #on prends ca
cor.test(sunshine,happiness)
lm(sunshine~happiness)$coefficient 

scatterplot(obesity,happiness,main="Niveau de bonheur en fonction de l'obésité",xlab="Obésité",ylab="Niveau de bonheur" ) #on prends ca
cor.test(happiness,obesity)
lm(obesity~happiness)$coefficient 
obesity[7]<-NA
obesity[26]<-NA
obesity[29]<-NA
scatterplot(obesity,happiness,main="Niveau de bonheur en fonction de l'obésité",xlab="Obésité",ylab="Niveau de bonheur" ) #on prends ca
cor.test(happiness,obesity)
lm(obesity~happiness)$coefficient 

scatterplot(pollution,happiness,main="Niveau de bonheur en fonction de la pollution",xlab="Taux de pollution",ylab="Niveau de bonheur" ) #on prends ca
cor.test(pollution,happiness)
lm(happiness~pollution)$coefficient 

scatterplot(pollution,cost,main="Prix de la bouteille d'eau en fonction de la pollution",ylab="Pris de la bouteille d'eau",xlab="Taux de pollution") #on prends ca
cor.test(cost,pollution)
lm(cost~pollution)$coefficient 


#Autre type de corrélation :


      #logarithmique 
happiness[26]<-3.57
cost[37]<-3.20
Estimate = lm(happiness~cost) #lm(y ~ x)
logEstimate = lm(happiness ~ log(cost))
logEstimate$coefficients
plot(cost,predict(Estimate),type='l',col='blue',main="Niveau de bonheur en fonction du prix de la bouteille d'eau",xlab="Prix d'une bouteille d'eau",ylab = "Niveau de bonheur")
lines(smooth.spline(cost,predict(logEstimate)),col='red')
points(cost,happiness)


      #Relation de Spearman 

life[39]<-56.3        #On remet les valeurs extremes
happiness[26]<-3.57

scatterplot(life,happiness,main="Niveau de bonheur en fonction de l'espérance de vie",xlab="Espérance de vie",ylab="Niveau de bonheur" ) #on prends ca
cor.test(life,happiness,method = "spearman")
lm(happiness~life)$coefficients
life[39]<-NA      
happiness[26]<-NA
scatterplot(life,happiness,main="Niveau de bonheur en fonction de l'espérance de vie",xlab="Espérance de vie",ylab="Niveau de bonheur" ) #on prends ca
cor.test(life,happiness,method = "spearman")
lm(happiness~life)$coefficients


