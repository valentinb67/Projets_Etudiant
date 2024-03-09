# Introduction : Chargement de la base de données.
load("DataGr11.RData")
View(data)
data2<-data[,-1:-2] # On décide de retirer les 2 premières colonnes car
# la première colonne n'apporte aucune information et on retire les années pour 
# la matrice de corrélation.
View(data2)


# I. Présentation des données et mise en forme

str(data2) # La base de données contient 47 observations ainsi que 5 variables.

names(data2) # Les variables sont le chômage, l'inflation, le PIB/habitant
# et le taux d'épargne

colnames(data2)<-c('Chomage','Inflation','PIBhab',"Epargne")
# On renomme les colonnes pour faciliter la compréhension du document.

attach(data2) # On utilise cette commande pour éviter de mettre les 
# dollars à chaque fois et alléger les commandes.


# II. Statistiques descriptives, analyse des relations entre variables et graphiques.

stats<- summary(data2)
# On édite un tableau de statistiques descriptives simple que l'on
# stocke dans une variable afin de l'éditer sur latex.

install.packages("xtable") # On installe le package nécessaire pour éditer sur 
# latex.
library(xtable)
# La commande suivante montre le code latex.
my_table <- xtable(stats, 
                   caption="Statistiques descriptives de la base de données")
print(my_table, caption.placement="top", include.rownames=FALSE,
      booktabs=TRUE, sanitize.colnames.function=identity)

# On calcule la skewness afin de voir l'asymétrie des variables d'intérêt.
install.packages("moments")
library(moments)
skewness(data2$Chomage)
skewness(data2$Inflation)

install.packages("corrplot")
library(corrplot)  
pc<-corrplot(cor(data2)) # On édite une matrice de corrélation que l'on 
# stocke dans une variable.

cor(Chomage,Inflation) # On calcule un coefficient de corrélation simple.

# On va désormais éditer les graphiques à l'aide ggplot.
install.packages("ggplot2")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)

# On édite un premier graphique afin de voir l'évolution du chômage dans le temps.

ggplot(data = data,aes(x=annee,y=chomage,color=annee)) +
  geom_point(color="black",size=2, shape=15) +
  geom_line(size=1)+   
  ggtitle("Evolution du chômage dans le temps (1975-2021)")+ # ajouter un titre
  scale_x_continuous(name="Année") +          # nom de l'axe x
  scale_y_continuous(name="Taux de chômage") +           # nom de l'axe y
  theme_calc()

# Le deuxième graphique.
ggplot(data = data,aes(x=annee,y=inf,color=annee)) +
  geom_point(color="black",size=2, shape=15) +
  geom_line(size=1)+      
  ggtitle("Evolution de l'inflation dans le temps (1975-2021)")+ # ajouter un titre
  scale_x_continuous(name="Année") +          # nom de l'axe x
  scale_y_continuous(name="Inflation") +           # nom de l'axe y
  theme_calc()


# III. Modèle économétrique et inférence.

# On crée les modèles économétriques.
model<-lm(Chomage~Inflation)
model2<-lm(Chomage~Inflation+Epargne)
model3<-lm(Chomage~Inflation+Epargne+PIBhab)

# On utilise la fonction stargazer pour créer une table et 
# l'importer sur latex.
install.packages("stargazer")
library(stargazer)
all<-stargazer(model,model2,model3,type="latex")

# Puis on calcule la valeur calculée de la table de student au seuil 5%.
qt(0.95,45)
# Ainsi que la valeur calculée de Fisher
qf(0.95,1,45)

# Test de Durbin-Watson afin de voir s'il y a de l'autocorrélation 
# dans nos modèles .
install.packages("lmtest")
library(lmtest)
dwtest(model)
dwtest(model2)
dwtest(model3)

# Régression log-log pour le modèle 2.
model2l<-lm(log(Chomage)~log(Inflation)+log(Epargne))
summary(model2l)

latex<-stargazer(model2l)
dwtest(model2l)

# Graphique pour le modèle 1.

ggplot(data2, aes(x =Inflation, y =Chomage))+
  geom_point(size=2) +
  geom_smooth(data=data2, method = "lm", formula = (y) ~ (x), 
              se = TRUE, level=0.99, size = 1,color="red") +
  ggtitle("Représentation graphique du modèle 1") +
  labs(x = "Taux d'inflation",
       y = "Taux de chômage") +
  theme_calc()
  
  
  # IV. Annexes
  
  # Désormais on souhaite voir si les erreurs suivent bien une loi normale 
  # pour le modèle 1 conformément aux hypothèses de Gauss-Markov.
  y_hat<-predict(model)
  y<- data2$Chomage
  res<-y-y_hat
  data2$predictions<-res
  data2
  round(mean(res)) # Moyenne qui vaut 0
  round(sd(res)) # Ecart-type de 1
  # On trace l'histogramme des résidus.
  hist(res,prob=T,main="Histogramme des résidus",
       xlab="Résidus", ylab="Densité",col="white")
  lines(density(res),col="blue")# On observe que les valeurs sont bien centrées.
  legend("topright", 
         legend = c("Histogramme", "Courbe de densité"), 
         fill = c("white", "blue"))
  # On crée un qq-plot.
  qplot(sample = res, geom = "qq") + 
    stat_qq_line() 
     
   
  