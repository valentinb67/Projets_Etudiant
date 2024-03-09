library(forecast)
library(zoo)
library(readr)
library(plm)
library(imputeTS)
library(dplyr)
library(MASS)
library(lmtest)
library(sandwich)
library(survival)
library(corrplot)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(GGally)
library(AER)
library(caret)
library(fBasics)
library(FactoMineR)
library(factoextra)
library(readxl)


#####################Traitement des données#####################
#Traitons les valeurs manquantes en remplacant celles-ci par la moyenne:
Base_de_donnée_modifiee <- Base_de_donnée_modifiee %>%
  mutate(
    PIBtete = ifelse(is.na(PIBtete), mean(PIBtete, na.rm = TRUE), PIBtete),
    Inflation = ifelse(is.na(Inflation), mean(Inflation, na.rm = TRUE), Inflation),
    Chômage = ifelse(is.na(Chômage), mean(Chômage, na.rm = TRUE), Chômage))

############################Stats descriptives####################################
ggplot(Base_de_donnée_modifiee, aes(x = EntreeOCDE, y = PIBtete, color = as.factor(UE))) +
  geom_point() + # Ajoute des points à chaque donnée
  theme_minimal() + # Utilise un thème minimaliste
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Non-Membre de l'UE", "1" = "Membre de l'UE"),
                     name = "Statut UE") + # Personnalise les couleurs et les labels de la légende
  labs(title = "Entrée dans l'OCDE et PIB par tête selon le statut UE",
       x = "Année d'entrée dans l'OCDE",
       y = "PIB par tête")

ggplot(Base_de_donnée_modifiee, aes(x = Année, y = PIBtete, color = as.factor(UE))) +
  geom_point() + # Ajoute des points à chaque donnée
  theme_minimal() + # Utilise un thème minimaliste
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Non-Membre de l'UE", "1" = "Membre de l'UE"),
                     name = "Statut UE") + # Personnalise les couleurs et les labels de la légende
  labs(title = "PIB/tête des pays adhérents à l'OCDE selon leur appartenance ou nous à l'UE de 2009 à 2022",
       x = "Année",
       y = "PIB par tête")


ggplot(Base_de_donnée_modifiee, aes(x = Année, y = log(PIBtete), color = Pays)) +
  geom_line(aes(linetype = as.factor(UE)), size = 1) + # Utiliser 'linetype' basé sur 'UE_zero'
  scale_linetype_manual(values = c("1" = "solid", "0" = "dotted")) + # Personnaliser les types de lignes
  labs(x = "Année", y = "Logarithme du PIB par tête", linetype = "Membre UE")


ggplot(Base2010, aes(x = EntreeOCDE, y = log(PIBtete), color = as.factor(UE))) +
  geom_point() + # Ajoute des points à chaque donnée
  theme_minimal() + # Utilise un thème minimaliste
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Non-Membre de l'UE", "1" = "Membre de l'UE"),
                     name = "Statut UE") + # Personnalise les couleurs et les labels de la légende
  labs(title = "Entrée dans l'OCDE et PIB par tête selon le statut UE",
       x = "Année d'entrée dans l'OCDE",
       y = "logarithme du PIB par tête")



ggplot(Base2010, aes(x = EntreeOCDE, y = log(PIBtete), color = as.factor(UE), shape = as.factor(UE))) +
  geom_point() + # Ajoute des points à chaque donnée
  theme_minimal() + # Utilise un thème minimaliste
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Non-Membre de l'UE", "1" = "Membre de l'UE"),
                     name = "Statut UE") + # Personnalise les couleurs et les labels de la légende
  scale_shape_manual(values = c("0" = 4, "1" = 16), # Choix des formes pour les non-membres et membres
                     labels = c("0" = "Non-Membre de l'UE", "1" = "Membre de l'UE"),
                     name = "Statut UE") +
  labs(title = "Entrée dans l'OCDE et PIB par tête selon le statut UE",
       x = "Année d'entrée dans l'OCDE",
       y = "logarithme  du PIB par tête")

install.packages("tibble")
library(tibble) # Pour rownames_to_column

# Convertir les noms de ligne en une nouvelle colonne 'NomDeLaLigne'
Base2010 <- Base2010 %>%
  rownames_to_column(var = "NomDeLaLigne")

# Utiliser cette nouvelle colonne pour les étiquettes dans ggplot2
ggplot(Base2010, aes(x = EntreeOCDE, y = PIBtete, color = as.factor(UE), shape = as.factor(UE))) +
  geom_point() + # Ajoute des points à chaque donnée
  geom_text(aes(label = NomDeLaLigne), nudge_x = 0.5, nudge_y = 0.5, check_overlap = F, size = 3) + # Ajoute des étiquettes pour chaque observation en utilisant la nouvelle colonne 'NomDeLaLigne'
  theme_minimal() + # Utilise un thème minimaliste
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Non-Membre de l'UE en 2010", "1" = "Membre de l'UE en 2010"),
                     name = "Statut UE") + # Personnalise les couleurs et les labels de la légende
  scale_shape_manual(values = c("0" = 4, "1" = 16), # Choix des formes pour les non-membres et membres
                     labels = c("0" = "Non-Membre de l'UE", "1" = "Membre de l'UE"),
                     name = "Statut UE") +
  labs(x = "Année d'entrée dans l'OCDE",
       y = "PIB par tête du pays en 2010")







ggplot(data = Base_de_donnée_modifiee, x= Année, y = Inflation)
ggplot(data = Base_de_donnée_modifiee, x= Année, y = Inflation)

#Rendons binaire la variable "EntreeOCDE"
Base_de_donnée_modifiee$EntreeOCDEbi <- ifelse(Base_de_donnée_modifiee$EntreeOCDE > 1990, 0, 1)
summary(Base_de_donnée_modifiee)

pays_UE_EntreeOCDEbi <- Base_de_donnée_modifiee %>%
  dplyr::filter(UE == 1, EntreeOCDEbi == 1) %>%
  dplyr::select(Pays)
print(nrow(pays_UE_EntreeOCDEbi))

pays_EntreeOCDEbi <- Base_de_donnée_modifiee %>%
  dplyr::filter(UE == 0, EntreeOCDEbi == 1) %>%
  dplyr::select(Pays)
print(nrow(pays_EntreeOCDEbi))

pays_UE <- Base_de_donnée_modifiee %>%
  dplyr::filter(UE == 1, EntreeOCDEbi == 0) %>%
  dplyr::select(Pays)
print(nrow(pays_UE))

pays_ <- Base_de_donnée_modifiee %>%
  dplyr::filter(UE == 0, EntreeOCDEbi == 0) %>%
  dplyr::select(Pays)
print(nrow(pays_))

# Calculer le nombre de pays pour chaque catégorie
n_pays_UE_EntreeOCDEbi <- nrow(pays_UE_EntreeOCDEbi)
n_pays_EntreeOCDEbi <- nrow(pays_EntreeOCDEbi)
n_pays_UE <- nrow(pays_UE)
n_pays_ <- nrow(pays_)

# Créer un tibble pour stocker ces informations
recap_table <- tibble(
  Categorie = c("UE & EntreeOCDEbi", "Non UE & EntreeOCDEbi", "UE & Non EntreeOCDEbi", "Non UE & Non EntreeOCDEbi"),
  Nombre_de_pays = c(n_pays_UE_EntreeOCDEbi, n_pays_EntreeOCDEbi, n_pays_UE, n_pays_)
)

# Afficher le tableau
knitr::kable(recap_table, format= "latex", caption = "Récapitulatif du nombre de pays par catégorie")
knitr::kable(recap_table, format= "latex", caption = "Récapitulatif du nombre de pays par catégorie")
mean(df$UE)


129/(129+207)

nrow(pays_UE_EntreeOCDEbi)/nrow(pays_UE_EntreeOCDEbi)+nrow(pays_EntreeOCDEbi)

#Supprimons les variables de types de characters afin de faire de l'analyse descriptive
df <- Base_de_donnée_modifiee[ , !(names(Base_de_donnée_modifiee) %in% c("Pays", "RegionMonde","OCDE","FondaUE","EntreeOCDEbi","FondaOCDE"))]
summary(df)
  #Tableau de corrélation
corrplot(cor(pdf), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.8)
ggpairs(df)

?ggpairs
#Créons le dataframe adéquat pour appliquer les pannels
pdf <-pdata.frame(Base_de_donnée_modifiee, index = c("Pays", "Année"), 
                      drop.index = TRUE, row.names= TRUE)
pdf <- pdf[ , !(names(pdf) %in% c("RegionMonde","OCDE","FondaUE","FondaOCDE"))]
summary(pdf)
pdf2 <- pdf[ , !(names(pdf) %in% c("EntreeOCDEbi","Change"))]
corrplot(cor(pdf2), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.8)
ggpairs(pdf2)

#####################Modèles sans covid############################
#OLS 
pOLS <- lm(log(PIBtete) ~ Chômage+Inflation 
           + EntreeOCDEbi+ UE + DepSanté 
           + RD +Change+ FBCF,
            data = pdf)
vif(pOLS)
bp(pOLS)
summary(pOLS)
#Between
  #Between_i
B_i <- plm(log(PIBtete) ~ Chômage+Inflation 
           + EntreeOCDEbi+ UE + DepSanté 
           + RD + FBCF, 
                       data = pdf, effect ="indiv",model = "between")
summary(B_i)
  #Between_t
B_t <- plm(log(PIBtete) ~ Chômage+Inflation 
           + EntreeOCDEbi+ UE + DepSanté 
           + RD + FBCF, 
                       data = pdf, effect ="time",model = "between")
summary(B_t)
#Within
  #Within_i
W_i<- plm(log(PIBtete) ~ Chômage+Inflation 
          + EntreeOCDEbi+ UE + DepSanté 
          + RD + FBCF, 
                     data = pdf, effect ="indiv",model = "within")
summary(W_i)
  #Within_t
W_t<- plm(log(PIBtete) ~ Chômage+Inflation 
          + EntreeOCDEbi+ UE + DepSanté 
          + RD + FBCF, 
                     data = pdf, effect ="time",model = "within")
summary(W_t)
  #Within_it
W_it <- plm(log(PIBtete) ~ Chômage+Inflation 
            + EntreeOCDEbi+ UE + DepSanté 
            + RD + FBCF, 
                       data = pdf, effect ="twoways",model = "within")
summary(W_it)
#GLS (Random effect) 
  #GLS_i
g_i <- plm(log(PIBtete) ~ Chômage+Inflation 
           + EntreeOCDEbi+ UE + DepSanté 
           + RD + FBCF,
             data= pdf, effect="indiv", model = "random")
summary(g_i)
  #GLS_t
g_t <- plm(log(PIBtete) ~ Chômage+Inflation 
           + EntreeOCDEbi+ UE + DepSanté 
           + RD + FBCF,
             data= pdf, effect="time", model = "random")
summary(g_t)
  #GLS_it
g_it <- plm(log(PIBtete) ~ Chômage+Inflation 
            + EntreeOCDEbi+ UE + DepSanté 
            + RD + FBCF,
              data= pdf, effect="twoways", model = "random")
summary(g_it)
#Régression augmentée?

######################Modèles avec covid############################
#OLScovid
OLSc <- lm(log(PIBtete) ~ Chômage+Inflation 
            + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
            + RD +Change+ FBCF,data = pdf)
vif(OLSc)
vif(GLSc_it)
vif(OLS)
vif(W_it)
  #Multicolinéarité -> Covid
summary(pOLSc)

#Betweencovid
  #Between_covid_i
Bc_i <- plm(log(PIBtete) ~ Chômage+Inflation 
            + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
            + RD +Change+ FBCF, 
                      data = pdf, effect ="indiv",model = "between")
summary(Bc_i)

  #Between_covid_t
Bc_t <- plm(log(PIBtete) ~ Chômage+Inflation 
            + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
            + RD +Change+ FBCF, 
    data = pdf, effect ="time",model = "between")
summary(Bc_t)
#Within_covid
  #Within_covid_i
Wc_i<- plm(log(PIBtete) ~ Chômage + Inflation+ DepSanté +
             + RD + FBCF + Covid*EntreeOCDEbi+ Covid*UE, 
                     data = pdf, effect ="indiv",model = "within")
summary(Wc_i)
  #Within_covid_t
Wc_t<- plm(log(PIBtete) ~ Chômage + Inflation+ DepSanté +
             + RD + FBCF + Covid*EntreeOCDEbi+ Covid*UE, 
                     data = pdf, effect ="time",model = "within")
summary(Wc_t)
  #Within_covid_it
Wc_it <- plm(log(PIBtete) ~ Chômage + Inflation+ DepSanté +
             + RD + FBCF + Covid*EntreeOCDEbi+ Covid*UE, 
             data = pdf, effect ="twoways",model = "within")
summary(Wc_it)

#GLS (Random effect) covid
  #GLS_covid_i
gc_i <- plm(log(PIBtete) ~ Chômage+Inflation 
            + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
            + RD +Change+ FBCF,
            data= pdf, effect="indiv", model = "random")
summary(gc_i)
  #GLS_covid_t
gc_t <- plm(log(PIBtete) ~ Chômage + Inflation+ DepSanté +
              + RD + FBCF + Covid*EntreeOCDEbi+ Covid*UE,
            data= pdf, effect="time", model = "random")
summary(gc_t)

  #GLS_covid_it
gc_it <- plm(log(PIBtete) ~ Chômage + Inflation+ DepSanté +
               + RD + FBCF + Covid*EntreeOCDEbi+ Covid*UE,
             data= pdf, effect="twoways", model = "random")
summary(gc_it)
vif(gc_it)

#Régression augmentée covid ?
#Test sans covid 
#BP test
#H_0: Effet indiv/time n'existe pas (pvalue=)
#H_1: Effet indiv/time existe (pvalue<) 
#-> Si H_1 pour indiv et time -> Within temp 

pFtest(log(PIBtete) ~ Chômage+Inflation 
       + EntreeOCDEbi+ UE + DepSanté 
       + RD +Change+ FBCF, 
       data = pdf, effect ="indiv")
pFtest(log(PIBtete) ~ Chômage+Inflation 
       + EntreeOCDEbi+ UE + DepSanté 
       + RD +Change+ FBCF, 
       data = pdf, effect ="time")
pFtest(log(PIBtete) ~ Chômage+Inflation 
       + EntreeOCDEbi+ UE + DepSanté 
       + RD +Change+ FBCF,
       data = pdf, effect ="twoways")



#L'effet time et indiv existent tout les deux, on peut tej le between
#On observe effectivement que twoways existe (_it)
#Hausman test
#H_O = Effet aléatoire -> GLS (pvalue=)
#H_1 = Effet fixe ->Within (pvalue<)
test_indiv <- phtest(W_i,g_i)
test_indiv
#DHO,-> GLS
test_time <- phtest(W_t,g_t)
test_time
#DH1,-> Within
test_twoways <- phtest(W_it,g_it)
test_twoways
#DH0,-> GLS
##############
#Test covid
#LM test-BP ->
plmtest(log(PIBtete) ~ Chômage+Inflation 
        + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
        + RD +Change+ FBCF,
        data = pdf, effect = "indiv", type = "bp")
#H1
plmtest(log(PIBtete) ~ Chômage+Inflation 
        + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
        + RD +Change+ FBCF,
        data = pdf, effect = "time", type = "bp")
#H1
plmtest(log(PIBtete) ~ Chômage+Inflation 
        + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
        + RD +Change+ FBCF,
        data = pdf, effect = "twoways", type = "bp")
#H1

#pFtest test -> Existance

pFtest(log(PIBtete) ~ Chômage+Inflation 
       + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
       + RD +Change+ FBCF,
       data = pdf, effect = "indiv")
#H1

pFtest(log(PIBtete) ~ Chômage+Inflation 
       + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
       + RD +Change+ FBCF,
       data = pdf, effect = "time")
#H1

pFtest(log(PIBtete) ~ Chômage+Inflation 
       + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
       + RD +Change+ FBCF,
       data = pdf, effect = "twoways")
#H1
#L'effet time et indiv existent tout les deux, on peut tej le between
#On observe effectivement que twoways existe (_it)
#Hausman test
#H_O = Effet aléatoire -> GLS
#H_1 = Effet fixe ->Within
phtest(Wc_i,gc_i)
#DHO,-> GLS aléatoire
phtest(Wc_t,gc_t)
#DH1,-> Within_t fixe
phtest(Wc_it,gc_it)
#DH0,-> GLS aléatoire


############################Stargazer##########################################
stargazer()
###OLS###
stargazer(pOLS,
          type = "text", # ou "html" ou "latex" pour une sortie dans ces formats
          title = "Résultats des modèles de panel",
          model.names = FALSE, # Mettez TRUE si vous souhaitez afficher les noms des modèles
          intercept.bottom = FALSE, # Place l'intercept en bas du tableau
          df = FALSE, # Cache les degrés de liberté si désiré
          #omit.stat = c("LL", "ser", "f"), # Omet certaines statistiques, ajustez selon les besoins
          #covariate.labels = c("Chômage", "Entree OCDE", "UE", "Inflation"), # Personnalisez les étiquettes des covariables
          label = "tab:results" # Label pour référencer le tableau dans un document LaTeX, par exemple
)
stargazer(pOLSc,
          type = "text", # ou "html" ou "latex" pour une sortie dans ces formats
          title = "Résultats des modèles de panel",
          model.names = FALSE, # Mettez TRUE si vous souhaitez afficher les noms des modèles
          intercept.bottom = FALSE, # Place l'intercept en bas du tableau
          df = FALSE, # Cache les degrés de liberté si désiré
          #omit.stat = c("LL", "ser", "f"), # Omet certaines statistiques, ajustez selon les besoins
          #covariate.labels = c("Chômage", "Entree OCDE", "UE", "Inflation"), # Personnalisez les étiquettes des covariables
          label = "tab:results" # Label pour référencer le tableau dans un document LaTeX, par exemple
)
###Sans Covid###
stargazer(pOLS, W_i, W_t,W_it, g_i,g_t,g_it,
          type = "text", # ou "html" ou "latex" pour une sortie dans ces formats
          title = "Résultats des modèles de panel",
          #model.names = FALSE, # Mettez TRUE si vous souhaitez afficher les noms des modèles
          intercept.bottom = FALSE, # Place l'intercept en bas du tableau
          df = FALSE, # Cache les degrés de liberté si désiré
          #omit.stat = c("LL", "ser", "f"), # Omet certaines statistiques, ajustez selon les besoins
          #covariate.labels = c("Chômage", "Entree OCDE", "UE", "Inflation"), # Personnalisez les étiquettes des covariables
          label = "tab:results" # Label pour référencer le tableau dans un document LaTeX, par exemple
)
###Covid###
stargazer(pOLSc, Wc_i, Wc_t, Wc_it, gc_i, gc_t, gc_it,
          type = "text", # ou "html" ou "latex" pour une sortie dans ces formats
          title = "Résultats des modèles de panel",
          model.names = FALSE, # Mettez TRUE si vous souhaitez afficher les noms des modèles
          intercept.bottom = FALSE, # Place l'intercept en bas du tableau
          df = FALSE, # Cache les degrés de liberté si désiré
          #omit.stat = c("LL", "ser", "f"), # Omet certaines statistiques, ajustez selon les besoins
          #covariate.labels = c("Chômage", "Entree OCDE", "UE", "Inflation"), # Personnalisez les étiquettes des covariables
          label = "tab:results" # Label pour référencer le tableau dans un document LaTeX, par exemple
)
#Mélange
stargazer(pOLS,pOLSc, W_it, Wc_it,g_it, gc_it,
          type = "text", # ou "html" ou "latex" pour une sortie dans ces formats
          title = "Résultats des modèles de panel",
          model.names = FALSE, # Mettez TRUE si vous souhaitez afficher les noms des modèles
          intercept.bottom = FALSE, # Place l'intercept en bas du tableau
          df = FALSE, # Cache les degrés de liberté si désiré
          #omit.stat = c("LL", "ser", "f"), # Omet certaines statistiques, ajustez selon les besoins
          #covariate.labels = c("Chômage", "Entree OCDE", "UE", "Inflation"), # Personnalisez les étiquettes des covariables
          label = "tab:results" # Label pour référencer le tableau dans un document LaTeX, par exemple
)
#Les modèles retenus:

OLS <- lm(log(PIBtete) ~ Chômage+Inflation 
           + EntreeOCDEbi+ UE + DepSanté 
           + RD + FBCF,data = pdf)
summary(OLS)
OLSc <- lm(log(PIBtete) ~ Chômage+Inflation 
           + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
           + RD + FBCF,data = pdf)
Within_it <- plm(log(PIBtete) ~ Chômage+Inflation 
                  + EntreeOCDEbi+ UE + DepSanté 
                  + RD + FBCF, 
              data = pdf, effect ="twoways",model = "within")
Withinc_it <- plm(log(PIBtete) ~ Chômage+Inflation 
                 + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
                 + RD + FBCF, 
             data = pdf, effect ="twoways",model = "within")

GLS_it<- plm(log(PIBtete) ~ Chômage+Inflation 
             + EntreeOCDEbi+ UE + DepSanté 
             + RD + FBCF,
             data= pdf, effect="twoways", model = "random")
summary(GLS_it)

GLSc_it <- plm(log(PIBtete) ~ Chômage+Inflation 
            + Covid*EntreeOCDEbi+ Covid*UE + DepSanté 
            + RD + FBCF,
            data= pdf, effect="twoways", model = "random")

vif(OLSc)
vif(Withinc_it)
vif(GLSc_it)

dwtest(OLSc)
dwtest(OLS)
pbgtest(Within_it)
pbgtest(Withinc_it)
pbgtest(GLS_it)
pbgtest(GLSc_it)



summary(OLS)
summary(OLSc)
summary(Within_it)
summary(Withinc_it)
X <-summary(GLS_it)
Y <-summary(GLSc_it)



stargazer(OLS,Within_it, GLS_it,OLSc, Withinc_it, GLSc_it,
          type = "text", # ou "html" ou "latex" pour une sortie dans ces formats
          title = "Résultats des modèles de panel",
          column.labels = c("OLS","Within_it", "GLS_it","OLSc", "Withinc_it", "GLSc_it"),
          model.names = F, # Mettez TRUE si vous souhaitez afficher les noms des modèles
          intercept.bottom = F, # Place l'intercept en bas du tableau
          df = FALSE, # Cache les degrés de liberté si désiré
          #omit.stat = c("LL", "ser", "f"), # Omet certaines statistiques, ajustez selon les besoins
          #covariate.labels = c("Chômage", "Entree OCDE", "UE", "Inflation"), # Personnalisez les étiquettes des covariables
          label = "tab:results", # Label pour référencer le tableau dans un document LaTeX, par exemple
          add.lines = list(c("Var. Idiosyncratique (GLS_it)", var_idio_GLS_it, "", "", "", ""),
                           c("Var. Individuelle (GLS_it)", var_ind_GLS_it, "", "", "", ""),
                           c("Var. Temporelle (GLS_it)", var_temp_GLS_it, "", "", "", ""),
                           c("Theta (GLS_it)", theta_GLS_it, "", "", "", ""),
                           c("Var. Idiosyncratique (GLSc_it)", "", "", var_idio_GLSc_it, "", ""),
                           c("Var. Individuelle (GLSc_it)", "", "", var_ind_GLSc_it, "", ""),
                           c("Var. Temporelle (GLSc_it)", "", "", var_temp_GLSc_it, "", ""),
                           c("Theta (GLSc_it)", "", "", theta_GLSc_it, "", "")),
          label = "tab:results"
          )

rob_se <- list(sqrt(diag(vcovHC(OLS, type = "HC1"))),
               sqrt(diag(vcovHC(OLSc, type = "HC1"))),
               sqrt(diag(vcovHC(Within_it, type = "HC1"))),
               sqrt(diag(vcovHC(Withinc_it, type = "HC1"))),
               sqrt(diag(vcovHC(GLS_it, type = "HC1"))),
               sqrt(diag(vcovHC(GLSc_it, type = "HC1"))))

                 
stargazer(list(OLS,Within_it, GLS_it,OLSc, Withinc_it, GLSc_it),title = "Résultat des estimations OLS, WITHIN et GLS à effet double avec et sans indicatrice Covid ",
          column.labels = c("OLS","Within", "GLS","OLS Covid", "Within Covid", "GLS Covid"),
          model.names = F,
          align = T, 
          type = "latex",
          no.space = F,
          intercept.bottom = F,
          df = FALSE,  
          label = "tab:results")


#Pour GLS_it
var_idio_GLS_it <- "0.003103"
var_ind_GLS_it <- "0.047220"
var_temp_GLS_it <- "0.001005"
theta_GLS_it <- "0.7239"

# Pour GLSc_it
var_idio_GLSc_it <- "0.0029941"
var_ind_GLSc_it <- "0.0478864"
var_temp_GLSc_it <- "0.0009003"
theta_GLSc_it <- "0.7147"

stargazer(list(OLS, Within_it, GLS_it, OLSc, Withinc_it, GLSc_it),
          type = "text", 
          title = "Résultats des modèles de panel",
          column.labels = c("OLS", "Within_it", "GLS_it", "OLSc", "Withinc_it", "GLSc_it"),
          model.names = FALSE, 
          intercept.bottom = FALSE, 
          df = FALSE, 
          add.lines = list(c("Var. Idiosyncratique (GLS_it)", var_idio_GLS_it, "", "", "", ""),
                           c("Var. Individuelle (GLS_it)", var_ind_GLS_it, "", "", "", ""),
                           c("Var. Temporelle (GLS_it)", var_temp_GLS_it, "", "", "", ""),
                           c("Theta (GLS_it)", theta_GLS_it, "", "", "", ""),
                           c("Var. Idiosyncratique (GLSc_it)", "", "", var_idio_GLSc_it, "", ""),
                           c("Var. Individuelle (GLSc_it)", "", "", var_ind_GLSc_it, "", ""),
                           c("Var. Temporelle (GLSc_it)", "", "", var_temp_GLSc_it, "", ""),
                           c("Theta (GLSc_it)", "", "", theta_GLSc_it, "", "")),
          label = "tab:results"
)

#Stargazer Stat descriptives:
summary(pdf)
stargazer(basicStats(df),type = "text", title = "Statistiques descriptives", summary = F)
stargazer(basicStats(df),type = "latex", title = "Statistiques descriptives", summary = F)
stargazer(basicStats(pdf),type = "text", title = "Statistiques descriptives", summary = F)
stargazer(basicStats(pdf),type = "latex", title = "Statistiques descriptives", summary = F)
stargazer(basicStats(pdf2),type = "latex", title = "Statistiques descriptives", summary = F)
stargazer(basicStats(pdf2),type = "text", title = "Statistiques descriptives", summary = F)
ggpairs(df)
ggpairs(pdf)
######
#Graphique résidu temporel: 
residus <- residuals(W_t)

residus

temps <- index(W_t)[,2]
Ti <-Base_de_donnée_modifiee$Année
summary(pdf)
moyennes <- aggregate(cbind(PIBtete, Inflation, Chômage, UE, EntreeOCDEbi, DepSanté, RD, Change, FBCF) ~ Ti, data = pdf, FUN = mean)
modele_moyennes <- lm(PIBtete ~ Inflation + Chômage +UE + RD + DepSanté,
                      data = moyennes)
alpha_hat <- coef(modele_moyennes)[1] # Intercept
alpha_hat
beta_hat <- coef(modele_moyennes)[-1] # Coefficients des variables explicatives
beta_hat
epsilon_t_hat <- moyennes$PIBtete - (alpha_hat + rowSums(sweep(moyennes[, c("Inflation", "Chômage","UE","RD","DepSanté")],2, beta_hat, "*")))
epsilon_t_hat
residus_temporels <- data.frame(Ti = moyennes$Ti, epsilon_t_hat = epsilon_t_hat)
residus_temporels

ggplot(moyennes, aes(x = Ti, y = epsilon_t_hat)) +
  geom_line(group = 1, color = "blue") +  # Trace les résidus estimés en ligne bleue
  geom_point(color = "red") +  # Ajoute des points pour chaque observation de résidu
  theme_minimal() +  # Utilise un thème minimal pour le graphique
  labs(title = "Résidus Temporels par Année",
       x = "Année",
       y = "Résidus Temporels Estimés") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

temps_c <- index(Wc_t)[,2]

moyennes <- aggregate(cbind(PIBtete, Inflation, Chômage,UE,EntreeOCDEbi,DepSanté,RD,Change,FBCF, Covid) ~ Ti, data = pdf, FUN = mean)
modele_moyennes <- lm(PIBtete ~ Inflation + Chômage +UE + RD +DepSanté + Covid,
                      data = moyennes)
alpha_hat <- coef(modele_moyennes)[1] # Intercept
alpha_hat
beta_hat <- coef(modele_moyennes)[-1] # Coefficients des variables explicatives
beta_hat
epsilon_t_hat <- moyennes$PIBtete - (alpha_hat + rowSums(sweep(moyennes[, c("Inflation", "Chômage","UE","RD","DepSanté","Covid")],2, beta_hat, "*")))
epsilon_t_hat
residus_temporels <- data.frame(Ti = moyennes$Ti, epsilon_t_hat = epsilon_t_hat)
residus_temporels


ggplot(moyennes, aes(x = Ti, y = epsilon_t_hat)) +
  geom_line(group = 1, color = "blue") +  # Trace les résidus estimés en ligne bleue
  geom_point(color = "red") +  # Ajoute des points pour chaque observation de résidu
  theme_minimal() +  # Utilise un thème minimal pour le graphique
  labs(title = "Résidus Temporels par Année avec Covid",
       x = "Année",
       y = "Résidus Temporels Estimés") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################Modèle ascendant hierarchique
Base2009 <-pdata.frame(Base_de_donnée_modifiee, index = c("Pays"), 
                       drop.index = TRUE, row.names= TRUE)
Base2009 <- subset(Base2009, Année ==2009)
Base2009 <- Base2009[ , !(names(Base2009) %in% c("RegionMonde"))]
Base2009_scaled <- scale(Base2009)
Base2009_scaled
dist_matrixBase2009 <- dist(Base2009_scaled, method = "euclidean")
dist_matrixBase2009
hcBase2009 <- hclust(dist_matrixBase2009, method = "complete")
hcBase2009
par(mar=c(5, 4, 4, 2) + 0.1)
plot(hcBase2009, hang = -1)

Base2010 <-pdata.frame(Base_de_donnée_modifiee, index = c("Pays"), 
                       drop.index = TRUE, row.names= TRUE)
Base2010 <- subset(Base2010, Année ==2010)
Base2010 <- Base2010[ , !(names(Base2010) %in% c("RegionMonde"))]
Base2010_scaled <- scale(Base2010)
Base2010_scaled
dist_matrixBase2010 <- dist(Base2010_scaled, method = "euclidean")
dist_matrixBase2010
hcBase2010 <- hclust(dist_matrixBase2010, method = "complete")
hcBase2010
par(mar=c(5, 4, 4, 2) + 0.1)
plot(hcBase2010, hang = -1)


Base2019 <-pdata.frame(Base_de_donnée_modifiee, index = c("Pays"), 
                       drop.index = TRUE, row.names= TRUE)
Base2019 <- subset(Base2019, Année ==2019)
Base2019 <- Base2019[ , !(names(Base2019) %in% c("RegionMonde"))]
Base2019_scaled <- scale(Base2019)
Base2019_scaled
dist_matrixBase2019 <- dist(Base2019_scaled, method = "euclidean")
dist_matrixBase2019
hcBase2019 <- hclust(dist_matrixBase2019, method = "complete")
hcBase2019
par(mar=c(5, 4, 4, 2) + 0.1)
plot(hcBase2019, hang = -1)

Base2021 <-pdata.frame(Base_de_donnée_modifiee, index = c("Pays"), 
                       drop.index = TRUE, row.names= TRUE)
Base2021 <- subset(Base2021, Année ==2021)
Base2021 <- Base2021[ , !(names(Base2021) %in% c("RegionMonde"))]
Base2021_scaled <- scale(Base2021)
Base2021_scaled
dist_matrixBase2021 <- dist(Base2021_scaled, method = "euclidean")
dist_matrixBase2021
hcBase2021 <- hclust(dist_matrixBase2021, method = "complete")
hcBase2021
par(mar=c(5, 4, 4, 2) + 0.1)
plot(hcBase2021, hang = -1)

Base2022 <-pdata.frame(Base_de_donnée_modifiee, index = c("Pays"), 
                       drop.index = TRUE, row.names= TRUE)
Base2022 <- subset(Base2022, Année ==2022)
Base2022 <- Base2022[ , !(names(Base2022) %in% c("RegionMonde"))]
Base2022_scaled <- scale(Base2022)
Base2022_scaled
dist_matrixBase2022 <- dist(Base2022_scaled, method = "euclidean")
dist_matrixBase2022
hcBase2022 <- hclust(dist_matrixBase2022, method = "complete")
hcBase2022
par(mar=c(5, 4, 4, 2) + 0.1)
plot(hcBase2022, hang = -1)

######################ACM

