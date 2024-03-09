#Code Projet Econometrie 
# Lien vers la base de donnees: https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data

install.packages("sandwich")
install.packages("survival")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("GGally")
install.packages("AER")
install.packages("corrplot")
install.packages("+.gg")
install.packages("caret")
install.packages("randomForest")
install.packages("imputeTS")
library(imputeTS)
library(dplyr)
library(MASS)
library(lmtest)
library(sandwich)
library(survival)
library(corrplot)
library(stargazer)
library(ggplot2)
library(GGally)
library(AER)
library(randomForest)
library(caret)

#Importer la base de donnees et supprimer la premiere colonne correspondant au numero de l'observation
Homeprice <- subset(VarHomeprice, select = -V1)
Homeprice <- Homeprice %>%slice(-1)

#Renommer les variables utilisees par la suite
colnames(Homeprice)[colnames(Homeprice) == "V81"] <- "Saleprice"
Homeprice <- Homeprice[, c("Saleprice", names(Homeprice)[-which(names(Homeprice) == "Saleprice")])]
summary(Homeprice)
View(Homeprice)

# Convertir les colonnes spécifiques en numérique
colonnes_a_convertir <- c("Saleprice", "V2", "V4", "V5", "V18", "V19", "V20", 
                          "V21", "V27", "V35", "V37", "V38", "V39", "V44", 
                          "V45", "V46", "V47", "V48", "V49", "V50","V51","V52",
                          "V53","V55","V57","V60","V62","V63","V67","V68","V69",
                          "V70","V71","V72","V76","V77","V78")
Homeprice[colonnes_a_convertir] <- lapply(Homeprice[colonnes_a_convertir], as.numeric)
summary(Homeprice)
numeric_Homeprice <- Homeprice %>%
  select_if(is.numeric)
summary(numeric_Homeprice)

corrplot(cor(numeric_Homeprice), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.4)

numeric_HomepriceNA <- na.omit(numeric_Homeprice)
summary(numeric_HomepriceNA)

nrow(numeric_Homeprice)
nrow(numeric_HomepriceNA)

corrplot(cor(numeric_HomepriceNA), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.4)


nonumeric_Homeprice <- Homeprice %>% 
  select_if(~ !is.numeric(.))
summary(nonumeric_Homeprice)

conditionSell <- model.matrix(~ V80 - 1, data = nonumeric_Homeprice)
zoning <- model.matrix(~ V3 - 1, data = nonumeric_Homeprice)
#Numérisation des variables ordinales, encodage par étiquette
#V8 V10 v12 V28 V29 V30 V31 V32 V33 V34 V36 V41 V42 V43 V54 V58 V59 V61 V64 V65 V66 V73

nonumeric_Homeprice$V8 <- as.integer(factor(nonumeric_Homeprice$V8, levels = c("IR3", "IR2", "IR1", "Reg")))
nonumeric_Homeprice$V10 <- as.integer(factor(nonumeric_Homeprice$V10, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub")))
nonumeric_Homeprice$V12 <- as.integer(factor(nonumeric_Homeprice$V12, levels = c("Sev", "Mod", "Gtl")))
nonumeric_Homeprice$V28 <- as.integer(factor(nonumeric_Homeprice$V28, levels = c("Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V29 <- as.integer(factor(nonumeric_Homeprice$V29, levels = c("Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V30 <- as.integer(factor(nonumeric_Homeprice$V30, levels = c("Wood", "Stone", "Slab","PConc","CBlock","BrkTil")))
nonumeric_Homeprice$V31 <- as.integer(factor(nonumeric_Homeprice$V31, levels = c("NA","Po", "Fa", "TA","Gd","Ex"))) #Tranches
nonumeric_Homeprice$V32 <- as.integer(factor(nonumeric_Homeprice$V32, levels = c("NA","Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V33 <- as.integer(factor(nonumeric_Homeprice$V33, levels = c("NA","Po", "Fa", "TA","Gd")))
nonumeric_Homeprice$V34 <- as.integer(factor(nonumeric_Homeprice$V34, levels = c("NA","Unf", "LwQ", "Rec","BLQ","ALQ","GLQ")))
nonumeric_Homeprice$V36 <- as.integer(factor(nonumeric_Homeprice$V36, levels = c("NA","Unf", "LwQ", "Rec","BLQ","ALQ","GLQ")))
nonumeric_Homeprice$V41 <- as.integer(factor(nonumeric_Homeprice$V41, levels = c("Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V42 <- ifelse(nonumeric_Homeprice$V42 == "Y", 1, ifelse(nonumeric_Homeprice$V42 == "N", 0, nonumeric_Homeprice$V42))
nonumeric_Homeprice$V42 <- as.numeric(nonumeric_Homeprice$V42)
nonumeric_Homeprice$V43 <- as.integer(factor(nonumeric_Homeprice$V43, levels = c("Mix", "FuseP", "FuseF","FuseA","SBrkr")))
nonumeric_Homeprice$V54 <- as.integer(factor(nonumeric_Homeprice$V54, levels = c("Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V58 <- as.integer(factor(nonumeric_Homeprice$V58, levels = c("NA","Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V59 <- as.integer(factor(nonumeric_Homeprice$V59, levels = c("NA","Detchd", "CarPort", "BuiltIn","Basment","Attchd","2Types"))) #Garage oui ou non et type de garage
nonumeric_Homeprice$V61 <- as.integer(factor(nonumeric_Homeprice$V61, levels = c("NA", "Unf", "RFn", "Fin"))) #Degré auquel le garage est construit
nonumeric_Homeprice$V64 <- as.integer(factor(nonumeric_Homeprice$V64, levels = c("NA","Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V65 <- as.integer(factor(nonumeric_Homeprice$V65, levels = c("NA","Po", "Fa", "TA","Gd","Ex")))
nonumeric_Homeprice$V66 <- as.integer(factor(nonumeric_Homeprice$V66, levels = c("N", "P", "Y")))
nonumeric_Homeprice$V73 <- as.integer(factor(nonumeric_Homeprice$V73, levels = c("NA", "Fa", "TA","Gd","Ex")))

#Variable Quartier 
Quartier <- as.character(nonumeric_Homeprice$V3)

#Creer une base de donnees avec les variables numerisees

selected_columns2 <- nonumeric_Homeprice[, c("V8", "V10", "V12", "V28", "V29", "V30", "V31", "V32", "V33", "V34", "V36", "V41", "V42", "V43", "V54", "V58", "V59", "V61", "V64", "V65", "V66", "V73", "V80")]
noetiqnumeric_Homeprice <- selected_columns2
summary(noetiqnumeric_Homeprice)


DF2 <- data.frame(noetiqnumeric_Homeprice, zoning)
DF <- data.frame(numeric_Homeprice)

DF$V20.5 <- pmax(DF$V20, DF$V21, na.rm = TRUE)
summary(DF)

DFNA <- imputeTS::na_mean(DF)
summary(Quartier)
#Base de donnees avec les variables numeriques et le facteur V3
DFNAA<-data.frame(DFNA,Quartier,zoning,conditionSell)
DFNAA$Quartier <- as.character(DFNAA$Quartier)
niveaux_quartier <- c("RL", "RH", "RM", "C", "FV")
DFNAA$Quartier <- factor(DFNAA$Quartier, levels = niveaux_quartier)
str(DFNAA$Quartier)
DFNAA_sans_na_quartier <- DFNAA[complete.cases(DFNAA$Quartier), ]
dummy_quartier <- model.matrix(~ Quartier - 1, data = DFNAA)
colnames(dummy_quartier) <- c("Quartier_RL", "Quartier_RH", "Quartier_RM", "Quartier_C", "Quartier_FV")
DFNAA <- cbind(DFNAA_sans_na_quartier, dummy_quartier)
DFNAA <- DFNAA[, !names(DFNAA) %in% "Quartier"]
View(DFNAA)
#Renommer les variables utilisees par la suite

Prix<- DFNAA$Saleprice
Qualite<- DFNAA$V18
Renovation <- DFNAA$V21
Garage<-DFNAA$V62
Surface<- DFNAA$V47
V80Partial <- DFNAA$V80Partial

 #Specification OLS
colnames(DFNAA)
graph <- DFNAA[, c("Saleprice", "V18","V20","V78", "V21", "V62", "V47", "V80Partial")]
ggpairs(graph)

MCO<- lm(Prix~ Qualite+Renovation+Garage+Surface, data=DFNAA) 
summary(MCO)
bptest(MCO) #Détection d'hétéroscedasticité
dwtest(MCO)

MCOc<- lm(Prix~ Qualite+Renovation+Garage+Surface + V80Partial, data=DFNAA) 
summary(MCOc)
bptest(MCOc)
dwtest(MCOc)

MCOlog<- lm(log(Prix)~ Qualite+Renovation+Garage+Surface + V80Partial, data=DFNAA) 
summary(MCOlog)
bptest(MCOlog)
dwtest(MCOlog)

summary(conditionSell)
summary(zoning)

# Sélectionner les colonnes pertinentes
variables_selectionnees <- DFNAA[, c("Saleprice", "V80Abnorml", "V80AdjLand", "V80Alloca", "V80Family", "V80Normal", "V80Partial")]
# Calculer la matrice de corrélation
matrice_correlation <- cor(variables_selectionnees, use = "complete.obs")
# Afficher la matrice de corrélation
print(matrice_correlation)

#DFNAA$Abnormal <- DFNAA$V80Abnorml + DFNAA$V80Adjand + DFNAA$V80Alloca
#DFNAA$Abnormal <- rowSums(DFNAA[, c("V80Abnorml", "V80Adjand", "V80Alloca")], na.rm = TRUE)
head(DFNAA)

#cor(Prix, DFNAA$V80Abnorml, DFNAA$V80Adjand, DFNAA$V80Alloca,DFNAA$V80Family,DFNAA$V80Normal,DFNAA$V80Partial)

#Potentiel problème d'endogénéité pour la surface habitable au-dessus du sol en raison d'une variable omise: Lors de la construction, le terrain 
#sur lequel la maison est construite était inclu dans SFHA ou pas? (special flood hasard area)/variable binaire Si oui => reglementations spécifiques en matière de construction
#=> diminution de la surface habitable en raison des mesures specifiques pour minimiser le risque d'inondation (elevation de la fondation,colonnes de soutien etc.)
#=> impact sur le prix de vente par le biais de l'attractivité de la zone et du niveau de sécurité (si on sait que lors de la construction le terrain était inclu; cela pourrait réduire le prix de vente
#car cela diminue l'attractivité de la zone)

#Variable instrumentale Localisation de la salle de bain(1 s'il y a au moins une salle de bain au dessus du sol, 0 sinon) 
DFNAA$V51 <- as.numeric(as.character(DFNAA$V51))
DFNAA$V51_binaire <- ifelse(!is.na(DFNAA$V51) & DFNAA$V51 >= 1, 1, 0)
Localisation_SB<-DFNAA$V51_binaire

#Specification IV
VIreg1 <- ivreg( Prix~ Qualite+Renovation+Garage+Surface| . - Surface+Localisation_SB, data=DFNAA) 
bptest(VIreg1)
coeftest(VIreg1, vcov. = vcovHC, type = "HC0")
summary(VIreg1, vcov = sandwich, df = Inf, diagnostics = TRUE)

VIreg1c <- ivreg( Prix~ Qualite+Renovation+Garage+Surface+V80Partial| . - Surface+Localisation_SB, data=DFNAA) 
bptest(VIreg1c)
coeftest(VIreg1c, vcov. = vcovHC, type = "HC0")
summary(VIreg1c, vcov = sandwich, df = Inf, diagnostics = TRUE)

VIreg1log <- ivreg( log(Prix)~ Qualite+Renovation+Garage+Surface+V80Partial| . - Surface+Localisation_SB, data=DFNAA) 
bptest(VIreg1log)
coeftest(VIreg1log, vcov. = vcovHC, type = "HC0")
summary(VIreg1log, vcov = sandwich, df = Inf, diagnostics = TRUE)

#First stage equation
Firststage<- lm(Surface~Localisation_SB+Renovation+Renovation+Garage, data=DFNAA)
summary(Firststage)

#Spécification IV avec des dummies (RL-quartier de référence)
VIreg2 <- ivreg( Prix~ Quartier_RM+Quartier_RH+Quartier_FV+Qualite+Renovation+Garage+Surface | . - Surface+Localisation_SB, data=DFNAA) 
bptest(VIreg2)
coeftest(VIreg2, vcov. = vcovHC, type = "HC0")
summary(VIreg2, vcov = sandwich, df = Inf, diagnostics = TRUE)

VIreg2c <- ivreg( Prix~ Quartier_RM+Quartier_RH+Quartier_FV+Qualite+Renovation+Garage+Surface +V80Partial | . - Surface+Localisation_SB, data=DFNAA) 
bptest(VIreg2c)
coeftest(VIreg2c, vcov. = vcovHC, type = "HC0")
summary(VIreg2c, vcov = sandwich, df = Inf, diagnostics = TRUE)

VIreg2log <- ivreg( log(Prix)~ Quartier_RM+Quartier_RH+Quartier_FV+Qualite+Renovation+Garage+Surface +V80Partial | . - Surface+Localisation_SB, data=DFNAA) 
bptest(VIreg2log)
coeftest(VIreg2log, vcov. = vcovHC, type = "HC0")
summary(VIreg2log, vcov = sandwich, df = Inf, diagnostics = TRUE)


#Tableau de synthese
stargazer(
  MCOc,VIreg1c, VIreg2c,
  title = "Régressions MCO, IV et IV_dummies",
  align = TRUE,
  type = "text",
  column.labels = c("MCO", "IV", " IV Dummies"))
stargazer(
  MCOc,VIreg1c, VIreg2c,
  title = "Régressions MCO, IV et IV_dummies",
  align = TRUE,
  type = "latex",
  column.labels = c("MCO", "IV", " IV Dummies"))

#Spécification pour chaque quartier: soustraction des bases de donnees pour chaque quartier 
data_RM <- filter(DFNAA, V3RM == 1)
View(data_RM)
data_RH <- filter(DFNAA, V3RH == 1)
data_FV <- filter(DFNAA, V3FV == 1)
data_RL <-filter(DFNAA, V3RL ==1)

#Specification IV Quartier RM
data_RM$V51 <- as.numeric(as.character(data_RM$V51))
data_RM$V51_binaire <- ifelse(!is.na(data_RM$V51) & data_RM$V51 >= 1, 1, 0)
cPartial <-data_RM$V80Partial
Localisation_SB<-data_RM$V51_binaire
Qualite<- data_RM$V18
Renovation <-data_RM$V21
Garage<-data_RM$V62
Surface<-data_RM$V47
Prix <- data_RM$Saleprice

IV_RM<- ivreg( Prix~ Qualite+Renovation+Garage+Surface| . - Surface+Localisation_SB, data=data_RM)
bptest(IV_RM)
coeftest(IV_RM, vcov. = vcovHC, type = "HC0")
summary(IV_RM, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_RMc<- ivreg( Prix~ Qualite+Renovation+Garage+Surface + cPartial | . - Surface+Localisation_SB, data=data_RM)
bptest(IV_RMc)
coeftest(IV_RMc, vcov. = vcovHC, type = "HC0")
summary(IV_RMc, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_RMlog<- ivreg( log(Prix)~ Qualite+Renovation+Garage+Surface + cPartial | . - Surface+Localisation_SB, data=data_RM)
bptest(IV_RMlog)
coeftest(IV_RMlog, vcov. = vcovHC, type = "HC0")
summary(IV_RMlog, vcov = sandwich, df = Inf, diagnostics = TRUE)

#Specification IV quartier RL
data_RL$V51 <- as.numeric(as.character(data_RL$V51))
data_RL$V51_binaire <- ifelse(!is.na(data_RL$V51) & data_RL$V51 >= 1, 1, 0)
cPartial <-data_RL$V80Partial
Localisation_SB<-data_RL$V51_binaire
Qualite<- data_RL$V18
Renovation <-data_RL$V21
Garage<-data_RL$V62
Surface<-data_RL$V47
Prix <- data_RL$Saleprice

IV_RL<- ivreg( Prix~Qualite+ Renovation+Garage+Surface | . - Surface+Localisation_SB, data=data_RL)
bptest(IV_RL)
coeftest(IV_RL, vcov. = vcovHC, type = "HC0")
summary(IV_RL, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_RLc<- ivreg( Prix~Qualite+ Renovation +Garage+Surface + cPartial | . - Surface+Localisation_SB, data=data_RL)
bptest(IV_RLc)
coeftest(IV_RLc, vcov. = vcovHC, type = "HC0")
summary(IV_RLc, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_RLlog<- ivreg( log(Prix)~Qualite+ Renovation +Garage+Surface + cPartial | . - Surface+Localisation_SB, data=data_RL)
bptest(IV_RLlog)
coeftest(IV_RLlog, vcov. = vcovHC, type = "HC0")
summary(IV_RLlog, vcov = sandwich, df = Inf, diagnostics = TRUE)

cor(data_RL$V51_binaire, data_RL$Saleprice)
cor(data_RL$V51_binaire,data_RL$V47)


#Specification IV quartier RH
data_RH$V51 <- as.numeric(as.character(data_RH$V51))
data_RH$V51_binaire <- ifelse(!is.na(data_RH$V51) & data_RH$V51 >= 1, 1, 0)
cPartial<-data_RH$V80Partial
Localisation_SB<-data_RH$V51_binaire
Qualite<- data_RH$V18
Renovation <-data_RH$V21
Garage<-data_RH$V62
Surface<-data_RH$V47
Prix <- data_RH$Saleprice

IV_RH<- ivreg( Prix~Qualite+ Renovation+Garage+Surface | . - Surface+Localisation_SB, data=data_RH)
bptest(IV_RH)
coeftest(IV_RH, vcov. = vcovHC, type = "HC0")
summary(IV_RH, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_RHc<- ivreg( Prix~Qualite+ Renovation+Garage+Surface+ cPartial | . - Surface+Localisation_SB, data=data_RH)
bptest(IV_RHc)
coeftest(IV_RHc, vcov. = vcovHC, type = "HC0")
summary(IV_RHc, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_RHclog <- ivreg( log(Prix)~Qualite+ Renovation+Garage+Surface+ cPartial | . - Surface+Localisation_SB, data=data_RH)
bptest(IV_RHclog)
coeftest(IV_RHclog, vcov. = vcovHC, type = "HC0")
summary(IV_RHclog, vcov = sandwich, df = Inf, diagnostics = TRUE)


#Specification IV quartier FV
data_FV$V51 <- as.numeric(as.character(data_FV$V51))
data_FV$V51_binaire <- ifelse(!is.na(data_FV$V51) & data_FV$V51 >= 1, 1, 0)
cPartial<-data_FV$V80Partial
Localisation_SB<-data_FV$V51_binaire
Qualite<- data_FV$V18
Renovation <-data_FV$V21
Garage<-data_FV$V62
Surface<-data_FV$V47
Prix <- data_FV$Saleprice

IV_FV<- ivreg( Prix~Qualite+ Renovation+Garage+Surface | . - Surface+Localisation_SB, data=data_FV)
bptest(IV_FV)
coeftest(IV_FV, vcov. = vcovHC, type = "HC0")
summary(IV_FV, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_FVc<- ivreg( Prix~Qualite+ Renovation+Garage+Surface+ cPartial | . - Surface+Localisation_SB, data=data_FV)
bptest(IV_FVc)
coeftest(IV_FVc, vcov. = vcovHC, type = "HC0")
summary(IV_FVc, vcov = sandwich, df = Inf, diagnostics = TRUE)

IV_FVlog<- ivreg( log(Prix)~Qualite+ Renovation+Garage+Surface+ cPartial | . - Surface+Localisation_SB, data=data_FV)
bptest(IV_FVlog)
coeftest(IV_FVlog, vcov. = vcovHC, type = "HC0")
summary(IV_FVlog, vcov = sandwich, df = Inf, diagnostics = TRUE)

#Tableau IV par quartier
stargazer(
  IV_RL, IV_RM, IV_RH, IV_FV, 
  title = "Régressions IV par quartier",
  align = TRUE,
  type = "text",
  column.labels = c("RL", "RM", "RH", "FV"))

stargazer(
  IV_RLc, IV_RMc, IV_RHc, IV_FVc, 
  title = "Régressions IV par quartier",
  align = TRUE,
  type = "text",
  column.labels = c("RLc", "RMc", "RHc", "FVc"))
stargazer(
  IV_RLc, IV_RMc, IV_RHc, IV_FVc, 
  title = "Régressions IV par quartier",
  align = TRUE,
  type = "latex",
  column.labels = c("RL", "RM", "RH", "FV"))


#TABLEAU DE SYNTHESE IV dummies et IV par quartier
library(stargazer)
stargazer(
  IV_RL, IV_RM, IV_RH, IV_FV, VIreg2,
  title = "Régressions IV par quartier et IV_dummies",
  align = TRUE,
  type = "text",
  column.labels = c("RL", "RM", "RH", "FV", "IV Dummies")
)

stargazer(
  IV_RLc, IV_RMc, IV_RHc, IV_FVc, VIreg2c,
  title = "Régressions IV par quartier et IV_dummies",
  align = TRUE,
  type = "text",
  column.labels = c("RLc", "RMc", "RHc", "FVc", "IV Dummies")
)

library(stargazer)
stargazer(
  IV_RL, IV_RM, IV_RH, IV_FV, VIreg2,
  title = "Régressions IV par quartier et IV_dummies",
  align = TRUE,
  type = "latex",
  column.labels = c("RL", "RM", "RH", "FV", " IV Dummies"))
stargazer(
  IV_RLc, IV_RMc, IV_RHc, VIreg2c,
  title = "Régressions IV par quartier et IV_dummies",
  align = TRUE,
  type = "latex",
  column.labels = c("RL", "RM", "RH", "FV", "IV Dummies")
)

#Graphiques pour présentation:
graph <- DFNAA[, c("Saleprice", "V18","V20","V78", "V21", "V62", "V47", "V80Partial")]
ggpairs(graph)

plot(DFNAA$V20)

CorrModel <- DFNAA[, c("Saleprice", "V18","V21","V47","V51_binaire","V62", 
                       "V3RL", "V3RM", "V3RH", "V3FV", "V80Abnorml","V80AdjLand"
                       ,"V80Alloca","V80Family","V80Normal","V80Partial")]
corrplot(cor(CorrModel), method = 'square',  diag = F, addCoef.col ='black'
         , number.cex = 0.4)

corrplot(cor(DFNAA), method = 'square',  diag = F, addCoef.col ='black'
         , number.cex = 0.4)

#Graphiques résidus:
ggplot(data.frame(residus = resid(MCOc), valeurs_ajustees = fitted(MCOc)), aes(x = valeurs_ajustees, y = residus)) +
  geom_point() +
  labs(title = "Nuage de points des résidus", x = "Valeurs ajustées", y = "Résidus")

ggplot(data.frame(residus = resid(MCOc)), aes(x = residus)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Histogramme des résidus", x = "Résidus", y = "Fréquence")

ggplot(data.frame(residus = resid(MCOc)), aes(sample = residus)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "Diagramme Q-Q des résidus", x = "Quantiles théoriques", y = "Quantiles observés")

#Résidus de tout les modèles sur un graphique:
residus_MCOc <- residuals(MCOc)
residus_VIreg1c <- residuals(VIreg1c)
residus_VIreg2c <- residuals(VIreg2c)
residus_IV_RLc <- residuals(IV_RLc)
residus_IV_RMc <- residuals(IV_RMc)
residus_IV_RHc <- residuals(IV_RHc)
residus_IV_FVc <- residuals(IV_FVc)

plot(residus_MCOc)
plot(residus_VIreg1c)
plot(residus_VIreg2c)
plot(residus_IV_RLc)
plot(residus_IV_RMc)
plot(residus_IV_RHc)
plot(residus_IV_FVc)

df_residus <- data.frame(
  Model = rep(c("MCO", "IVreg1", "IVreg2", "IV_RL", "IV_RM", "IV_RH", "IV_FV"), 
              c(length(residus_MCOc), 
                length(residus_VIreg1c), 
                length(residus_VIreg2c),
                length(residus_IV_RLc),
                length(residus_IV_RMc),
                length(residus_IV_RHc),
                length(residus_IV_FVc))),
  Residus = c(residus_MCOc, 
              residus_VIreg1c,
              residus_VIreg2c,
              residus_IV_RLc,
              residus_IV_RMc,
              residus_IV_RHc,
              residus_IV_FVc)
)

ggplot(df_residus, aes(x = Model, y = Residus)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Répartition des résidus pour chacune des estimations", x = "Modèle", y = "Résidus")

#Conditions de vente
  #Fréquence conditions de vente
  prop.table(table(Homeprice$V80))
  ggplot(Homeprice, aes(x = factor(V80))) +
    geom_bar(stat = "count", fill = "red") +
    labs(title = "Fréquence des conditions de vente", 
         x = "Valeurs des conditions de vente", 
         y = "Fréquence")
  #Matrice de corrélation condition de vente:
  corrplot(cor(conditionSell), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.4)
  #Graphc
  graphc <- DFNAA[, c("Saleprice", "V80Abnorml","V80AdjLand"
                      ,"V80Alloca","V80Family","V80Normal","V80Partial","V78")]
  ggpairs(graphc)
  
  
  
#Quartier
  #Fréquence quartiers
  prop.table(table(Homeprice$V3))
  ggplot(Homeprice, aes(x = factor(V3))) +
    geom_bar(stat = "count", fill = "blue") +
    labs(title = "Fréquence des quartiers", x = "Valeurs des quartiers", y = "Fréquence")
  #Matrice de corrélation quartier:
  CorrQ <- DFNAA[, c("Saleprice", "V3RL", "V3RM", "V3RH", "V3FV")]
  corrplot(cor(CorrQ), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.4)
  #Graphq
  graphq <- DFNAA[, c("Saleprice", "V3RL", "V3RM", "V3RH", "V3FV","V78")]
  ggpairs(graphq)
#Matrice de corrélation X_i et Z_i:
Corrxz <- DFNAA[, c("Saleprice", "V18","V21","V47","V51_binaire","V62")]
corrplot(cor(Corrxz), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.4)

corrplot(cor(DFNAA), method = 'square',  diag = F, addCoef.col ='black', number.cex = 0.4)


prop.table(table(Homeprice$V20))
ggplot(Homeprice, aes(x = factor(V20))) +
  geom_bar(stat = "count", fill = "blue") +
  labs(title = "Fréquence de construction", x = "Année de construction (1872-2010)", y = "Fréquence")


sampled_years <- sample(unique(Homeprice$V20), 50)

# Créer un graphique de barres avec échantillonnage des années
ggplot(subset(Homeprice, V20 %in% sampled_years), aes(x = factor(V20))) +
  geom_bar(stat = "count", fill = "blue") +
  labs(title = "Nombre de construction dans l'échantillon", x = "Année de construction (échantillonnage)", y = "Fréquence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclinaison des étiquettes pour une meilleure lisibilité

