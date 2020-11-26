#############    References + texte ####
# https://medium.com/analytics-vidhya/a-fresh-start-for-r-in-vscode-ec61ed108cf6
#Univarié : V de cramer, tableau des profils, taille d'effet
#Univarié qualitatif
# Caractérisation partition /	Caractérisations groupes
# V de Cramer	/ Valeur test h
# Tableaux + graphiques des profils /	Valeur test phi


#cramersV
#https://www.rdocumentation.org/packages/rcompanion/versions/2.3.25/topics/cramerV

#############   load packages######
#install.packages('clValid')
#install.packages("clusterCrit")
#install.packages('ClustOfVar')
library(tidyverse)
library(magrittr)
library(clValid)
library(clusterCrit)
library(ClustOfVar)
library(ggpubr)
library(factoextra)
library(ade4)

#############   import data test iris et auto#######
#data test
iris <- read.csv("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/iris_data.csv")
#View(iris)
auto <- read.csv("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/auto-mpg.csv")
#View(auto.mpg)

iris <- iris %>% mutate(species = factor(species))
auto <- auto %>% mutate(origin = factor(origin), cyl = factor(cyl))
str(auto)
str(iris)



#############   CLUSTERING MULTIVARIEE   #################
#Lorsque les variables sont toutes qualitatives, nous utilisons l’analyse des correspondances multiples (ACM).
#Lorsqu’elles sont composées d’un mélange de variables quantitatives et qualitatives, on utilise l’analyse factorielle des données mixtes (AFDM)
#qui se révèle être à la fois une généralisation de l’ACM et de l’ACP
#http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_Cat_Variable_Clustering.pd

##########quantitatif
df2 <- iris
head(df2)
str(df2)
#Calculer k-means avec k = 3
set.seed(123)
res <- kmeans(scale(df2[, -5]), 3, nstart = 25)

# Groupe de chaque individu
res.km$cluster
df2<- df2 %>% mutate(cluster=res$cluster)
#View(df2)
str(df2)
head(df2)
#df <- df%>% rename('V6' = "cluster")
#names(my_data)[names(my_data) == "Sepal.Width"] <- "sepal_width"
# names(df)[6] <- "cluster"

########## qualitatif
# test acp acm ricco
# Réduction de dimension en utilisant l'ACP
res.pca <- prcomp(df[, -5],  scale = TRUE)
# Coordonnées des individus
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Ajouter les clusters obtenus à l'aide de l'algorithme k-means
ind.coord$cluster <- factor(res.km$cluster)
# Ajouter les groupes d'espèces issues du jeu de données initial
ind.coord$Species <- df$Species
# Inspection des données
head(ind.coord)

# Pourcentage de la variance expliquée par les dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)


#############   UNIVARIE   ################
#quantitatif
## Loading required package: Ckmeans.1d.dp
#https://cran.r-project.org/web/packages/Ckmeans.1d.dp/vignettes/Ckmeans.1d.dp.html
#https://www.rdocumentation.org/packages/Ckmeans.1d.dp/versions/4.3.3/topics/Univariate%20Clustering
#install.packages('Ckmeans.1d.dp')
library(Ckmeans.1d.dp)


colnames(iris)[3]
df1<- subset(iris, select=3)
df1<- iris[,3]
result <- Ckmeans.1d.dp(df1$petal_length, 3)
plot(result)

#clusters 
result$cluster
df1 <- df1 %>% mutate(cluster=result$cluster )
head(df1)
str(df1)
#View(df1)






#############   test Clvalid##########
# https://www.rdocumentation.org/packages/clValid/versions/0.6-9/topics/clValid
library(clValid)
data(mouse)
## internal validation
express <- mouse[1:25,c("M1","M2","M3","NC1","NC2","NC3")]
rownames(express) <- mouse$ID[1:25]
intern <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                  validation="internal")
express
intern
## view results
summary(intern)
optimalScores(intern)
plot(intern)

## stability measures
stab <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                validation="stability")
optimalScores(stab)
plot(stab)

## biological measures
## first way - functional classes predetermined
fc <- tapply(rownames(express),mouse$FC[1:25], c)
fc <- fc[-match( c("EST","Unknown"), names(fc))]
bio <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
               validation="biological", annotation=fc)
optimalScores(bio)
plot(bio)

## second way - using Bioconductor
if(require("Biobase") && require("annotate") && require("GO.db") && require("moe430a.db")) {
  bio2 <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                  validation="biological",annotation="moe430a.db",GOcategory="all")
  optimalScores(bio2)
  plot(bio2)
}

#############   test cluster crit#########
# https://www.rdocumentation.org/packages/clusterCrit/versions/1.2.8



