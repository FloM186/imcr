# References :
# https://medium.com/analytics-vidhya/a-fresh-start-for-r-in-vscode-ec61ed108cf6
# https://www.rdocumentation.org/packages/clValid/versions/0.6-9/topics/clValid
# https://www.rdocumentation.org/packages/clusterCrit/versions/1.2.8


#Univarié : V de cramer, tableau des profils, taille d'effet
#Univarié qualitatif

# Caractérisation partition /	Caractérisations groupes
# V de Cramer	/ Valeur test h
# Tableaux + graphiques des profils /	Valeur test phi

iris <- read.csv("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/iris_data.csv")
View(iris_data)
auto <- read.csv("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/auto-mpg.csv")
View(auto.mpg)

library(tidyverse)
library(magrittr)

str(auto)
str(iris)

iris <- iris %>% mutate(species = factor(species))
auto <- auto %>% mutate(origin = factor(origin), cyl = factor(cyl))





