################ fromage
fromage <- read.delim("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/fromage.txt")
str(fromage)
fromage.cr <- scale(fromage[,-1],center=T,scale=T)
d.fromage <- dist(fromage.cr)
cah.ward <- hclust(d.fromage,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)

library(clEval)
?clEval

test_value(fromage[,-1], groupes.cah)
effect_size(fromage[,-1], groupes.cah)
corr_coef(fromage[,-1], groupes.cah, show_graph = TRUE, show_conditionnal_means = TRUE)

################ iris
data(iris)
iris.cr <- scale(iris[,-5],center=T,scale=T)
d.iris <- dist(iris.cr)
cah.ward <- hclust(d.iris,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)

test_value(iris[,-5], groupes.cah, show_graph = TRUE, digits=3)
effect_size(iris[,-5], groupes.cah)
corr_coef(iris[,-5], groupes.cah, show_graph = TRUE, show_conditionnal_means = TRUE)


v.cramer(iris, groupes.cah)
h.value.test(iris, groupes.cah)
phi.value.test(iris, groupes.cah)
