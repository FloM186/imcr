################Importation package
#Import sur github -> manip a faire ?
install.packages("devtools")
library("devtools")
install_github("flom186/projetR")

#Library
library(imcr)

#Documentation of the package
?imcr

################ Analyse univari?e qualitative avec le dataset auto
data = read.delim("http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/autos_caracterisation.txt", header=T,sep = "\t")
data = data[,-1]
data2 = data2[,-c(8:11)]
data2 = scale(data2,center=T,scale=T)
d = dist(data2)
cah = hclust(d, method="ward.D2")
plot(cah)
rect.hclust(cah,k=4)
classe <- cutree(cah,k=4)
classe = unname(classe)
data = data[,10:11]

#Function for Cramer's v
v.cramer(data, classe, show_graph = T, digits=4)

#Function for Row percentages of frenquency table
l.profil(data, classe, show_graph = T, digits=4)

#Function for Columns percentages of frenquency table
cl.profil(data, classe, show_graph = T, digits=4)

#Function for effect size for proportion comparison
h.value.test(data, classe, show_graph = T, digits=4)

#Function for effect size expressed by correlation
phi.value.test(data, classe, show_graph = T, digits=4)

#Fonction uni.quali
res<-uni.quali(data, classe, show_graph = T, digits=4)

#Gestion of error
res<-uni.quali(data[-1,], classe, show_graph = T, digits=4)
#Error because their is not enough row in active_variables

################ Analyse univari?e quantitative avec le dataset iris
#Upload dataset iris
data(iris)

#CAH
iris.cr <- scale(iris[,-5],center=T,scale=T)
d.iris <- dist(iris.cr)
cah.ward <- hclust(d.iris,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)

#Function for effect size
effect_size(iris[,-5],groupes.cah)

#Function for correlation index
corr_coef(iris[,-5],groupes.cah)

#Function for test value
test_value(iris[,-5],groupes.cah)

#Fonction uni.quanti
res<-uni.quanti(iris[,-5],groupes.cah)
res$effect_size$d.cohen

#Gestion of error
res<-uni.quanti(sapply(iris[,-5], as.character),groupes.cah)
#Eror because it doesn't work with qualitatives variables

################ Analyse multivari? quantitative avec le dataset villes universitaire
#Import dataset villes universitaires
villes_universitaires<-read.csv2("D:/M2-SISE/Prog_Stat_R/PROJET/tutorial/data/Villes universitaires.csv",header=TRUE,row.names="Villes")

#Realisation of a CAH on the data
d.active<-villes_universitaires[,1:9]
active.cr <- scale(d.active,center=T,scale=T)
d.active <- dist(active.cr)
cah.ward <- hclust(d.active,method="ward.D2")

#Division on 4 clusters
groupes.cah <- cutree(cah.ward,k=4)

#Data frame use for multivarial analysis
d.active<-villes_universitaires[,1:10]

#Execution of multi.quanti function
res<-multi.quanti(d.active,groupes.cah,10,show_graph=TRUE)

#Table of eigen values
res$eig.values

#Table of coord,contrib and cos2 by var for the first dimension
res$var.tab$Dim1

#Table of correlation between dimension and clusters
res$correlation$`Correlation coefficients table`

#Coordinates for qualitatives suppl?mentary variables
res$quali.supp$coord

#Gestion of error
d.active<-villes_universitaires[,1:9]
res<-multi.quanti(d.active,groupes.cah,10,show_graph=TRUE)
#Error because there is no 10th variable

################ Analyse multivari? qualitative avec le dataset careval
#Import dataset careval
careval<-read.csv("D:/M2-SISE/Python/data/careval.csv",header=FALSE)

#Function multi.quali
d.active<-careval[,1:6]
classe<-careval[,7]
res<-multi.quali(d.active,classe,show_graph=T)

#Table of eigen values
res$eig.values

#Table of coord,contrib and cos2 by var for the second dimension
res$var.tab$Dim2

#Table of correlation between dimension and clusters
res$correlation$`Correlation coefficients table`

#Gestion of error
d.active<-careval[,1]
classe<-careval[,7]
res<-multi.quali(d.active,classe,show_graph=T)
#Error because active variables doesn't contain the minimum of two variables

################ Evaluation of the partition
#Decision tree with iris dataset
g1 <-as.factor(sample(1:3, size=10, replace=TRUE))
g2 <-as.factor(sample(1:3, size=10, replace=TRUE))

#Evaluation of decision tree with real clusters
eval<-evaluation(g1,g2)

#Rand Index
eval$rand_index

#V measure
eval$v.measure

#Gestion of error
eval<-evaluation(iris[-1,5],clusters)
#Return an error because two parameters don't have the same number of rows
