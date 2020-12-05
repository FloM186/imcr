fromage <- read.table(file="D:/M2-SISE/Prog_Stat_R/PROJET/fromage.txt",header=T,row.names=1,sep="\t",dec=".")
fromage.cr <- scale(fromage,center=T,scale=T)
d.fromage <- dist(fromage.cr)
cah.ward <- hclust(d.fromage,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
groupes.cah=as.factor(groupes.cah)

data<-read.csv2("E:/M1 - INFO/S2/Clustering/Villes universitaires.csv",header=TRUE,row.names="Villes")
d.active<-data[,1:9]
active.cr <- scale(d.active,center=T,scale=T)
d.active <- dist(active.cr)
cah.ward <- hclust(d.active,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
groupes.cah=as.factor(groupes.cah)
d.active<-data[,1:10]

#Choix d'utiliser deux packages : FactoMineR (pour l'analyse) et factoextra (pour la visualisation, des donnÃ©es, basÃ©e sur ggplot2)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library(ggplot2)

corr <- runif(9, 0, 1)

tab<-function(obj,nb_dim){
  list.tab<-list()
  for(i in 1:nb_dim){
    coord<-obj$coord[,i]
    contrib<-obj$contrib[,i]
    cos2<-obj$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    display<-display[which(display[,2]>=median(display[,2])&display[,3]>=0.5),]
    nom<-paste("Dim",i,sep=" ")
    list.tab[[nom]]<-display
  }
  return(list.tab)
}

print.ACP_tab <- function (x, file = NULL, sep = ";", ...){
  res.pca <- x
  if (!inherits(res.pca, "ACP_tab")) stop("non convenient data")
  cat("**Results Mutltivarial Analysis using PCA**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))
  res[1, ] <- c("eig.values", "eigenvalues")
  res[2, ] <- c("$var.tab", "results for the variables")
  res[3, ] <- c("$ind.tab", "results for the individus")
  indice <- 4
  if (!is.null(res.pca$quali.sup)){
    res[indice, ] <- c("$quali.supp", "results for the supplementary categorical variables")
  }
  print(res[1:indice,])
  if (!is.null(file)) {
    write.infile(res.pca,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}

plot.ACP_tab<-function(x,y, axes = c(1, 2)){
  #Cercle corrélation cos2
  print(fviz_pca_var(x, col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     axes = axes
  ))
  
  #Cercle corrélation cramer
  print(fviz_pca_var(x, col.var = corr,
                     gradient.cols = c("blue", "yellow", "red"),
                     legend.title = "Cont.Var",axes = axes) ) 
  #Graph selon classe + cos2
  print(fviz_pca_ind(x,
                     repel=TRUE,pointsize = "cos2",
                     pointshape = 21,# Montre les points seulement (mais pas le "text")
                     col.ind = y, # colorer by groups
                     legend.title = "Classes",
                     axes = axes
  ))
  #geom.ind = "point",  
  #Graph selon classe + contrib
  print(fviz_pca_ind(x,
                     repel=TRUE,pointsize = "contrib",
                     pointshape = 21,# Montre les points seulement (mais pas le "text")
                     col.ind = y, # colorer by groups
                     legend.title = "Classes",
                     axes = axes
  ))
}

ACP_tab<-function(X,y,quali.supp=NULL,graph=NULL){
  if (length(X) < 2){
    stop("X doesn't contain enough variables")
  }
  if (length(y)!=nrow(X)){
    stop("X and y doesn't have the same length")
  }
  
  # Analyse en Composantes Principales (ACP)
  res.pca<-PCA(X,quali.sup = quali.supp, scale.unit = TRUE, graph = FALSE)
  
  #Visualisation des valeurs propres
  eig.val <- get_eigenvalue(res.pca)
  print(fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100)))
  
  nb_dim<-readline(prompt="How many axes do you want to keep ? " )
  nb_dim<-as.integer(nb_dim)
  
  #Tableau var
  var<-tab(get_pca_var(res.pca),nb_dim)
  
  #Tableau individu
  ind <- tab(get_pca_ind(res.pca),nb_dim)

  #creation de l'instance
  instance <- list()
  instance$eig.values<-eig.val
  instance$var.tab <- var
  instance$ind.tab<-ind
  if(is.null(quali.supp)==FALSE){
    instance$quali.supp<-res.pca$quali
  }
  class(instance) <- c("ACP_tab","list ")
  
  if(!is.null(graph)){
    plot.ACP_tab(res.pca,y)
  }
  #renvoyer le rÃ©sultat
  return(instance)
  
}

res1<-ACP_tab(d.active,groupes.cah,10,graph=TRUE)
print(res1)
res1$var.tab$`Dim 2`





