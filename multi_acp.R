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
library(tidyverse)
library(magrittr)
library(clValid)
library(clusterCrit)
library(ClustOfVar)
library(ggpubr)
library(factoextra)
library(ade4)
library(ggpubr)
library("factoextra")
library(ggplot2)
library("gridExtra")



#Rapport de corrélation classe dim
#Rapport de corrélation var
corr <- runif(9, 0, 1)

#Function to construct table with contribution, cos2 and coordinates
tab<-function(obj,nb_dim){
  list.tab<-list()
  for(i in 1:nb_dim){
    coord<-obj$coord[,i]
    contrib<-obj$contrib[,i]
    cos2<-obj$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    display<-display[which(display[,2]>=median(display[,2])&display[,3]>=0.5),]
    display<-display[order(display[,1],decreasing=FALSE), ]
    nom<-paste("Dim",i,sep=" ")
    list.tab[[nom]]<-display
  }
  return(list.tab)
}

#Function to know the differents elements of ACP_tab's object
print.multi.quanti <- function (x, file = NULL, sep = ";"){
  if (!inherits(x, "multi.quanti")) stop("non convenient data")
  cat("**Results Mutltivarial Analysis using PCA**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))
  
  #Description of the different elements
  res[1, ] <- c("eig.values", "eigenvalues")
  res[2, ] <- c("$var.tab", "results for the variables")
  res[3, ] <- c("$ind.tab", "results for the individus")
  indice <- 4
  if (!is.null(x$quali.sup)){
    res[indice, ] <- c("$quali.supp", "results for the supplementary categorical variables")
  }
  print(res[1:indice,])
  
  #Integration of the results in a file
  if (!is.null(file)) {
    write.infile(x,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}

#Function to show the differents plots 
plot.multi.quanti<-function(res.pca,clusters, axes = c(1, 2),sup=FALSE){
  #Circle of correlation with the indice of correlation
  print(fviz_pca_var(res.pca, col.var = corr,
                     gradient.cols = c("blue", "yellow", "red"),
                     legend.title = "Coeff correlation",axes = axes) ) 
  #Graph of the variables
  if (sup==FALSE){
    var<-res.pca$var$coord
  }else{
    var<-rbind(res.pca$var$coord,res.pca$quali.sup$coord)
  }
  
  b<-ggplot(as.data.frame(var),aes(x=var[,axes[1]], y=var[,axes[2]])) + 
    geom_point() +
    scale_fill_brewer(palette="BuPu")+ 
    theme(legend.position="top", legend.justification=c(0,1))+
    geom_text(label=rownames(var))+
    theme_minimal(base_size = 12)+
    ggtitle("Coordinates of the variable")+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"))
  print(b)
  
 
  #Graph of the qualitatives variables and individuals by class 
  if (sup==FALSE){
    ind<-res.pca$ind$coord
    grp<-clusters
    nom1<-rep("",nrow(res.pca$ind$coord))
    nom2<-rownames(res.pca$ind$coord)
  }else{
    ind<-rbind(res.pca$ind$coord,res.pca$quali.sup$coord)
    grp<-append(clusters, rep("var supp",nrow(res.pca$quali.sup$coord)))
    nom1<-c(rep("",nrow(res.pca$ind$coord)),rownames(res.pca$quali.sup$coord))
    nom2<-nom1<-c(rownames(res.pca$ind$coord),rownames(res.pca$quali.sup$coord))
  }
  a<-ggplot(as.data.frame(ind),aes(x=ind[,axes[1]], y=ind[,axes[2]], color=grp)) + 
    geom_point() +
    scale_fill_brewer(palette="BuPu")+ 
    geom_text(label=nom)+
    theme_minimal(base_size = 12)+
    ggtitle("Coordinates of the individuals and qualitatives variables")+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"),
          legend.position="top", 
          legend.justification=c(0,1))+
    labs(color = "Class and  quali. var")
  print(a)

  #Graph of the contribution for the individuals by class 
  data<-as.data.frame(res.pca$ind$contrib)
  scatterPlot <- ggplot(data,aes(x=data[,axes[1]], y=data[,axes[2]], color=clusters)) + 
    geom_point() +
    scale_fill_brewer(palette="BuPu")+ 
    theme_minimal(base_size = 12)+
    ggtitle("Coordinates of the contribution for the individuals by class")+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"),
          legend.position="top", 
          legend.justification=c(0,1))+
    labs(color = "Class")
  print(scatterPlot)
  
  #Graph of the cos2 for the individuals by class
  data<-as.data.frame(res.pca$ind$cos2)
  scatterPlot <- ggplot(data,aes(x=data[,axes[1]], y=data[,axes[2]], color=clusters)) + 
    geom_point() +
    ggtitle("Coordinates of the the cos2 for the individuals by class")+
    scale_fill_brewer(palette="BuPu")+
    theme_minimal(base_size = 12)+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"),
          legend.position="top", 
          legend.justification=c(0,1))+
    labs(color = "Class")
  print(scatterPlot)
  
  
}



#Function to realize multivariate analysis for numerical variables
multi.quanti<-function(active_variables, clusters,quali.supp=NULL,show_graph=NULL,axes=c(1,2)){
  if (length(active_variables) < 2){
    stop("active_variables doesn't contain enough variables")
  }
  if (length(clusters)!=nrow(active_variables)){
    stop("active_variables and y doesn't have the same length")
  }
  test<-active_variables[,-quali.supp]
  if(all(sapply(test, is.numeric))==FALSE){
    stop("Active variables (minus supplementary variables) aren't numeric")
  }
  test<-active_variables[,quali.supp]
  if(all(sapply(test, is.character))==FALSE){
    stop("supplementary aren't character")
  }
  
  # ACP 
  res.pca<-PCA(active_variables,quali.sup = quali.supp, scale.unit = TRUE, graph = FALSE)
  
  #Visualisation of eigen values
  eig.val <- get_eigenvalue(res.pca)
  data_eig<-data.frame("dimension"=rownames(as.data.frame(eig.val)),"eigenvalue"=eig.val[,1],"percentage"=eig.val[,2])
  plot_eig<-ggplot(data=data_eig, aes(x=dimension,y=percentage)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=paste(round(percentage,2),"%",sep="")), vjust=-0.3, size=3.5)+
    geom_text(aes(label=round(eigenvalue,2)), vjust=1.6, color="white", size=3.5)+
    theme_minimal(base_size = 12)+
    ggtitle("Percentage of variance and eigenvalue by dimension")+
    labs(x = "Dimensions", y = "% of variance")+
    scale_y_continuous(expand=c(0.004,0),limits = c(0, 100))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"))
  print(eig.val)
  print(plot_eig)
  #Choice of the number of axes
  nb_dim<-readline(prompt="How many axes do you want to keep ? " )
  nb_dim<-as.integer(nb_dim)
  
  #Table for variables
  var<-tab(get_pca_var(res.pca),nb_dim)
  
  #able for individus
  ind <- tab(get_pca_ind(res.pca),nb_dim)

  #List of the results
  instance <- list()
  instance$eig.values<-eig.val
  instance$var.tab <- var
  instance$ind.tab<-ind
  if(is.null(quali.supp)==FALSE){
    instance$quali.supp<-res.pca$quali
  }
  class(instance) <- c("multi.quanti","list ")
  
  #Show graphs
  if(show_graph==TRUE){
    if(!is.null(quali.supp)){
      sup=TRUE
    }
    plot.ACP_tab(res.pca,clusters,axes,sup)
  }
  #return the list of results
  return(instance)
  
}

res1<-ACP_tab(d.active,groupes.cah,10,show_graph=TRUE)
print(res1)
res1$var.tab$`Dim 2`





