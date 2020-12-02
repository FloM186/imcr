fromage <- read.table(file="D:/M2-SISE/Prog_Stat_R/PROJET/fromage.txt",header=T,row.names=1,sep="\t",dec=".")
fromage.cr <- scale(fromage,center=T,scale=T)
d.fromage <- dist(fromage.cr)
cah.ward <- hclust(d.fromage,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
groupes.cah=as.factor(groupes.cah)

#Choix d'utiliser deux packages : FactoMineR (pour l'analyse) et factoextra (pour la visualisation, des données, basée sur ggplot2)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library(ggplot2)

corr <- runif(9, 0, 1)

tab.var<-function(var,nb_dim){
  list.tab<-list()
  for(i in 1:nb_dim){
    coord<-var$coord[,i]
    contrib<-var$contrib[,i]
    cos2<-var$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    nom<-paste("dim",as.character(i))
    list.tab[[i]]<-display
  }
  return(list.tab)
}

ACP_tab<-function(X,y,nbr_dim,quali.supp=NULL){
  if (length(X) < 2){
    stop("X doesn't contain enough variables")
  }
  if (length(y)!=nrow(X)){
    stop("X and y doesn't have the same length")
  }
  if(is.null(quali.supp)==FALSE){
    test=TRUE
    for (i in 1:quali.supp){
      if(is.factor(quali.supp[,i])==FALSE){
        test=FALSE
      }
    }
    if(test==FALSE){
      stop("index quali_supp is not factor")
    }
  }
  # Analyse en Composantes Principales (ACP)
  res.pca<-PCA(X,quali.sup = quali.supp, scale.unit = TRUE, graph = FALSE)
  
  #Visualisation des valeurs propres
  eig.val <- get_eigenvalue(res.pca)
  graph<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 1)) 
  
  #création de l'instance
  instance <- list()
  instance$eig.values<-eig.val
  instance$graph.eig<-graph
  instance$var.tab <- tab.var(get_pca_var(res.pca),nbr_dim)
  if(is.null(quali.supp)==FALSE){
    instance$quali.supp<-res.pca$quali
  }
  class(instance) <- "ACP_tab"
  #renvoyer le résultat
  return(instance)
  
}

ACP_graph<-function(X,y,dim1,dim2,quanti.supp=NULL){
  if (length(X) < 2){
    stop("X doesn't contain enough variables")
  }
  if (length(y)!=nrow(X)){
    stop("X and y doesn't have the same length")
  }
  if(is.null(quali_supp)==FALSE){
    test=TRUE
    for (i in 1:quali.supp){
      if(is.factor(quali.supp[,i])==FALSE){
        test=FALSE
      }
    }
    if(test==FALSE){
      stop("index quali_supp is not factor")
    }
  }
  # Analyse en Composantes Principales (ACP)
  res.pca<-PCA(X,quali.sup = quali.supp, scale.unit = TRUE, ncp = 5, graph = FALSE)
  
  #Cercle des corrélations avec coloriage selon la contribution
  graph1<-fviz_pca_var(res.pca, col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(dim1, dim2)
  )
  
  # Colorer les variables en fonction d'une variable continue
  graph2<-fviz_pca_var(res.pca, col.var = corr,
               gradient.cols = c("blue", "yellow", "red"),
               legend.title = "Cont.Var",axes = c(dim1, dim2))
  
  #Colorer selon les classes
  graph3<-fviz_pca_ind(res.pca,
                       geom.ind = "point", # Montre les points seulement (mais pas le "text")
                       col.ind = y, # colorer by groups
                       legend.title = "Classes",
                       axes = c(dim1, dim2)
  )
  
  #création de l'instance
  instance <- list()
  instance$circle.contrib<-graph1
  instance$circle.corr<-graph2
  instance$graph.ind <- graph3
  class(instance) <- "ACP_tab"
  #renvoyer le résultat
  return(instance)
}

res1<-ACP_tab(fromage,groupes.cah,5)
res2<-ACP_graph(fromage,groupes.cah,1,2)