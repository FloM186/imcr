#Plusieurs packages pour faire de l'ADD sous R
#Choix d'utiliser deux packages : FactoMineR (pour l'analyse) et factoextra (pour la visualisation, des données, basée sur ggplot2)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library(plotly)

#
# Importation des données et sélection des éléments -----------------------
#
data<-read.csv("D:/M2-SISE/Python/data/careval.csv",header=FALSE)
data<-data[1:1000,]
for (i in 1:7){
  data[,i]<-as.factor(data[,i])
}
classe<-as.factor(floor(runif(1000, min=1, max=4)))

#Tableau v de cramer par classe et dimension
#Cramer prendre la derni�re version

v.cramer <- function(classe, var){
  #Teste si les variables sont sous forme de data.frame
  if(class(var) == "data.frame"){
    names_col = names(var)
    cramer_var = c()
    cramer_val = c()
    #On calcul le v de cramer pour toutes les variables qualitatives
    for(i in 1:ncol(var)){
      if(is.factor(var[,i])){
        contingence = table(classe,var[,i])
        khi = chisq.test(contingence)$statistic
        dim = min(nrow(contingence),ncol(contingence)) - 1
        v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),5)
        cramer_var = append(cramer_var,names_col[i])
        cramer_val = append(cramer_val,as.numeric(v_cramer))
        tab_cramer = cbind(cramer_var,cramer_val)
      }
    }
    
    data <- as.data.frame(matrix(as.numeric(tab_cramer[,2]), ncol=nrow(tab_cramer)))
    colnames(data) = tab_cramer[,1]
    data = rbind(rep(1,length(tab_cramer)),rep(0,length(tab_cramer)),data)
    
    #radarchart(data, axistype=2, title = "Cramer's v by variable", pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=0.8 )
    
    return(tab_cramer)
    
    # On calcul le v de cramer si seulement une variable qualitative a été passée en paramètre
  }else if(is.factor(var)){
    contingence = table(classe,var)
    khi = chisq.test(contingence, correct=F)$statistic
    dim = min(nrow(contingence),ncol(contingence)) - 1
    v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),5)
    return(v_cramer)
  }
}

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

#Function to the description of the dimension
desc.dim<-function(res,nb_dim){
  list.desc<-list()
  for(i in 1:nb_dim){
    list.desc[[i]]<-res[[i]]$quali
  }
  return(list.desc)
}

# Analyse factorielle des Correspondances (ACM) --------------------------------
#
multi.quali<-function(active_variables, clusters,quanti.supp=NULL,axes = c(1, 2),graph=NULL){
  if (length(active_variables) < 2){
    stop("Active_variables doesn't contain enough variables")
  }
  if (length(clusters)!=nrow(active_variables)){
    stop("active_variables and y doesn't have the same length")
  }
  test<-active_variables[,-quanti.supp]
  if(all(sapply(test, is.character))==FALSE){
    stop("Active variables (minus supplementary variables) aren't numeric")
  }
  test<-active_variables[,quanti.supp]
  if(all(sapply(test, is.numeric))==FALSE){
    stop("supplementary aren't character")
  }
  
  res.mca <- MCA (active_variables,show_graph = FALSE,quanti.sup = quanti.supp)
  ##Valeurs propres
  #Visualisation of eigen values
  eig.val <- get_eigenvalue(res.mca)
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
  
  nb_dim<-readline(prompt="How many axes do you want to keep ? " )
  nb_dim<-as.integer(nb_dim)
  
  #Tableau var
  var<-tab(get_mca_var(res.mca),nb_dim)
  
  #Tableau individu
  ind<-tab(get_mca_ind(res.mca),nb_dim)
  
  # Description de la dimension
  res.desc <- dimdesc(res.mca, axes = 1:nb_dim)
  desc <- desc.dim(res.desc,nb_dim)
  
  #creation de l'instance
  instance <- list()
  instance$eig.values<-eig.val
  instance$var.tab <- var
  instance$ind.tab<-ind
  instance$desc.dim<-desc
  if(is.null(quanti.supp)==FALSE){
    instance$quanti.supp<-res.mca$quanti
  }
  class(instance) <- c("multi.quali","list ")
  
  if(!show_graph==FALSE){
    #cramer en enlevant var sup
    #axes a ajouter
    if(!is.null(quanti.supp)){
      sup=TRUE
      cramer<-as.numeric(v.cramer(clusters,actives_variables[,-quali.supp])[,2])
      plot.ACM_val(res.mca,clusters,cramer,axes,sup)
    }else{
      sup=FALSE
      cramer<-as.numeric(v.cramer(clusters,actives_variables)[,2])
      plot.ACM_val(res.mca,clusters,cramer,axes,sup)
    }
    
  }
  #renvoyer le résultat
  return(instance)
}

plot.multi.quali<-function(res.mca,clusters, cramer,sup,axes){
  #Graphique des variables colorés selon le v de cramer
  print(fviz_mca_var (res.mca, choice="mca.cor",col.var = cramer,
                        gradient.cols = c("blue", "yellow", "red"),
                        legend.title = "V de Cramer",
                        repel = TRUE,axes = axes))
  
  #Graph of the qualitatives variables and individuals by class 
  ind<-rbind(res.mca$ind$coord,res.mca$var$coord)
  grp<-append(clusters, rep("Modalit�s",nrow(res.mca$var$coord)))
  nom<-c(rep("",nrow(res.mca$ind$coord)),rownames(res.mca$var$coord))
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
  data<-as.data.frame(res.mca$ind$contrib)
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
  data<-as.data.frame(res.mca$ind$cos2)
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
  
  
  if(sup==TRUE){
    #Quanti supp
    print(fviz_mca_var(x, choice = "quanti.sup",
                         ggtheme = theme_minimal(),axes=axes))
  }
  
  
}

print.multi.quali <- function (res.mca, file = NULL, sep = ";", ...){
  if (!inherits(res.mca, "ACM_val")) stop("non convenient data")
  cat("**Results Mutltivarial Analysis using PCA**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))
  res[1, ] <- c("eig.values", "eigenvalues")
  res[2, ] <- c("$var.tab", "results for the variables")
  res[3, ] <- c("$ind.tab", "results for the individus")
  res[4, ] <- c("$desc.dim", "description of the dimension")
  indice <- 5
  if (!is.null(res.mca$quanti.sup)){
    res[indice, ] <- c("$quanti.supp", "results for the supplementary numerical variables")
  }
  print(res[1:indice,])
  if (!is.null(file)) {
    write.infile(res.mca,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}

res1<-ACM_val(data,classe,graph=F)


print(res1)


