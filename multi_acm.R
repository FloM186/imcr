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
#for (i in 1:7){
  #data[,i]<-as.factor(data[,i])
#}
classe<-as.vector(floor(runif(1000, min=1, max=4)))

#Tableau v de cramer par classe et dimension
#Commentaire
#Test
#idem acp

v.cramer <- function(active_variables, clusters, show_graph=TRUE, digits=5){
  #Tests if the variables passed in parameter are in the form of data.frame
  if(class(active_variables) == "data.frame"){
    names_col = names(active_variables)
    cramer_active_variables = c()
    cramer_val = c()
    
    #We calculate the cramer's v for all the qualitative variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        contingence = table(clusters,active_variables[,i])
        khi = chisq.test(contingence, simulate.p.value = TRUE)$statistic
        dim = min(nrow(contingence),ncol(contingence)) - 1
        v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),digits)
        
        #Two vectors, one for variables names, the other for values
        cramer_active_variables = append(cramer_active_variables,names_col[i])
        cramer_val = append(cramer_val,as.numeric(v_cramer))
      }
    }
    
    #Creation of a data frame from the two vectors for ggplot
    tab_cramer = cbind(cramer_active_variables,cramer_val)
    data <- as.data.frame(matrix(as.numeric(tab_cramer[,2]), ncol=nrow(tab_cramer)))
    colnames(data) = tab_cramer[,1]
    data = rbind(rep(1,length(tab_cramer)),rep(0,length(tab_cramer)),data)
    
    if(show_graph==TRUE){
      plot.uni.quali(tab_cramer,"cramer",digits = digits)
      
    }
    vec.cramer=setNames(cramer_val,cramer_active_variables)
    return(vec.cramer)
    
    
    #We calculate the cramer's v if only one qualitative variable has been passed as a parameter
  }else if(is.factor(active_variables) || is.character(active_variables)){
    contingence = table(clusters,active_variables)
    khi = chisq.test(contingence, simulate.p.value = TRUE)$statistic
    dim = min(nrow(contingence),ncol(contingence)) - 1
    v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),digits)
    return(v_cramer)
  }
}


library(formattable)    #le package

#Function to construct table with contribution, cos2 and coordinates
tab<-function(obj,nb_dim){
  list.tab<-list()
  for(i in 1:nb_dim){
    coord<-obj$coord[,i]
    contrib<-obj$contrib[,i]
    cos2<-obj$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    colnames(display)<-c(paste("Coord Dim",as.character(i),sep=""),paste("Contrib Dim",as.character(i),sep=""),paste("Cos2 Dim",as.character(i),sep=""))
    print(display%>% formattable(align = c("c","c", "r"),
                              list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                   area(col = 3) ~ formatter("span",
                                                             style = x ~ style(
                                                               font.weight = "bold",
                                                               color = ifelse( x > 0.5,  "#00CC00", "black"))),
                                   area(col = 2) ~ formatter("span",
                                                             style = x ~ style(
                                                               font.weight = "bold",
                                                               color = ifelse( x > median(display[,2]),  "#00CC00", "black"))))))
    
    nom<-paste("Dim",i,sep="")
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
multi.quali<-function(active_variables, clusters,quanti.supp=NULL,axes = c(1, 2),show_graph=NULL){
  #Test of the parameters
  if (length(active_variables) < 2){
    stop("Active_variables doesn't contain enough variables")
  }
  if (length(clusters)!=nrow(active_variables)){
    stop("active_variables and y doesn't have the same length")
  }
  if(!is.null(quanti.supp)){
    test<-active_variables[,-quanti.supp]
  }else{
    test<-active_variables
  }
  if(all(sapply(test, is.character))==FALSE){
    stop("Active variables (minus supplementary variables) aren't numeric")
  }
  if(!is.null(quanti.supp)){
    test<-active_variables[,quanti.supp]
    if(all(sapply(test, is.numeric))==FALSE){
      stop("supplementary aren't character")
    }
  }
  
  #MCA
  res.mca <- MCA (active_variables,graph = FALSE,quanti.sup = quanti.supp)

  #Visualisation of eigen values
  eig.val <- get_eigenvalue(res.mca)
  print(fviz_eig(res.mca, addlabels = TRUE, ylim = c(0, 50)))
  print(eig.val)
  
  #Choice number of dimensions
  nb_dim<-readline(prompt="How many axes do you want to keep ? " )
  nb_dim<-as.integer(nb_dim)
  
  #Table for variables
  var<-tab(get_mca_var(res.mca),nb_dim)
  
  #Table for individuals
  ind<-tab(get_mca_ind(res.mca),nb_dim)
  
  # Description of dimension
  res.desc <- dimdesc(res.mca, axes = 1:nb_dim)
  desc <- desc.dim(res.desc,nb_dim)
  
  #Correlation between dimension and clusters
  correlation<-corr_coef(as.data.frame(res.mca$ind$coord),clusters)
  
  #List of results
  instance <- list()
  instance$eig.values<-eig.val
  instance$var.tab <- var
  instance$ind.tab<-ind
  instance$desc.dim<-desc
  instance$correlation<-correlation
  if(is.null(quanti.supp)==FALSE){
    instance$quanti.supp<-res.mca$quanti
  }
  class(instance) <- c("multi.quali","list ")
  
  if(!show_graph==FALSE){
    if(!is.null(quanti.supp)){
      sup=TRUE
      cramer<-v.cramer(active_variables[,-quanti.supp],clusters,show_graph = FALSE)
      plot.multi.quali(res.mca,as.factor(clusters),cramer,sup,axes)
    }else{
      sup=FALSE
      cramer<-v.cramer(active_variables,clusters,show_graph = FALSE)
      plot.multi.quali(res.mca,as.factor(clusters),cramer,sup,axes)
    }
    
  }
  #renvoyer le résultat
  return(instance)
}

plot.multi.quali<-function(x,clusters=FALSE, cramer=FALSE,sup=FALSE,axes=c(1,2)){
  res.mca<-x
  #Graphique des variables colorés selon le v de cramer
  print(fviz_mca_var (res.mca, choice="mca.cor",col.var = cramer,
                      gradient.cols = brewer.pal(n=3, name="Dark2"),
                        legend.title = "V de Cramer",
                        repel = TRUE,axes = axes))
  
  #Graph of the qualitatives variables and individuals by class 
  ind<-rbind(res.mca$ind$coord,res.mca$var$coord)
  grp<-append(clusters, rep("Modalit�s",nrow(res.mca$var$coord)))
  nom<-c(rep("",nrow(res.mca$ind$coord)),rownames(res.mca$var$coord))
  a<-ggplot(as.data.frame(ind),aes(x=ind[,axes[1]], y=ind[,axes[2]], color=grp)) + 
    geom_point() +
    scale_fill_brewer(palette="Dark2")+ 
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
    scale_fill_brewer(palette="Dark2")+ 
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
    scale_fill_brewer(palette="Dark2")+
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

print.multi.quali <- function (x, file = NULL, sep = ";", ...){
  x<-res.mca
  #Test of the class of x
  if (!inherits(res.mca, "ACM_val")) stop("non convenient data")
  cat("**Results Mutltivarial Analysis using PCA**\n")
  cat("*The results are available in the following objects:\n\n")
  
  #Description of the results
  res <- array("", c(24, 2), list(1:24, c("name", "description")))
  res[1, ] <- c("eig.values", "eigenvalues")
  res[2, ] <- c("$var.tab", "results for the variables")
  res[3, ] <- c("$ind.tab", "results for the individus")
  res[4, ] <- c("$desc.dim", "description of the dimension")
  res[5, ]<- c("$correlation", "correlation between dimensions and clusters")
  res[6, ]<- c("$correlation$`Conditionnal means table`", "table of conditionnaly means between clusters and dimension")
  res[7, ]<- c("$correlation$`Correlation coefficients table`", "table of correlation between dimensions and clusters")
  indice <- 8
  if (!is.null(res.mca$quanti.sup)){
    res[indice, ] <- c("$quanti.supp", "results for the supplementary numerical variables")
  }
  print(res[1:indice,])
  
  #Write results in a file
  if (!is.null(file)) {
    write.infile(res.mca,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}

res1<-multi.quali(data,classe,show_graph=T)


print(res1)


