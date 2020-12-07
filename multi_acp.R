fromage <- read.table(file="D:/M2-SISE/Prog_Stat_R/PROJET/fromage.txt",header=T,row.names=1,sep="\t",dec=".")
fromage.cr <- scale(fromage,center=T,scale=T)
d.fromage <- dist(fromage.cr)
cah.ward <- hclust(d.fromage,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
#groupes.cah=as.factor(groupes.cah)

data<-read.csv2("E:/M1 - INFO/S2/Clustering/Villes universitaires.csv",header=TRUE,row.names="Villes")
d.active<-data[,1:9]
active.cr <- scale(d.active,center=T,scale=T)
d.active <- dist(active.cr)
cah.ward <- hclust(d.active,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
#groupes.cah=as.factor(groupes.cah)
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
library(RColorBrewer)
library(formattable)


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


#Function to know the differents elements of ACP_tab's object
print.multi.quanti <- function (x, file = NULL, sep = ";",...){
  if (!inherits(x, "multi.quanti")) stop("non convenient data")
  cat("**Results Mutltivarial Analysis using PCA**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))
  
  #Description of the different elements
  res[1, ] <- c("$eig.values", "eigenvalues")
  res[2, ] <- c("$var.tab", "results for the variables")
  res[3, ] <- c("$ind.tab", "results for the individus")
  res[4, ]<- c("$correlation", "correlation between dimensions and clusters")
  res[5, ]<- c("$correlation$`Conditionnal means table`", "table of conditionnaly means between clusters and dimension")
  res[6, ]<- c("$correlation$`Correlation coefficients table`", "table of correlation between dimensions and clusters")
  indice <- 7
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
plot.multi.quanti<-function(x,clusters=NULL, axes = c(1, 2),sup=FALSE,corr=NULL,...){
  res.pca<-x
  #Circle of correlation with the indice of correlation
  print(fviz_pca_var(res.pca, col.var = corr,
                     gradient.cols = brewer.pal(n=3, name="Dark2"),
                     legend.title = "Coeff correlation",axes = axes) ) 
  #Graph of the variables
  if (sup==FALSE){
    var<-res.pca$var$coord
  }else{
    var<-rbind(res.pca$var$coord,res.pca$quali.sup$coord)
  }
  
  b<-ggplot(as.data.frame(var),aes(x=var[,axes[1]], y=var[,axes[2]])) + 
    geom_point() +
    scale_fill_brewer(palette="Dark2")+ 
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
    nom<-rep("",nrow(res.pca$ind$coord))
  }else{
    ind<-rbind(res.pca$ind$coord,res.pca$quali.sup$coord)
    grp<-append(clusters, rep("var supp",nrow(res.pca$quali.sup$coord)))
    nom<-c(rep("",nrow(res.pca$ind$coord)),rownames(res.pca$quali.sup$coord))
  }
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
  data<-as.data.frame(res.pca$ind$contrib)
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
  data<-as.data.frame(res.pca$ind$cos2)
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
  print(fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)))
  print(eig.val)
  #Choice of the number of axes
  nb_dim<-readline(prompt="How many axes do you want to keep ? " )
  nb_dim<-as.integer(nb_dim)
  
  #Table for variables
  var<-tab(get_pca_var(res.pca),nb_dim)
  
  #Table for individus
  ind <- tab(get_pca_ind(res.pca),nb_dim)
  
  #Table of correlation between coorninates and clusters
  correlation<-corr_coef(as.data.frame(res.pca$ind$coord),clusters)

  #List of the results
  instance <- list()
  instance$eig.values<-eig.val
  instance$var.tab <- var
  instance$ind.tab<-ind
  instance$correlation<-correlation
  if(is.null(quali.supp)==FALSE){
    instance$quali.supp<-res.pca$quali
  }
  class(instance) <- c("multi.quanti","list ")
  
  #Show graphs
  if(show_graph==TRUE){
    if(!is.null(quali.supp)){
      sup=TRUE
      corr<-corr_coef(active_variables[,-quali.supp],clusters)$`Correlation coefficients table`
      c<-rep(0,length(corr))
      for(i in 1:length(corr)){c[i]<-corr[1,i]}
      }else{
      corr<-corr_coef(active_variables,clusters)$`Correlation coefficients table`
      c<-rep(0,length(corr))
      for(i in 1:length(corr)){c[i]<-corr[1,i]}
    }
    
    plot.multi.quanti(res.pca,as.factor(clusters),axes,sup,c)
  }
  #return the list of results
  return(instance)
  
}

corr_coef <- function(active_variables, clusters, show_graph=TRUE, show_conditionnal_means=TRUE, digits=3) {
  
  if(all(sapply(active_variables, is.numeric))==FALSE) { #Check if input variables are numeric
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE ) { #Check if input clusters are a vector
    print("Clusters must be a vector")
  } else {
    
    #Concatenate variables and clusters
    active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters))
    
    #Calculate mean for each cluster
    cluster_mean <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))
    
    #Calculate length for each cluster
    cluster_n <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(length))  %>%
      select_if(function(x) is.numeric(x))
    
    #Calculate mean across all clusters
    active_variables_mean <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
    
    #Input for k and c
    k=length(unique(clusters))
    c = ncol(active_variables)
    
    #Creating empty data frame of wanted dimension
    sct <-as.data.frame(matrix(nrow = nrow(active_variables), ncol=c))
    colnames(sct) = colnames(active_variables)
    
    #Loop to fill data frame
    for(i in 1:c) {
      sct[,i] <- active_variables %>% select(,i) %>% mutate_all(~ (.x - as.numeric(active_variables_mean[i]))^2)
    }
    
    #Calculate SCT
    sct <-sct %>% summarise_all(sum)
    
    #Creating empty data frame of wanted dimension
    sce <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(sce) = colnames(active_variables)
    
    #Loop to fill data frame
    for(j in 1:k){
      for(i in 1:ncol(active_variables)) {
        sce[j,i]<-  as.numeric(cluster_n[j,i]*(cluster_mean[j,i] - active_variables_mean[i])^2)
      }
    }
    #Calculate SCE
    sce <-sce %>% summarise_all(sum)
    
    #Calculate correlation coefficient
    rcor <- sce/sct
    
    #Printing the plot
    if(show_graph==TRUE) {
      print(rcor %>% gather(key, value) %>%
              ggplot(aes(x=key, y=value))+
              geom_bar(stat="identity", width=0.75, fill= brewer.pal(n = 3, name = "Dark2")[1])+
              labs(x = "Variables", y = "Correlation coefficient")+
              scale_y_continuous(expand=c(0.004,0))+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")))} else {}
    
    #Returning the conditionnal means table and correlation coefficient table
    if(show_conditionnal_means==TRUE){results <- list("Conditionnal means table"= cluster_mean %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
                                                      "Correlation coefficients table" = rcor %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)))
    
    #Printing the formatted conditionnal means table in Viewer tab (html format)
    print(cluster_mean %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (cond. means)"= 1:k) %>% select("clusters (cond. means)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    
    } else {
      #Returning the correlation coefficient table
      results <- list("Conditionnal means table" = rcor)}
    
    #Printing the correlation coefficient table in Viewer tab (html format)
    print(rcor %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)) %>%
            mutate("correlation coefficient"= "value") %>% select("correlation coefficient", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    
    #Return for the function output
    return(results)
  }
}

res1<-multi.quanti(d.active,groupes.cah,10,show_graph=TRUE)
print(res1)
res1$var.tab$`Dim 2`





