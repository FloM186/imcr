#' @title Multivarial quantitatives variable analysis to describe clusters
#' @description This function realize multivarial analysis to describe clusters whith numerical variables
#'
#' @param active_variables a data frame. Must contains quantitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param quali.supp index of supplementary variables in the data frame. Muste be character
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param axes choice of dimensions for graph
#'
#' @return List of results (table for individuals and variables, correlation between dimension and clusters)
#' @export
#' @import FactoMineR ggplot2 factoextra RColorBrewer formattable

multi.quanti<-function(active_variables, clusters,quali.supp=NULL,show_graph=NULL,axes=c(1,2)){
  if (class(active_variables) !="data.frame"&class(active_variables)!="numeric"){
    stop("Active_variables doesn't contain enough variables")
  }else{
    if (length(clusters)!=nrow(active_variables)){
      stop("active_variables and y doesn't have the same length")
    }
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
