#' @title Multivarial qualitatives variable analysis to describe clusters
#' @description This function realize multivarial analysis to describe clusters whith qualitatives variables
#'
#' @param active_variables a data frame. Must contains qualitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param quanti.supp index of supplementary variables in the data frame. Muste be character
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param axes choice of dimensions for graph
#'
#' @return List of results (table for individuals and variables, correlation between dimension and clusters)
#' @export
#' @import FactoMineR ggplot2 factoextra RColorBrewer formattable


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
  #return results
  return(instance)
}
