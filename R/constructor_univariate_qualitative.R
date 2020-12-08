#'@title  Constructor for qualitative univariate characterization
#' @description Returns a list of element which allows to characterize univariate qualitative variable
#'
#' @param active_variables a factor vector or matrix. Must contains qualitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param digits a number. Number of digits to display
#'
#' @return List including the result of each function for qualitative univariate characterization
#' @export
#' @examples
#' var = c(rep("yes",7), rep("no",7))
#' clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
#' uni.quali(var,clust)
#'
 #constructor for S3 class univariate qualitative
uni.quali <- function(active_variables, clusters,show_graph=FALSE,digits=5){

  #instance creation
  instance <- list()
  instance$v.cramer <- v.cramer(active_variables, clusters, show_graph, digits)
  instance$l.profil <- l.profil(active_variables, clusters, show_graph, digits)
  instance$cl.profil <- cl.profil(active_variables, clusters, show_graph, digits)
  instance$h <- h.value.test(active_variables, clusters, show_graph, digits)
  instance$phi <- phi.value.test(active_variables, clusters, show_graph, digits)
  class(instance) <- c("uni.quali","list ")
  return(instance)
}
