#Function to create the constructor for the uni.quanti s3 class
#' @title  Create the constructor
#' @description  Constructor for the uni.quanti s3 class
#'
#' @param active_variables a numeric vector, data frame or matrix
#' @param clusters a vector of same length as variables containing the clustering values

#'
#' @return uni.quanti class attributes
#' @export
#' @import stats
#' @examples
#' data(iris)
#' iris.cr <- scale(iris[,-5],center=T,scale=T)
#' d.iris <- dist(iris.cr)
#' cah.ward <- hclust(d.iris,method="ward.D2")
#' groupes.cah <- cutree(cah.ward,k=4)
#' uni.quanti(iris[,-5], groupes.cah)
uni.quanti <- function(active_variables, clusters){

  #instance creation
  instance <- list()
  instance$test_value <- test_value(active_variables, clusters)
  instance$corr_coef <- corr_coef(active_variables, clusters)
  instance$effect_size <- effect_size(active_variables, clusters)
  class(instance) <- c("uni.quanti","list ")
  return(instance)
}
