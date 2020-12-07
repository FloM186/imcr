#' @title  Compute Rand index and V measure between real and predicted clusters
#' @description  This function returns Rand index and V measure between real clusters and predicted clusters
#'
#' @param true_label a factor vector of real clusters
#' @param pred_label a factor vector of predicted clusters
#'
#' @return Rand index and V measure
#' @export
#' @examples
#' true_label <-as.factor(c("1","2","2","1"))
#' pred_label <-as.factor(c("1","2","1","2"))
#' evaluation(true_label,pred_label)
#'
evaluation<-function(true_label,pred_label){
  if (length(true_label) != length(pred_label) || length(true_label) < 2){
    stop("two parameters doesn't have the same length")
  }

  #creation of the result's list
  instance <- list()
  instance$rand_index <- rand(true_label,pred_label)
  instance$v.measure <- v.measure(true_label,pred_label)
  class(instance) <- "evaluation"

  return(instance)
}
