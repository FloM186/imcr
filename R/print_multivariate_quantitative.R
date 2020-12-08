#' @title  Description of multi.quanti function
#' @description  This function print the names and descriptions of multi.quanti function's results
#'
#' @param x an object of class multi.quanti
#' @param file name of file to exports results
#' @param sep separator to use in the file
#' @param ... further arguments of print method
#'
#' @return Description of multi.quanti object
#' @export
#'

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
