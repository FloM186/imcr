#' @title  Description of multi.quali function
#' @description  This function print the names and descriptions of multi.quali function's results
#'
#' @param x an object of class multi.quali
#' @param file name of file to exports results
#' @param sep separator to use in the file
#' @param ... further arguments of print method
#'
#' @return Description of multi.quanti object
#' @export
#'

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
