#' @title Print the results of each function for univariate qualitative
#' @description This functions allows to print all the results of functions for univariate qualitative variables
#'
#' @param x a list
#' @param file a string. NULL by default, name for the file to export results
#' @param sep a string. ";" by default. Separtor for the file
#' @param ... parameters passed to other methods
#'
#' @return an array and a file if informed
#' @export
#'
#' @importFrom FactoMineR write.infile
#'

#Function to describe each results
print.uni.quali<-function(x, file = NULL, sep = ";",...){
  if (!inherits(x, "uni.quali")) stop("non convenient data")
  cat("**Results Univariate Analysis for qualitative variable**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))

  #Description of the different elements
  res[1, ] <- c("$v.cramer", "table of Cramer's v between each variable and clusters")
  res[2, ] <- c("$l.profil", "row's profil for each variable")
  res[3, ] <- c("$c.profil", "column's profil for each variable")
  res[4, ] <- c("$h", "table of h value between modalities and clusters for each class")
  res[5, ] <- c("$phi", "table of phi value between variable and clusters")

  print(res[1:5,])

  #Integration of the results in a file
  if (!is.null(file)) {
    write.infile(x,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}
