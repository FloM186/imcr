#' @title  Modify print for uni.quanti class
#' @description  This function overload the print function to facilitate printing several attributes frome the univariate quantitative function suite
#'
#' @param x an object containing the output of one of the univariate quantitative function
#' @param file a string. NULL by default, name for the file to export results
#' @param sep a string. ";" by default. Separtor for the file
#' @param ... parameters passed to other methods
#'#'
#' @return
#' @export
#' @import stats


print.uni.quanti <- function (x, file = NULL, sep = ";",...){
  if (!inherits(x, "uni.quanti")) stop("non convenient data")
  cat("**Results Univariate Analysis for quantitative variable**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))

  #Description of the different elements
  res[1, ] <- c("$test_value", "")
  res[2, ] <- c("$corr_coef", "")
  res[3, ] <- c("$corr_coef$`Conditionnal means table", "")
  res[4, ] <- c("$corr_coef$`Correlation coefficients table`", "")
  res[5, ] <- c("$effect_size", "")
  res[6, ] <- c("$effect_size$d.cohen", "")
  res[7, ] <- c("$effect_size$g.hedges", "")
  res[8, ] <- c("$effect_size$u1", "")
  res[9, ] <- c("$effect_size$u2", "")
  res[10, ] <- c("$effect_size$u3", "")
  res[11, ] <- c("$effect_size$besd", "")
  res[12, ] <- c("$effect_size$cles", "")
  indice <- 13
  if (!is.null(x$effect_size$density_normality)){
    res[indice, ] <- c("$effect_size$density_normality", "")
  }
  print(res[1:indice,])

  #Integration of the results in a file
  if (!is.null(file)) {
    write.infile(x,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}
