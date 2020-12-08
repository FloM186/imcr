#' @title  Compute binomial coefficient
#' @description  This function returns the binomial coefficient
#'
#' @param n an integer
#' @param k an integer
#'
#' @return Binomial coefficient
#' @export
#' @examples
#' n=5
#' k=2
#' bin(n,k)
#'

bin<-function(n,k){
  if(n>k){
    return (factorial(n)/(factorial(k)*factorial(n-k)))
  }else{
    return(0)
  }
}
