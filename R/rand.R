#' @title  Compute Rand index between real and predicted clusters
#' @description  This function returns the Rand index between real clusters and predicted clusters
#'
#' @param true_label a factor vector of real clusters
#' @param pred_label a factor vector of predicted clusters
#'
#' @return Rand index
#' @export
#' @examples
#' true_label <-as.factor(c("1","2","2","1"))
#' pred_label <-as.factor(c("1","2","1","2"))
#' rand(true_label,pred_label)
#'

rand<-function(true_label,pred_label){
  #Table of contingence
  tab=table(true_label,pred_label)

  #Calculation binomials coefficients
  sum=0
  for(i in 1:nrow(tab)){
    for(j in 1:ncol(tab)){
      sum=sum+bin(tab[i,j],2)
    }
  }

  resi=0
  for(i in 1:nrow(tab)){
    resi=resi+bin(sum(tab[i,]),2)
  }

  resj=0
  for(j in 1:ncol(tab)){
    resj=resj+bin(sum(tab[,j]),2)
  }
  res=resi*resj

  #Calculation rand index
  ari=(sum-(res/bin(sum(tab),2)))/(0.5*res-(res/bin(sum(tab),2)))
  return(ari)
}
