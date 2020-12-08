#' @title  Compute V measure between real and predicted clusters
#' @description  This function returns the V measure between real clusters and predicted clusters
#'
#' @param true_label a factor vector of real clusters
#' @param pred_label a factor vector of predicted clusters
#'
#' @return The V measure
#' @export
#' @examples
#' true_label <-as.factor(c("1","2","2","1"))
#' pred_label <-as.factor(c("1","2","1","2"))
#' v.measure(true_label,pred_label)
#'

v.measure<-function(true_label,pred_label){

  #Table of contingence between real and predicted clusters
  tab=table(true_label,pred_label)

  #Calculation of differents indicators
  hck=0
  n=length(true_label)
  for (k in 1:ncol(tab)){
    for (c in 1:nrow(tab)){
      if (sum(tab[,k])==0){
        t=1
      }else{
        if(tab[c,k]/sum(tab[,k])==0){
          t=1
        }else{
          t=tab[c,k]/sum(tab[,k])
        }
      }
      hck=hck+(tab[c,k]/n)*log(t)
    }
  }
  hck=-hck

  hkc=0
  for(c in 1:nrow(tab)){
    for (k in 1:ncol(tab)){
      if (sum(tab[c,])==0){
        t=1
      }else{
        if(tab[c,k]/sum(tab[c,])==0){
          t=1
        }else{
          t=tab[c,k]/sum(tab[c,])
        }
      }
      hkc=hkc+(tab[c,k]/n)*log(t)
    }
  }
  hkc=-hkc

  hc=0
  nbc=length(true_label)
  for (c in 1:nrow(tab)){
    hc=hc+(sum(tab[c,])/nbc)*log(sum(tab[c,])/nbc)
  }
  hc=-hc

  hk=0
  for(k in 1:nrow(tab)){
    hk=hk+(sum(tab[,k])/nbc)*log(sum(tab[,k])/nbc)
  }
  hk=-hk

  h=1-(hck/hc)
  c=1-(hkc/hk)

  #Calculation of the v measure
  v=2*((h*c)/(h+c))

  return(v)
}

