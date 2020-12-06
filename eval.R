g1 <-as.factor(sample(1:3, size=10, replace=TRUE))
g2 <-as.factor(sample(1:3, size=10, replace=TRUE))

autos <- read.table(file="D:/M2-SISE/Prog_Stat_R/PROJET/autos.txt",header=T,row.names=1,sep="\t",dec=".")
autos.cr <- scale(autos[,1:9],center=T,scale=T)
d.autos <- dist(autos.cr)
cah.ward <- hclust(d.autos,method="ward.D2")
groupes.cah <- cutree(cah.ward,k=4)
groupes.cah=as.factor(groupes.cah)

#Coefficients binomiaux
bin<-function(n,k){
  if(n>k){
    return (factorial(n)/(factorial(k)*factorial(n-k)))
  }else{
    return(0)
  }
}
evaluation<-function(true_label,pred_label){
  if (length(true_label) != length(pred_label) || length(true_label) < 2){
    stop("two parameters doesn't have the same length")
  }

  #création de l'instance
  instance <- list()
  instance$rand_index <- rand(true_label,pred_label)
  instance$v_mesure <- v_mesure(true_label,pred_label)
  class(instance) <- "ealuation metrix"
  #renvoyer le résultat
  return(instance)
}


#Indice de rand
rand<-function(true_label,pred_label){
  tab=table(true_label,pred_label)
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
  
  ari=(sum-(res/bin(sum(tab),2)))/(0.5*res-(res/bin(sum(tab),2)))
  return(ari)
}

#V-mesure
v_mesure<-function(true_label,pred_label){
  tab=table(true_label,pred_label)
  hck=0
  n=length(g1)
  for (k in 1:ncol(tab)){
    for (c in 1:nrow(tab)){
      hck=hck+(tab[c,k]/n)*log(tab[c,k]/sum(tab[,k]))
    }
  }
  hck=-hck
  
  hkc=0
  for(c in 1:nrow(tab)){
    for (k in 1:ncol(tab)){
      hkc=hkc+(tab[c,k]/n)*log(tab[c,k]/sum(tab[c,]))
    }
  }
  hkc=-hkc
  
  hc=0
  nbc=length(g1)
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
  v=2*((h*c)/(h+c))
}

barycentre<-function(X,grp){
  nb.col<-ncol(X)
  bary<-list()
  for (i in unique(grp)){
    print(i)
    data<-X[which(grp==i),]
    res<-list()
    j=1
    while(j <=nb.col){
      if(class(data[,j])=="integer"){
        med<-median(data[,j])
        res<-append(med,res)
      }else{
        mod<-count(data[,j])[which.max(count(data[,j])[,2]),1]
        res<-append(mod,res)
      }
      j=j+1
    }
    nom=paste("grp",as.character(i),sep=" ")
    bary[[nom]]<-res
  }
  return(bary)
}

ind.type<-function(X,grp){
  bary<-barycentre(autos,groupes.cah)
  score_quanti<-rep(0,nrow(X))
  score_quali<-rep(0,nrow(X))
  for(i in grp){
    data<-X[whih(grp==i),]
    nom=paste("grp",as.character(i),sep=" ")
  }
  
}


library(plyr)

test<-function(varactive,cluster){
  varactivegrp <- varactive %>% mutate(grp = factor(cluster))
  
  meantab <-varactivegrp %>% group_by(grp) %>%
    summarise_if(.predicate = function(x) is.numeric(x),
                 .funs = list(mean)) %>% select_if(function(x) is.numeric(x))
  
  modtab <-varactivegrp %>% group_by(grp) %>%
    summarise_if(.predicate = function(x) is.character(x),
                 .funs = list(max(length))) %>% select_if(function(x) is.numeric(x))
  
  ntab <-varactivegrp %>% group_by(grp) %>%
    summarise_if(.predicate = function(x) is.numeric(x),
                 .funs = list(length))  %>% select_if(function(x) is.numeric(x))
  
  meanfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
  
  nfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length))
  
  varfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(var))
}

m<-evaluation(g1,g2)


