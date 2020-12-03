#install.packages(fmsb)
library(fmsb)
library(questionr)
library(ggplot2)
data(hdv2003)
X=hdv2003

X = read.delim("http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/autos_caracterisation.txt", header=T,sep = "\t")
X = X[,-1]
X2 = X[,-c(8:11)]
X2 = scale(X2,center=T,scale=T)
d = dist(X2)
cah = hclust(d, method="ward.D2")
plot(cah)
#dendrogramme avec matérialisation des groupes
rect.hclust(cah,k=4)
#découpage en 4 groupes
classe <- cutree(cah,k=4)
classe = unname(classe)

classe=sample(c(1,2,3),2000,replace=T)

#"constructeur" pour classe S3
correler <- function(class,var){
  #création de l'instance
  instance <- list()
  instance$tab <- table(class,var)
  instance$v.cramer <- v.cramer(class, var)
  instance$l.profil <- l.profil(class,var)
  instance$c.profil <- c.profil(class,var)
  instance$h <- h.value.test(instance$tab)
  instance$phi <- phi.value.test(instance$tab)
  class(instance) <- "univariate qualitative"
  return(instance)
}



v.cramer <- function(classe, var){
  #Teste si les variables sont sous forme de data.frame
  if(class(var) == "data.frame"){
    names_col = names(var)
    cramer_var = c()
    cramer_val = c()
    #On calcul le v de cramer pour toutes les variables qualitatives
    for(i in 1:ncol(var)){
      if(is.factor(var[,i]) || is.character(var[,i])){
        contingence = table(classe,var[,i])
        khi = chisq.test(contingence, simulate.p.value = TRUE)$statistic
        dim = min(nrow(contingence),ncol(contingence)) - 1
        v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),5)
        cramer_var = append(cramer_var,names_col[i])
        cramer_val = append(cramer_val,as.numeric(v_cramer))
      }
    }
    tab_cramer = cbind(cramer_var,cramer_val)
    data <- as.data.frame(matrix(as.numeric(tab_cramer[,2]), ncol=nrow(tab_cramer)))
    colnames(data) = tab_cramer[,1]
    data = rbind(rep(1,length(tab_cramer)),rep(0,length(tab_cramer)),data)
    
    radarchart(data, axistype=2, title = "Cramer's v by variable", pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=0.8 )
    
    vec.cramer=setNames(cramer_val,cramer_var)
    return(vec.cramer)
    
    # On calcul le v de cramer si seulement une variable qualitative a été passée en paramètre
  }else if(is.factor(var)){
      contingence = table(classe,var)
      khi = chisq.test(contingence, correct=F)$statistic
      dim = min(nrow(contingence),ncol(contingence)) - 1
      v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),5)
      return(v_cramer)
    }
}



l.profil <- function(classe, var, digits=2){
  tab=table(classe,var)
  name = names(dimnames(tab))
  
  print(ggplot(data=as.data.frame(round(prop.table(tab,1)*100,digits)), aes(x=var, y=Freq, fill=classe)) +
          geom_bar(stat="identity", position=position_dodge()) + ggtitle("Barplot by variables according to class"))
  
  tab=rbind(tab,Ensemble = apply(tab,2,sum))
  tab = round(prop.table(tab,1)*100,digits)
  tab=cbind(tab, Total = apply(tab,1,sum))
  tab = as.table(tab)
  names(dimnames(tab)) = name
  return(tab)
}



c.profil <- function(classe, var,digits=2){
  tab=table(classe,var)
  name = names(dimnames(tab))
  
  print(ggplot(data=as.data.frame(round(prop.table(tab,2)*100,digits)), aes(x=var, y=Freq, fill=classe)) +
          geom_bar(stat="identity", position=position_dodge()) + ggtitle("Barplot by variables according to class"))
  
  tab=cbind(tab,Ensemble = apply(tab,1,sum))
  tab = round(prop.table(tab,2)*100,digits)
  tab=rbind(tab, Total = apply(tab,2,sum))
  tab = as.table(tab)
  names(dimnames(tab)) = name
  return(tab)
}



h.value.test <- function(tab, indice.tab.modalite = 1, indice.tab.group = 1){
  tab = cbind(tab,apply(tab,1,sum))
  phi.lg = 2*asin(sqrt(tab[indice.tab.group,indice.tab.modalite]/tab[indice.tab.group,ncol(tab)]))
  phi.la = 2*asin(sqrt( (sum(tab[,indice.tab.modalite])-tab[indice.tab.group,indice.tab.modalite]) / (sum(tab[,ncol(tab)])-tab[indice.tab.group,ncol(tab)]) ))
  h = phi.lg - phi.la
  h2 = abs(h)
  print(ggplot() +
    geom_hline(aes(yintercept = 0.2,color = "small absolute value"),linetype = 1, size=1.5)+
    geom_hline(aes(yintercept = 0.5,colour = "medium absolute value"),linetype = 1, size=1.5)+
    geom_hline(aes(yintercept = 0.8,colour = "large absolute value"),linetype = 1, size=1.5)+
    geom_hline(aes(yintercept = h2 ,colour = "h absolute value"),linetype = 2, size=2)+
    ggtitle("h according to statistical significance"))
  return(h)
}

phi.value.test <- function(tab){
  if(ncol(tab) != 2 && nrow(tab) != 2){
    stop("Table must be 2x2")
  }
  khi = chisq.test(tab)$statistic
  n = sum(tab)
  phi = as.numeric(sqrt(khi/n))
  signe = h.value.test(tab)
  if(sign(signe) == -1){
    phi = -phi
  }
  phi2 = abs(phi)
  print(ggplot() +
          geom_hline(aes(yintercept = 0.1,color = "small absolute value"),linetype = 1, size=1.5)+
          geom_hline(aes(yintercept = 0.3,colour = "medium absolute value"),linetype = 1, size=1.5)+
          geom_hline(aes(yintercept = 0.5,colour = "large absolute value"),linetype = 1, size=1.5)+
          geom_hline(aes(yintercept = phi2 ,colour = "phi absolute value"),linetype = 2, size=2)+
          ggtitle("phi according to statistical significance"))
  return(phi)
}
  
phi.value.test(tab)

