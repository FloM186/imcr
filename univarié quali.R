#install.packages(fmsb)
library(fmsb)

data(hdv2003)
X=hdv2003

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
      if(is.factor(var[,i])){
        contingence = table(classe,var[,i])
        khi = chisq.test(contingence)$statistic
        dim = min(nrow(contingence),ncol(contingence)) - 1
        v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),5)
        cramer_var = append(cramer_var,names_col[i])
        cramer_val = append(cramer_val,as.numeric(v_cramer))
        tab_cramer = cbind(cramer_var,cramer_val)
      }
    }
    
    data <- as.data.frame(matrix(as.numeric(tab_cramer[,2]), ncol=nrow(tab_cramer)))
    colnames(data) = tab_cramer[,1]
    data = rbind(rep(1,length(tab_cramer)),rep(0,length(tab_cramer)),data)
    
    radarchart(data, axistype=2, title = "Cramer's v by variable", pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=0.8 )
    
    return(tab_cramer)
    
    # On calcul le v de cramer si seulement une variable qualitative a été passée en paramètre
  }else if(is.factor(var)){
      contingence = table(classe,var)
      khi = chisq.test(contingence, correct=F)$statistic
      dim = min(nrow(contingence),ncol(contingence)) - 1
      v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),5)
      return(v_cramer)
    }
}



l.profil <- function(classe, var, digits=1){
  tab=table(classe,var)
  name = names(dimnames(tab))
  
  print(ggplot(data=as.data.frame(round(prop.table(tab,1)*100,digits)), aes(x=var, y=Freq, fill=classe)) +
          geom_bar(stat="identity", position=position_dodge()) + ggtitle("Barplot by variables according to class"))
  
  tab=rbind(tab,Ensemble = apply(tab,2,sum))
  tab = round(prop.table(tab,1)*100,digits)
  tab=cbind(tab, Total = apply(tab,2,sum))
  tab = as.table(tab)
  names(dimnames(tab)) = name
  return(tab)
}


c.profil <- function(classe, var,digits=1){
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
  h = abs(h)
  print(ggplot() +
    geom_hline(aes(yintercept = 0.2,color = "small"),linetype = 2)+
    geom_hline(aes(yintercept = 0.5,colour = "medium"),linetype = 2)+
    geom_hline(aes(yintercept = 0.8,colour = "large"),linetype = 2)+
    geom_hline(aes(yintercept = h ,colour = "h"),linetype = 1)+
    ggtitle("h according to statistical significance"))
  return(h)
}

h.value.test(tab)


