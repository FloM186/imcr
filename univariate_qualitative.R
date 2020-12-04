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
  instance$v.cramer <- v.cramer(classe, var)
  instance$l.profil <- l.profil(classe, var)
  instance$c.profil <- c.profil(classe, var)
  instance$h <- h.value.test(classe, var)
  instance$phi <- phi.value.test(classe, var)
  class(instance) <- "univariate qualitative"
  return(instance)
}



v.cramer <- function(classe, var, digits=5){
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
  }else if(is.factor(var) || is.character(var)){
      contingence = table(classe,var)
      khi = chisq.test(contingence, correct=F)$statistic
      dim = min(nrow(contingence),ncol(contingence)) - 1
      v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),digits)
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


h.value.test <- function(classe, var, digits=4){
  if(is.factor(var) || is.character(var)){
    tab=table(classe,var)
    name = colnames(tab)
    nbr_classe = length(levels(as.factor(classe)))
    nbr_mod = length(levels(as.factor(var)))
    results = data.frame(NA, ncol=3, nrow = nbr_classe*nbr_mod)
    colnames(results) = c("class", "modality", "h")
    cpt=1
    for(i in 1:nbr_classe){
      for(j in 1:nbr_mod){
        phi.lg = 2*asin(sqrt(tab[i,j] / sum(tab[i,])))
        mod_target = sum(tab[,j]) - tab[i,j] 
        phi.la = 2*asin(sqrt(mod_target / sum(tab[-i,])))
        h = phi.lg - phi.la
        results[cpt,] = c(i,name[j], round(h,digits))
        cpt = cpt+1
      }
    }
    results[,3] = as.numeric(results[,3])
    print(ggplot(data=results, aes(x=modality, y=phi, fill=class)) +
            geom_bar(stat="identity", position=position_dodge()) + ggtitle("Barplot for h values by modalities according to class")+
            geom_hline(aes(yintercept = 0.2,colour = "small value"),linetype = 1, size=1.5)+
            geom_hline(aes(yintercept = 0.5,colour = "medium value"),linetype = 1, size=1.5)+
            geom_hline(aes(yintercept = 0.8,colour = "large value"),linetype = 1, size=1.5)+
            geom_text(aes(label=h), position=position_dodge(width=0.9), vjust=-0.25, size=3))
    return(results)
  }  else if(class(var) == "data.frame"){
    ls = list()
    cpt=1
    for(i in 1:ncol(var)){
      if(is.factor(var[,i]) || is.character(var[,i])){
        ls[[cpt]] = h.value.test(classe, var[,i])
        cpt = cpt +1
      }
    }
    return(ls)
  }  
}


sign_h_value <- function(tab){
  tab = cbind(tab,apply(tab,1,sum))
  phi.lg = 2*asin(sqrt(tab[1,1]/tab[1,ncol(tab)]))
  phi.la = 2*asin(sqrt( (sum(tab[,1])-tab[1,1]) / (sum(tab[,ncol(tab)])-tab[1,ncol(tab)]) ))
  h = phi.lg - phi.la
  h2 = abs(h)
    # print(ggplot() +
    #   geom_hline(aes(yintercept = 0.2,color = "small absolute value"),linetype = 1, size=1.5)+
    #   geom_hline(aes(yintercept = 0.5,colour = "medium absolute value"),linetype = 1, size=1.5)+
    #   geom_hline(aes(yintercept = 0.8,colour = "large absolute value"),linetype = 1, size=1.5)+
    #   geom_hline(aes(yintercept = h2 ,colour = "h absolute value"),linetype = 2, size=2)+
    #   ggtitle("h according to statistical significance"))
  return(h)
}

phi.value.test <- function(classe, var, digits=4){
  if(is.factor(var) || is.character(var)){
    tab=table(classe,var)
    name = colnames(tab)
    nbr_classe = length(levels(as.factor(classe)))
    nbr_mod = length(levels(as.factor(var)))
    results = data.frame(NA, ncol=3, nrow = nbr_classe*nbr_mod)
    colnames(results) = c("class", "modality", "phi")
    cpt=1
    for(i in 1:nbr_classe){
      for(j in 1:nbr_mod){
        target = tab[i,j]
        group_target = sum(tab[i,]) - target
        mod_target = sum(tab[,j]) - target
        other = sum(tab) - (target+group_target+mod_target)
        tab2 = as.table(cbind(c(target,mod_target),c(group_target, other)))
        phi = sqrt(chisq.test(tab2,simulate.p.value = TRUE)$statistic/sum(tab2))
        signe = sign_h_value(tab2, bool = TRUE)
        if(sign(signe) == -1){
          phi = -phi
        }
        results[cpt,] = c(i,name[j], round(phi,digits))
        cpt = cpt+1
      }
    }
    results[,3] = as.numeric(results[,3])
    print(ggplot(data=results, aes(x=modality, y=phi, fill=class)) +
            geom_bar(stat="identity", position=position_dodge()) + ggtitle("Barplot for phi values by modalities according to class")+
            geom_hline(aes(yintercept = 0.1,colour = "small value"),linetype = 1, size=1.5)+
            geom_hline(aes(yintercept = 0.3,colour = "medium value"),linetype = 1, size=1.5)+
            geom_hline(aes(yintercept = 0.5,colour = "large value"),linetype = 1, size=1.5)+
            geom_text(aes(label=phi), position=position_dodge(width=0.9), vjust=-0.25, size=3))
    return(results)
  }else if(class(var) == "data.frame"){
    ls = list()
    cpt=1
    for(i in 1:ncol(var)){
      if(is.factor(var[,i]) || is.character(var[,i])){
        ls[[cpt]] = phi.value.test(classe, var[,i])
        cpt = cpt +1
      }
    }
    return(ls)
  }
}



