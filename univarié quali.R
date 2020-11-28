#install.packages(fmsb)
library(fmsb)
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
data(hdv2003)
X=hdv2003

classe=sample(c(1,2,3),2000,replace=T)

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
  tab=cbind(tab,Ensemble = apply(tab,1,sum))
  tab = round(prop.table(tab,2)*100,digits)
  tab=rbind(tab, Total = apply(tab,2,sum))
  tab = as.table(tab)
  names(dimnames(tab)) = name
  return(tab)
}




