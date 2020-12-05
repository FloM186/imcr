#Plusieurs packages pour faire de l'ADD sous R
#Choix d'utiliser deux packages : FactoMineR (pour l'analyse) et factoextra (pour la visualisation, des données, basée sur ggplot2)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("corrplot")

#
# Importation des données et sélection des éléments -----------------------
#
data<-read.csv("D:/M2-SISE/Python/data/careval.csv",header=FALSE)
data<-data[1:1000,]
for (i in 1:7){
  data[,i]<-as.factor(data[,i])
}
classe<-as.factor(floor(runif(1000, min=1, max=4)))

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
    
    #radarchart(data, axistype=2, title = "Cramer's v by variable", pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=0.8 )
    
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

tab<-function(obj,nb_dim){
  list.tab<-list()
  for(i in 1:nb_dim){
    coord<-obj$coord[,i]
    contrib<-obj$contrib[,i]
    cos2<-obj$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    display<-display[which(display[,2]>=median(display[,2])&display[,3]>=0.5),]
    nom<-paste("Dim",i,sep=" ")
    list.tab[[nom]]<-display
  }
  return(list.tab)
}

desc.dim<-function(res,nb_dim){
  list.desc<-list()
  for(i in 1:nb_dim){
    list.desc[[i]]<-res[[i]]$quali
  }
  return(list.desc)
}

# Analyse factorielle des Correspondances (ACM) --------------------------------
#
ACM_val<-function(X,y,quanti.supp=NULL,axes = c(1, 2),graph=NULL){
  if (length(X) < 2){
    stop("X doesn't contain enough variables")
  }
  res.mca <- MCA (X,graph = FALSE,quanti.sup = quanti.supp)
  ##Valeurs propres
  eig.val <- get_eigenvalue(res.mca)
  print(fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 100)))                          
  
  nb_dim<-readline(prompt="How many axes do you want to keep ? " )
  nb_dim<-as.integer(nb_dim)
  
  #Tableau var
  var<-tab(get_mca_var(res.mca),nb_dim)
  
  #Tableau individu
  ind<-tab(get_mca_ind(res.mca),nb_dim)
  
  # Description de la dimension
  res.desc <- dimdesc(res.mca, axes = 1:nb_dim)
  desc <- desc.dim(res.desc,nb_dim)
  
  #creation de l'instance
  instance <- list()
  instance$eig.values<-eig.val
  instance$var.tab <- var
  instance$ind.tab<-ind
  instance$desc.dim<-desc
  if(is.null(quanti.supp)==FALSE){
    instance$quanti.supp<-res.mca$quanti
  }
  class(instance) <- c("ACM_val","list ")
  
  if(!is.null(graph)){
    cramer<-as.numeric(v.cramer(y,X)[,2])
    plot.ACM_val(res.mca,y,cramer,quanti.supp)
  }
  #renvoyer le résultat
  return(instance)
}

plot.ACM_val<-function(x,y, cramer,quanti.supp,axes = c(1, 2)){
  #Graphique des variables colorés selon le v de cramer
  
  print(fviz_mca_var (x, choice="mca.cor",col.var = cramer,
                        gradient.cols = c("blue", "yellow", "red"),
                        legend.title = "V de Cramer",
                        repel = TRUE,axes = axes))
  

  print(corrplot(x$var$cos2, is.corr=FALSE))# graphique du Cos2 des points colonnes sur tous les axes
  print(corrplot(x$var$contrib, is.corr=FALSE))# graphique de la contributions des points colonnes sur tous les axes
  
  #Individus colorié selon classes
  print(fviz_mca_ind (x,
                        label = "none", # masquer le texte des individus
                        habillage = y, # colorer par groupes
                        
                        addEllipses = TRUE, ellipse.type = "confidence",
                        ggtheme = theme_minimal (),axes = axes))
  if(is.null(quanti.supp)==FALSE){
    #Quanti supp
    print(fviz_mca_var(x, choice = "quanti.sup",
                         ggtheme = theme_minimal(),axes=axes))
  }
  
  
}

print.ACM_val <- function (x, file = NULL, sep = ";", ...){
  res.mca <- x
  if (!inherits(res.mca, "ACM_val")) stop("non convenient data")
  cat("**Results Mutltivarial Analysis using PCA**\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(24, 2), list(1:24, c("name", "description")))
  res[1, ] <- c("eig.values", "eigenvalues")
  res[2, ] <- c("$var.tab", "results for the variables")
  res[3, ] <- c("$ind.tab", "results for the individus")
  res[4, ] <- c("$desc.dim", "description of the dimension")
  indice <- 5
  if (!is.null(res.mca$quanti.sup)){
    res[indice, ] <- c("$quanti.supp", "results for the supplementary numerical variables")
  }
  print(res[1:indice,])
  if (!is.null(file)) {
    write.infile(res.mca,file = file, sep=sep)
    print(paste("All the results are in the file",file))
  }
}

res1<-ACM_val(data,classe,graph=TRUE)


print(res1)

res.mca <- MCA (data,graph = FALSE)
#Graphique des classes par variables a coder
ggplot(data) + 
  geom_point(aes(x = res.mca$ind$coord[,1], y = res.mca$ind$coord[,2], 
                 color = classe, size = data[,7]))
