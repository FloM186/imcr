#Plusieurs packages pour faire de l'ADD sous R
#Choix d'utiliser deux packages : FactoMineR (pour l'analyse) et factoextra (pour la visualisation, des données, basée sur ggplot2)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

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

tab.var<-function(var,nb_dim){
  list.tab<-list()
  for(i in 1:nb_dim){
    coord<-var$coord[,i]
    contrib<-var$contrib[,i]
    cos2<-var$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    nom<-paste("dim",as.character(i))
    list.tab[[i]]<-display
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
ACM_val<-function(X,nbr_dim,quanti.supp=NULL){
  if (length(X) < 2){
    stop("X doesn't contain enough variables")
  }
  if (nbr_dim < 1){
    stop("nbr_dim have to be superior than 1")
  }
  if(is.null(quanti.supp)==FALSE){
    test=TRUE
    for (i in 1:quanti.supp){
      if(is.factor(quanti.supp[,i])==FALSE){
        test=FALSE
      }
    }
    if(test==FALSE){
      stop("index quali_supp is not factor")
    }
  }
  res.mca <- MCA (X,graph = FALSE,quanti.sup = quanti.supp)
  ##Valeurs propres
  eig.val <- get_eigenvalue(res.mca)
  graph_eig<-fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 100))                          
  # Description de la dimension
  res.desc <- dimdesc(res.mca, axes = 1:nbr_dim)#fonction dimdesc() [dans FactoMineR]
  
  #création de l'instance
  instance <- list()
  instance$eig.values<-eig.val
  instance$eig.graph<-graph_eig
  instance$var.tab <- tab.var(get_mca_var(res.mca),nbr_dim)
  instance$desc_dim<-desc.dim(res.desc,nbr_dim)
  if(is.null(quanti.supp)==FALSE){
    instance$quanti.supp<-res.mca$quanti
  }
  class(instance) <- "ACM_val"
  #renvoyer le résultat
  return(instance)
}

ACM_graph<-function(X,y,dim1,dim2,quanti.supp=NULL){
  if (length(X) < 2){
    stop("X doesn't contain enough variables")
  }
  if (length(y)!=nrow(X)){
    stop("X and y doesn't have the same length")
  }
  if(is.null(quanti.supp)==FALSE){
    test=TRUE
    for (i in 1:quanti.supp){
      if(is.factor(quanti.supp[,i])==FALSE){
        test=FALSE
      }
    }
    if(test==FALSE){
      stop("index quali_supp is not factor")
    }
  }
  res.mca <- MCA (X,graph = FALSE,quanti.sup = quanti.supp)

  #Graphique des variables colorés selon le v de cramer
  cramer<-as.numeric(v.cramer(y,X)[,2])
  graph1<-fviz_mca_var (res.mca, choice="mca.cor",col.var = cramer,
                gradient.cols = c("blue", "yellow", "red"),
                legend.title = "V de Cramer",
                repel = TRUE,axes = c(dim1, dim2))
  #Individus colorié selon classes
  graph2<-fviz_mca_ind (res.mca,
                label = "none", # masquer le texte des individus
                habillage = y, # colorer par groupes
                
                addEllipses = TRUE, ellipse.type = "confidence",
                ggtheme = theme_minimal (),axes = c(dim1, dim2))
  if(is.null(quanti.supp)==FALSE){
    #Quanti supp
    graph3<-fviz_mca_var(res.mca, choice = "quanti.sup",
                 ggtheme = theme_minimal())
  }
  
  
  #création de l'instance
  instance <- list()
  instance$var_cramer<-graph1
  instance$ind_class<-graph2
  if(is.null(quanti.supp)==FALSE){
    instance$quanti.supp<-graph3
  }
  class(instance) <- "ACM_graph"
  #renvoyer le résultat
  return(instance)
}

res1<-ACM_val(data,2)
res2<-ACM_graph(data,classe,1,2)





res.mca <- MCA (data,graph = FALSE)
#Graphique des classes par variables a coder
ggplot(data) + 
  geom_point(aes(x = res.mca$ind$coord[,1], y = res.mca$ind$coord[,2], 
                 color = classe, size = data[,7]))
