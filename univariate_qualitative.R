#install.packages(fmsb)
library(fmsb)
library(questionr)
library(ggplot2)
library(RColorBrewer)
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

#classe=sample(c(1,2,3),2000,replace=T)

#constructor for S3 class
quali_caracterisation <- function(active_variables, clusters){
  
  #instance creation
  instance <- list()
  instance$v.cramer <- v.cramer(active_variables, clusters)
  instance$l.profil <- l.profil(active_variables, clusters)
  instance$c.profil <- c.profil(active_variables, clusters)
  instance$h <- h.value.test(active_variables, clusters)
  instance$phi <- phi.value.test(active_variables, clusters)
  class(instance) <- "univariate_qualitative"
  return(instance)
}



v.cramer <- function(active_variables, clusters, digits=5){
  
  #Tests if the variables passed in parameter are in the form of data.frame
  if(class(active_variables) == "data.frame"){
    names_col = names(active_variables)
    cramer_active_variables = c()
    cramer_val = c()
    
    #We calculate the cramer's v for all the qualitative variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        contingence = table(clusters,active_variables[,i])
        khi = chisq.test(contingence, simulate.p.value = TRUE)$statistic
        dim = min(nrow(contingence),ncol(contingence)) - 1
        v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),digits)
        
        #Two vectors, one for variables names, the other for values
        cramer_active_variables = append(cramer_active_variables,names_col[i])
        cramer_val = append(cramer_val,as.numeric(v_cramer))
      }
    }
    
    #Creation of a data frame from the two vectors for ggplot
    tab_cramer = cbind(cramer_active_variables,cramer_val)
    data <- as.data.frame(matrix(as.numeric(tab_cramer[,2]), ncol=nrow(tab_cramer)))
    colnames(data) = tab_cramer[,1]
    data = rbind(rep(1,length(tab_cramer)),rep(0,length(tab_cramer)),data)
    
    #Radarchart only if the number of variables is greater than 2
    if(length(cramer_active_variables) > 2){
      print(radarchart(data, axistype=2, title = "Cramer's v by variable", pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=0.8 ))
    
    #Otherwise, we print a barplot
    }else{
      tab_cramer = as.data.frame(tab_cramer)
      tab_cramer[,2] = as.numeric(tab_cramer[,2])
      print(ggplot(data=tab_cramer, aes(x=cramer_active_variables, y=cramer_val)) +
              geom_bar(stat="identity", position=position_dodge(), width=0.75, fill=brewer.pal(n = 3, name = "Dark2")[1]) + ggtitle("Cramer's v by variables")+
              scale_y_continuous(limits=c(0,1))+
              geom_text(aes(label=cramer_val), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
              labs(x = "Variables", y = "Cramer's value")+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")))
    }
    
    vec.cramer=setNames(cramer_val,cramer_active_variables)
    return(list("Cramer's v", vec.cramer))
    
    
    #We calculate the cramer's v if only one qualitative variable has been passed as a parameter
    }else if(is.factor(active_variables) || is.character(active_variables)){
      contingence = table(clusters,active_variables)
      khi = chisq.test(contingence, simulate.p.value = TRUE)$statistic
      dim = min(nrow(contingence),ncol(contingence)) - 1
      v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),digits)
      return(list("Cramer's v", v_cramer))
    }
}



l.profil <- function(active_variables, clusters, digits=2){
  
  #Tests if only one variable passed in parameter
  if(is.factor(active_variables) || is.character(active_variables)){
    
    #We create the contingency variable
    tab=table(clusters,active_variables)
    name = names(dimnames(tab))
    
    #Print a barplot for the contingency table
    print(ggplot(data=as.data.frame(round(prop.table(tab,1)*100,digits)), aes(x=active_variables, y=Freq, fill=clusters)) +
            geom_bar(stat="identity", position=position_dodge()) + ggtitle("Proportion by modalities according to class") +
            labs(x = "Variables", y = "Proportion")+
            scale_y_continuous(expand=c(0.004,0)) +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
            axis.title.y=element_text(size=rel(1.4)),
            axis.title.x=element_text(size=rel(1.4)),
            panel.background = element_rect(fill = NA, color = "gray40")) +
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
    
    #Add a row Ensemble which is the sum of each columns 
    tab=rbind(tab,Ensemble = apply(tab,2,sum))
    #We put the values in percentage
    tab = round(prop.table(tab,1)*100,digits)
    #Add a column Total which is the sum of each rows
    tab=cbind(tab, Total = apply(tab,1,sum))
    tab = as.table(tab)
    names(dimnames(tab)) = name
    return(list("Row Profile Table", tab))
  
    #Tests if many variables are passed in parameter
    } else if(class(active_variables) == "data.frame"){
      ls = list()
      cpt=1
      
      #recursiveness of the function for each variables
      for(i in 1:ncol(active_variables)){
        if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
          ls[[cpt]] = l.profil(active_variables[,i], clusters)
          cpt = cpt +1
        }
      }
      return(ls)
    }
}



c.profil <- function(active_variables, clusters, digits=2){
  
  #Tests if only one variable passed in parameter
  if(is.factor(active_variables) || is.character(active_variables)){
    
    #We create the contingency variable
    tab=table(clusters,active_variables)
    name = names(dimnames(tab))
    
    #Print a barplot for the contingency table
    print(ggplot(data=as.data.frame(round(prop.table(tab,2)*100,digits)), aes(x=active_variables, y=Freq, fill=clusters)) +
            geom_bar(stat="identity", position=position_dodge()) + ggtitle("Proportion by modalities according to class") +
            labs(x = "Variables", y = "Proportion")+
            scale_y_continuous(expand=c(0.004,0)) +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40")) +
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
    
    #Add a row Ensemble which is the sum of each rows
    tab=cbind(tab,Ensemble = apply(tab,1,sum))
    #We put the values in percentage
    tab = round(prop.table(tab,2)*100,digits)
    #Add a column Total which is the sum of each columns
    tab=rbind(tab, Total = apply(tab,2,sum))
    tab = as.table(tab)
    names(dimnames(tab)) = name
    return(list("Column Profile Table", tab))
 
    #Tests if many variables are passed in parameter
    } else if(class(active_variables) == "data.frame"){
    ls = list()
    cpt=1
    
    #recursiveness of the function for each variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        ls[[cpt]] = c.profil(active_variables[,i], clusters)
        cpt = cpt +1
      }
    }
    return(ls)
  }
}


h.value.test <- function(active_variables, clusters, digits=4){
  
  #Tests if only one variable passed in parameter
  if(is.factor(active_variables) || is.character(active_variables)){
    
    tab=table(clusters,active_variables)
    name = colnames(tab)
    
    #Several values are retrieved for the calculation of the test value h
    nbr_clusters = length(levels(as.factor(clusters)))
    nbr_mod = length(levels(as.factor(active_variables)))
    
    #Creation of the data frame that will contain the results
    results = data.frame(NA, ncol=3, nrow = nbr_clusters*nbr_mod)
    colnames(results) = c("clusters", "modality", "h")
    
    #For each cluster and modalities of the variable, we calculate the test value h and store it in our data frame.
    cpt=1
    for(i in 1:nbr_clusters){
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
    
    #We print our results in a barplot with threshold values
    print(ggplot(data=results, aes(x=modality, y=h, fill=clusters)) +
            geom_bar(stat="identity", position=position_dodge()) + ggtitle("h value by modalities according to class")+
            geom_hline(aes(yintercept = 0.2,linetype = "small value"),colour = "yellow", size=1)+
            geom_hline(aes(yintercept = 0.5,linetype = "medium value"),colour = "orange", size=1)+
            geom_hline(aes(yintercept = 0.8,linetype = "large value"),colour = "darkred", size=1)+
            scale_linetype_manual(name = "Thresholds", values=c(2,2,2),guide = guide_legend(override.aes = list(color = c("darkred", "orange","yellow"))))+
            geom_text(aes(label=h), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
            labs(x = "Variables", y = "h")+
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40"))+
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
    
    print(results%>%  as.data.frame() %>% formattable(align = c("c","c", "r"),
           list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
          area(col = 3) ~ formatter("span",style = x ~ style(font.weight = "bold",color = ifelse( x > 0.8,  "#00CC00",
                                                                                          ifelse(x > 0.5, "#FF8000",
                                                                                          ifelse(x>0.2,"#FF0000", "black"))))))))
    
    return(list("h value", results))
    
    #Tests if many variables are passed in parameter
  }else if(class(active_variables) == "data.frame"){
    ls = list()
    cpt=1
    
    #recursiveness of the function for each variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        ls[[cpt]] = h.value.test(active_variables[,i], clusters)
        cpt = cpt +1
      }
    }
    return(ls)
  }  
}


sign_h_value <- function(tab){
  
  #This function is used to calculate h to get his sign for calculating phi value 
  
  tab = cbind(tab,apply(tab,1,sum))
  phi.lg = 2*asin(sqrt(tab[1,1]/tab[1,ncol(tab)]))
  phi.la = 2*asin(sqrt( (sum(tab[,1])-tab[1,1]) / (sum(tab[,ncol(tab)])-tab[1,ncol(tab)]) ))
  h = phi.lg - phi.la
  return(h)
}

phi.value.test <- function(active_variables, clusters, digits=4){
  
  #Tests if only one variable passed in parameter
  if(is.factor(active_variables) || is.character(active_variables)){
    tab=table(clusters,active_variables)
    name = colnames(tab)
    
    #Several values are retrieved for the calculation of the test value phi
    nbr_clusters = length(levels(as.factor(clusters)))
    nbr_mod = length(levels(as.factor(active_variables)))
    
    #Creation of the data frame that will contain the results
    results = data.frame(NA, ncol=3, nrow = nbr_clusters*nbr_mod)
    colnames(results) = c("clusters", "modality", "phi")
    
    #For each cluster and modalities of the variable, we calculate the test value phi and store it in our data frame.
    cpt=1
    for(i in 1:nbr_clusters){
      for(j in 1:nbr_mod){
        target = tab[i,j]
        group_target = sum(tab[i,]) - target
        mod_target = sum(tab[,j]) - target
        other = sum(tab) - (target+group_target+mod_target)
        tab2 = as.table(cbind(c(target,mod_target),c(group_target, other)))
        phi = sqrt(chisq.test(tab2,simulate.p.value = TRUE)$statistic/sum(tab2))
        signe = sign_h_value(tab2)
        if(sign(signe) == -1){
          phi = -phi
        }
        results[cpt,] = c(i,name[j], round(phi,digits))
        cpt = cpt+1
      }
    }
    results[,3] = as.numeric(results[,3])
    
    #We print our results in a barplot with threshold values
    print(ggplot(data=results, aes(x=modality, y=phi, fill=clusters)) +
            geom_bar(stat="identity", position=position_dodge()) + ggtitle("Phi value by modalities according to class")+
            geom_hline(aes(yintercept = 0.1,linetype = "small value"),colour = "yellow", size=1)+
            geom_hline(aes(yintercept = 0.3,linetype = "medium value"),colour = "orange", size=1)+
            geom_hline(aes(yintercept = 0.5,linetype = "large value"),colour = "darkred", size=1)+
            scale_linetype_manual(name = "Thresholds", values=c(2,2,2),guide = guide_legend(override.aes = list(color = c("darkred", "orange","yellow"))))+
            geom_text(aes(label=phi), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
            labs(x = "Variables", y = "phi")+
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40"))+
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
    
    
    print(results%>%  as.data.frame() %>% formattable(align = c("c","c", "r"),
          list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
           area(col = 3) ~ formatter("span",style = x ~ style(font.weight = "bold",color = ifelse( x > 0.5,  "#00CC00",
                                                                                           ifelse(x > 0.3, "#FF8000",
                                                                                           ifelse(x>0.1,"#FF0000", "black"))))))))
    
    
    return(list("phi value", results))
    
    
    #Tests if many variables are passed in parameter
  }else if(class(active_variables) == "data.frame"){
    ls = list()
    cpt=1
    
    #recursiveness of the function for each variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        ls[[cpt]] = phi.value.test(active_variables[,i], clusters)
        cpt = cpt +1
      }
    }
    return(ls)
  }
}



