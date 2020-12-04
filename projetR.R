#############   References + texte ####
# https://medium.com/analytics-vidhya/a-fresh-start-for-r-in-vscode-ec61ed108cf6
#cramersV
#https://www.rdocumentation.org/packages/rcompanion/versions/2.3.25/topics/cramerV

#############   load packages######
#install.packages('clValid')
#install.packages("clusterCrit")
#install.packages('ClustOfVar')
library(tidyverse)
library(magrittr)
library(clValid)
library(clusterCrit)
library(ClustOfVar)
library(ggpubr)
library(factoextra)
library(ade4)
library(ggpubr)
#############   data test iris#######
#data test
iris <- read.csv("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/iris_data.csv")
#View(iris)
iris <- iris %>% mutate(species = factor(species))
str(iris)

#############  K means UNIVARIE
##########quantitatif
## Loading required package: Ckmeans.1d.dp
#https://cran.r-project.org/web/packages/Ckmeans.1d.dp/vignettes/Ckmeans.1d.dp.html
#https://www.rdocumentation.org/packages/Ckmeans.1d.dp/versions/4.3.3/topics/Univariate%20Clustering
#install.packages('Ckmeans.1d.dp')
library(Ckmeans.1d.dp)
colnames(iris)[3]
df1<- subset(iris, select=3)
df1<- iris[,3]
result <- Ckmeans.1d.dp(df1$petal_length, 3)
plot(result)
#clusters
result$cluster
df1 <- df1 %>% mutate(cluster=result$cluster )
head(df1)
str(df1)
#View(df1)

############# KMEANS MULTIVARIEE
#Lorsque les variables sont toutes qualitatives, nous utilisons l'analyse des correspondances multiples (ACM).
#Lorsqu'elles sont composees d'un melange de variables quantitatives et qualitatives, on utilise l'analyse factorielle des donnees mixtes (AFDM)
#qui se revele Ãªtre Ã  la fois une generalisation de l'ACM et de l'ACP
#http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_Cat_Variable_Clustering.pd

##########quantitatif
df2 <- iris
head(df2)
str(df2)
#Calculer k-means avec k = 3
set.seed(123)
res <- kmeans(scale(df2[, -5]), 3, nstart = 25)

# Groupe de chaque individu
res.km$cluster
df2<- df2 %>% mutate(cluster=res$cluster)
#View(df2)
str(df2)
head(df2)
#df <- df%>% rename('V6' = "cluster")
#names(my_data)[names(my_data) == "Sepal.Width"] <- "sepal_width"
# names(df)[6] <- "cluster"

########## qualitatif
#s test acp acm ricco
# Reduction de dimension en utilisant l'ACP
res.pca <- prcomp(df[, -5],  scale = TRUE)
# Coordonnees des individus
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Ajouter les clusters obtenus Ã  l'aide de l'algorithme k-means
ind.coord$cluster <- factor(res.km$cluster)
# Ajouter les groupes d'especes issues du jeu de donnees initial
ind.coord$Species <- df$Species
# Inspection des donnees
head(ind.coord)

# Pourcentage de la variance expliquee par les dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)
  
#############   data test auto#######
auto <- read.csv("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/auto-mpg.csv")
#View(auto.mpg)
auto <- auto %>% mutate(origin = factor(origin), cyl = factor(cyl))
str(auto)
#############   data test fromage#######
fromage <- read.delim("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/fromage.txt")
View(fromage)
str(fromage)

#centrage reduction des donnees
#pour eviter que variables à forte variance pèsent indûment sur les resultats
fromage.cr <- scale(fromage[,-1],center=T,scale=T)
#matrice des distances entre individus
d.fromage <- dist(fromage.cr)
#CAH - critère de Ward
#method = « ward.D2 » correspond au vrai critère de Ward
#utilisant le carre de la distance
cah.ward <- hclust(d.fromage,method="ward.D2")
#affichage dendrogramme
plot(cah.ward)
#dendrogramme avec materialisation des groupes
rect.hclust(cah.ward,k=4)
#decoupage en 4 groupes
groupes.cah <- cutree(cah.ward,k=4)
#liste des groupes
print(sort(groupes.cah))
groupes.cah
head(fromage)
str(fromage)
is.numeric(fromage)
sapply(fromage, is.numeric)
sapply(fromage[,2:10], mean)
length(unique(groupes.cah))

frm <-fromage %>% mutate(grp = factor(groupes.cah))

#############   brouillon ###########
##

#install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")     
fonts()  
right_join(right_join(meantab %>% mutate(cluster=1:4) %>%
                        gather(key, mean, -cluster), sdtab  %>%
                        mutate(cluster=1:4) %>%
                        gather(key, sd, -cluster)), vttab %>% rename(cluster=grp) %>% gather(key, vt, -cluster)) %>%
  ggplot(aes(x=key, y=mean))+
  geom_bar(stat="identity", width=0.75, fill="deepskyblue")+
  geom_point(aes(y=(vt -min_axis_vt)*scale_second_axis ), col = 'violetred2', shape=19)+
  scale_y_continuous(labels=pretty(test2$msd),breaks=pretty(test2$msd),expand = c(0.004,0), limits = c(min_axis_y,amplitude_y),
                     sec.axis = sec_axis(~./scale_second_axis +min_axis_vt, name="vt"))+
  geom_errorbar(aes(ymax=mean + sd, ymin= mean - sd), colour="black", width=.2)+
  labs(x = "Key", y = "Mean")+
  theme_minimal(base_size = 12) +
  theme(text=element_text(family="Calibri"),
        axis.title.y=element_text(size=rel(1.4)),
        axis.title.x=element_text(size=rel(1.4)),
        panel.background = element_rect(fill = NA, color = "gray40")) +
  facet_grid(cluster ~ ., labeller = labeller("Cluster")) + facet_wrap(~ cluster, ncol=2)


#scale sec axis
#amplitude(leftaxis)/amplitude(rightaxis)
#breaksright = 412/(412/12) -6
#yaxis/(412/12) -6
#Left axis LA, right axis RA
#amplitudeLA(A transformer)/(amplitudeLA/amplitudeRA) + premierevaleurRA
#newydataright = (ydata --6)*(412/12)
#(ydata - premierevaleurRA )*(amplitudeLA/amplitudeRA)


##
#############   fonctions du projet ###########
k=NULL
c=NULL
varactivegrp = NULL
varactive = NULL
val_test <- function(varactive, cluster, showgraph=TRUE) {
  if(all(sapply(varactive, is.numeric))==FALSE) {
    print("Les variables actives quantitatives doivent etre numeriques")
  } else if (is.vector(cluster)==FALSE) {
    print("Le clustering des observations doit etre sous forme de vecteur")
  } else {
    varactivegrp <- varactive %>% mutate(grp = factor(cluster))
    
    meantab <-varactivegrp %>% group_by(grp) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>% select_if(function(x) is.numeric(x))
    
    ntab <-varactivegrp %>% group_by(grp) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(length))  %>% select_if(function(x) is.numeric(x))
    
    meanfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
    
    nfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length))
    
    varfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(var))
    
    k=length(unique(cluster))
    c = ncol(varactive)
    vttab <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(vttab)<- colnames(varactive)
    for(j in 1:k) {
      for(i in 1:c) {
        vttab[j,i] <- as.numeric((meantab[j,i] - meanfull[,i])/sqrt(((nfull[,i] - ntab[j,i])/(nfull[,i]-1))*(varfull[,i]/ntab[j,i])))
      }
    }
    
    vttab <- vttab %>% mutate(cluster= 1:k) %>% select(cluster, everything())
    
    full_table<-right_join(right_join(meantab %>% mutate(cluster=1:k) %>%
                                        gather(key, mean, -cluster),
                                      sdtab  %>%
                                        mutate(cluster=1:k) %>%
                                        gather(key, sd, -cluster)),
                           vttab %>% gather(key, vt, -cluster))
    
    barheight<- full_table %>% group_by(key, cluster) %>% summarise(msd = mean + sd)
    
    min_axis_vt<-min(pretty(full_table$vt))
    amplitude_vt<-abs(min(pretty(full_table$vt)))+abs(max(pretty(full_table$vt)))
    
    min_axis_y <- min(pretty(barheight$msd))
    max_axis_y <-barheight %>% ungroup() %>%
      summarise(max1= max(msd), maxtot=round(max1+max1*(1/40))) %>% select(maxtot) %>% as.numeric()
    
    scale_second_axis<-max_axis_y/amplitude_vt
    if(showgraph==TRUE) {
      print(full_table %>%
              ggplot(aes(x=key, y=mean))+
              geom_bar(stat="identity", width=0.75, fill="deepskyblue")+
              geom_point(aes(y=(vt -min_axis_vt)*scale_second_axis ), col = 'violetred2', shape=19)+
              geom_line(aes(y=(vt -min_axis_vt)*scale_second_axis, group=1 ), size= 0.65,col = 'violetred1')+
              scale_y_continuous(labels=pretty(barheight$msd),breaks=pretty(barheight$msd),expand = c(0.004,0), limits = c(min_axis_y,amplitude_y),
                                 sec.axis = sec_axis(~./scale_second_axis +min_axis_vt, name="Test value"))+
              geom_errorbar(aes(ymax=mean + sd, ymin= mean - sd), colour="black", width=.2)+
              labs(x = "Variables", y = "Mean")+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")) +
              facet_grid(cluster ~ ., labeller = labeller("Cluster")) + facet_wrap(~ cluster, ncol=2))} else {}
    
    
    
    results <- list("Tableau des valeurs propres. Plus la valeur de la variable est elevee plus elle contribue à la constitution des groupes",
                    vttab)
    return(results)
    
  }
}



#test:
val_test(fromage[,-1], groupes.cah, showgraph = TRUE)


#graph etoile/spider:
#ne marche pas encore...
#https://webdevdesigner.com/q/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r-65407/
full_table %>% ggplot(aes(x=key, y=vt, group=cluster, color=factor(cluster))) + 
  geom_point() + 
  geom_line(size=2) + 
  xlab("Decils") + 
  ylab("% difference in nº Pk") + 
  #ylim(-50,25) + ggtitle("CL")  + 
  #geom_line(aes(yintercept=0), lwd=1, lty=2) + 
  #scale_x_discrete(limits=c(orden_deciles)) +
  coord_polar()






#################rapport de correlation
rapp_corr <- function(varactive, cluster, showgraph=TRUE, show_moy_conditionnelles=TRUE) {
  if(all(sapply(varactive, is.numeric))==FALSE) {
    print("Les variables actives quantitatives doivent etre numeriques")
  } else if (is.vector(cluster)==FALSE) {
    print("Le clustering des observations doit etre sous forme de vecteur")
  } else {
    varactivegrp <- varactive %>% mutate(grp = factor(cluster))
    
    meantab <-varactivegrp %>% group_by(grp) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>% select_if(function(x) is.numeric(x))
    
    ntab <-varactivegrp %>% group_by(grp) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(length))  %>% select_if(function(x) is.numeric(x))
    
    meanfull <- varactive %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
    
    k=length(unique(cluster))
    c = ncol(varactive)
    sct <-as.data.frame(matrix(nrow = nrow(varactive), ncol=c))
    colnames(sct) = colnames(varactive)
    for(i in 1:c) {
      sct[,i] <- varactive %>% select(,i) %>% mutate_all(~ (.x - as.numeric(meanfull[i]))^2)
    }
    sct <-sct %>% summarise_all(sum)
    
    sce <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(sce) = colnames(varactive)
    for(j in 1:k){
      for(i in 1:ncol(fromage[,-1])) {
        sce[j,i]<-  as.numeric(ntab[j,i]*(meantab[j,i] - meanfull[i])^2)
      }
    }
    sce <-sce %>% summarise_all(sum)
    
    rcor <- sce/sct
    
    if(showgraph==TRUE) {
      print(rcor %>% gather(key, value) %>%
              ggplot(aes(x=key, y=value))+
              geom_bar(stat="identity", width=0.75, fill="deepskyblue")+
              labs(x = "Variables", y = "Rapport de correlation")+
              scale_y_continuous(expand=c(0.004,0))+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")))} else {}
    if(show_moy_conditionnelles==TRUE){results <- list("Tableau des moyennes conditionnelles", meantab,
      "Tableau des rapports de correlation. Il represente la proportion de variance expliquee par les groupes pour chaque variable. Plus il est eleve, plus la variance de la variable pourra etre expliquee par les groupes",
                                                       rcor)} else {
                                                         results <- list("Tableau des rapports de correlation. Il represente la proportion de variance expliquee par les groupes pour chaque variable. Plus il est eleve, plus la variance de la variable pourra etre expliquee par les groupes",
                                                                         rcor)}
    
    return(results)
  }
}
#test :
rapp_corr(fromage[,-1], groupes.cah)

################# effect size


effect_size <- function(varactive, cluster) {
  if(all(sapply(varactive, is.numeric))==FALSE) {
    print("Active variables must be numeric")
  } else if (is.vector(cluster)==FALSE) {
    print("Clusters must be a vector")
  } else {
    
    varactivegrp <- varactive %>% mutate(grp = factor(cluster))
    
    meantab <-varactivegrp %>% group_by(grp) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>% select_if(function(x) is.numeric(x))
    
    
    sdfull <- varactivegrp %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd))

    #j = k  ::: i = col
    k=length(unique(cluster))
    c = ncol(varactive)
    es <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(es)<- colnames(varactive)
    for(j in 1:k) {
      for(i in 1:c) {
        es[j,i] <- as.numeric((meantab[j,i] - meantab[-j,] %>% summarise_all(mean) %>% select(i) %>% as.numeric())/sdfull[,i] )
      }
    }
    
    #U3
    u3 <- sapply(es, pnorm)
    
    #U2
    u2 <- sapply(as.data.frame(sapply(es, abs)/2) ,pnorm)
    
    #U1   (2u2 - 2 / u2)
    u1 <- ((2*u2) -1)/u2
    
    variable <- readline("What is the variable you want to inspect (displays density and normality test) ? Enter a name or skip by pressing enter: \n")
    if(variable != "") {
    vargrp <-varactivegrp %>% gather(key, value, -grp) %>% filter(key==variable)
   
    df = data.frame()
    for(i in 1:k) {
      x<-density(vargrp[which(vargrp$grp==i),]$value)$x
      y<-density(vargrp[which(vargrp$grp==i),]$value)$y
      assign(paste("df", i, sep = ""), data.frame(x, y)%>%mutate(cluster=factor(i))) }
    
    assign(paste("df",k+1, sep = ""), data.frame(x= density(vargrp$value)$x, y= density(vargrp$value)$y) %>% 
             mutate(cluster=factor("Variable (unclustered)")))
    
    df_total = data.frame()
    for(i in 1:(k+1)) {
      df_total <- bind_rows(df_total,get(paste("df", i, sep = "")))
    }
    
    graph_density_cluster<- df_total %>% filter(cluster!="Variable (unclustered)") %>% ggplot(aes(x=x, y=y, color=cluster))+
      geom_area(aes(x=x, y=y, fill=cluster),alpha=0.2)+
      geom_line(size=0.75)+
      scale_y_continuous(expand=c(0.004,0))+
      labs(x = "", y = "Clusters density")+
      theme_minimal(base_size = 12) +
      theme(axis.title.y=element_text(size=rel(1.4)),
            axis.title.x=element_text(size=rel(1.4)),
            panel.background = element_rect(fill = NA, color = "gray40"),
            legend.position="bottom")
    
    graph_density_total<-df_total %>% filter(cluster=="Variable (unclustered)") %>% ggplot(aes(x=x, y=y, color=cluster))+
      geom_area(aes(x=x, y=y, fill=cluster),alpha=0.2)+
      geom_line(size=0.75)+
      scale_y_continuous(expand=c(0.004,0))+
      labs(x = "", y = "Toal density")+
      theme_minimal(base_size = 12) +
      theme(axis.title.y=element_text(size=rel(1.4)),
            axis.title.x=element_text(size=rel(1.4)),
            panel.background = element_rect(fill = NA, color = "gray40"),
            legend.position="bottom")
    
    
    print(ggarrange(graph_density_total, graph_density_cluster, 
              labels = c("", ""),
              ncol = 2))
    
    normality_test <- shapiro.test(vargrp$value)
    
    results <- list("Tableau des rapports de correlation :",
                    es, "U3 value :", u3, "U2 value :", u2, "U1 value :", u1, "Affichage de la densit� de : " , variable, "Test de normalit� pour cette variable",  normality_test)
    } else {results <- list("Tableau des rapports de correlation :",
                            es, "interpretation u3", u3, "interpretation u2", u2, "interpretation u1", u1)
    }
    return(results)
  }
}

#Tableau de corr : Il represente la proportion de variance
#expliquee par les groupes pour chaque variable. Plus il est eleve,
#plus la variance de la variable pourra etre expliquee par les groupes

#test
effect_size(fromage[,-1], groupes.cah)





#############   test Clvalid##########
# https://www.rdocumentation.org/packages/clValid/versions/0.6-9/topics/clValid
library(clValid)
data(mouse)
## internal validation
express <- mouse[1:25,c("M1","M2","M3","NC1","NC2","NC3")]
rownames(express) <- mouse$ID[1:25]
intern <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                  validation="internal")
express
intern
## view results
summary(intern)
optimalScores(intern)
plot(intern)

## stability measures
stab <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                validation="stability")
optimalScores(stab)
plot(stab)

## biological measures
## first way - functional classes predetermined
fc <- tapply(rownames(express),mouse$FC[1:25], c)
fc <- fc[-match( c("EST","Unknown"), names(fc))]
bio <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
               validation="biological", annotation=fc)
optimalScores(bio)
plot(bio)

## second way - using Bioconductor
if(require("Biobase") && require("annotate") && require("GO.db") && require("moe430a.db")) {
  bio2 <- clValid(express, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                  validation="biological",annotation="moe430a.db",GOcategory="all")
  optimalScores(bio2)
  plot(bio2)
}

#############   test cluster crit#########
# https://www.rdocumentation.org/packages/clusterCrit/versions/1.2.8