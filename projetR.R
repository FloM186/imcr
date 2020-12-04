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
active_variables_clusters = NULL
active_variables = NULL

val_test <- function(active_variables, clusters, show_graph=TRUE) {
  if(all(sapply(active_variables, is.numeric))==FALSE) {
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) {
    print("Clusters must be a vector")
  } else {
    active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters))
    
    cluster_mean <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))
    
    cluster_n <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
      select_if(function(x) is.numeric(x))
    
    cluster_sd <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
      select_if(function(x) is.numeric(x))
    
    active_variables_mean <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
    
    active_variables_n <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length))
    
    active_variables_var <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(var))
    
    k = length(unique(clusters))
    c = ncol(active_variables)
    test_value <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(test_value)<- colnames(active_variables)
    
    for(j in 1:k) {
      for(i in 1:c) {
        test_value[j,i] <- as.numeric((cluster_mean[j,i] - active_variables_mean[,i])/sqrt(((active_variables_n[,i] - cluster_n[j,i])/(active_variables_n[,i]-1))*(active_variables_var[,i]/cluster_n[j,i])))
      }
    }
    
    test_value <- test_value %>% mutate(clusters= 1:k) %>% select(clusters, everything())
    
    full_table<-right_join(
      right_join(
        cluster_mean %>% mutate(clusters=1:k) %>% gather(key, mean, -clusters),
        cluster_sd  %>% mutate(clusters=1:k) %>% gather(key, sd, -clusters)
        ), test_value %>% gather(key, vt, -clusters))
    
    barheight <- full_table %>% group_by(key, clusters) %>% summarise(msd = mean + sd)
    
    min_axis_vt<-min(pretty(full_table$vt))
    amplitude_vt<-abs(min(pretty(full_table$vt)))+abs(max(pretty(full_table$vt)))
    
    min_axis_y <- min(pretty(barheight$msd))
    max_axis_y <-barheight %>% ungroup() %>%
      summarise(max1= max(msd), maxtot=round(max1+max1*(1/40))) %>% select(maxtot) %>% as.numeric()
    
    scale_second_axis<-max_axis_y/amplitude_vt
    
    if(show_graph==TRUE) {
      print(full_table %>%
              ggplot(aes(x=key, y=mean))+
              geom_bar(stat="identity", width=0.75, fill="deepskyblue")+
              geom_point(aes(y=(vt -min_axis_vt)*scale_second_axis ), col = 'violetred2', shape=19)+
              geom_line(aes(y=(vt -min_axis_vt)*scale_second_axis, group=1 ), size= 0.65,col = 'violetred1')+
              scale_y_continuous(labels=pretty(barheight$msd),breaks=pretty(barheight$msd),expand = c(0.004,0), limits = c(min_axis_y,max_axis_y),
                                 sec.axis = sec_axis(~./scale_second_axis +min_axis_vt, name="Test value"))+
              geom_errorbar(aes(ymax=mean + sd, ymin= mean - sd), colour="black", width=.2)+
              labs(x = "Variables", y = "Mean")+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")) +
              facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))} else {}
    
    

    results <- list("Eigenvalues table:", test_value)
    return(results)
    
  }
}


#valeurs propres :  Plus la valeur de la variable est elevee plus elle contribue a la constitution des groupes
#test:
val_test(fromage[,-1], groupes.cah, show_graph = TRUE)


#graph etoile/spider:
#ne marche pas encore...
#https://webdevdesigner.com/q/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r-65407/
full_table %>% ggplot(aes(x=key, y=vt, group=clusters, color=factor(clusters))) + 
  geom_point() + 
  geom_line(size=2) + 
  xlab("Decils") + 
  ylab("% difference in nº Pk") + 
  #ylim(-50,25) + ggtitle("CL")  + 
  #geom_line(aes(yintercept=0), lwd=1, lty=2) + 
  #scale_x_discrete(limits=c(orden_deciles)) +
  coord_polar()






#################rapport de correlation
rapp_corr <- function(active_variables, clusters, show_graph=TRUE, show_conditionnal_means=TRUE) {
  if(all(sapply(active_variables, is.numeric))==FALSE) {
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) {
    print("Clusters must be a vector")
  } else {
    active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters))
    
    cluster_mean <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))
    
    cluster_n <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(length))  %>%
      select_if(function(x) is.numeric(x))
    
    active_variables_mean <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
    
    k=length(unique(clusters))
    c = ncol(active_variables)
    
    sct <-as.data.frame(matrix(nrow = nrow(active_variables), ncol=c))
    colnames(sct) = colnames(active_variables)
    
    for(i in 1:c) {
      sct[,i] <- active_variables %>% select(,i) %>% mutate_all(~ (.x - as.numeric(active_variables_mean[i]))^2)
    }
    sct <-sct %>% summarise_all(sum)
    
    sce <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(sce) = colnames(active_variables)
    
    for(j in 1:k){
      for(i in 1:ncol(fromage[,-1])) {
        sce[j,i]<-  as.numeric(cluster_n[j,i]*(cluster_mean[j,i] - active_variables_mean[i])^2)
      }
    }
    sce <-sce %>% summarise_all(sum)
    
    rcor <- sce/sct
    
    if(show_graph==TRUE) {
      print(rcor %>% gather(key, value) %>%
              ggplot(aes(x=key, y=value))+
              geom_bar(stat="identity", width=0.75, fill="deepskyblue")+
              labs(x = "Variables", y = "Correlation coefficient")+
              scale_y_continuous(expand=c(0.004,0))+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")))} else {}
    if(show_conditionnal_means==TRUE){results <- list("Conditionnal means table", cluster_mean,
      "Correlation coefficients table", rcor)} else {
                                                         results <- list("Conditionnal means table",
                                                                         rcor)}
    
    return(results)
  }
}
#test :
rapp_corr(fromage[,-1], groupes.cah)

#tableau rapport ocrr : Il represente la proportion de variance expliquee par les groupes pour chaque variable.
#Plus il est eleve, plus la variance de la variable pourra etre expliquee par les groupes
################# effect size


effect_size <- function(active_variables, clusters) {
  if(all(sapply(active_variables, is.numeric))==FALSE) {
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) {
    print("Clusters must be a vector")
  } else {
    
    active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters))
    
    cluster_mean <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))

    active_variables_sd <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
      select_if(function(x) is.numeric(x))
    
    active_variables_n <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
      select_if(function(x) is.numeric(x))
    
    sd1 <- active_variables_sd[j,i] %>% as.numeric()
    
    sd2 <- active_variables_clusters %>%
      filter(clusters != j) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
      select_if(function(x) is.numeric(x)) %>%
      select(,i) %>% as.numeric()
    
    
    #j = k  ::: i = col
    k = length(unique(clusters))
    c = ncol(active_variables)
    
    es_d <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(es_d)<- colnames(active_variables)
    
    for(j in 1:k) {
      for(i in 1:c) {
        es_d[j,i] <- as.numeric((cluster_mean[j,i] - cluster_mean[-j,] %>% summarise_all(mean) %>% select(i) %>% as.numeric())/sqrt((sd1^2 + sd2^2)/2) )
      }
    }
    
    n1 <- active_variables_n[j,i] %>% as.numeric()
    n2 <- active_variables_clusters %>% filter(clusters != j) %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
      select_if(function(x) is.numeric(x)) %>% select(,i) %>% as.numeric()
    
    es_g <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(es_g)<- colnames(active_variables)
    for(j in 1:k) {
      for(i in 1:c) {
        es_g[j,i] <- as.numeric((cluster_mean[j,i] - cluster_mean[-j,] %>% summarise_all(mean) %>% select(i) %>% as.numeric())/sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)))
      }
    }
    
    
    #U3
    u3 <- sapply(es_g, pnorm)
    
    #U2
    u2 <- sapply(as.data.frame(sapply(es_g, abs)/2) ,pnorm)
    
    #U1   (2u2 - 2 / u2)
    u1 <- ((2*u2) -1)/u2
    
    variable <- readline("What is the variable you want to inspect (displays density and normality test) ? Enter a name or skip by pressing enter: ")
    if(variable != "") {
    var_clusters <-active_variables_clusters %>% gather(key, value, -clusters) %>% filter(key==variable)
   
    df = data.frame()
    for(i in 1:k) {
      x<-density(var_clusters[which(var_clusters$clusters==i),]$value)$x
      y<-density(var_clusters[which(var_clusters$clusters==i),]$value)$y
      assign(paste("df", i, sep = ""), data.frame(x, y)%>%mutate(clusters=factor(i))) }
    
    assign(paste("df",k+1, sep = ""), data.frame(x= density(var_clusters$value)$x, y= density(var_clusters$value)$y) %>% 
             mutate(clusters=factor("Variable (unclustersed)")))
    
    df_total = data.frame()
    for(i in 1:(k+1)) {
      df_total <- bind_rows(df_total,get(paste("df", i, sep = "")))
    }
    
    graph_density_clusters<- df_total %>% filter(clusters!="Variable (unclustersed)") %>% ggplot(aes(x=x, y=y, color=clusters))+
      geom_area(aes(x=x, y=y, fill=clusters),alpha=0.2)+
      geom_line(size=0.75)+
      scale_fill_brewer(palette="Dark2")+
      scale_color_brewer(palette="Dark2")+
      scale_y_continuous(expand=c(0.004,0))+
      labs(x = "", y = "clusters density")+
      theme_minimal(base_size = 12) +
      theme(axis.title.y=element_text(size=rel(1.4)),
            axis.title.x=element_text(size=rel(1.4)),
            panel.background = element_rect(fill = NA, color = "gray40"),
            legend.position="bottom")
    
    graph_density_total<-df_total %>% filter(clusters=="Variable (unclustersed)") %>% ggplot(aes(x=x, y=y, color=clusters))+
      geom_area(aes(x=x, y=y, fill=clusters),alpha=0.2)+
      geom_line(size=0.75)+
      scale_fill_brewer(palette="Dark2")+
      scale_color_brewer(palette="Dark2")+
      scale_y_continuous(expand=c(0.004,0))+
      labs(x = "", y = "Toal density")+
      theme_minimal(base_size = 12) +
      theme(axis.title.y=element_text(size=rel(1.4)),
            axis.title.x=element_text(size=rel(1.4)),
            panel.background = element_rect(fill = NA, color = "gray40"),
            legend.position="bottom")
    
    
    print(ggarrange(graph_density_total, graph_density_clusters, 
              labels = c("", ""),
              ncol = 2))
    
    normality_test <- shapiro.test(var_clusters$value)
    
    results <- list("Cohen's d",
                    es_d, "Hedge's g", es_g, "U3 value table", u3, "U2 value table", u2, "U1 value table", u1, "Displaying density and normality test of " , variable, normality_test)
    } else {results <- list("Effect size value table",
                            es, "U3 value table", u3, "U2 value table", u2, "U1 value table", u1)
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