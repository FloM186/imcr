library(RColorBrewer)
library(tidyverse)
library(ggpubr)

#############   data test fromage#######
fromage <- read.delim("C:/Users/moret/Downloads/_documents/GitHub/projetR/dataset test/fromage.txt")
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


test_value(fromage[,-1], groupes.cah, show_graph = TRUE)

corr_coef(fromage[,-1], groupes.cah, show_graph = TRUE, show_conditionnal_means = TRUE)

effect_size(fromage[,-1], groupes.cah)



#Function to calculate test value
test_value <- function(active_variables, clusters, show_graph=TRUE, digits=3) {
  if(all(sapply(active_variables, is.numeric))==FALSE) { #Check if input variables are numeric
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) { #Check if input clusters are a vector
    print("Clusters must be a vector")
    } else {
      
      #Concatenate variables and clusters
      active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters)) 
      
      #Calculate mean for each cluster
      cluster_mean <- active_variables_clusters %>%
        group_by(clusters) %>%
        summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean)) %>%
        select_if(function(x) is.numeric(x))
      
      #Calculate length for each cluster
      cluster_n <- active_variables_clusters %>%
        group_by(clusters) %>%
        summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
        select_if(function(x) is.numeric(x))
      
      #Calculate standard deviation for each cluster
      cluster_sd <- active_variables_clusters %>%
        group_by(clusters) %>%
        summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
        select_if(function(x) is.numeric(x))
      
      #Calculate mean across all clusters
      active_variables_mean <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
      
      #Calculate length across all clusters
      active_variables_n <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length))
      
      #Calculate standard deviation across all clusters
      active_variables_var <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(var))
      
      #input for k and c
      k = length(unique(clusters))
      c = ncol(active_variables)
      
      #Creating empty data frame of wanted dimension
      test_value <- as.data.frame(matrix(ncol=c, nrow=k))
      colnames(test_value)<- colnames(active_variables)
      
      #Loop to fill the data frame
      for(j in 1:k) {
        for(i in 1:c) {
          test_value[j,i] <- as.numeric((cluster_mean[j,i] - active_variables_mean[,i])/sqrt(((active_variables_n[,i] - cluster_n[j,i])/(active_variables_n[,i]-1))*(active_variables_var[,i]/cluster_n[j,i])))
        }
      }
      
      #Creating a dataframe with values of mean, sd and test value, used for 
      full_table<-right_join(
        right_join(
          cluster_mean %>% mutate(clusters=1:k) %>% gather(key, mean, -clusters),
          cluster_sd  %>% mutate(clusters=1:k) %>% gather(key, sd, -clusters),
          by=c("clusters","key")), test_value %>% mutate(clusters= 1:k) %>% select(clusters, everything()) %>% gather(key, vt, -clusters),by=c("clusters","key"))
      
      #Removing summarise() info that is display since a recent update, and that will pollute function outup if not removed
      options(dplyr.summarise.inform = FALSE)
      
      #Creating max height for the plot (mean+sd) in order to manually label y axis (needed when using a second axis with different labels)
      barheight <- full_table %>% group_by(key, clusters) %>% summarise(msd = mean + sd)
      
      #Values used for the plot : need for converting test values to scale on the original y axis 
      min_axis_vt<-min(pretty(full_table$vt))
      amplitude_vt<-abs(min(pretty(full_table$vt)))+abs(max(pretty(full_table$vt)))
      min_axis_y <- min(pretty(barheight$msd))
      max_axis_y <-barheight %>% ungroup() %>%
        summarise(max1= max(msd), maxtot=round(max1+max1*(1/40))) %>% select(maxtot) %>% as.numeric()
      scale_second_axis<-max_axis_y/amplitude_vt

      #Printing the plot
      if(show_graph==TRUE) {
        print(full_table %>%
                ggplot(aes(x=key, y=mean))+
                geom_bar(stat="identity", width=0.75, fill=brewer.pal(n = 3, name = "Dark2")[1])+
                geom_point(aes(y=(vt -min_axis_vt)*scale_second_axis ), col = brewer.pal(n = 3, name = "Dark2")[2], shape=19)+
                geom_line(aes(y=(vt -min_axis_vt)*scale_second_axis, group=1 ), size= 0.65,col = brewer.pal(n = 3, name = "Dark2")[2])+
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
      
      #printing formatted table in Viewer tab (html format)
      print(test_value %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
              mutate("clusters (test value)"= 1:k) %>% select("clusters (test value)", everything()) %>%
              formattable(align = c("l",rep("r", c )),
                          list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                               area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
      
      #Return for the function outup
      results <- list("Eigenvalues table:" = test_value %>% mutate(clusters= 1:k) %>% select(clusters, everything()) %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)))
      return(results)
      
    
    
  }
}




#Function to calculate correlation coefficient
corr_coef <- function(active_variables, clusters, show_graph=TRUE, show_conditionnal_means=TRUE, digits=3) {
  
  if(all(sapply(active_variables, is.numeric))==FALSE) { #Check if input variables are numeric
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) { #Check if input clusters are a vector
    print("Clusters must be a vector")
  } else {
    
    #Concatenate variables and clusters
    active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters))
    
    #Calculate mean for each cluster
    cluster_mean <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))
    
    #Calculate length for each cluster
    cluster_n <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(length))  %>%
      select_if(function(x) is.numeric(x))
    
    #Calculate mean across all clusters
    active_variables_mean <- active_variables %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean))
    
    #Input for k and c
    k=length(unique(clusters))
    c = ncol(active_variables)
    
    #Creating empty data frame of wanted dimension
    sct <-as.data.frame(matrix(nrow = nrow(active_variables), ncol=c))
    colnames(sct) = colnames(active_variables)
    
    #Loop to fill data frame
    for(i in 1:c) {
      sct[,i] <- active_variables %>% select(,i) %>% mutate_all(~ (.x - as.numeric(active_variables_mean[i]))^2)
    }
    
    #Calculate SCT
    sct <-sct %>% summarise_all(sum)
    
    #Creating empty data frame of wanted dimension
    sce <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(sce) = colnames(active_variables)
    
    #Loop to fill data frame
    for(j in 1:k){
      for(i in 1:ncol(active_variables)) {
        sce[j,i]<-  as.numeric(cluster_n[j,i]*(cluster_mean[j,i] - active_variables_mean[i])^2)
      }
    }
    #Calculate SCE
    sce <-sce %>% summarise_all(sum)
    
    #Calculate correlation coefficient
    rcor <- sce/sct
    
    #Printing the plot
    if(show_graph==TRUE) {
      print(rcor %>% gather(key, value) %>%
              ggplot(aes(x=key, y=value))+
              geom_bar(stat="identity", width=0.75, fill= brewer.pal(n = 3, name = "Dark2")[1])+
              labs(x = "Variables", y = "Correlation coefficient")+
              scale_y_continuous(expand=c(0.004,0))+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")))} else {}
    
    #Printing the conditionnal means table and correlation coefficient table
    if(show_conditionnal_means==TRUE){results <- list("Conditionnal means table"= cluster_mean %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
                                                      "Correlation coefficients table" = rcor %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)))
    
    #Printing the formatted conditionnal means table in Viewer tab (html format)
    print(cluster_mean %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (cond. means)"= 1:k) %>% select("clusters (cond. means)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    
    } else {
      #Printing the correlation coefficient table
      results <- list("Conditionnal means table" = rcor)}
    
    #Printing the correlation coefficient table in Viewer tab (html format)
    print(rcor %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)) %>%
            mutate("correlation coefficient"= "value") %>% select("correlation coefficient", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    
    #Return for the function output
    return(results)
  }
}



################# effect size

effect_size <- function(active_variables, clusters, digits=3) {
  if(all(sapply(active_variables, is.numeric))==FALSE) { #Check if input variables are numeric
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) { #Check if input clusters are a vector
    print("Clusters must be a vector")
  } else {
    
    #Concatenate variables and clusters
    active_variables_clusters <- active_variables %>% mutate(clusters = factor(clusters))
    
    #Calculate mean for each cluster
    cluster_mean <-active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x),
                   .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))
    
    #Calculate standard deviation across all clusters
    active_variables_sd <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
      select_if(function(x) is.numeric(x))
    
    #Calculate length across all clusters
    active_variables_n <- active_variables_clusters %>%
      group_by(clusters) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
      select_if(function(x) is.numeric(x))
    
    #Input for k and c
    k = length(unique(clusters))
    c = ncol(active_variables)
    
    #Creating empty data frame of wanted dimension
    es_d <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(es_d)<- colnames(active_variables)
    
    #Loop to fill data frame and calculate cohen's d value. Inside loop we also calculate sd1 (sd for j) and sd2 (sd for other clusters than j)
    for(j in 1:k) {
      for(i in 1:c) {
        sd1 <- active_variables_sd[j,i] %>% as.numeric()
        
        sd2 <- active_variables_clusters %>%
          filter(clusters != j) %>%
          summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
          select_if(function(x) is.numeric(x)) %>%
          select(,i) %>% as.numeric()
        
        es_d[j,i] <- as.numeric((cluster_mean[j,i] - cluster_mean[-j,] %>% summarise_all(mean) %>% select(i) %>% as.numeric())/sqrt((sd1^2 + sd2^2)/2) )
      }
    }
    

    
    #Creating empty data frame of wanted dimension
    es_g <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(es_g)<- colnames(active_variables)
    
    #Loop to fill data frame and calculate hedge's g value. Inside loop we also calculate sd1 and n1 (sd, n for j) and sd2 and n2 (sd, n for other clusters than j)
    for(j in 1:k) {
      for(i in 1:c) {
        sd1 <- active_variables_sd[j,i] %>% as.numeric()
        
        sd2 <- active_variables_clusters %>%
          filter(clusters != j) %>%
          summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
          select_if(function(x) is.numeric(x)) %>%
          select(,i) %>% as.numeric()
        
        n1 <- active_variables_n[j,i] %>% as.numeric()
        n2 <- active_variables_clusters %>% filter(clusters != j) %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
          select_if(function(x) is.numeric(x)) %>% select(,i) %>% as.numeric()
        
        es_g[j,i] <- as.numeric((cluster_mean[j,i] - cluster_mean[-j,] %>% summarise_all(mean) %>% select(i) %>% as.numeric())/sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)))
      }
    }
    
    #Creating empty data frame of wanted dimension
    besd <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(besd)<- colnames(active_variables)
    
    #Calculating values for all variables, will be used in the loop
    s <- active_variables_clusters %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
      select_if(function(x) is.numeric(x))
    n <- active_variables_clusters %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
      select_if(function(x) is.numeric(x)) 
    m <- cluster_mean[-j,] %>% summarise_all(mean)
    
    #Loop to fill data frame and calculate BESD value. 
    for(j in 1:k) {
      for(i in 1:c) {
        s_ <- s %>% select(,i) %>% as.numeric()
        n_ <- n %>% select(,i) %>% as.numeric()
        m_ <- m %>% select(,i) %>% as.numeric()
        n1 <- active_variables_n[j,i] %>% as.numeric()
        n2 <- active_variables_clusters %>% filter(clusters != j) %>% summarise_if(.predicate = function(x) is.numeric(x), .funs = list(length)) %>%
          select_if(function(x) is.numeric(x)) %>% select(,i) %>% as.numeric()
        
        besd[j,i] <- as.numeric( ((cluster_mean[j,i] - m_ )/s_) * sqrt( (n1*n2)/(n_*(n_-1))) )
      }
    }
    

    #Creating empty data frame of wanted dimension
    cles_temp <- as.data.frame(matrix(ncol=c, nrow=k))
    colnames(cles_temp)<- colnames(active_variables)
    
    #Calculating values for all variables, will be used in the loop
    m2full <- active_variables_clusters %>%
      filter(clusters != j) %>%
      summarise_if(.predicate = function(x) is.numeric(x), .funs = list(mean)) %>%
      select_if(function(x) is.numeric(x))
    
    #Loop to fill data frame and calculate CLES.
    for(j in 1:k) {
      for(i in 1:c) {
        sd1 <- active_variables_sd[j,i] %>% as.numeric()
        sd2 <- active_variables_clusters %>%
          filter(clusters != j) %>%
          summarise_if(.predicate = function(x) is.numeric(x), .funs = list(sd)) %>%
          select_if(function(x) is.numeric(x)) %>%
          select(,i) %>% as.numeric()
        m1 <- cluster_mean[j,i] %>% as.numeric()
        m2 <-  m2full %>% select(,i) %>% as.numeric()
        
        cles_temp[j,i] <- as.numeric( abs(m1-m2)/sqrt(sd1^2+sd2^2) )
        
      }
    }
    
    #Calculating CLES
    cles <- sapply(cles_temp, pnorm)
    
    #Calculating U3
    u3 <- sapply(es_g, pnorm)
    
    #Calculating U2
    u2 <- sapply(as.data.frame(sapply(es_g, abs)/2) ,pnorm)
    
    #Calculating U1
    u1 <- ((2*u2) -1)/u2
    
    #Printing tables
    print(list("Cohen's d" = es_d %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
               "Hedge's g" = es_g %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
               "U3 value table" = u3 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
               "U2 value table"=  u2 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
               "U1 value table" = u1 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
               "Binomial effect size display" = besd %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
               "Common language effect size"= cles %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))))
    
    #Printing formatted tables in Viewer tab (html format)
    print(es_d %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (Cohen's d)"= 1:k) %>% select("clusters (Cohen's d)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    
    print(es_g %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (Hedge's g)"= 1:k) %>% select("clusters (Hedge's g)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    print(u3 %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (U3)"= 1:k) %>% select("clusters (U3)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    print(u2%>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (U2)"= 1:k) %>% select("clusters (U2)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    print(u1 %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (U1)"= 1:k) %>% select("clusters (U1)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    print(besd %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (BESD)"= 1:k) %>% select("clusters (BESD)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    print(cles %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (CLES)"= 1:k) %>% select("clusters (CLES)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
    
    #Prompt to ask which variable to inspect
    variable <- readline("What is the variable you want to inspect (displays density and normality test) ? Enter a name or skip by pressing enter: ")
    
    if(variable != "") {
      #Prompt to ask which cluster to inspect
      inspect_cluster <- as.character(readline("What is the number of the cluster you to inspect ? Enter a name or leave empty to get all cluster comparisons: "))
      var_clusters <-active_variables_clusters %>% gather(key, value, -clusters) %>% filter(key==variable)
      
      #Creating empty data frame
      df = data.frame()
      
      #Loop to fill data frame and calculate density curve coordinates for each cluster
      for(i in 1:k) {
        x<-density(var_clusters[which(var_clusters$clusters==i),]$value)$x
        y<-density(var_clusters[which(var_clusters$clusters==i),]$value)$y
        assign(paste("df", i, sep = ""), data.frame(x, y)%>%mutate(clusters=factor(i))) }
      
      #Creating a dataframe with density curve coordinates across all clusters
      assign(paste("df",k+1, sep = ""), data.frame(x= density(var_clusters$value)$x, y= density(var_clusters$value)$y) %>% 
               mutate(clusters=factor("Variable (unclustersed)")))
      
      #Creating empty data frame
      df_total = data.frame()
      
      #Loop to fill data frame and concatenate all dfs previously created
      for(i in 1:(k+1)) {
        df_total <- bind_rows(df_total,get(paste("df", i, sep = "")))
      }
      
      #Density curve for all clusters
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
      
      #Density curve across all clusters (ungrouped)
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
      
      if(inspect_cluster != "") {
        #Creating dataframe with density curve coordinates for selected cluster and others clusters if something is selected in the prompt
        df_clust <- data.frame(x= density(var_clusters[which(var_clusters$clusters==inspect_cluster),]$value)$x, y=density(var_clusters[which(var_clusters$clusters==inspect_cluster),]$value)$y ) %>% mutate(clusters = inspect_cluster)
        df_clust_other <- data.frame(x= density(var_clusters[which(var_clusters$clusters!=inspect_cluster),]$value)$x, y= density(var_clusters[which(var_clusters$clusters!=inspect_cluster),]$value)$y ) %>% mutate(clusters="others")
        
        #Density curve of selected cluster vs other clusters
        graph_density_cluster_vs_other<- bind_rows(df_clust,df_clust_other) %>% ggplot(aes(x=x, y=y, color=clusters))+
          geom_area(aes(x=x, y=y, fill=clusters),alpha=0.2)+
          geom_line(size=0.75)+
          scale_fill_brewer(palette="Dark2")+
          scale_color_brewer(palette="Dark2")+
          scale_y_continuous(expand=c(0.004,0))+
          labs(x = "", y = "Selected cluster versus others")+
          theme_minimal(base_size = 12) +
          theme(axis.title.y=element_text(size=rel(1.4)),
                axis.title.x=element_text(size=rel(1.4)),
                panel.background = element_rect(fill = NA, color = "gray40"),
                legend.position="bottom")
        
        #Printing multiple graphs on the same window. If a cluster is selected it displays graph_density_cluster_vs_other
        print(ggarrange(graph_density_total, graph_density_clusters, graph_density_cluster_vs_other,
                        labels = c("", "", ""),
                        ncol = 2, nrow=2)) } else {
                          
                          #Printing multiple graphs on the same window. 
                          print(ggarrange(graph_density_total, graph_density_clusters,
                                          labels = c("", ""),
                                          ncol = 2)) 
                          
                          #If no cluster is selected, this loop will generate 1 graph per cluster, showing this cluster density vs others
                          for( i in factor(1:k)) {
                            
                            #Creating dataframe with density curve coordinates for selected cluster and others clusters if something is selected in the prompt
                            df_clust <- data.frame(x= density(var_clusters[which(var_clusters$clusters==i),]$value)$x, y=density(var_clusters[which(var_clusters$clusters==i),]$value)$y ) %>% mutate(clusters = i)
                            df_clust_other <- data.frame(x= density(var_clusters[which(var_clusters$clusters!=i),]$value)$x, y= density(var_clusters[which(var_clusters$clusters!=i),]$value)$y ) %>% mutate(clusters="others")
                            
                            #Density curve of selected cluster vs other clusters
                            graph_density_cluster_vs_other<- bind_rows(df_clust,df_clust_other) %>% ggplot(aes(x=x, y=y, color=clusters))+
                              geom_area(aes(x=x, y=y, fill=clusters),alpha=0.2)+
                              geom_line(size=0.75)+
                              scale_fill_brewer(palette="Dark2")+
                              scale_color_brewer(palette="Dark2")+
                              scale_y_continuous(expand=c(0.004,0))+
                              labs(x = "", y = "Selected cluster versus others")+
                              theme_minimal(base_size = 12) +
                              theme(axis.title.y=element_text(size=rel(1.4)),
                                    axis.title.x=element_text(size=rel(1.4)),
                                    panel.background = element_rect(fill = NA, color = "gray40"),
                                    legend.position="bottom")
                            print(graph_density_cluster_vs_other)
                          }
                          
                        }
      
      #Calculating shapiro-wilk test for normality
      normality_test <- shapiro.test(var_clusters$value)
      
      #Return for the function output
      results <- list("Displaying density and normality test of " = variable, normality_test)
      return(results)
    }
    
    else {}
  }
}


