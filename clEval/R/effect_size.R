#' @title  Compute effect size of quantitative variables and clusters
#' @description  This function returns the effect size from quantitative variables and clusters
#'
#' @param active_variables a numeric vector, data frame or matrix
#' @param clusters a vector of same length as variables containing the clustering values
#' @param digits a number. Number of digits displayed in tables
#'
#' @return The effect size and a plot with means, standard deviation and effect size for each variable and for each cluster
#' @export
#' @import stats RColorBrewer ggpubr ggplot2 formattable dplyr tidyr


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

    #Printing tables in order to help decide which variable or cluster to inspect in next prompt
    print(list("Hedge's g" = es_g %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))))
    #Printing formatted table in Viewer tab (html format)
    print(es_g %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (Hedge's g)"= 1:k) %>% select("clusters (Hedge's g)", everything()) %>%
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

      #Printing formatted table in Viewer tab (html format)
      print(es_d %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
              mutate("clusters (Cohen's d)"= 1:k) %>% select("clusters (Cohen's d)", everything()) %>%
              formattable(align = c("l",rep("r", c )),
                          list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                               area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
      print(u3 %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
              mutate("clusters (U3)"= 1:k) %>% select("clusters (U3)", everything()) %>%
              formattable(align = c("l",rep("r", c )),
                          list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                               area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))
      print(u2 %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
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

      #Calculating shapiro-wilk test for normality
      normality_test <- shapiro.test(var_clusters$value)

      #Return for the function output

      #Returning tables
      results<-list()
      results$d.cohen<-es_d %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$g.hedges<-es_g %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$u3<-u3 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$u2<-u2 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$u1<-u1 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$besd<-besd %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$cles<-cles %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$density_normality<-c(variable, normality_test)
      return(results)
    }

    else {
      results<-list()
      results$d.cohen<-es_d %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$g.hedges<-es_g %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$u3<-u3 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$u2<-u2 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$u1<-u1 %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$besd<-besd %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      results$cles<-cles %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
      return(results)
    }
  }
}
