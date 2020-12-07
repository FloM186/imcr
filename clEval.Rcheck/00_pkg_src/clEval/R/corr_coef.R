#' @title  Compute correlation coefficient of quantitative variables and clusters
#' @description  This function returns the correlation coefficient from quantitative variables and clusters
#'
#' @param active_variables a numeric vector, data frame or matrix
#' @param clusters a vector of same length as variables containing the clustering values
#' @param show_graph logical. If FALSE, doesn't return the graph
#' @param show_conditionnal_means logical. If false, doesn't return the graph
#' @param digits a number. Number of digits displayed in tables
#'
#' @return The correlation coefficient and a plot with means, standard deviation and correlation coefficient for each variable and for each cluster
#' @export
#' @import dplyr RColorBrewer ggpubr ggplot2 formattable stats tidyr



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

    #Returning the conditionnal means table and correlation coefficient table
    if(show_conditionnal_means==TRUE){results <- list("Conditionnal means table"= cluster_mean %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)),
                                                      "Correlation coefficients table" = rcor %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits)))

    #Printing the formatted conditionnal means table in Viewer tab (html format)
    print(cluster_mean %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (cond. means)"= 1:k) %>% select("clusters (cond. means)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))

    } else {
      #Returning the correlation coefficient table
      results <- rcor}

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
