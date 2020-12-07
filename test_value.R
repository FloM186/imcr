#' @title  Compute test value of quantitative variables and clusters
#' @description  This function returns the test value from quantitative variables and clusters
#'
#' @param active_variables a numeric vector, data frame or matrix
#' @param clusters a vector of same length as variables containing the clustering values
#' @param show_graph logical. If FALSE, doesn't return the graph
#' @param digits a number. Number of digits displayed in tables
#'
#' @return The test value and a plot with means, standard deviation and test value for each variable and for each cluster
#' @export
#' @import stats RColorBrewer ggpubr ggplot2 formattable dplyr tidyr
#' @examples
#' data(iris)
#' iris.cr <- scale(iris[,-5],center=T,scale=T)
#' d.iris <- dist(iris.cr)
#' cah.ward <- hclust(d.iris,method="ward.D2")
#' groupes.cah <- cutree(cah.ward,k=4)
#' test_value(iris[,-5], groupes.cah, show_graph = TRUE, digits=3)
test_value <- function(active_variables, clusters, show_graph=TRUE, digits=3) {
  if(all(sapply(active_variables, is.numeric))==FALSE) { #Check if input variables are numeric
    print("Active variables must be numeric")
  } else if (is.vector(clusters)==FALSE) { #Check if input clusters are a vector
    print("Clusters must be a vector")
  } else {
    library(RColorBrewer)
    library(ggpubr)
    library(ggplot2)
    library(formattable)
    library(dplyr)
    library(tidyr)
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
              facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
    }

    #printing formatted table in Viewer tab (html format)
    print(test_value %>%  as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))%>%
            mutate("clusters (test value)"= 1:k) %>% select("clusters (test value)", everything()) %>%
            formattable(align = c("l",rep("r", c )),
                        list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                             area(col = 1:c+1) ~ color_tile("#DeF7E9", "#71CA97"))))

    #Returning for the function outup
    results <- test_value %>% mutate(clusters= 1:k) %>% select(clusters, everything()) %>% as.data.frame() %>% mutate_if(is.numeric, ~round(., digits))
    return(results)



  }
}


