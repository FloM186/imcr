#'@title  Effect size for proportion comparison
#' @description Returns the effect size h for proportion comparison for a modality of interest of a variable and a target group of a cluster
#'
#' @param active_variables a factor vector or matrix. Must contains qualitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param nom a string. Name of the variable if only one variable is informed in active_variables
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param digits a number. Number of digits to display
#'
#' @return The effect size h for proportion comparaison.
#' @export
#' @import formattable dplyr
#' @examples
#' var = c(rep("yes",7), rep("no",7))
#' clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
#' h.value.test(var,clust)
#'
h.value.test <- function(active_variables, clusters,nom=NULL, show_graph=TRUE, digits=4){

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

    if(show_graph==TRUE){
      plot.uni.quali(results,"h",nom)
    }

    print(results%>%  as.data.frame() %>% formattable(align = c("c","c", "r"),
                                                      list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                           area(col = 3) ~ formatter("span",style = x ~ style(font.weight = "bold",color = ifelse( x > 0.8,  "#00CC00",
                                                                                                                                                   ifelse(x > 0.5, "#FF8000",
                                                                                                                                                          ifelse(x>0.2,"#FF0000", "black"))))))))


    return( results)

    #Tests if many variables are passed in parameter
  }else if(class(active_variables) == "data.frame"){
    ls = list()
    cpt=1

    #recursiveness of the function for each variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        ls[[cpt]] = h.value.test(active_variables[,i], clusters,colnames(active_variables)[i])
        cpt = cpt +1
      }
    }
    return(ls)
  }
}
