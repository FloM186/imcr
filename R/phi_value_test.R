#'@title  Effect size expressed by correlation
#' @description Returns the effect size phi for by correlation for a modality of interest of a variable and a target group of a cluster
#'
#' @param active_variables a factor vector or matrix. Must contains qualitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param nom a string. Name of the variable if only one variable is informed in active_variables
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param digits a number. Number of digits to display
#'
#' @return The effect size phi for by correlation. Phi is a number between -1 < phi < 1
#' @export
#' @import formattable dplyr
#' @examples
#' var = c(rep("yes",7), rep("no",7))
#' clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
#' phi.value.test(var,clust)
#'
phi.value.test <- function(active_variables, clusters,nom=NULL, show_graph=TRUE, digits=4){

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
    		tab3 = cbind(tab2,apply(tab2,1,sum))
    		phi.lg = 2*asin(sqrt(tab3[1,1]/tab3[1,ncol(tab3)]))
    		phi.la = 2*asin(sqrt( (sum(tab3[,1])-tab3[1,1]) / (sum(tab3[,ncol(tab3)])-tab3[1,ncol(tab3)]) ))
    		h = phi.lg - phi.la
        if(sign(h) == -1){
          phi = -phi
        }
        results[cpt,] = c(i,name[j], round(phi,digits))
        cpt = cpt+1
      }
    }
    results[,3] = as.numeric(results[,3])

    if(show_graph==TRUE){
      plot.uni.quali(results,"phi",nom)
    }

    print(results%>%  as.data.frame() %>% formattable(align = c("c","c", "r"),
                                                      list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                           area(col = 3) ~ formatter("span",style = x ~ style(font.weight = "bold",color = ifelse( x > 0.5,  "#00CC00",
                                                                                                                                                   ifelse(x > 0.3, "#FF8000",
                                                                                                                                                          ifelse(x>0.1,"#FF0000", "black"))))))))


    return(results)


    #Tests if many variables are passed in parameter
  }else if(class(active_variables) == "data.frame"){
    ls = list()
    cpt=1

    #recursiveness of the function for each variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        ls[[cpt]] = phi.value.test(active_variables[,i], clusters,colnames(active_variables)[i])
        cpt = cpt +1
      }
    }
    return(ls)
  }
}
