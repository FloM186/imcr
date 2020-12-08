#' @title  Row percentages of a frequency table from variables and clusters
#' @description  This function returns a rows percentage frequency table from variables and clusters with formatting
#'
#' @param active_variables a factor vector or matrix. Must contains qualitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param nom a string. Name of the variable if only one variable is informed in active_variables
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param digits a number. Number of digits to display
#'
#' @return The results is an object of class table and proptable
#' @export
#'
#' @examples
#' var = c(rep("yes",7), rep("no",7))
#' clust = c(1,1,2,1,2,3,1,2,3,3,2,1,3,2)
#' l.profil(var,clust)
#'
l.profil <- function(active_variables, clusters,nom=NULL, show_graph=TRUE, digits=2){

  #Tests if only one variable passed in parameter
  if(is.factor(active_variables) || is.character(active_variables)){

    #We create the contingency variable
    tab=table(clusters,active_variables)
    name = names(dimnames(tab))

    if(show_graph==TRUE){
      plot.uni.quali(tab,"l.profil",nom,digits)
      }
    #Add a row Ensemble which is the sum of each columns
    tab=rbind(tab,Ensemble = apply(tab,2,sum))
    #We put the values in percentage
    tab = round(prop.table(tab,1)*100,digits)
    #Add a column Total which is the sum of each rows
    tab=cbind(tab, Total = apply(tab,1,sum))
    tab = as.table(tab)
    names(dimnames(tab)) = name
    return(tab)

    #Tests if many variables are passed in parameter
  } else if(class(active_variables) == "data.frame"){
    ls = list()
    cpt=1

    #recursiveness of the function for each variables
    for(i in 1:ncol(active_variables)){
      if(is.factor(active_variables[,i]) || is.character(active_variables[,i])){
        ls[[cpt]] = l.profil(active_variables[,i], clusters,nom=colnames(active_variables)[i])
        cpt = cpt +1
      }
    }
    return(ls)
  }
}
