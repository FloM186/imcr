#' @title  Compute Cramer's v of qualitatives variables and clusters
#' @description  This function returns the cramer's v from qualitatives variables and clusters
#'
#' @param active_variables a factor vector or matrix. Must contains qualitatives variables
#' @param clusters a numeric vector. Must contains clusters from a clustering
#' @param show_graph logical. If FALSE, doesn't return any graph
#' @param digits a number. Number of digits for cramer's v
#'
#' @return The Cramer's v
#' @export
#' @importFrom stats chisq.test setNames
#' @examples
#' var = c('yes','no','yes')
#' clust = c(1,1,2)
#' v.cramer(var,clust)
#'

v.cramer <- function(active_variables, clusters, show_graph=TRUE, digits=5){
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

    if(show_graph==TRUE){
      plot.uni.quali(tab_cramer,"cramer",digits = digits)

    }
    vec.cramer=setNames(cramer_val,cramer_active_variables)
    return(vec.cramer)


    #We calculate the cramer's v if only one qualitative variable has been passed as a parameter
  }else if(is.factor(active_variables) || is.character(active_variables)){
    contingence = table(clusters,active_variables)
    khi = chisq.test(contingence, simulate.p.value = TRUE)$statistic
    dim = min(nrow(contingence),ncol(contingence)) - 1
    v_cramer = round(as.numeric(sqrt(khi/(sum(contingence)*dim))),digits)
    return(v_cramer)
  }
}
