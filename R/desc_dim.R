#' @title  Description dimension of MCA analysis
#' @description  This function returns table of dimension description form MCA analysis
#'
#' @param res results of MCA analysis
#' @param nb_dim an integer, number of dimension
#'
#' @return list of description's table
#' @export


desc.dim<-function(res,nb_dim){
  list.desc<-list()
  for(i in 1:nb_dim){
    list.desc[[i]]<-res[[i]]$quali
  }
  return(list.desc)
}
