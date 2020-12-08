#' @title  Construction of table with contribution, cos2 and coordinates
#' @description  This function returns table of contribution,cos2 and coordinates for each dimension
#'
#' @param obj Results of variables or individuals from factorial analysis (PCA,MCA)
#' @param nb_dim Number of dimension
#'
#' @return List of table to describe dimension
#' @export
#' @importFrom FactoMineR PCA
#'

tab<-function(obj,nb_dim){
  #List of results
  list.tab<-list()
  for(i in 1:nb_dim){

    #Table of coordinates, contribution and cos2 for each dimension
    coord<-obj$coord[,i]
    contrib<-obj$contrib[,i]
    cos2<-obj$cos2[,i]
    display<-as.data.frame(cbind(coord,contrib,cos2))
    colnames(display)<-c(paste("Coord Dim",as.character(i),sep=""),paste("Contrib Dim",as.character(i),sep=""),paste("Cos2 Dim",as.character(i),sep=""))

    #Table of coordinates, contributions and cos2 with color
    print(display%>% formattable(align = c("c","c", "r"),
    list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    area(col = 3) ~ formatter("span",
    style = x ~ style(
    font.weight = "bold",
    color = ifelse( x > 0.5,  "#00CC00", "black"))),
    area(col = 2) ~ formatter("span",
    style = x ~ style(
    font.weight = "bold",
    color = ifelse( x > median(display[,2]),  "#00CC00", "black"))))))

    #Integration of the table in the list
    nom<-paste("Dim",i,sep="")
    list.tab[[nom]]<-display
  }
  return(list.tab)
}
