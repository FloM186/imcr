#' @title Graphics for all the functions of multivariate qualitative analysis
#' @description This function allows to print any graphics for the multivariate qualitative analysis. We override the plot function.
#'
#' @param x a data frame. Contains all the data which needs to be print
#' @param clusters clusters of the data
#' @param axes choice of dimensions
#' @param sup boolean which indicates presence of supplementary variable
#' @param cramer v.creamer of the variable with clusters
#' @param ... further arguments of print method
#'
#' @return graphics for the multivariate qualitative analysis
#' @export
#' @import FactoMineR ggplot2 factoextra RColorBrewer


plot.multi.quali<-function(x,clusters=FALSE, cramer=FALSE,sup=FALSE,axes=c(1,2),...){
  res.mca<-x
  #Graph of variables with color according to V of cramer
  print(fviz_mca_var (res.mca, choice="mca.cor",col.var = cramer,
                      gradient.cols = brewer.pal(n=3, name="Dark2"),
                      legend.title = "V de Cramer",
                      repel = TRUE,axes = axes))

  #Graph of the qualitatives variables and individuals by class
  ind<-rbind(res.mca$ind$coord,res.mca$var$coord)
  grp<-append(clusters, rep("Modalités",nrow(res.mca$var$coord)))
  nom<-c(rep("",nrow(res.mca$ind$coord)),rownames(res.mca$var$coord))
  a<-ggplot(as.data.frame(ind),aes(x=ind[,axes[1]], y=ind[,axes[2]], color=grp)) +
    geom_point() +
    scale_fill_brewer(palette="Dark2")+
    geom_text(label=nom)+
    theme_minimal(base_size = 12)+
    ggtitle("Coordinates of the individuals and qualitatives variables")+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"),
          legend.position="top",
          legend.justification=c(0,1))+
    labs(color = "Class and  quali. var")
  print(a)

  #Graph of the contribution for the individuals by class
  data<-as.data.frame(res.mca$ind$contrib)
  scatterPlot <- ggplot(data,aes(x=data[,axes[1]], y=data[,axes[2]], color=clusters)) +
    geom_point() +
    scale_fill_brewer(palette="Dark2")+
    theme_minimal(base_size = 12)+
    ggtitle("Coordinates of the contribution for the individuals by class")+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"),
          legend.position="top",
          legend.justification=c(0,1))+
    labs(color = "Class")
  print(scatterPlot)

  #Graph of the cos2 for the individuals by class
  data<-as.data.frame(res.mca$ind$cos2)
  scatterPlot <- ggplot(data,aes(x=data[,axes[1]], y=data[,axes[2]], color=clusters)) +
    geom_point() +
    ggtitle("Coordinates of the the cos2 for the individuals by class")+
    scale_fill_brewer(palette="Dark2")+
    theme_minimal(base_size = 12)+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"),
          legend.position="top",
          legend.justification=c(0,1))+
    labs(color = "Class")
  print(scatterPlot)


  if(sup==TRUE){
    #Quanti supp
    print(fviz_mca_var(x, choice = "quanti.sup",
                       ggtheme = theme_minimal(),axes=axes))
  }


}
