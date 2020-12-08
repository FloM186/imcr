#' @title Graphics for all the functions of multivariate quantitative analysis
#' @description This function allows to print any graphics for the multivariate quantitative analysis. We override the plot function.
#'
#' @param x a data frame. Contains all the data which needs to be print
#' @param clusters clusters of the data
#' @param axes choice of dimensions
#' @param sup boolean which indicates presence of supplementary variable
#' @param corr correlation of the variable with clusters
#' @param ... further arguments of print method
#'
#' @return graphics for the multivariate quantitative analysis
#' @export
#' @import FactoMineR ggplot2 factoextra RColorBrewer

plot.multi.quanti<-function(x,clusters=NULL, axes = c(1, 2),sup=FALSE,corr=NULL,...){
  res.pca<-x
  #Circle of correlation with the indice of correlation
  print(fviz_pca_var(res.pca, col.var = corr,
                     gradient.cols = brewer.pal(n=3, name="Dark2"),
                     legend.title = "Coeff correlation",axes = axes) )
  #Graph of the variables
  if (sup==FALSE){
    var<-res.pca$var$coord
  }else{
    var<-rbind(res.pca$var$coord,res.pca$quali.sup$coord)
  }

  b<-ggplot(as.data.frame(var),aes(x=var[,axes[1]], y=var[,axes[2]])) +
    geom_point() +
    scale_fill_brewer(palette="Dark2")+
    theme(legend.position="top", legend.justification=c(0,1))+
    geom_text(label=rownames(var))+
    theme_minimal(base_size = 12)+
    ggtitle("Coordinates of the variable")+
    labs(x = "Dim 1", y = "Dim 2")+
    scale_x_continuous(expand=c(0.05,0.05))+
    scale_y_continuous(expand=c(0.05,0.05))+
    theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
          axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          panel.background = element_rect(fill = NA, color = "gray40"))
  print(b)


  #Graph of the qualitatives variables and individuals by class
  if (sup==FALSE){
    ind<-res.pca$ind$coord
    grp<-clusters
    nom<-rep("",nrow(res.pca$ind$coord))
  }else{
    ind<-rbind(res.pca$ind$coord,res.pca$quali.sup$coord)
    grp<-append(clusters, rep("var supp",nrow(res.pca$quali.sup$coord)))
    nom<-c(rep("",nrow(res.pca$ind$coord)),rownames(res.pca$quali.sup$coord))
  }
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
  data<-as.data.frame(res.pca$ind$contrib)
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
  data<-as.data.frame(res.pca$ind$cos2)
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



}
