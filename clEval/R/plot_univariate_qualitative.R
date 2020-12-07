#' @title Graphics for all the functions of variable univariate qualitative
#' @description This function allows to print any graphics for the variable univariate qualitative. We override the plot function.
#'
#' @param x a data frame. Contains all the data which needs to be print
#' @param type a string. Choose the graphics to be print
#' @param name a string. Name of the variable to be print
#' @param digits a number. Number of digits for the values
#' @param ... parameters passed to other methods
#'
#' @return graphics for the univariate qualitative function
#' @export
#' @import ggplot2 RColorBrewer fmsb
#' @importFrom grDevices rgb

plot.uni.quali<-function(x,type=NULL,name=NULL,digits=5,...){
  if(type=="cramer"){
    #Radarchart only if the number of variables is greater than 2
    if(nrow(x) > 2){
      data <- as.data.frame(matrix(as.numeric(x[,2]), ncol=nrow(x)))
      print(radarchart(data, axistype=2, title = "Cramer's v by variable", pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=0.8 ))

      #Otherwise, we print a barplot
    }else{
      x = as.data.frame(x)
      x[,2] = as.numeric(x[,2])
      print(ggplot(data=x, aes(x=cramer_active_variables, y=cramer_val)) +
              geom_bar(stat="identity", position=position_dodge(), width=0.75, fill=brewer.pal(n = 3, name = "Dark2")[1]) + ggtitle("Cramer's v by variables")+
              scale_y_continuous(limits=c(0,1))+
              geom_text(aes(label=cramer_val), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
              labs(x = "Variables", y = "Cramer's value")+
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                    axis.title.y=element_text(size=rel(1.4)),
                    axis.title.x=element_text(size=rel(1.4)),
                    panel.background = element_rect(fill = NA, color = "gray40")))
    }
  }
  if(type=="l.profil"){
    title=paste("Proportion by modalities according to class for",name,sep=" ")
    #Print a barplot for the contingency table
    print(ggplot(data=as.data.frame(round(prop.table(x,1)*100,digits)), aes(x=active_variables, y=Freq)) +
            geom_bar(stat="identity", position=position_dodge(),fill=brewer.pal(n = 3, name = "Dark2")[1]) + ggtitle(title) +
            labs(x = "Modalities", y = "Proportion (row profils)")+
            scale_y_continuous(expand=c(0.004,0)) +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40")) +
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
  }
  if(type=="c.profil"){
    title=paste("Proportion by modalities according to class for",name,sep=" ")
    #Print a barplot for the contingency table
    print(ggplot(data=as.data.frame(round(prop.table(x,2)*100,digits)), aes(x=active_variables, y=Freq)) +
            geom_bar(stat="identity", position=position_dodge(),fill=brewer.pal(n = 3, name = "Dark2")[1]) + ggtitle(title) +
            labs(x = "Modalities", y = "Proportion (col. profil)")+
            scale_y_continuous(expand=c(0.004,0)) +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40")) +
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
  }
  if(type=="h"){
    title=paste("h value by modalities according to class for",name,sep=" ")
    #We print our results in a barplot with threshold values
    print(ggplot(data=x, aes(x=modality, y=h)) +
            geom_bar(stat="identity", position=position_dodge(),fill=brewer.pal(n = 3, name = "Dark2")[1]) + ggtitle(title)+
            geom_hline(aes(yintercept = 0.2,linetype = "small value"),colour = "yellow", size=1)+
            geom_hline(aes(yintercept = 0.5,linetype = "medium value"),colour = "orange", size=1)+
            geom_hline(aes(yintercept = 0.8,linetype = "large value"),colour = "darkred", size=1)+
            scale_linetype_manual(name = "Thresholds", values=c(2,2,2),guide = guide_legend(override.aes = list(color = c("darkred", "orange","yellow"))))+
            geom_text(aes(label=h), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
            labs(x = "Modalities", y = "h")+
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40"))+
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
  }
  if(type=="phi"){
    title=paste("Phi value by modalities according to class for",name,sep=" ")
    #We print our results in a barplot with threshold values
    print(ggplot(data=x, aes(x=modality, y=phi)) +
            geom_bar(stat="identity", position=position_dodge(),fill=brewer.pal(n = 3, name = "Dark2")[1]) + ggtitle(title)+
            geom_hline(aes(yintercept = 0.1,linetype = "small value"),colour = "yellow", size=1)+
            geom_hline(aes(yintercept = 0.3,linetype = "medium value"),colour = "orange", size=1)+
            geom_hline(aes(yintercept = 0.5,linetype = "large value"),colour = "darkred", size=1)+
            scale_linetype_manual(name = "Thresholds", values=c(2,2,2),guide = guide_legend(override.aes = list(color = c("darkred", "orange","yellow"))))+
            geom_text(aes(label=phi), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
            labs(x = "Modalities", y = "phi")+
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = -45, hjust=0, vjust=00),
                  axis.title.y=element_text(size=rel(1.4)),
                  axis.title.x=element_text(size=rel(1.4)),
                  panel.background = element_rect(fill = NA, color = "gray40"))+
            facet_grid(clusters ~ ., labeller = labeller("clusters")) + facet_wrap(~ clusters, ncol=2))
  }
}
