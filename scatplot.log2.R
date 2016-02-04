scatplot.log2<-function(data,x,y,title,xlabs,ylabs,min,max,a=0.65,
                                xcor=3.3,ycor=16.999,binx=0.2,biny=0.2,
                                methodcor='all',dens=T,rug=T,geomsize=2,fig='all',scaleCol,
                                smooth=TRUE,xmin,ymin,xmax,ymax,abline=TRUE,publi=FALSE,col=NULL,labCol,...){
      require(stats)
      require(scales)
      require(ggplot2)
      require(mgcv)
      require(gridExtra)
      
      g_legend<-function(a_ggplot){
        tmp <- ggplot_gtable(ggplot_build(a_ggplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)
        }
      
      
      ##creation of the main plot
      x2=paste(paste("log2(",substitute(x)),")")
      y2=paste(paste("log2(",substitute(y)),")")

      BW<-TRUE
      if(!is.null(col)){
          col<-substitute(col)
          BW<-FALSE
          if(missing(labCol))
              labCol<-col
      }

      if(missing(xmax)){
          if(missing(max)){
              xmax <- 15
          }else{
              xmax<-max
          }
      }
      if(missing(ymax)){
          if (missing(max)) {
              ymax <- 15
          }else{
              ymax<-max
          }
      }
    if(missing(xmin)){
          if (missing(min)){
              xmin <- -2
          }else{
              xmin <- min
          }
      }

      if(missing(ymin)){
          if (missing(min)){
              ymin<- -2
          }else{
              ymin<-min
          }
      }
      
         row_sub <- apply(data[,c(x,y)], 1, function(row) all(row !=0 ))
      data2<-data[row_sub,]
      if(exists("verbose"))
          if(verbose)   print(paste('removed' ,nrow(data)-nrow(data2)), 'points since 0 in one case')

      if (length(col)>0){
          p<-ggplot(data2, aes_string(x=x2, y=y2, colour=col))
          if(!missing(scaleCol)){
              p <- p + scale_colour_manual(values=scaleCol)
                                }
          p<- p+geom_point(alpha=a)
      }else{
          p <- ggplot(data2, aes_string(x=x2, y=y2))
          p <- p+geom_point(alpha=a)
      }
      if (publi) {
          p<- p + theme_bw() + theme(panel.border=element_rect(linetype=NULL,colour=NA))
      }

      p <- p + labs(title=title, x=xlabs, y=ylabs)
      p <- p + coord_cartesian(xlim=c(xmin,xmax),ylim=c(ymin,ymax))

      if (abline)
          p <- p + geom_abline(colour = "black")

      #for the r coefficient (on the graph):
      switch(methodcor,
             'all'={p <-p + annotate('text',label=paste("𝝆 (spearman) =",signif(cor(log2(data[x]+1),log2(data[y]+1),use="pairwise.complete.obs",
                                                                            method='spearman')[1,1],digit=3),
                                               " - ","r (pearson) =",signif(cor(log2(data[x]+1),log2(data[y]+1),use="pairwise.complete.obs",
                                                                      method='pearson')[1,1],digit=3)),x=xcor,y=ycor)},
             {p <-p + annotate('text',label=paste0("r (",methodcor,") = ",signif(cor(log2(data[x]+1),log2(data[y]+1),use="pairwise.complete.obs",
                                                                      method=methodcor)[1,1],digit=3)),x=xcor,y=ycor)
              }
             )


      if(smooth)  p <- p + geom_smooth(aes_string(x=x2,y=y2),
                                       data=data2,#data[data[,x] > 0 &  data[,y]>0,],
                                       method="gam",
                                       formula= y ~ s(x, bs = "cs")
                                       )
      if(dens){#contour (circles are circonvening  area of same points density)
          p <- p + geom_density2d(data=data2,na.rm=TRUE,aes_string(x=x2,y=y2,alpha='..level..', colour=NULL))
      }
      #the legend is formatted and then it is extracted from the main plot and would be added at the end
      
        if(!BW){
          p <- p + guides(ncol=2,
                          colour = guide_legend(title=labCol,
                                                keywidth=0.5,keyheight=0.5,title.position = "right",
                                                title.theme = element_text(face='bold',size=8,angle = 90),
                                                label.theme = element_text(size=8,angle=0)),
                          alpha = guide_legend(keywidth=0.5,keyheight=0.5,title.position = "right",
                                               title.theme = element_text(face='bold',size=8,angle = 90),
                                               label.theme = element_text(size=8,angle=0))
                          )
      }else{
          p<- p +guides(alpha=guide_legend(keywidth=0.5,keyheight=0.5,title.position = "right",
                                           title.theme = element_text(face='bold',size=8,angle = 90),
                                           label.theme = element_text(size=8,angle=0)))
      }

      legend <- g_legend(p)

      if (rug){#allows some sort of visualization of the most dense parties of one axis
          if(BW){
              p <- p + geom_rug(col=rgb(0,0,0.5,alpha=0.015))
          }else{
              p <- p + geom_rug(alpha=0.25)
          }
      }
      p1 <- p + theme(legend.position='none')#legend stripped from this plot

      ##creation of the histogram for the x axis
      #note: other possible way: instead of count => density
      p2 <- ggplot(data2, aes_string(x=x2, fill=col))
      if (!missing(scaleCol))
          p2 <- p2 + scale_fill_manual(values=scaleCol)
      if(publi)   p2<- p2 +theme_bw() +theme(panel.border=element_rect(linetype=NULL,colour=NA))

      p2 <- p2 + geom_histogram(aes(y=(..count..)),binwidth=binx)
      p2 <- p2 + coord_cartesian(xlim=c(xmin,xmax))
      p2 <- p2 + theme(axis.title.y=element_blank(), axis.title.x=element_blank())
      p2 <- p2 + scale_y_reverse()
      p2 <- p2 + theme(axis.text.x=element_blank())
      p2 <- p2 + theme(legend.title = element_blank(),legend.position='none')

      ##creation of the histogram for the y axis
      #note other possible way: instead of count => density
      p3 <- ggplot(data2, aes_string(x=y2, fill=col))
      if (!missing(scaleCol))
          p3 <- p3+  scale_fill_manual(values=scaleCol)
      if(publi)   p3<-p3+theme_bw() +theme(panel.border=element_rect(linetype=NULL,colour=NA))

      p3 <- p3 + geom_histogram(aes(y=(..count..)),binwidth=biny)
          p3 <- p3 + theme(axis.title.y=element_blank(), axis.title.x=element_blank())
      p3 <- p3 + coord_cartesian(xlim=c(ymin,ymax))+ coord_flip()
      p3 <- p3 + scale_y_reverse()
      p3 <- p3 + theme(axis.text.x  = element_text(angle=90, vjust=0))
      p3 <- p3 + theme(axis.text.y=element_blank())
      p3 <- p3 + theme(legend.position="none")

      #creation of the object before drawing them
      gp1<-suppressWarnings(ggplot_gtable(ggplot_build(p1)))
      gp2<-suppressWarnings(ggplot_gtable(ggplot_build(p2)))
      gp3<-suppressWarnings(ggplot_gtable(ggplot_build(p3)))

      #to synchronize the x axis of the main plot with the histogram of the x variable
      gp2$widths <- gp1$widths

      #to synchronize the x axis of the main plot with the histogram of the x variable
      gp3$heights <-gp1$heights

      #to synchronise the legend
      #legend$heights <- gp3$heights
      #legend$widths <-  gp2$widths

      if (fig=='all'){
          suppressWarnings(grid.arrange(arrangeGrob(gp3, gp1,legend,gp2,  widths=c(1,5), heights=c(5,1))))
      }else{
          if (fig=='list'){
              return(list(gp3, gp1,legend,gp2))#changed to list of something
          }else{
              return(p)
          }
      }
      return()
  }
  
