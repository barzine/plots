scatplot.log2.color<-function(data,x,y,title,xlabs,ylabs,min,max,a=0.65,
                              xcor=3.3,ycor=16.999,binx=0.2,biny=0.2,
                              methodcor='pearson',dens=T,rug=T,geomsize=2,fig='all',scaleCol,
                              smooth=TRUE,xmin,ymin,xmax,ymax,abline=TRUE,publi=FALSE,col=NULL,labCol,...){
    require(ggplot2)
    require(gridExtra)

    ##creation of the main plot
    x2=paste(paste("log2(",substitute(x)),")")
    y2=paste(paste("log2(",substitute(y)),")")
    if(!missing(col))
        col<-substitute(col)

    if(missing(labCol))
        labCol<-col

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

    if (length(col)>0){
        p<-ggplot(data, aes_string(x=x2, y=y2, colour=col))
        if(!missing(scaleCol)){
            p <- p + scale_colour_manual(values=scaleCol)
                              }
        p<- p+geom_point(alpha=a)
    }else{
        p <- ggplot(data, aes_string(x=x2, y=y2))
        p <- p+geom_point(alpha=a)
    }
    if (publi) {
        p<- p + theme_bw() + theme(panel.border=element_rect(linetype=NULL,colour=NA))
    }

    p <- p + labs(title=title, x=xlabs, y=ylabs)
    p <- p + coord_cartesian(xlim=c(xmin,xmax),ylim=c(ymin,ymax))
    if (abline)
        p <- p + geom_abline(colour = "black")

    #adjustement due to some problem within ggplot2 and harmonisation of the plot
    p <- p + theme(title=element_text(vjust=1.5))
    p <- p + theme(axis.title.x=element_text(vjust=-0.5,hjust=0.5))
    p <- p + theme(axis.title.y=element_text(vjust=-0.02))
    p <- p + theme(axis.text.x  = element_text(vjust=2,hjust=0.3))
    p <- p + theme(axis.text.y  = element_text(vjust=0.015,hjust=1))
    #p <- p + theme(plot.margin=unit(c(10,5,8,5),"mm"))

    #for the r coefficient (on the graph):
    p <-p + annotate('text',label=paste("r= ",signif(cor(data[x],data[y],use="pairwise.complete.obs",
                                                         method=methodcor)[1,1],digit=6)),x=xcor,y=ycor)

    if(smooth)  p <- p + geom_smooth()

    if(dens){#contour (circles are circonvening  area of same points density)
        p <- p + geom_density2d(data=data,na.rm=TRUE,aes_string(x=x2,y=y2,alpha='..level..', colour=NULL))#,legend.title=element_blank()))
       # p <- p + theme(legend.title=element_blank())

    }#else{
     #   legend <- ggplot(data)+geom_point(aes(1,1), colour="white")+
     #   theme(axis.ticks=element_blank(),
     #         panel.background=element_blank(),
     #         axis.text.x=element_blank(), axis.text.y=element_blank(),
     #         axis.title.x=element_blank(), axis.title.y=element_blank())
    #}
    #the legend is formatted and then it is extracted from the main plot and would be added at the end
    p <- p + guides(ncol=2,
                    colour = guide_legend(title=labCol,
                                          keywidth=0.5,keyheight=0.5,title.position = "right",
                                          title.theme = element_text(face='bold',size=8,angle = 90),
                                          label.theme = element_text(size=8,angle=0)),
                    alpha = guide_legend(keywidth=0.5,keyheight=0.5,title.position = "right",
                                         title.theme = element_text(face='bold',size=8,angle = 90),
                                         label.theme = element_text(size=8,angle=0))
                    )

    legend <- g_legend(p)

    if (rug){#allows some sort of visualization of the most dense parties of one axis
        p <- p + geom_rug(alpha=0.25)#col=rgb(0,0,0.5,alpha=0.015))
    }

    p1 <- p + theme(legend.position='none')#legend stripped from this plot

    ##creation of the histogram for the x axis
    #note: other possible way: instead of count => density
    p2 <- ggplot(data, aes_string(x=x2, fill=col))
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
    p3 <- ggplot(data, aes_string(x=y2, fill=col))
    if (!missing(scaleCol))
        p3 <- p3+  scale_fill_manual(values=scaleCol)
    if(publi)   p3<-p3+theme_bw() +theme(panel.border=element_rect(linetype=NULL,colour=NA))

    p3 <- p3 + geom_histogram(aes(y=(..count..)),binwidth=biny)
    p3 <- p3 + theme(axis.title.y=element_blank(), axis.title.x=element_blank())
    p3 <- p3 + coord_flip(coord_cartesian(ylim=c(ymin,ymax)))
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
   # legend$heights<-gp2$heights
   # legend$widths<-gp3$widths

    if (fig=='all'){
        suppressWarnings(grid.arrange(arrangeGrob(gp3, gp1,legend,gp2,  widths=c(1,5), heights=c(5,1))))
        #suppressWarnings(grid.arrange(gp3, gp1,legend,gp2,  widths=c(1,3), heights=c(3,1)))
    }else{
        if (fig=='list'){
            return(list(gp3, gp1,legend,gp2))#changed to list of something
        }else{
            return(p)
        }
    }
    return()
}
