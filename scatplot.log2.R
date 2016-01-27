scatplot.log2<-function(data,x,y,title,xlabs,ylabs,min,max,
                        a=0.65,xcor=3.3,ycor=16.999,binx=0.2,biny=0.2,
                        methodcor='spearman',dens=T,rug=T,geomsize=2,fig=TRUE,
                        smooth=TRUE,xmin,ymin,xmax,ymax,abline=TRUE,publi=FALSE,...){
    require(ggplot2)
    require(scales)
    require(gridExtra)

    ##creation of the main plot
    x2=paste(paste("log2(",substitute(x)),")")
    y2=paste(paste("log2(",substitute(y)),")")

    if(missing(xmax))
        xmax <- ifelse(!missing(max), max, 15)

    if(missing(ymax))
        ymax <- ifelse(!missing(max), max, 15)

    if(missing(xmin))
        xmin <- ifelse(!missing(min), min, -2)

    if(missing(ymin))
        ymin <- ifelse(!missing(min), min, -2)
    #create the new dataframes with the needed data


    data.label<-data.frame(
                           cor=cor(log2(data[x]+1),log2(data[y]+1),use="pairwise.complete.obs", method=methodcor),
                           x=xcor,y=ycor,
                           label=paste("r= ",signif(cor(log2(data[x]+1),log2(data[y]+1),use="pairwise.complete.obs", method=methodcor)[1,1],digit=6))
                           )
    print(data.label$cor)

    row_sub <- apply(data, 1, function(row) all(row !=0 ))
    data<-data[row_sub,]

    p <- ggplot(data, aes_string(x=x2, y=y2))
    if (publi)
        p<- p + theme_bw()

    p <- p + labs(title=title, x=xlabs, y=ylabs)
    p <- p + coord_cartesian(xlim=c(xmin,xmax),ylim=c(ymin,ymax))
    p <- p + geom_point(size=geomsize,alpha=a)
    if (abline)
        p <- p + geom_abline(colour = "black")

    #adjustement due to some problem within ggplot2 and harmonisation of the plot
    p <- p + theme(title=element_text(vjust=1.5))
    p <- p + theme(axis.title.x=element_text(vjust=-0.5,hjust=0.5))
    p <- p + theme(axis.title.y=element_text(vjust=-0.02))
    p <- p + theme(axis.text.x  = element_text(vjust=2,hjust=0.3))
    p <- p + theme(axis.text.y  = element_text(vjust=0.015,hjust=1))
    #p <- p + theme(plot.margin=unit(c(10,5,8,5),"mm"))

    if(smooth)  p <- p + geom_smooth()


    if(dens){#contour (circles are circonvening  area of same points density)
        p <- p + geom_density2d(na.rm=TRUE,aes(color=..level..))
        p <- p + theme(legend.title=element_blank())

        #for the r coefficient (on the graph):
        p <- p + geom_text(data=data.label,aes(x=x,y=y,label=label))

        #the legend is extracted from the main plot and would be added at the end
        legend <- g_legend(p)
    }else{
        legend <- ggplot(data)+geom_point(aes(1,1), colour="white")+
        theme(axis.ticks=element_blank(),
              panel.background=element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank())
    }

    if (rug){#allows some sort of visualization of the most dense parties of one axis
        p <- p + geom_rug(col=rgb(0,0,0.5,alpha=0.015))
    }
    
    p1 <- p + theme(legend.position='none')#legend stripped from this plot

    ##creation of the histogram for the x axis
    #note: other possible way: instead of count => density
    p2 <- ggplot(data, aes_string(x=x2))
    if(publi)
        p2<-p2 +theme_bw()
    p2 <- p2 + geom_histogram(aes(y=(..count..)),binwidth=binx)
    p2 <- reset.legend.x(p2)
    p2 <- p2 + coord_cartesian(xlim=c(xmin,xmax))
    p2 <- p2 + theme(axis.title.y=element_blank(), axis.title.x=element_blank())
    p2 <- p2 + scale_y_reverse()
    p2 <- p2 + theme(panel.grid=element_blank())
    p2 <- p2 + theme(axis.text.x=element_blank())

    ##creation of the histogram for the y axis
    #note other possible way: instead of count => density
    p3 <- ggplot(data, aes_string(x=y2))
    if(publi)
        p3<-p3+theme_bw()
    p3 <- p3 + geom_histogram(aes(y=(..count..)),binwidth=biny)
    p3 <- p3 + theme(legend.position="none")
    p3 <- reset.legend.y(p3)
    p3 <- p3 + theme(axis.title.y=element_blank(), axis.title.x=element_blank())
    p3 <- p3 + coord_flip(coord_cartesian(ylim=c(ymin,ymax)))

    p3 <- p3 + scale_y_reverse()
    p3 <- p3 + theme(panel.grid=element_blank())
    p3 <- p3 + theme(axis.text.x  = element_text(angle=90, vjust=0))
    p3 <- p3 + theme(axis.text.y=element_blank())


    #creation of the object before drawing them
    gp1<-suppressWarnings(ggplot_gtable(ggplot_build(p1)))
    gp2<-suppressWarnings(ggplot_gtable(ggplot_build(p2)))
    gp3<-suppressWarnings(ggplot_gtable(ggplot_build(p3)))

    #to synchronize the x axis of the main plot with the histogram of the x variable
    gp2$widths <- gp1$widths

    #to synchronize the x axis of the main plot with the histogram of the x variable
    gp3$heights <-gp1$heights

    if (fig){
        suppressWarnings(grid.arrange(arrangeGrob(gp3, gp1,legend,gp2,  widths=c(1,5), heights=c(5,1))))

    }else{
        return(list(gp3, gp1,legend,gp2))#changed to list of something
    }

}
