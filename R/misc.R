###bits of code useful as example/teaching purposes


## Anscombe quartet
AnscombePanel<-function(base_family = ""){
library(ggplot2)
library(ggthemes)
library(gridExtra)

data(anscombe)
p.list<-lapply(1:4, function(z){
               Z1=paste0('x',z)
               Z2=paste0('y',z)
               return(suppressWarnings(ggplot_gtable(ggplot_build(
                      ggplot(anscombe,aes_string(x=Z1,y=Z2)) + geom_point() + 
                      coord_cartesian(xlim=c(0,max(anscombe)+0.5),ylim=c(0,max(anscombe)+0.5)) + 
                      geom_smooth(method = "lm", se = FALSE)+theme_minimaliste(base_family=base_family)
                      ))))
               })
##need to be fixed later              
suppressWarnings(grid.arrange(arrangeGrob(p.list[[1]],p.list[[2]],p.list[[3]],p.list[[4]])))
}



##to avoid issues in case of default of memory: (same as source('../theme/ggplot_extra.R')

require(ggplot2)
legacy.theme.grey<-theme_grey() %+replace% theme(plot.title=element_text(hjust=0.5))
theme_set(legacy.theme.grey)

theme_bw<-theme_bw() %+replace% theme(plot.title=element_text(hjust=0.5))
theme_grey<-theme_grey() %+replace% theme(plot.title=element_text(hjust=0.5))

theme_minimaliste<-function(base_size = 11 , base_family = "", ticks = TRUE,frame=TRUE){
    ret <- theme_bw(base_family = base_family, base_size = base_size) +
        theme(panel.background = element_blank(),# panel.grid = element_blank(),
              panel.grid.major = element_line(colour = "gray97",size=0.02),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(), #strip.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank(), legend.key = element_blank(),
              axis.line = element_line(arrow=arrow()),
              plot.title=element_text(hjust=0.5)
              )
    if (!ticks) {
        ret <- ret + theme(axis.ticks = element_blank())
    }
    return(ret)
}


#change fonts for plot (and include them in the pdf)
#available fonts can be retrieved through names(pdffonts())
library(extrafont)
pdf(filename,family='fontname')
dev.off()

