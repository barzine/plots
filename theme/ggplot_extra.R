require(ggplot2)
legacy.theme.grey<-theme_grey() %+replace% theme(plot.title=element_text(hjust=0.5))
theme_set(legacy.theme.grey)

theme_bw<-theme_bw() %+replace% theme(plot.title=element_text(hjust=0.5))
theme_grey<-theme_grey() %+replace% theme(plot.title=element_text(hjust=0.5))

theme_minimaliste<-function(base_size = 11 , base_family = "", ticks = TRUE,frame=TRUE){
    ret <- theme_bw(base_family = base_family, base_size = base_size) +
        theme(panel.background = element_blank(),# panel.grid = element_blank(),
              panel.grid.major = element_line(colour = "gray30",size=0.02),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(), #strip.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank(), legend.key = element_blank(),
              axis.line = element_blank(),
              plot.title=element_text(hjust=0.5)
              )
    if (!ticks) {
        ret <- ret + theme(axis.ticks = element_blank())
    }
    return(ret)
}
