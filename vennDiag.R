DrawVenn<-function(...,a,b,c,d,e,names,fills,cols,titlelab,fig=TRUE,type='vec',titlesize,
                        xlwd=2, xlty="solid", xalpha=0.3){
#wrapper around the VennDiagram package: allows to use special caracter for example
    ###import library
    require(VennDiagram)
    require(gridExtra)

    #check common variable
    if(missing(fills)) fills=cols
    
    #prepare cleaned data to be processed after
    switch(type,
      "vec"={
            a1<-a
            if(!missing(b)) b1<-b
            if(!missing(c)) c1<-c
            if(!missing(d)) d1<-d
            if(!missing(e)) e1<-e
            },
      "namedV"={
            a1<-names(a)
            if(!missing(b)) b1<-names(b)
            if(!missing(c)) c1<-names(c)
            if(!missing(d)) d1<-names(d)
            if(!missing(e)) e1<-names(e)
            },
      "rownames"={
            a1<-rownames(a)
            if(!missing(b) b1<-rownames(b)
            if(!missing(c) c1<-rownames(c)
            if(!missing(d) d1<-rownames(d)
            if(!missing(e) e1<-rownames(e)
            },
      "colnames"={
            a1<-colnames(a)
            if(!missing(b) b1<-colnames(b)
            if(!missing(c) c1<-colnames(c)
            if(!missing(d) d1<-colnames(d)
            if(!missing(e) e1<-colnames(e)
            })
    
    
    ### define the functions
    Draw5Venn<-function(...,a,b,c,d,e,nameABCDE,fillABCDE,colABCDE){
        p<-draw.quintuple.venn(ind=FALSE,
                              area1=length(a1),
                              area2=length(b1),
                              area3=length(c1),
                              area4=length(d1),
                              area5=length(e1),
                              n12=length(intersect(a1,b1)),
                              n13=length(intersect(a1,c1)),
                              n14=length(intersect(a1,d1)),
                              n15=length(intersect(a1,e1)),
                              n23=length(intersect(b1,c1)),
                              n24=length(intersect(b1,d1)),
                              n25=length(intersect(b1,e1)),
                              n34=length(intersect(c1,d1)),
                              n35=length(intersect(c1,e1)),
                              n45=length(intersect(d1,e1))),
                              n123=length(intersect(intersect(a1,b1),c1)),
                              n124=length(intersect(intersect(a1,b1),d1)),
                              n125=length(intersect(intersect(a1,b1),e1)),
                              n134=length(intersect(intersect(a1,c1),d1)),
                              n135=length(intersect(intersect(a1,c1),e1)),
                              n145=length(intersect(intersect(a1,d1),e1)),
                              n234=length(intersect(intersect(b1,c1),d1)),
                              n235=length(intersect(intersect(b1,c1),e1)),
                              n245=length(intersect(intersect(b1,d1),e1)),
                              n345=length(intersect(intersect(c1,d1),e1)),
                              n1234=length(intersect(intersect(intersect(a1,b1),c1),d1)),
                              n1235=length(intersect(intersect(intersect(a1,b1),c1),e1)),
                              n1245=length(intersect(intersect(intersect(a1,b1),d1),e1)),
                              n1345=length(intersect(intersect(intersect(a1,c1),d1),e1)),
                              n2345=length(intersect(intersect(intersect(b1,c1),d1),e1)),
                              n12345=length(intersect(intersect(intersect(intersect(b1,c1),d1),e1),a1)),
                              category=nameABCDE,
                              fill=fillABCDE,
                              col=colABCDE,cat.col=colABCDE,
                              lwd=rep(xlwd,5),lty=rep(xlty,5), alpha=rep(xalpha,5),
                              ... )
        return(p)
    }

    Draw4Venn<-function(...,a,b,c,d,nameABCD,fillABCD,colABCD){
        p<-draw.quad.venn(ind=FALSE,
                          area1=length(a1),
                          area2=length(b1),
                          area3=length(c1),
                          area4=length(d1),
                          n12=length(intersect(a1,b1)),
                          n13=length(intersect(a1,c1)),
                          n14=length(intersect(a1,d1)),
                          n23=length(intersect(b1,c1)),
                          n24=length(intersect(b1,d1)),
                          n34=length(intersect(c1,d1)),
                          n123=length(intersect(intersect(a1,b1),c1)),
                          n124=length(intersect(intersect(a1,b1),d1)),
                          n134=length(intersect(intersect(a1,c1),d1)),
                          n234=length(intersect(intersect(b1,c1),d1)),
                          n1234=length(intersect(intersect(intersect(a1,b1),c1),d1)),
                          category=nameABCD,
                          fill=fillABCD,
                          cat.col=fillABCD,
                          col=c(colABCD[1],colABCD[3],colABCD[4],colABCD[2]),
                          lwd=rep(xlwd,4),lty=rep(xlty,4), alpha=rep(xalpha,4),... )
        return(p)

    }

    Draw3Venn<-function(...,a,b,c,nameABC,fillABC,colABC){
        p<-draw.triple.venn(ind=FALSE,
                            area1=length(a1),
                            area2=length(b1),
                            area3=length(c1),
                            n12=length(intersect(a1,b1)),
                            n23=length(intersect(b1,c1)),
                            n13=length(intersect(a1,c1)),
                            n123=length(intersect(intersect(a1,b1),c1)),
                            category=nameABC,
                            fill=fillABC,
                            col=colABC,cat.col=colABC,
                            lwd=rep(xlwd,3),lty=rep(xlty,3), alpha=rep(xalpha,3), ... )
        return(p)
    }

    Draw2Venn<-function(...,a,b,nameAB,fillAB,colAB){
        p<-draw.pairwise.venn(ind=FALSE,
                              area1=length(a1),
                              area2=length(b1),
                              cross.area=length(intersect(a1,b1)),
                              category=nameAB,
                              fill=fillAB,
                              col=colAB,
                              lwd=rep(xlwd,2),lty=rep(xlty,2), alpha=rep(xalpha,2),
                              ... )
    return(p)
    }




    #main
    #select the correct plot
    if(!missing(e)){
        p<-Draw5Venn(a=a1,b=b1,c=c1,d=d1,e=e1,nameABCDE=names,fillABCDE=fills,colABCDE=cols,
                               type=type,cex=cex,cat.cex=cat.cex,...)
    }else{
        if(!missing(d)){
            p<-Draw4Venn(a=a1,b=b1,c=c1,d=d1,nameABCD=names,fillABCD=fills,colABCD=cols,
                         type=type,cex=cex,cat.cex=cat.cex,...)
        }else{
            if(!missing(c)){
                p<-Draw3Venn(a=a1,b=b1,c=c1,nameABC=names,fillABC=fills,colABC=cols,
                             type=type,cex=cex,cat.cex=cat.cex,...)
            }else{
                if(!missing(b)){
                    p<-Draw2Venn(a=a1,b=b1,nameAB=names,fillAB=fills,colAB=cols,
                                 type=type,cex=cex,cat.cex=cat.cex,...)
                         }else{
                             return(print('Can not make a venn diagram with only one set'))
            }}}
    }

    #draw the plot
    grid.newpage()
    if(!missing(titlelab)){
        if(missing(titlesize)) titlesize=30
        grid.arrange(gTree(children=p), sub=textGrob(titlelab,gp=gpar(fontsize=titlesize)))
    }else{
        if(fig){
            grid.draw(p)
        }else{
            return(p)
        }
    }

}
