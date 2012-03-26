plot.occup.S <- function (x,namstates.desired,colours,title,area,xmin,xmax,...)
{
    if (!inherits(x, "occup.S"))
        stop("'x' must be a 'occup.S' object")
occup.S <- x
namstates2 <- colnames(occup.S)[-ncol(occup.S)]
if (namstates2[numstates+1]=="Censored") numstates2 = numstates+1 else numstates2 = numstates
if (missing(namstates.desired)) namstates.desired <- colnames(occup.S)[-ncol(occup.S)]
if (length(match(namstates2,namstates.desired))<length(namstates2)) stop("Inconsistency between names of states and desired sequence of names of states")
if (missing(title)) title<-"Title missing"
if (missing(area)) area <- TRUE
if (missing(colours)) colours <- rainbow(numstates2)
if (missing(xmin)) xmin= min(namage)
if (missing(xmax)) xmax= max(namage)
if (length(colours) < numstates2) 
   { print ("Number of colours is less than number of states. The states are: ")
   	 print (namstates2)  }
library (ggplot2)
zmf <- occup.S[,-ncol(occup.S)]
age <- as.numeric(rownames(zmf))
require (reshape)
z <- melt(zmf)
zz <- data.frame(age=rep(age,(numstates2)),state=z[,2],count=z[,3])
zz$state <- factor(zz$state,levels=namstates.desired)
h5 <- ggplot (zz,aes(age,count,fill=state)) +xlim(xmin,xmax)
# ========  to get bar: replace geom_area by geom_bar (also below)  =======

#colours.fill <- c(muted("red",l=65,c=800),colours[2:(numstates2)])
colours.fill=colours
colours.outline <- rep("green",length(namstates2))
if (area==TRUE)
p2 <- h5+geom_area(aes(fill=state,colour=state),binwidth=1,stat="identity")+scale_colour_manual(values=colours.outline)+scale_fill_manual(values=colours.fill) else
p2 <- h5+geom_bar(aes(fill=state,colour=state),binwidth=1,stat="identity")+scale_colour_manual(values=colours.outline)+scale_fill_manual(values=colours.fill)

p3<- p2+ opts(title=title)
ddx <- seq((xmin+10),(xmax+10),by=10)
ymax <- max(occup.S[,(numstates+1)])
ddy <- seq(0,ymax,by=trunc(ymax/10))
p4 <- p3+opts(plot.title=theme_text(size=11))+opts(plot.background=theme_rect(fill="lightskyblue1",colour=NA),
  panel.background=theme_rect("black"),
  axis.text.x=theme_text(colour="black"),
  axis.text.y=theme_text(colour="black"),
  axis.title.x=theme_text(colour="darkgreen",face="bold"),
  axis.title.y=theme_text(colour="darkgreen",face="bold",angle=90))  # +facet_grid(cov~.)
  
 p4 <- p4 + geom_vline(xintercept = ddx,colour="yellow",linetype=2)
print(p4)

return(list(occup=occup.S,
            plot=p4))
}
