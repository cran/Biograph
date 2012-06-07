plot.MSLT.S <-
function (x,e0,order,colours,title,area,xmin,xmax,...)
 {# ======== Plot state occupancies  =============    
    if (!inherits(x, "MSLT.S"))
        stop("'x' must be a 'MSLT' object")
    S <- x
 	if (missing(e0)) stop("Life expectancy (e0) is missing.")
 	if (missing (title)) title <- "Title"
 	if (missing (area)) area <- TRUE
 	if (missing (order)) order <- NULL
 	if (missing(xmin)) xmin <- iagelow
 	if (missing(xmax)) xmax <- iagehigh
 	if (missing(colours)) colours <- rainbow(numstates)
    if (length(colours) < numstates) 
        { print ("Number of colours is less than number of states. The states are: ")
          print (namstates)  }
 	
  # Graph the state probabilities
   require (ggplot2)
   numstates.case = numstates # to plot occup: numstates+1 (censoring)
   namrates <- "Age-cohort rates"
   namst <- c(namstates,"Total")
   for (i in 1:numstates)
     { namst[i] <- paste(namstates[i]," (e0=",round(e0[i,1],2),")",sep="")
     }
   namst[numstates+1] <- paste ("Total"," (e0=",round(sum(e0[,1]),2),")",sep="")
   title_sub <- paste("Life table (",namrates,")",sep="")
 require (reshape)
 z <- melt(S[,,1])  # function of reshape package
 age <- as.numeric(rownames(S[,,1]))
 zz <- data.frame(age=rep(age,numstates.case),state=z[,2],count=z[,3],cov=z[,1])

 
# Changing the Default Order of Legend Labels and Stacking of Data
#levels(zz$state)
#zz$state <- factor(zz$state, levels = rev(levels(zz$state)))
if (!is.null(order)) zz$state <- factor(zz$state,levels=order,labels=namst[1:numstates][match(order,namstates)])
levels (zz$state)
#zz$state <- factor(zz$state,levels=order)

h5 <- ggplot (zz,aes(age,count,fill=state)) +xlim(xmin,xmax)
# ========  to get bar: replace geom_area by geom_bar (also below)  =======
#colours3 <- c("red","blue","lightgrey") #  numstates.case (censoring)
#colours6 <- c("red","blue","yellow","brown","green","lightgrey") #  numstates.case (censoring)
#if (numstates==2) colours <- colours3 else colours<- colours6
colours.fill <- c("red",colours[2:numstates.case])
colours.outline <- rep("green",numstates)
if (area==TRUE)
p2 <- h5+geom_area(aes(fill=state))+ scale_colour_manual(values=colours.outline)+scale_fill_manual(values=colours.fill) else
p2 <- h5+geom_bar(aes(fill=state),binwidth=1,stat="identity")+ scale_colour_manual(values=colours.outline)+scale_fill_manual(values=colours.fill)
# earlier: geom_area (,colour=state)

p3<- p2+ opts(title=title)+opts(legend.position = "none") 
p4 <- p3+opts(plot.title=theme_text(size=11))+opts(plot.background=theme_rect(fill="lightskyblue1",colour=NA),
  panel.background=theme_rect("black"),
  axis.text.x=theme_text(colour="black"),
  axis.text.y=theme_text(colour="black"),
  axis.title.x=theme_text(colour="darkgreen",face="bold"),
  axis.title.y=theme_text(colour="darkgreen",face="bold",angle=90)) 
     #   +facet_grid(cov~.)

p5 <- p4 +opts(legend.position=c(0.80,0.78),legend.background = theme_rect(colour = 1))
namst8 <- levels(zz$state)   #   namst[1:numstates]
# USE  scale_linetype_discrete(name = "Fancy Title")
 p6 <- p5 + scale_linetype(name="Life\nExpectancy",breaks=namst8,labels=namst8) +
     scale_colour_discrete (name="Life\nExpectancy",breaks=namst8,labels=namst8)
     
p7 <- p5 + scale_fill_manual(values=colours[1:length(namst8)],name="Life\nExpectancy",breaks=namst8,labels=namst8)
# 0 gets rid of border
#pdf("MSLT.NLOG98.pdf")
print(p7)
#dev.off()
  return(list(S=S,
              plot=p7))
}
