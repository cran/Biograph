StackGraph <-
function (x,xlabel,ylabel,xlegend,ylegend,title_main,title_sub,namst)
{ colours <- c("green","blue","lightgrey","greenyellow","darkblue","pink",
"orange3","purple","yellow","turquoise3","tomato","skyblue3","white")
if (length(namst) > numstates)
  { colours[numstates+1] <- "white"
  }  
barplot(t(x),beside=FALSE,axes=FALSE,col=colours,
 names.arg=NULL,axisnames=FALSE,space=0,xlab=xlabel,ylab=ylabel)
xPYR <- seq(0,length(rownames(x)),by=5)
# yPYR <- pretty(0: (max(x[,1])+max(x[,2])),n=20) # seq(from=0, to=nsam, by=z20)
z<- max(apply(x,1,sum))
yPYR <- pretty(0: z,n=20) 
xPYRlab <-  seq(as.numeric(rownames(x))[1],max(as.numeric(rownames(x)))+1,by=5)   #  xPYR
yPYRlab <- as.character (abs(yPYR))
axis (side=1,at=xPYR,labels=xPYRlab,cex.axis=0.8)
axis (side=2,las=1,at=yPYR,labels=yPYRlab,cex.axis=0.8)
box()
if (is.null(title_main)) 
   {title(sub= title_sub,font.main = 4,font.sub=1,cex.sub=0.8)} else
   {title(main = title_main,sub= title_sub,font.main = 4,font.sub=1,cex.sub=0.8)}
 	
legend (xlegend,ylegend,col=colours,legend=namst,fill=colours,cex=0.8,bg="white")
}

