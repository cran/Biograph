Lexis.points <- function (Bdata,transition,title,cov,group,legend.pos,pdf)
{  	require (ggplot2)
	require (plyr)
  z<- check.par (Bdata) 
  if (missing(cov)) cov=NULL
  if (missing(title)) title <- "Title missing" 
  if (missing(legend.pos)) legend <- "topleft"
  if (missing(group)) group=NULL
 # year <- data.frame(YearTrans(Bdata))
  Bdata2 <- date.b(Bdata=Bdata,format.in=attr(Bdata,"format.date"),selectday=1,format.out="year",covs=NULL)  
  z <- TransitionAB(Bdata=Bdata2,transition=transition,keep=FALSE)
  Bdata2 <- Bdata2[Bdata2$ID%in%z$id,] # select subjects that experienced the transition
  
 date <- z$date
 age <- z$age
 # covariates
# location of selected covariates
  if (!is.null(cov)) { poscov <- which (colnames(Bdata2)%in%cov) 
    # covcat <- sort(unique(Bdata2[,poscov]))
    # colnames(GLHS[poscov])
   }
#group <- Bdata2[,poscov[4]]
#colnames(Bdata2)[which(colnames(Bdata2)==cov)] ="test55"
groupn <- group
if (!is.null(group)) group <- Bdata2[,colnames(Bdata2)==groupn]
if (is.null(cov))
  { covn <= "one"
  	covd <- rep(1,nrow(Bdata2)) }  else
  { covn <- cov
    covd <- Bdata2[,colnames(Bdata2)==covn[1]]  }
#namecov1 <- colnames(Bdata2[,covd])
lex <- data.frame(age=age,date=date,covd,gp=group) 
#lex <- subset (lex,!is.na(lex$group))
lex$gp <- factor(lex$gp)
#  unique (levels(lex$gp))

ddc <- round_any(date,5,floor)   # function from Plyr
colours<- c("Red","blue","green","yellow")
#cov1 <- Bdata2[,poscov[1]]
#cov2 <- Bdata2[,poscov[4]]
# namecov2 <- colnames(Bdata2[poscov[4]])

lex2 <- ggplot(lex,aes(x=date,y=age,colour=covd)) + opts(title=title) # colour p. 48
lex3 <- lex2+geom_point(aes(colour=covd),size=1.2)+scale_colour_manual(values=colours)+coord_equal() 
lex4 <- lex3+layer(geom="point")+
    opts(legend.direction = "vertical",legend.position = legend.pos,legend.background = theme_rect(colour = 'purple', fill = 'pink'))+
    scale_colour_manual(values=colours)+opts(plot.background=theme_rect(fill="lightskyblue1",colour=NA),
  panel.background=theme_rect("black"),
  axis.text.x=theme_text(colour="black"),
  axis.text.y=theme_text(colour="black"),
  axis.title.x=theme_text(colour="darkgreen",face="bold"),
  axis.title.y=theme_text(colour="darkgreen",face="bold",angle=90))   # equal scales p. 136

lex5 <- lex4 + scale_colour_manual(values=colours,name=covn) +  opts(axis.text.x=theme_text(colour="black",size=6))+opts(title=title)
if (is.null(groupn)) lex6<- lex5 else lex6 <- lex5+facet_wrap(~gp,ncol=3)
print (lex6)
if (pdf)
{ pdf("graph.pdf", width = 8, height = 16)  # portrait
  print (lex6)
  dev.off()  }
 return(lex6)
}

