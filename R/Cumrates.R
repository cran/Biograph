Cumrates <-
function (irate,data,plot.cumrates)
{ # Calculate MSLT from original data (survey) and radix
  # irate = 1 estimate Nelson-Aalen
  # irate = 2 estimate occurrence-exposure rates
  # irate = 3 both
library (mvna)
if (is.logical(plot.cumrates) | !is.null(plot.cumrates[1])) 
  { print ("Cumrates: plot.cumrates is logical variable or is zero") 
  	plot.cumrates=NULL
  }
print (". . . . . .  Removing intrastate transitions . . . . . ")
locpat <- locpath(data)
removed <- Remove.intrastate(data)
data <- removed$D
# ========  Step 1: ESTIMATE TRANSITION RATES ========
print (". . . . . . Estimating rates . . . . .")
Lambda <- 0
Lambda.oe <- 0 
M.oe <- 0
namstates2 <- vector (mode="numeric",length=length(namstates))
for (i in 1:length(namstates))
{ namstates2[i] <- grep(namstates[i],namstates)} 
if (irate %in% c(1,3)) # ===  mvna estimates transition rates  ====
  {   print ("Cumrates: calls function Biograph.mvna  . . . ")
  	Dmvna <- Biograph.mvna (data)
  	# see GLHS_mvna.r
  	D2 <- Dmvna$D
    tra <- Dmvna$par$trans_possible
    cens <- Dmvna$cens
    #D3 <- data.frame(cbind(D2$id,D2$from,as.character(D2$to),D2$entry,D2$exit,D2$time))
    D3 <- data.frame(id=D2$id,from=D2$from, 
                              to=D2$to,
                              entry=round(D2$entry,2),
                              exit=round(D2$exit,2))
    D3$id <- as.numeric(D3$id)
    D3$entry <- as.numeric(D3$entry)
    D3$exit <- as.numeric(D3$exit)
    D3$exit <- ifelse (D3$exit<=D3$entry,D3$entry+1,D3$exit) # CORRECT if CMC exit = CMC entry
# ------------------------------------------------
    print (". . . . . . . . . . . . ")
    print ("Running mvna . . . . . . ")
    zz2 <- subset (D3,D3$from!=5) # was zz
    na <- mvna(data=zz2,state.names=namstates2,tra=Dmvna$par$trans_possible,cens.name=Dmvna$cens)
    cumh <- predict (na,times=seq(0,iagehigh,by=1))
# see MSLT.mvna.r
  #  Lambda = cumulative hazard by age, destination, origin
   	 nage = nrow(cumh[[1]])
   Lambda <- array (NA,dim=c(nage,numstates,numstates,3))
   dimnames(Lambda) <- list(age=namage[1:nage],destination=namstates,origin=namstates,variant=c("Expected","Upper","Lower"))
   
   	for (i in 1:removed$par$ntrans)
   	{ des <- as.numeric(as.character(removed$par$transitions$DES[i]))
   	  or <- as.numeric(as.character(removed$par$transitions$OR[i]))
   	  Lambda[,des,or,1] <- cumh[[i]]$na
      Lambda[,des,or,2] <- cumh[[i]]$upper
      Lambda[,des,or,3] <- cumh[[i]]$lower
   	}

    for (iter in 1:3)
    { for (ix in 1:nage)
      {  diag(Lambda[ix,,,iter]) <- - apply(Lambda[ix,,,iter],2,function(x) sum(x,na.rm=TRUE)) # column sum
        for (i in 1:numstates) {for (j in 1:numstates) 
        	 Lambda[ix,j,i,iter] <- ifelse (is.na(Lambda[ix,j,i,iter]),0,Lambda[ix,j,i,iter]) }
      }
    } 
  } 
  
  if (irate %in% c(2,3))
  { # === occurrence-exposure rates ====
  	print ("Computing occurrence-exposure rates",quote=FALSE)
  	print ("Running statesequence.ind",quote=FALSE)  
  	ist <- statesequence.ind (data)
  	print ("Running Occup",quote=FALSE)    
  	occup <- Occup (data)
  	print ("Running Trans",quote=FALSE)  
  	trans <- Trans (data,ist)
  	print ("Running RateTable",quote=FALSE)  
    ratetable <- RateTable (data, occup,trans)
    pc_ac <- 2    # age-cohort rates  ( 1 - age-cohort rates)
    print ("Running Rates",quote=FALSE)  
    M <- Rates(pc_ac,ratetable$Stable) 
# NOTE: cum oe rates: Lambda.oe[14,,] = sum of rates to and including age 12
   Lambda.oe<- M$Mcum
   M.oe <- M$M
   # compare Lambda[51,,] and Lambda.oe[50,,]
  }
# ========  Step 2: PLOT TRANSITION RATES ========
#           Plot cumulative hazard rates
#          Plot 2: cumulative hazard (as Putter)
if (length(plot.cumrates)>0)   # no plot if plot.cumrates = NULL
{ if (!exists("na")) 
	{ print ("No plot since Neslon-Aalen estimator has not been computed")
	  return
	}
  colour <- c("red","darkgreen","blue","purple")
  ymax <- 0  
  nlegend <- length(plot.cumrates)
  for (i in 1:removed$par$ntrans)  ymax <- max(c(ymax,na[[i]]$na))
   title <- "Cumulative hazard rates"
   namtransitions <- removed$par$transitions$ODN
	if (irate %in% c(1,3))
  {  iz <- plot.cumrates[1]  # first element of vector of transitions to be plotted
  	 x11 <- c(0, na[[iz]]$time)
   	 y11 <- c(0,na[[iz]]$na)
   	 y12 <- c(0,na[[iz]]$na-sqrt(na[[iz]]$var.aalen))
   	 y13 <- c(0,na[[iz]]$na+sqrt(na[[iz]]$var.aalen)) 
     plot(x11,y11,type="l",xlab="Age (years)",ylab="cumulative hazard",
        xlim=c(10,50), ylim=c(0,ymax),
        main=title,col=colour[1],axes=FALSE,lwd=2)
     lines (x11,y12,col=colour[1],lty=2,lwd=1)
     lines (x11,y13,col=colour[1],lty=2,lwd=1)
     axis (side=1,at=seq(10,50,by=5),labels=seq(10,50,by=5),cex.axis=0.8)
     axis (side=2,las=1,at=seq(0,ymax,by=0.5),
        labels=seq(0,ymax,by=0.5),cex.axis=0.8)
     # axis (side=2,las=1,at=seq(0,max(-log(sf1$surv)),by=0.5),labels=seq(0,max(-log(sf1$surv)),by=0.5),cex.axis=0.8)
     box()
     abline (h=seq(0,ymax+1,by=0.5),lty=2,col="lightgrey")
     abline (v=seq(10,50,by=5),lty=2,col="lightgrey")  # line at median age
   
    for (ij in plot.cumrates[2:length(plot.cumrates)])
    { 
     x21 <- c(0, na[[ij]]$time)
   	 y21 <- c(0,na[[ij]]$na)
   	 y22 <- c(0,na[[ij]]$na-sqrt(na[[ij]]$var.aalen))
   	 y23<- c(0,na[[ij]]$na+sqrt(na[[ij]]$var.aalen))

     lines (x21,y21,col=colour[match(ij,plot.cumrates)],lty=1,lwd=2)
     lines (x21,y22,col=colour[match(ij,plot.cumrates)],lty=2)
     lines (x21,y23,col=colour[match(ij,plot.cumrates)],lty=2)
     legend(10,ymax,namtransitions[plot.cumrates],col=colour[1:nlegend],
          lty=1,      # colour[1:nlegend],
          cex=0.9,bg="white",title="Ne-Aa estimator")
    }  
  }
    if (irate %in% c(2,3) & length(Lambda.oe)>1)
    { for (jj in plot.cumrates)
      { des <- as.numeric(as.character(removed$par$transitions$DES[jj]))
      	or <-  as.numeric(as.character(removed$par$transitions$OR[jj]))
      	lines (rownames(Lambda.oe),Lambda.oe[,des,or],col=colour[match(jj,plot.cumrates)],lty=3,lwd=2)
      }
       legend (10,ymax/1.5,namtransitions[plot.cumrates],col=colour[1:nlegend],
              lty=3,lwd=2,cex=0.9,bg="white",title="oe rates")
    }
 }
 return (list(NeAa = Lambda,
              oeCum = Lambda.oe,
              oe=M.oe))
}

