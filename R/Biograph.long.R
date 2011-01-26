Biograph.long <-
function (survey)
{ survey2 <- survey
  if (exists("namstates") & exists ("timeunit")) tmat <- attr(survey,"trans") else 
            #  tmat <- ex_survey$tmat 
     {print ("Biograph.long: Parameters missing. Biograph runs Parameters . . . . ",quote=FALSE)
      ex_survey <- Parameters(survey2)
      tmat <- attr(survey2,"trans")
     }
  print (". . . . .  Creating long format  . . . . . .",quote=FALSE)
  survey2$start2 <- survey2$start
  locpat <- locpath(survey)
  print (". . . . .  running reshape  . . . . . ",quote=FALSE)
  zx <- reshape (survey2,idvar="ID",varying=list(c(3,(locpat+1):(ncol(survey)),4)),
     v.names="date",direction="long",drop=NULL)
  print (" . . .  . Adjusting long format  . . . . ",quote=FALSE)
  zx2 <- zx[!is.na(zx$date),]
  D <- zx2[do.call(order,list(zx2$ID,zx2$date)),] # sort by 2 variables
  D$OD <- substr(paste("B",D$path,"Ce",sep=""),D$time,D$time+1)
  D$time <- ifelse (D$time==max(D$time),D$ns+1,D$time)   # time = line number of episode in trajectory (first= from birth; last = open to censored)
  D$OD[D$time==D$ns+1]<- "cens"
  D$Tstart <- rep(0,nrow(D))
  D$Tstart[2:nrow(D)] <- ifelse (D$date[2:nrow(D)] ==D$born[2:nrow(D)] ,
         D$born[2:nrow(D)],D$date[2:nrow(D)] -diff(D$date,lag=1))
  D$Tstop <- D$date
  D <- subset(D,D$time > 1)  # remove first episode (birth to entry in first state)  was: D$date!=D$born & 
     # result should be same as sum(survey$ns)
  if (nrow(D)!=sum(survey$ns)) stop (paste("Biograph.long: Number of records in long format differs from total number of episodes in Biograph object: ",nrow(D),"    ",sum(survey$ns),sep=""))
  D$OR <- ifelse (D$Tstart==0,"B",ifelse (D$time >D$ns, substr(D$path,D$ns,D$ns), substr(D$path,(D$time-1),(D$time-1))))  
  D$DES <-   ifelse (D$time > D$ns, "cens",substr(D$path,(D$time),(D$time)))

  # D$DES <- ifelse (D$time > D$ns,"cens",substr(D$path,(D$time),(D$time)))
  D$status <- ifelse (D$DES=="cens",0,1)
  D$trans <- apply(D,1,function (x) {ifelse (x[ncovariates+11+2]=="cens", 
           grep(x[ncovariates+11+1],namstates),
           tmat[grep(x[ncovariates+11+1],namstates),grep(x[ncovariates+11+2],namstates)])})
  code88 <- ifelse(timeunit=="year",1,ifelse(timeunit=="month",12,0))
  D$Tstarta <- (D$Tstart-D$born)/code88
  D$Tstopa  <- (D$Tstop-D$born)/code88
  
  De <- subset(D,D$time!=1)  # remove transition from birth to first state
  De$time <- De$time-1
  Depisode = cbind(ID=De$ID,OR=De$OR,DES=De$DES,Tstart=De$Tstart,
                Tstop=De$Tstop,status=De$status,trans=De$trans,
                De[,(3:(2+ncovariates))],
                born=De$born,OD=De$OD,Episode=De$time,Tstarta=D$Tstarta,Tstopa=D$Tstopa)
                             
  attr(D, "trans") <- tmat   
  attr(Depisode, "trans") <- tmat  
  
  return (list (Devent = D,
                Depisode = Depisode))
}

