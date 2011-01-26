LexisOccExp <-
function (survey,transition,nyear)
{  locpat <- locpath(survey)
   if (!exists ("date_in_month_or_year"))
    { print ("Parameters called first. Please wait.")
      z <- Parameters (survey)
    }
  require (Epi)
	# select the subjects that experience the given transition (eg "JN") (determine their ID)
  subjectsID1 <- survey$ID[grep(transition,survey$path,value=FALSE)]
  subjects1 <- grep(transition,survey$path,value=FALSE) # line number of subject with given transition
  # Select subjects that 
  #      a) experiences the transtions, OR
  #      b) do not experience the transition and are censored in the origin state (J)
  # Select subjects who are censored in the given origin state (J)
  z <- apply (survey,1,function (x) substr(x[locpat],x[(locpat-1)],x[(locpat-1)])==substr(transition,1,1))
  # Select subjects who do not experience given transition but are censored in origin state (determine their ID)
  subjectsID2<- survey$ID[(survey$ID %in% survey$ID[subjects1]==FALSE & z==TRUE)]
  subjects2 <- survey$ID %in% survey$ID[subjects1]==FALSE & z==TRUE
  # surveyT : data for subjects with the given transition OR who are censored in origin state (J)
  surveyT <- survey[survey$ID %in%c(subjectsID1,subjectsID2),]
  # Number of closed intervals
   print (paste("Closed intervals  =  ",length(subjectsID1),sep=""))
   # Number of open intervals
   print (paste("Open   intervals  =  ",length(subjectsID2),sep=""))

  # Determine the position of entry into risk set (first entry) [transition)))[1]]  
  # Determine the starting date and ending date of episode (exposure) in YEARS
  surveyT <- CMC.years(surveyT,covsCMC=NULL) # convert to years
  pos <- vector (mode="numeric",length=nrow(surveyT))
  Tstart <- vector (mode="numeric",length=nrow(surveyT))
  Tstop <- vector (mode="numeric",length=nrow(surveyT))
  Tstatus <- vector (mode="numeric",length=nrow(surveyT))
  for (i in 1:nrow(surveyT))
  { pos[i] <- nchar(unlist(strsplit(surveyT$path[i],transition)))[1]+ 1 }
  # SURVIVAL OBJECT
  
  for (i in 1:nrow(surveyT))
    { zz <- ifelse (pos[i]==1,surveyT$start[i],surveyT[i,(locpat+pos[i]-1)])
      if (surveyT$ID[i] %in% survey$ID[subjects1])
      { Tstart[i] <- zz
      	Tstop[i]  <- surveyT[i,(locpat+pos[i])]
      	Tstatus[i] <- 1 } else
      { Tstart[i] <- surveyT[i,(locpat+surveyT$ns[i]-1)]
      	Tstop[i] <- surveyT$end[i]
      	Tstatus[i] <- 0 }
    }
    Tstop <- ifelse (is.na(Tstop),surveyT$end,Tstop)
    
   #   Tstart[i] <- ifelse (surveyT$ID[i] %in% survey$ID[subjects1]== TRUE,
   #           zz,surveyT[i,(locpat+surveyT$ns[i]-1)])
   #   Tstop[i] <- ifelse (surveyT$ID[i] %in% survey$ID[subjects1]== TRUE,
   #           surveyT[i,(locpat+pos[i])],surveyT$end[i])
   #   Tstatus[i] <- ifelse (surveyT$ID[i] %in% survey$ID[subjects1]== TRUE,1,0) }
  
 # ===============  Create Lexis object ======================
  print  ("Create Lexis object",quote=FALSE)
  require (Epi)
  # Transform data in years   
  bt <- surveyT$born
  endt <- surveyT$censored
  en1 <-  surveyT$start
  ex1 <- Tstart
  en2 <- ex1
  ex2 <- Tstop
  event2 <- Tstatus
  Lcoh <- Lexis( id = surveyT$ID,
               entry = list( CalTime=en2),
               exit  = list( CalTime=ex2, Age=ex2-bt ),
               exit.status = event2)

# Split Lexis object into disjoint follow-up intervals of nyear years
N <- 1   #   365.25
AgeLow <- nyear*trunc(min(na.omit(en2-bt)/nyear))
AgeHigh <- nyear*trunc(max(na.omit(ex2-bt)/nyear))
PerLow <- nyear* trunc(min(na.omit(en2)/nyear))
PerHigh <- nyear* (trunc(max(na.omit(ex2)/nyear))+1)
PerHigh[PerHigh-PerLow < AgeHigh-AgeLow] <- PerLow + AgeHigh - AgeLow
AgeHigh[AgeHigh-AgeLow < PerHigh-PerLow] <- AgeLow + PerHigh - PerLow

Lcoh_tr1_p <- splitLexis(Lcoh, breaks=seq(PerLow,PerHigh,nyear), time.scale="CalTime" )
# Lcoh_tr1_ap is Lexis object with observations on individuals divided in disjoint follow-up intervals of nyear years
Lcoh_tr1_ap <- splitLexis(Lcoh_tr1_p, breaks=seq(AgeLow,AgeHigh,nyear), time.scale="Age" )
Lcoh_tr1_ap$AGE <- timeBand(Lcoh_tr1_ap,"Age","left")
Lcoh_tr1_ap$PER <- timeBand(Lcoh_tr1_ap,"CalTime","left")
Lcoh_tr1_ap$COHORT <- (Lcoh_tr1_ap$PER - Lcoh_tr1_ap$AGE)
zzz <- Lcoh_tr1_ap$CalTime - Lcoh_tr1_ap$COHORT   #CalTime = year of birth (real value)
Lcoh_tr1_ap$UL <- ifelse (zzz < 0, 1,0) # Upper and Lower Triangle
        # EPI p. 78; APC in computer age p. 5
#       To get figures in Biographies. Real and synthetic, multiply by 365.25
# Determine number of events by Age-Period interval
nevents <- tapply (status(Lcoh_tr1_ap,"exit")==1,
  list(Lcoh_tr1_ap$AGE,Lcoh_tr1_ap$PER),sum)
#  list(timeBand(Lcoh_tr1_ap,"Age","left"),timeBand(Lcoh_tr1_ap,"CalTime","left")),sum)
# Determine exposure time by Age-Period interval   (in days)
ndur <- round(N * tapply (dur(Lcoh_tr1_ap),list(Lcoh_tr1_ap$AGE,Lcoh_tr1_ap$PER),sum),2)
ndur <- ifelse(is.na(ndur),0,ndur) # replace NA by 0 for Lexis diagram
rates <- nevents / ndur           # First transition rate PER month
date.mid <- timeBand(Lcoh_tr1_ap,"CalTime","mid")
age.mid <-  timeBand(Lcoh_tr1_ap,"Age","mid")
# =============  Draw Lexis diagram  ============================
#  EXPOSURE  (in years)
title1 <- paste ("Exposures to transition ",transition," (years)",sep="")
print (paste("Exposure time = ",sum(ndur,na.rm=TRUE), " years or ",sum(ndur,na.rm=TRUE)*12," months",sep=""))
Lexis.diagram( age=c(AgeLow,AgeHigh), date=c(PerLow,PerHigh), coh.grid=FALSE,
   int=5,lab.int=5,main=title1)
date66 <- sort(unique(date.mid))
age66 <- sort(unique(age.mid))
for (ix in 1:length(age66)) {for (iy in 1:length(date66))
        text( date66[iy],age66[ix], trunc(ndur[ix,iy]), cex=0.7 ) }
par (ask=TRUE) 
# EVENTS
title1 <- paste ("Count of transition ",transition," (years)",sep="")
sum(nevents,na.rm=TRUE)
Lexis.diagram( age=c(AgeLow,AgeHigh), date=c(PerLow,PerHigh), coh.grid=FALSE,
   int=5,lab.int=5,main=title1)
for (ix in 1:length(age66)) {for (iy in 1:length(date66))
        text( date66[iy],age66[ix], trunc(nevents[ix,iy]), cex=0.8 ) }
par (ask=TRUE) 
# RATES
title1 <- paste("Transition ",transition,": Occurrence-exposure rates (per year)",sep="" )
d88 <- ifelse (date_in_month_or_year==1,"month","year")
Lexis.diagram( age=c(AgeLow,AgeHigh), date=c(PerLow,PerHigh), coh.grid=FALSE,int=5,
  lab.int=5,main=title1)
for (ix in 1:length(age66)) {for (iy in 1:length(date66))
        text( date66[iy],age66[ix], round(nevents[ix,iy]/ndur[ix,iy],3), cex=0.7 ) }
# close.screen()
par (ask=TRUE) 

# Survival object: dates in years
  require (survival)
  surv <- Surv(Tstart,Tstop,Tstatus)
  
return (list(
			 surv=surv,
			 Lcoh = Lcoh,
             nevents=nevents,
             ndur = ndur,
             rates = rates))
}

