Lexislines.episodes <-
function (Dlong,subjectsID,title1)
{ # From lifelines.r
  # subjectsID <- survey$ID
  #  subjectsID <- c(1,19,20,208)
  require (Epi)
  print (subjectsID)
       if (!exists("timeunit"))
     { print ("CMC.years: Biograph first runs Parameters . . . ",quote=FALSE)
       z <- Parameters(survey)
     }
  if (timeunit == "month")
  { z1900 <- 1900
  	code88 <- 12 } else
  { z1900 <- 0
  	code88 <- 1 
  }
  Dlong2 <- Dlong
  Dlong2$TstartY <- z1900+trunc(Dlong2$Tstart/code88)+(Dlong2$Tstart/code88-trunc(Dlong2$Tstart/code88))
  Dlong2$Tstartage <- (Dlong2$Tstart-Dlong2$born)/code88
  Dlong2$TstopY <- z1900+trunc(Dlong2$Tstop/code88)+(Dlong2$Tstop/code88-trunc(Dlong2$Tstop/code88))
  Dlong2$Tstopage <- (Dlong2$Tstop-Dlong2$born)/code88
  bt <- z1900+trunc(Dlong2$born/code88)+(Dlong2$born/code88-trunc(Dlong2$born/code88))
  #endt <- year_trans$censored
  en1 <- Dlong2$TstartY
  ex1 <-  Dlong2$TstopY
  Lcoh1 <- Lexis( id = Dlong2$ID,
               entry = list( CalTime=en1 ),
               exit  = list( CalTime=ex1, Age=ex1-bt ),
               exit.status = Dlong2$status,
               data=Dlong2,
               merge=TRUE)
  AgeLow <- 5*trunc(min(na.omit(en1-bt)/5))
  AgeHigh <- 5*trunc(max(na.omit(ex1-bt)/5)+1)
  PerLow <- 5* trunc(min(na.omit(en1)/5))
  PerHigh <- 5* (trunc(max(na.omit(ex1)/5))+1)
  PerHigh[PerHigh-PerLow < AgeHigh-AgeLow] <- PerLow + AgeHigh - AgeLow
  AgeHigh[AgeHigh-AgeLow < PerHigh-PerLow] <- AgeLow + PerHigh - PerLow

  print (subjectsID)
  subjectsID2 <- subjectsID[subjectsID %in% as.numeric(Dlong2$ID)]  # delete IDs that do not exist
  print (c(subjectsID2,Lcoh1$lex.id[1:100]))
str(subjectsID)
str(subjectsID2)
str(Lcoh1$lex.id)
  
  #subjectsID <- subjectsID[survey$ns[Dlong2$ID %in%zz] %in% c(2:20)]  # delete IDs without transitions
  #Lcoh11 <- subset(Lcoh1, subjectsID2 %in% lex.id)
  Lcoh11 <- Lcoh1[Lcoh1$ID %in% subjectsID2,]
  print (subjectsID2)
  class (subjectsID2)
  
  colours <- rainbow(length(namstates))
  # plot lifelines
  Lcoh11$col<- 1
  for (i in 1:nrow(Lcoh11))
  { Lcoh11$col[i] <- colours[grep(Lcoh11$OR[i],namstates)]  }
  plot.Lexis( Lcoh11, grid=0:20*5, col=Lcoh11$col, xlim=c(PerLow,PerHigh),
     ylim=c(AgeLow,AgeHigh), lwd=2, las=1,col.grid="gray",
     main=title1)
  
 # Mark the location of the events in the Lexis diagram
  pchh <- c(19,16,18)
  #Dlong22 <- subset(Dlong2,Dlong2$ID %in%subjectsID)
  if (length(subjectsID2) < 10) points( Lcoh11$TstopY,Lcoh11$Tstopage,pch=substr(Lcoh11$DES,1,1), cex=0.7 )
 # Display ID 
  if (length(subjectsID2) < 20) 
   {   Lcoh12 <- subset (Lcoh11,DES=="cens",select=c(TstopY,Tstopage,lex.id)) # select open episodes
       text (Lcoh12$TstopY+1.0,
                Lcoh12$Tstopage,Lcoh12$lex.id,cex=0.7,adj=0)
   }
  legend ("topleft",legend=namstates,lty=1,col=colours,bg="white",cex=0.8)
  return (list(Lcoh11=Lcoh11,
               Lcoh12 = Lcoh12,
               sub =subjectsID,
               k = length(subjectsID2)))
}

