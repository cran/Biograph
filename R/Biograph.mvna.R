Biograph.mvna <-
function (survey)
{
# ========== SURVEY TO MVNA FORMAT ======================
# need to remove intrastate transitions
  removed <- Remove.intrastate (survey) 
  survey2<- removed$D
  tmat <- attr(survey2,"trans")
  if (timeunit == "month")
  { z1900 <- 1900
  	code88 <- 12 } else
  { z1900 <- 0
  	code88 <- 1 
  }
  namstates2 <- vector (mode="numeric",length=length(namstates))
  for (i in 1:length(namstates))
    { namstates2[i] <- grep(namstates[i],namstates)} 

  print ("      Biograph.mvna: Calls function Biograph.long . . . ")
  zm <- Biograph.long (survey2)
  Dmvna <- zm$Depisode
  colnames(Dmvna[1:5]) <- c("id","from","to","entry","exit")
  Dmvna <- subset(Dmvna,!is.na(Dmvna[,1]))
   
  Dmvna$from <- apply (Dmvna,1,function(x) grep(x[2],namstates))
  Dmvna$to[Dmvna$DES=="cens"] <- "cens"
  to <- ifelse (Dmvna$DES=="cens","cens",apply(Dmvna,1,function (x) grep(x[3],namstates)))
  Dmvna$to <- unlist (to)

  Dmvna$entry <- (Dmvna$Tstart-Dmvna$born)/code88  
  Dmvna$exit <- (Dmvna$Tstop-Dmvna$born)/code88
  colnames(Dmvna)[1] <- "id"

  D2 <- data.frame(id=Dmvna$id,from=Dmvna$from,to=as.character(Dmvna$to),
    entry=as.numeric(Dmvna$entry),exit=as.numeric(Dmvna$exit))
  D2$exit <- ifelse (D2$exit <= D2$entry,D2$exit+1,D2$exit) # CORRECT if CMC exit = CMC entry
  attr(D2, "trans") <- attr(survey2,"trans")   
  attr(Dmvna, "trans") <- attr(survey2,"trans")   
  
  print ("     Biograph.mvna completed: Object produced by Surveydat.mvna: .$mvna ",quote=FALSE)
  return (list (D=D2,  
                D.cov=Dmvna,
                par = removed$par,
                cens = "cens"))
}

