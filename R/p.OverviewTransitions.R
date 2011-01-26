p.OverviewTransitions <-
function (data,path,file.in,file.out)
{ curpath <- getwd()
  setwd(path)
 meanage<- data$meanage
 Ttrans <- data$Ttrans
 sink(file.out,append=TRUE)
 print (" ",quote=FALSE)
 print (" ",quote=FALSE)
 print (paste("OVERVIEW OF TRANSITIONS","   Data = ",file.in),quote=FALSE)

  print (" ",quote=FALSE)
  print ("Direct transitions and censored cases", quote=FALSE)   
  print (Ttrans)
  
  print (" ",quote=FALSE)
  print (" ",quote=FALSE)
  print ("Mean ages at transition",quote=FALSE)

  print (meanage)
  sink ()
  if (sink.number() > 0 ) sink()
  if (sink.number() > 0 )  sink()
  print (paste("Overview episodes in file ",file.out," in working directory ",getwd(),sep=""))
  setwd(curpath)
}

