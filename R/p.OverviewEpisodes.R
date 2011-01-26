p.OverviewEpisodes <-
function (survey,seq.ind,file.in,path,file.out) {
  nsample <- nrow(survey)
  locpat <- locpath(survey)
  curpath <- getwd()
  setwd(path)
  sink(file.out) 
  print (paste("OVERVIEW OF EPISODES"," (Input =",file.in,")"),quote=FALSE)
  print (paste("A. General"),quote=FALSE)
  print (paste("Number of observations         ",nsample),sep="",quote=FALSE)
  print (paste("Number of episodes             ",sum(survey$ns)),sep="",quote=FALSE)
  print (paste("Number of transitions          ",sum(survey$ns-1)),sep="",quote=FALSE)
  ndur <- sum(survey$end-survey$start)
  print (paste("Total duration of observation  ",ndur," ",timeunit,"s)",sep=""),quote=FALSE)
  print (" ",quote=FALSE)
  print ("B. Number of episodes ",quote=FALSE)
  print ("Number of subjects by number of episodes in observation window")
  NE <- vector(mode="numeric",length=max(survey$ns)+1)
  NE <- c(table(survey$ns),sum(table(survey$ns)))
  zz <- length(NE)-1
  names(NE) <- c(1:zz,"Total")
  print (NE)
  print (" ",quote=FALSE)
  # Open and closed episodes
  # no transition: determine state at onset of observation
  NumEpisodes <- array(0,c(numstates+1,5))
  for (i in 1:nsample) {
    nns <- survey$ns[i]
    nns2 <- nns-1
  #     LRO and LO
    if(nns==1) NumEpisodes[seq.ind[i,1],1] <- NumEpisodes[seq.ind[i,1],1] + 1 
    else {NumEpisodes[seq.ind[i,1],2] <- NumEpisodes[seq.ind[i,1],2] + 1 
  # RO
          NumEpisodes[seq.ind[i,nns],3] <-  NumEpisodes[seq.ind[i,nns],3] + 1
  # Closed
          if (nns>2) for(j in 2:nns2) NumEpisodes[seq.ind[i,j],4] <- NumEpisodes[seq.ind[i,j],4]+1
          }
   }
  NumEpisodes[,5] <- rowSums(NumEpisodes)
  NumEpisodes[numstates+1,] <- colSums(NumEpisodes)
  dimnames(NumEpisodes) <- list(Episode=c(namstates,"Total"),Type=c("LROpen","LOpen","ROpen","Closed","Total"))
  print (NumEpisodes)
  print (" ",quote=FALSE)
  print (paste("C. Total duration of episodes (in ",timeunit,"s)",sep=""),quote=FALSE)
  DurEpisodes <- array(0,c(numstates+1,5))  
  for (i in 1:nsample) {
    nns <- survey$ns[i]
    nns2 <- nns-1
    nns77 <- locpat+survey$ns[i]-1    # number of last transition before censoring
    if(nns==1) DurEpisodes[seq.ind[i,1],1] <- DurEpisodes[seq.ind[i,1],1] + survey$end[i]-survey$start[i] 
    else {DurEpisodes[seq.ind[i,1],2] <- DurEpisodes[seq.ind[i,1],2] + survey[i,(locpat+1)]-survey$start[i] 
          DurEpisodes[seq.ind[i,nns],3] <-  DurEpisodes[seq.ind[i,nns],3] + survey$end[i]-survey[i,nns77]
          if (nns>2) for(j in 2:nns2) { 
              jj21 <- locpat + j -1
              jj22 <- jj21 + 1 
              DurEpisodes[seq.ind[i,j],4] <- DurEpisodes[seq.ind[i,j],4]+ survey[i,jj22] - survey[i,jj21] }
          }
   }    
  DurEpisodes[,5] <- rowSums(DurEpisodes)
  DurEpisodes[numstates+1,] <- colSums(DurEpisodes)
  dimnames(DurEpisodes) <- dimnames(NumEpisodes)
  print (DurEpisodes)
  print (" ",quote=FALSE)
  print (paste("D. Mean duration of episodes (in ",timeunit,"s)",sep=""),quote=FALSE)
  print (noquote(format(DurEpisodes/NumEpisodes,digits=2,nsmall=2)))
  
  sink()
  if (sink.number() > 0 ) sink()
  if (sink.number() > 0 )  sink()
  print (paste("Overview episodes in file ",file.out," in working directory ",getwd(), sep=""))
  setwd(curpath)
  return (list(n= nsample,
               ne = sum(survey$ns),
               nt = sum(survey$ns-1),
  			   sojourn=DurEpisodes))
 }

