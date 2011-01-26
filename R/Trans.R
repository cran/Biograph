Trans <-
function (survey,seq.ind) {
# seq.ind  = individual sequence 
# trans[ix,io,id] = Transitions by age [ix], origin [io] and destination [id]
#   origin = state before j-th transition: seq.ind[i,j]
#   destin = state after j-th transition: seq.ind[i,j+1]
# trans[i,j] is from ist[i,j] to seq.ind[i,j+1]     
#    (eg survey[6,] ; ages[6,]; events[6,] ; seq.ind[6,]
# input
#   -   tstate   Global variable
  #  if (!exists("ist")) seq.ind <- istates(survey)
  if (!exists("numstates"))
  {  print ("Biograph first runs Parameters",quote=FALSE)   
     z <- Parameters (survey)
  }
  agetrans <- AgeTrans(survey)
  ages <- agetrans$ages
  agecens <- agetrans$agecens
  nsample <- nrow(survey)
  agelist2 <- c(iagelow:iagehigh)
  zz <- length(agelist2)
  trans <- array(0,c(zz,numstates,numstates+1))
  zz1 <- zz - 1                 
  dimnames(trans) <- list (Age = c(0:zz1),origin=namstates,destination=c(namstates,"censored"))
  case88 <- ifelse (date_in_month_or_year==1,12,1)
  for (i in 1:nsample) {
    agecens <- trunc((survey$end[i]-survey$born[i])/case88)-iagelow + 1
    if (survey$ns[i] > 1) 
      { for (j in 1:survey$ns[i]-1) 
       {  age_at_trans <- trunc(ages[i,j])-iagelow+1   # + 1 because of vector definition
#         occupx[i,ix] <<-  seq.ind         
         trans[age_at_trans,seq.ind[i,j],seq.ind[i,j+1]] <-  trans[age_at_trans,seq.ind[i,j],seq.ind[i,j+1]] + 1
       }}
    trans[agecens,seq.ind[i,survey$ns[i]],numstates+1] <- trans[agecens,seq.ind[i,survey$ns[i]],numstates+1] + 1
  }
 #occupxt <- tstate
 
  # Total number of direct transitions and censored cases 
  n1 <- numstates + 1 
  n2 <- numstates + 2
  Ttrans <-array(0,c(numstates+1,numstates+2))
  Ttrans[1:numstates,1:n1] <- apply(trans,c(2,3),sum)
  Ttrans[,n2] <- Ttrans[,n1]  # move censored cases to last column
  Ttrans[,n1] <- 0
  Ttrans[,n1] <- rowSums(Ttrans[,-n2])
  Ttrans[n1,] <- colSums(Ttrans)
  dimnames(Ttrans) <- list(Origin=c(namstates,"Total"),Destination=c(namstates,"Total","Censored"))
  # Mean age at transition 
  sweep(trans,c(2,3),colSums(trans),"+")[1,,] #total number of transitions by origin and destination
  agedistrib <- sweep(trans,c(2,3),colSums(trans),"/")  # age distribution
  aged <- array(0,c(iagehigh-iagelow,numstates,numstates+1))
  for (ix in 1:(nage-1)) aged[ix,,] <- agedistrib[ix,,] * (agelist2[ix]-0.5) #  was iagelow:(iagehigh-1)  7 Aug 08
  meanage <- apply(aged,c(2,3),sum)
  dimnames(meanage) <- list(Origin=namstates,Destination=c(namstates,"censored"))
  meanage <- round(meanage,2)    

 
 #  Numer of states occupied during a unit interval
 tt <-array(0,c(nsample,(iagehigh-iagelow)))
# ttt <- array (0,c(10,(iagehigh-iagelow)))
 for (i in 1:nsample) tt[i,] <- table(cut(ages[i,],1:(iagehigh-iagelow+1),include.lowest=TRUE,right=FALSE))
 # for (ix in 1:iagehigh) ttt[,ix] <- hist(tt[,ix],breaks=c(0,1,2,3,4),plot=F)     # TODO
# tt <<- tt
 #  apply(tt,1,sum) is number of events (ns-1) during lifetime by subject
 #  apply(tt,1,sum)  isnumber of transitions by age (aal subjects combined)

# Ttrans <<- Ttrans        # added 18 -7-08
# meanage <<- meanage      # idem

 return(list (Ttrans = Ttrans,
              meanage = meanage,
              trans = aperm(trans,c(1,3,2)),
              trans_during_interval = tt ))              
}

