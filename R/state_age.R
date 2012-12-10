state_age <-
function (Bdata,age,ID) {
	
	if (missing (age)) stop("No age")
	if (missing (ID)) stop ("No ID")
    #if (missing(agetrans)) stop ("Compute agetrans")
    if (length(ID)>nrow(Bdata)) ID=Bdata$ID
    Bdata2 <- Bdata[Bdata$ID%in%ID,]
    namstates <- attr(Bdata,"param")$namstates
    iagelow <- attr(Bdata,"param")$iagelow
    iagehigh <- attr(Bdata,"param")$iagehigh 
    agetrans <- AgeTrans(Bdata2)
    state <- matrix (data=0,nrow=length(ID),ncol=length(age),dimnames=list(ID=ID,age=age))
      for (i in 1:length(ID))  # length (ID) = nrow(Bdata2)
    {  # List ages, including ages at entry, transition and exit
       # to determine states after these ages 
        if (length(na.omit(agetrans$ages[i,]))==0) agecens <- agetrans$agecens[i] else
           {if (max(agetrans$ages[i,],na.rm=TRUE)==agetrans$agecens[i])  agecens <- agetrans$agecens[i]+0.000001 else agecens <- agetrans$agecens[i]
           }
      # if (max(agetrans$ages[i,],na.rm=TRUE)==agetrans$agecens[i])  agecens <- agetrans$agecens[i]+0.000001 else
      #      agecens <- agetrans$agecens[i]
       agelist.z <-unname(c(-1,agetrans$ageentry[i],agetrans$ages[i,],agecens,10000))
       agelist <- sort(agelist.z)
       str_char <- stringf(Bdata2$path[i])
       d <- c("-",str_char,"+","+")
       for (j in 1:length(age)) # determine state occupancy at ages 'age'
         # Determine location of the max value of agelist for which [age[j]>=agelist]
        # get that element of d
        # if observation is censored at precisely age 30 (GLHS ID 72,122 and 187), person is NOT in state "+" at age 30
        # state = state just prior to the transition, except for first age
        # if person is in state + at age 30: d[which.max(agelist[age[j]>=agelist])] 
         {  	if (j==1) state[i,j] <- d[which.max(agelist[age[j]>=agelist])] else
         	state[i,j] <- d[which.max(agelist[age[j]>agelist])]    }
     }
      # table (state[HRSs.a$end>=100,])  # indiv state at age AGE
       nam = c("-",namstates,"+")
       state.n <- matrix(data=0,nrow=length (age),ncol=length(nam)+1,dimnames=list(age=age,nam=c(nam,"Total")))
       #print (c(length(age),age))
       for (j in 1:length(age)) #  iagelow:iagehigh))
        {  for (k in 1:length(nam))
        	 { for (i in 1:length(ID)) {
             	 if (state[i,j]==nam[k]) state.n[j,k] <- state.n[j,k] + 1 
        	 }}
        }
        state.n[,(length(nam)+1)] <- apply (state.n,1,sum)

    return(list (nam=nam,
                 state=state,
                 state.n=state.n))
}
