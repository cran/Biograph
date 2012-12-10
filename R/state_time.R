state_time <-
function (Bdata,ID) {
   if (FALSE%in%(ID%in%Bdata$ID)) stop("state_time: ID contains identification numbers that are not part of the data.")	
   #   ------------ To prevent checking note: "no visible binding for global variables"---
   # --------------    see Lexis.lines   --------------------------------------------------
   if (length(ID)>nrow(Bdata)) ID=Bdata$ID
   Bdata2 <- Bdata[Bdata$ID%in%ID,]
  ## state2 <- state[Bdata$ID%in%ID,]
   nsample <- length(ID)
   if (!exists("namstates"))  param <- Parameters (Bdata)
  namstates <- attr(Bdata,"param")$namstates
  numstates <- length (namstates)
  iagelow <- attr(Bdata,"param")$iagelow
  iagehigh <- attr(Bdata,"param")$iagehigh
   
   #print ("state_time  88")
   agetrans <- AgeTrans(Bdata2)
   #print ("state_time 89")
   nam = c("-",namstates,"+")
   #tsjt <- array (0,c(iagehigh-iagelow+1,numstates+2),dimnames=list(Age=iagelow:iagehigh,State=nam))
   sjt <- array (0,c(nsample,iagehigh-iagelow+1,numstates+2),dimnames=list(ID=Bdata2$ID,Age=iagelow:iagehigh,State=nam))
   state <- matrix (data=0,nrow=nsample,ncol=iagehigh-iagelow+1,dimnames=list(ID=ID,age=iagelow:iagehigh))
   for (i in 1:nrow(Bdata2))    
	{  agelist.z <-unname(c(iagelow:iagehigh,agetrans$ageentry[i],agetrans$ages[i,],agetrans$agecens[i]))
       agelist <- unique(sort(agelist.z))
      # Determine state occupied at each time point in agelist vector
      st.char <- state_age(Bdata2,age=agelist,ID=Bdata2$ID[i])$state
      state[i,] <- st.char[as.numeric(colnames(st.char))%in%c(iagelow:iagehigh)] # state at birthday
     # st.char = state occupied at exact age 20,21,22,etc (in character). From state_age
     #st.char = state[Bdata$ID%in%ID[i],]
      st <- match(st.char,nam)
         # state occupied after transition age; numeric value
  # Sojourn time
       for (ix in 1:length(agelist)-1) { 
       	# Select agelist values between ages 19 and 20:
       	#for (ix in c(which (trunc(agelist)==19)))  {                     
      sjt[i,trunc(agelist[ix])-iagelow+1,st[ix+1]] <- sjt[i,trunc(agelist[ix])-iagelow+1,st[ix+1]] + agelist[ix+1]-agelist[ix]
       }
      ix <- length(agelist)  # last age
      sjt[i,trunc(agelist[ix])-iagelow+1,st[ix]] <- sjt[i,trunc(agelist[ix])-iagelow+1,st[ix]] + trunc(agelist[ix]) + 1 - agelist[ix]
     # tsjt <- tsjt + sjt[i,,] # sjt is sojourn time in each state (column) by age (row) for subject i
   } 
   #  apply(f$tsjt,1,sum)
   sjt_age_1=sjt #    aperm(sjt,c(2,3,1))
   tsjt <- matrix (nrow=attr(Bdata,"param")$nage,ncol=length(nam)+1,dimnames=list (Age=attr(Bdata,"param")$namage,Case=c(nam,"Total")))
   tsjt[,1:length(nam)] <- apply(sjt_age_1,c(2,3),sum)   #  c(1,2),sum)
   # state occupation time for given ID and given age (single year)
   #    sjt[which(Bdata$ID==122),which(colnames(sjt)==19),]
    tsjt [,(length(nam)+1)] <- apply(tsjt[,1:length(nam)],1,sum)
       
    print ("state_time: compute state.n")
      # table (state[HRSs.a$end>=100,])  # indiv state at age AGE
       state.n <- matrix(data=0,nrow=length (iagelow:iagehigh),ncol=length(nam)+1,dimnames=list(age=iagelow:iagehigh,nam=c(nam,"Total")))
       #print (c(length(age),age))
       for (j in 1:length(iagelow:iagehigh))
        {  for (k in 1:length(nam))
        	 { for (i in 1:length(ID)) {
             	 if (state[i,j]==nam[k]) state.n[j,k] <- state.n[j,k] + 1 
        	 }}
        }
        state.n[,(length(nam)+1)] <- apply (state.n,1,sum)
        
   return (list(state=state,
                state.n = state.n,
                sjt_age_1=sjt_age_1,
                tsjt=tsjt))
}
