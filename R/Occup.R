# ========================  OCCUP  ======================================
Occup <- function (Bdata) {
# A. State occupancies at exact ages 
# Input  = Bdata.dat
#          iagelow, iagehigh (global variables, defined in MAIN programme)
#          nsample, numstates, namstates (global variables, produced in StateSpace)
#          ages  (global variable, produced in AgeTrans )
#          iagelow = lowest age
#          iagehigh = highest age
#          ageprint: selection of ages to be printed (1 = age 0)
#          subjects: list of subjects for which data should be printed
# Output:
#   a. state_occup : state occupancies at consecutive ages
#   b.  tsjt_p : sojourn time 
#  ageentry[i]
#  agecens[i]
# METHOD:
#    With each element of agelist(continuous stay in state) is associated a state (st[ix])
#
# B. Sojourn times by age and state are grouped over individuals => tsjt
# st_agelist[i,ix] = state occupied at exact age ix by subject i
# st_age_1[i,ixx] = state occupied at exact age ixx by subject i (ixx = one-year age groups)
#        After censoring: state occupied is numstates + 1 
# sjt[ixx,j]  = number of years lived in state j during age interval (ix,ix+1) by given individual
# sjt_age_1[i,ixx,j]  = months spent in state j by individual i between ages ixx and ixx+1
# tsjt  = total sojourn time (entire population) by age and state
#     Note: tsjt[1:numstates+1,] with tsjt[numstates+1,] = person years after censoring
#            Hence: apply(tsjt_p[,-(numstates+1)],1,sum) is person years in observation window 
#                   For og98: apply(tsjt_p[,-6],1,sum)
# NOTE: tstate differs a little from TAB.OUT in Fortran. If indiv makes transition at exact age ix,
#    he occupies the OLD state in the fortran programme and the NEW state in the R programme

 z<- check.par (Bdata)
nsample <- nrow(Bdata)
if (is.null(attr(Bdata,"format.date"))) stop ('in Occup: attr(Bdata,"format.date") missing.')
format.in <- attr(Bdata,"format.date")
nage <- iagehigh - iagelow + 1 
if (nage > 150) warning("Occup: Please check ages. The number of age groups exceeds 150. Running Parameters to be certain.")
if (nage > 150) warning("Occup: Please check ages. The number of age groups exceeds 150. ") else print (paste ("The number of age groups is ",iagehigh - iagelow + 1 ,sep=""),quote=FALSE)
param <- Parameters(Bdata)
agetrans <- AgeTrans(Bdata) # at exit: timeunit is year!!
print (". . . .  Running Occup . . . . ")
ages <- agetrans$ages
agecens <- agetrans$agecens
ageentry <- agetrans$ageentry
#st_agelist TOO BIG: Error: cannot allocat vector of size 781.9 Mb
#  see  Memory-limits {base}
#  object.size(data)
#  was array(0,c(nsmaple,200,2))
#88 st_agelist <- array(0,c(nsample,10,1)) # for each subject, state occupied at different ages (including after ages at transition)
if (length(namage)!=nage) stop("Occup: length namage (",length(namage),")  is not nage (number of age groups) (",nage,"). Namage is: ",namage,"  Solution: rerun Parameters.")
st_age_1 <- array(0,c(nsample,iagehigh-iagelow+1,2))
dimnames(st_age_1) <- list(ID=Bdata$ID,Age=namage,c("Age","State"))
tstate <- array(0,c(iagehigh-iagelow+1,numstates+2)) # same as state_occup
sjt_age_1 <- array (0,c(nsample,iagehigh-iagelow+1,numstates+2))
tsjt <- array (0,c(iagehigh-iagelow+1,numstates+2))
state88 <- array(0,dim=c(nsample,nage,numstates+2))
sjt <- array (0,c(nsample,iagehigh-iagelow+1,numstates+2))
for (i in 1:nsample) {
   agelist <- vector(mode="numeric",length=400)
         #  see AGE_TRANS  (+ 0.00001)
   agelist <-c(iagelow:iagehigh,ages[i,],ageentry[i],agecens[i])
   agelist <- unique(sort(agelist))    # agelist has ages at transition and censoring
   agelist2 <- c(iagelow:iagehigh)    # ages in completed years
# state occupancy at age in agelist (state occupied AFTER event)  
   str_char <- array(" ",c(30))
   str_char <- stringf(Bdata$path[i])
   st <- vector(mode="numeric",length=length(agelist))  # state occupied at age x (0...nage, + events)
   st2 <- vector(mode="numeric",length=iagehigh-iagelow+1) # state occupied at age x (0...nage)
   state <- array(0,c(nage,numstates+2)) 
   ixx <- 1 
   st <- rep(0,length(agelist))                                                                                                           
   for (ix in 1:length(agelist)) 
     {
     if (agelist[ix] == ageentry[i])     st[ix] <- grep(str_char[1],namstates) else
       {if (agelist[ix] > agecens[i]) st[ix] <- numstates + 1  else
       	   # was ==agecens[i]  Changed February 2012 (ABM migration)
            { jvec <- which(ages[i,]==agelist[ix])   #CHECK Quebec paper
            #	print (c(i,ix))
           # 	print (ages[i,])
            #	print (agelist)
           # 	print (paste("jvec = ",jvec,sep=""))
            	   # agelist = ages at events (entry, transition, exit)
            	   # ages[i,] ages at transition in Bdata
            	   # CHANGED JAN 2012: agelist replaced by agelist2 (agelist inconsistent with trunc(ages)): see Real_synthetic
            	   #         jan 2012: reset to agelist and trunc(ages[i,] replaced by ages[i,]
            	          #  print (c(ages[i,] ,jvec,agelist[ix],agecens[i]))
           # Determine which transition takes place at this age:      
           #     = Select in ages, the age at transition that is equal to agelist[ix]. TRANSITION AT IX
           # vector jvec(tor) Which transition(s) take place at this age? = jvec-th transition
           # at jvec-th transition, subject enters (jvec+1)st state
           # In case of multiple transitions at this exact age, the state entered at last transition is taken
              if (length(jvec) >0) {jj <- jvec[length(jvec)]+1
                                    st[ix] <- grep(str_char[jj],namstates)}
                                                  # st is state occupied after each transition
                                                  # str_char is the path for subject i
                                                  # dates at transition: agelist
                                     # was st[is+1] changed feb 2012 when running EBMT (for first age Z: sjt was 0)
        } } 
       }
       # og98 first respondent: trans at age 33.75: st[33]=1 and st[34]=2
       # st = state occupied or entered at each age in agelist
       # st_age_1 : state occupied at birthday
       
     for (ix in 1:length(agelist)) # was agelist (changed January 2012). Should be agelist. Otherwise
     # at higher ages total number of subjects differs from sample size
        {
     if (ix > 1 & st[ix] == 0) st[ix] <- st[ix-1]
     if (agelist[ix] == agelist2[ixx]){st2[ixx] <- st[ix]   # CHECK: here is reason for different R and FORTRAN (occup) 
                                      state[ixx,st[ix]] <- 1  # subject i: ages at transition = ages[i,]
                                                              # st = state occupied by subject i by age (vector)
                                                              # state = state occupied by subject i by age (matrix)
                                                              # tstate = number of members by state by age (state occupancies state_occup)
                                      ixx <- ixx + 1 }         #   print (paste ("Occup: ix,agelist ixx,agelist2 ",ix," ",agelist[ix]," ",ixx," ",agelist2[ixx],sep=""),quote=FALSE)
          # st_occup[i,ixx] <<- st[ix]
       }      
     state88[i,,] <- state
  #88 29_8_2010 st_agelist[i,1:length(agelist),]   <- cbind(agelist,st) # agelist and associated states for subject i 
   st_age_1[i,1:length(agelist2),] <- cbind(agelist2,st2)
   tstate <- tstate + state  
   # state occupancies at calendar time tt
   # tt <- 90*12 + 1   #  January 1990
   # ixt <- trunc((tt - Bdata$born[i])/12) + 1   #  7 September 2008 (for simulation and projection) (+1 because of first age 0)
   # print (c(ixt-1,st[ixt],namstates[st[ixt]]))
                                                     
# Sojourn time
      for (ix in 1:length(agelist)-1) {                        
          # age[1] = 0 
      #if (ix == length(agelist)) sjt[trunc(agelist[ix]),st[ix]] <- 0 
      sjt[i,trunc(agelist[ix])-iagelow+1,st[ix]] <- sjt[i,trunc(agelist[ix])-iagelow+1,st[ix]] + agelist[ix+1]-agelist[ix]
   }
   sjt_age_1[i,,] <- cbind(sjt[i,,])    # !!!!!! agelist2 = vector and sjt == array
   tsjt <- tsjt + sjt[i,,] # sjt is sojourn time in each state (column) by age (row) for subject i
} 
# apply(state88,c(2,3),sum)
# sum(state88[,20,6]) #  =  100 ERROR
# length(agecens[agecens<19])  # 94 OK 
# length(which(agecens<19))  # 94 OK
# cbind(which(agecens<19),agecens[agecens<19])
# cbind(1:nsample,state88[,20,6]) # sum 100  sum(zz[,2])  see subject 179 and 184 (censoring at exact age 19)
#apply(sjt,c(2,3),sum) 
#apply(sjt[,19,],2,sum) # sum over all subjects, age 18
   
# ****   5 oct 08
 numstates2 <- numstates + 1 
 state_occup <- cbind(tstate[,1:numstates2], apply(tstate,1,sum))
 dimnames (state_occup) <-  list (Age = c(iagelow:iagehigh),state=c(namstates,"Censored","Total"))
 # ***  5 Oct 08
#dimnames (tstate) <- list (Age = c(iagelow:iagehigh),state=c(namstates,"Censored","Total"))
# tstate <<- tstate
dimnames (sjt_age_1) <-  list (ID=c(1:nsample),Age = c(iagelow:iagehigh),state=c(namstates,"Censored","Total"))
# st_age_1 <<-st_age_1
# sjt_age_1 <<- sjt_age_1
numstates2 <- numstates + 2 
tsjt[,numstates2] <- apply(tsjt,1,sum)    # total sojourn time (all states combined)
dimnames (tsjt) <-  list (Age = c(iagelow:iagehigh),state=c(namstates,"Censored","Total"))
# tsjt <<- tsjt
class(state_occup) <- "occup.S"
return (list ( state_occup = state_occup,
               st_age_1 = st_age_1[,,2],
               sjt_age_1 = sjt_age_1,
               tsjt = tsjt))
}
