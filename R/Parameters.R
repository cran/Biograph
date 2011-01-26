Parameters <-
function (survey) {
# ---- Parameters from SURVEY.DAT: state space, sequence of states occupied -----
# -------- and CMC at transition from SURVEY.DAT ---------
# Input =  survey.dat     
# Output = The results are stored in global variables:
#    a. nsample, numstates and namstates
#    b. Flow table
#    c. Matrix of possible transitions

nsample <<- nrow(survey)    # 201 records in survey.dat
 # determine position of character variable path
if(max(survey$born,na.rm=TRUE) > 500 & max(survey$born,na.rm=TRUE) < 1800) date_in_month_or_year <- 1 else date_in_month_or_year <-2
timeunit <- ifelse (date_in_month_or_year==1,"month","year")
#  date_in_monbths_or_year = 1 (dates in months) or 2 (dates in years, e.g. in simulation: in ages)
if (date_in_month_or_year==1) case88 <- 12 else case88 <- 1
date_in_month_or_year <<- date_in_month_or_year
timeunit <<- timeunit
  #  date in CMC or date in year (e.g. age)
statespace <- StateSpace (survey)
# --------- Determine OR and DE and generate flow table ------------------------------------
maxns <- max(survey$ns)
str_char <- array(" ",c(maxns))
nntrans <- array(0,c(numstates,numstates))    
dimnames (nntrans) <- list(Origin=namstates,Destination=namstates)
survey$path <- as.character(survey$path)
for (i in 1:nsample)
 { if (survey$ns[i] > 1) 
    { str_char <- stringf(survey$path[i])
      for (k in 2:(survey$ns[i]))
      { io <- grep(substr(survey$path[i],k-1,k-1),namstates)
        id <- grep(substr(survey$path[i],k,k),namstates)
        nntrans[io,id] <- nntrans[io,id] + 1  # nntrans = flow table
   }}}                                                  
  # Allocate to each possible transition a number (for survival package etc)
tmat <- matrix(NA,ncol=numstates,nrow=numstates)
dimnames(tmat) <- list(From=namstates,To=namstates)
or <- numeric(length=numstates*numstates)
des <- numeric(length=numstates*numstates) 
kk <- 0
for (i in 1:numstates) { for (j in 1:numstates) 
     {if (nntrans[i,j] > 0) {kk <- kk + 1
                            tmat[i,j] <- kk
                            or[kk] <- i
                            des[kk] <- j}}}
#                        or and des: see alsp Putter_GLHS.r
#for  (i in 1:numstates) msdata$from[msdata$OR==namstates[i]] <- i
#for  (i in 1:numstates) msdata$to[msdata$DES==namstates[i]] <- i
ntrans <-  length(nntrans[nntrans>0]) # number of transitions
    #ntrans <- sum(apply(!is.na(tmat),1,sum)) # number of transitions 
transitions <- data.frame(cbind(Trans=1:ntrans,OR=or[1:ntrans],DES=des[1:ntrans],ORN=namstates[or[1:ntrans]],DESN=namstates[des[1:ntrans]]))
transitions$ODN <- paste(transitions$ORN,transitions$DESN,sep="")

# Possible transitions
trans_possible <- array(TRUE,c(numstates,numstates))
dimnames(trans_possible) <- list(Origin=namstates,Destination=namstates)
for (i in 1:numstates)
     { for (j in 1:numstates)
       { if (nntrans[i,j] == 0) trans_possible[i,j] <-FALSE
     }}

# --------- Determine minimum and highest age --------------------
iagelow <<- trunc(min(survey$start-survey$born,na.rm=TRUE)/case88)
# iagehigh <<- trunc(max(max(survey$end)-survey$born)/case88 + 1)
iagehigh <<- trunc(max(survey$end-survey$born,na.rm=TRUE)/case88 + 1)  # changed on 29/8/2010 working on India with Sabu (max age is 50 and not 65)
nage <<- iagehigh - iagelow + 1 # number of age groups   
namage <<- iagelow:iagehigh
# survey$ns  = number of states occupied by respondent i
# Read survey$path[i] (which is the state sequence) and convert the sequence 
#     of characters (string) into numerical values stored in the i-th row of the 
#     matrix seq.ind[i,j]: 1 = namstates[1], 2 = namstates[2], etc. 
#  NOTE 1: the character representing the j-th state for subject i is:
#         namstates[seq.ind[i,j]]  = sequence of state occupancies
#  NOTE 2: seq.ind[i,j] is the state occupied by the i-th respondent BEFORE the j-th transition
#          seq.ind[i,j+1] is the state occupied AFTER the j-th transition
#         The first state, seq.ind[i,1], is the state occupied at onset of observation 
#          In Fortran:  seq.ind[i,j]  = e(j,1) and e(j,2): state before and after j-th transition
#          Last state before end of observation (state at censoring) = seq.ind[i,survey$ns[i]]
# converts character into integer (location in state space)


# ..... Dates of transitions experienced by respondent i  .......
#  Subject i experiences the j-th transition at cmc[i,j]. Source: SURVEY.DAT                
# Obtain cmc from survey
# locpath <<- which(survey[1,] == survey[1,"path"],arr.ind=TRUE)[2]
ncovariates  <<- locpath(survey) - 7
ncmc_tr <<-  (ncol(survey)-locpath(survey)) # max(nchar(survey$path))
maxtrans <<- ncmc_tr
#cmc <- array(0,c(nsample,maxtrans)) 
#dimnames(cmc) <- list (Subjects=c(1:nsample),Transitions= paste("tr",1:maxtrans,sep=""))
#cmc <- survey[,(locpath(survey)+1):(locpath(survey)+maxtrans)]
# cmc at last transition for subject 6 : cmc[6,survey$ns[6]-1] 
# ------------------- end of state space and CMC ---------------- 
# global variables:
#cmc <<- cmc
print (paste("Number of states = ",numstates,sep=""),quote=FALSE)
print (c("Names of the states = ",namstates))
print ("To change the sequence of states, type: <namstates <- c(...)> after calling Parameters",quote=FALSE)
print (paste("Number of feasible transitions = ",ntrans,sep=""),quote=FALSE)
return (list (ntrans = ntrans,
              nntrans = nntrans,
              trans_possible = trans_possible,
              transitions = transitions,
              tmat = tmat))
}

