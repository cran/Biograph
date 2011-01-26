p.Trans <-
function (trans,ratetable,path,filnam.trans,filnam.Stable)
{ curpath <- getwd()
  setwd(path)
  Ttrans <- trans$Ttrans
  trans2 <- trans$trans
  meanage <- trans$meanage
  Stable <- ratetable$Stable
  censored_by_age <- ratetable$censored_by_age
  
  trans2 <- aperm(trans2,c(1,3,2))
  sink(filnam.trans)
  print ("Overview of transitions (TAB_Trans)",quote=FALSE)
  print (" ",quote=FALSE)
  print ("Direct transitions and censored cases", quote=FALSE)
  print (Ttrans)
  print (" ",quote=FALSE)
  print (" ",quote=FALSE)
  print ("Mean ages at transition",quote=FALSE)
  print (meanage)
  sink ()
  # ***********************
  print (paste("Transitions by age, origin and destination saved in file ",filnam.trans," in working directory ",getwd(),sep=""))
 
  sink (filnam.Stable)
  for (itrans in 1:numstates) {
    print (" ",quote=FALSE)
    print (c("state of origin = ",namstates[itrans]),quote=FALSE)
    print (Stable[,,itrans] ) # see   Stable = aperm(Stable,c(1,3,2))
    print (" ",quote=FALSE)
    }

  print (meanage)
  sink() 
  print ("Output files: Stable.out (State Occupancies, PY and events) and TAB_trans.out (direct transitions)",quote=FALSE)
  setwd(curpath)
  
  #censored_by_age <<- censored_by_age
  # state occupied at censoring (for microsimulation)
 # sink("censored_by_age.out")
 # print (censored_by_age)
 # sink ()
}

