ChangeObservationWindow.e <-
function (survey,entrystate,exitstate)
{ #  Check whether Parameters was called
  if (!exists("nsample")|!exists("namstates")) 
    { print ("ERROR in ChangeObservationWindow.e: parameters missing; call Parameters first")
      return ()
    }
  locpat <- locpath(survey)
  maxtrans <- (ncol(survey)-locpat) 
  windowe <- function (data,refstate)
  { entry88 <- function (x,state)
    {   # Get for a state following another state the position in path and the date of occurrence
        #  x[locpath(survey)] is the state sequence path
        #  x[length(x)] is the first state occupied in current sequence (=low)
        #  x[locpath(survey)-1] is the last state occupied (= ns)
        
    	lowx <- as.numeric(x[length(x)]) # see below
    	if (is.na(lowx))
    	 { return (list(loctrans = NA,
    	 	            date = NA))} else
    	# Is state in substring that starts at position 'lowx' [= x[length(x)]] in path?   
    	{ yn <- pos.char(x[locpat],state)
    	  if (is.na(yn))  # state in not in path
    	    {return (list(loctrans = NA,
    	    	            date = NA))} else
         { # State is in substring of path
           # Get the substring
             kk <- substr(x[locpat],x[length(x)],x[(locpat-1)])
           # Determine position of state (loc) in substring and date at entry into state
             loc <- grep(state,stringf(kk))[1]  # loc=first location of (reference) state
          # z98 <- ifelse (is.na(loc),as.numeric(x[(locpat-1)])-as.numeric(x[length(x)])+1,(z99 -1 + as.numeric(x[length(x)])) )
          # Position of state in path
           locp <- ifelse (loc==1,lowx,lowx + loc -1)
           date <- ifelse (loc==1,x[3],x[(locpat+locp-1)]) # date at entry in state
            # if loc==1, date is survey$start
           # if (is.na(state)) date <- x[(locpat+z98)]
           return(list(loctrans = locp,    #  location of transition (entry or exit)
                       date=date))   #  date of transition
         } 
       }
    } # end entry88
    
    if (is.na(refstate)) # user gives NA as reference state
     { loc <- data[,(locpat-1)]
       date <- data[,4]
     }  else
     { date90 <- apply (data,1,function(x) entry88(x,refstate))
       xx<- sapply(date90,function(x) {x[1]})
       loc <- unname(unlist(xx)) #  location of state at entry in observation
       xx<- sapply(date90,function(x) x[2])
       date <- as.numeric(unname(unlist(xx)) )# date at entry in observation 
     }
    return (list (loc=loc,
                  date=date))
  }  # end windowe
  
  low1 <- rep(1,nsample)
  entry <- windowe (cbind(survey,low1),entrystate)   # GLHS low[1] = 2
  exit <- windowe (cbind(survey,low1=entry$loc),exitstate)
 # exit$loc <- exit$loc + entry$loc - rep(1,length(exit$loc))  # CHANGED 10_11_2010
   # nn <- nrow(survey[!is.na(exit$loc),]) 
   
# Create a new Biogaph data file: subset with observations in the interval
  entryloc <- entry$loc
  exitloc <-  exit$loc
  #cmc_select <- subset(cmc,!is.na(entrydate))
  entrydate <- entry$date  # STARTING DATE for subjects with entry event
  exitdate <-  ifelse (is.na(exit$date),survey$end,exit$date)  # idem
  #  if exit (censoring) can be at survey date
  survey2 <- survey
  for (i in 1:nsample)
   { survey2$path[i] <- ifelse (is.na(entry$loc[i])," ",ifelse (is.na(exit$date[i]),entrystate,substr(survey$path[i],entry$loc[i],exit$loc[i])))}  
  for (i in 1:nsample)
  {  survey2$ns[i] <-  nchar(survey2$path[i])  
     survey2$start[i] <- ifelse (entry$loc[i]==1,survey$start[i],survey[i,(locpat+entry$loc[i]-1)])
     nn <- ifelse (survey2$ns[i]==1,1,survey2$ns[i]-1)
     if (nn==1) {survey2[i,(locpat+1)] <- survey[i,(locpat+entry$loc[i])]} else
           {survey2[i,(locpat+1):(locpat+nn)] <- survey[i,(locpat+entry$loc[i]):(locpat+exit$loc[i]-1)]}
     #   ifelse (is.na(exit$loc[i]),NA,survey[i,(locpat+entry$loc[i]):(locpat+exit$loc[i]-1)])
     survey2[i,(locpat+survey2$ns[i]):(locpat+maxtrans)] <- NA  
     survey2$end <- exitdate
   }
   survey2 <- survey2[survey2$path!=" ",]
  return (survey2)
 }

