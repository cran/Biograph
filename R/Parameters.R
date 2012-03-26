# ========================  Parameters  =================================
Parameters <- function (Bdata) {
print (". . . . Running function Parameters . . . . ")
#-------------   state space   ----------
if (exists("namstates")) 
  { namst8 <- namstates
  	statespace <- StateSpace (Bdata)
  	if (TRUE%in%(namstates%in%namst8)) namstates <- namst8
  	     # state space of Bdata is same as state space in memory
  } else statespace <- StateSpace (Bdata)

#  ---------  Determine date format ----------
  format.in <- attr(Bdata,"format.date") 
  if (is.null(format.in))
    { print (" ",quote=FALSE)
      if(max(Bdata$born,na.rm=TRUE) > 500 & max(Bdata$born,na.rm=TRUE) < 1800) format.in <- "CMC" else format.in <- "year"
      stop (paste ('Function Parameters: date format (attribute format.in) missing from Biograph object (data). Please add, e.g.: <attr(GLHS,"format.date") <- "CMC">. Biograph expects date format to be: ',format.in,sep=""))
    } 
  assign("format.in",format.in,envir=.GlobalEnv)

# --------- Determine minimum and highest age --------------------
if (attr(Bdata,"format.date") == "age")
  { iagelow <- min(Bdata$start)
    iagehigh <- max(Bdata$end)  	
  } else
  { iagelow <- as.numeric(min(Bdata$start-Bdata$born,na.rm=TRUE))
    iagehigh <- as.numeric(max(Bdata$end-Bdata$born,na.rm=TRUE))
  }
if (substr(attr(Bdata,"format.date"),1,1)=="%") 
  { iagelow <- trunc(iagelow/365.25)
  	iagehigh <- trunc(iagehigh/365.25)+1
  } 
if (attr(Bdata,"format.date")=="CMC"|attr(Bdata,"format.date")=="cmc") {iagelow <- iagelow/12; iagehigh <- iagehigh/12}  
iagelow <- trunc(iagelow)
iagehigh <- trunc(iagehigh)+1

#ntimeunit <- ifelse (attr(Bdata,"format.date")=="CMC",12,1)
#if (format.in=="age") {iagelow <- trunc(min(Bdata$start));iagehigh<-trunc(max(Bdata$end)+1)} else
#{iagelow <- ifelse (min(Bdata$start - Bdata$born)<0,0,trunc(min(Bdata$start-Bdata$born,na.rm=TRUE)/ntimeunit))
## iagehigh <<- trunc(max(max(Bdata$end)-Bdata$born)/ntimeunit + 1)
#iagehigh <- ifelse (min(Bdata$end - Bdata$born)<0,trunc(max(Bdata$end-Bdata$start))+1,trunc(max(Bdata$end-Bdata$born,na.rm=TRUE)/ntimeunit))+1}

 # with Sabu (max age is 50 and not 65)
assign("iagelow",iagelow,envir=.GlobalEnv)
assign("iagehigh",iagehigh,envir=.GlobalEnv)
assign("nage",iagehigh-iagelow+1,envir=.GlobalEnv)
#nage <<- iagehigh - iagelow + 1 # number of age groups   
namage <- iagelow:iagehigh
assign("namage",namage,envir=.GlobalEnv)

# ---- Parameters from Bdata: state space, sequence of states occupied -----
test88 <- ifelse (!exists("nsample"),0,1) 
assign ("nsample",nrow(Bdata),envir=.GlobalEnv)  

# ---------- covariates -------------
locpat <- locpath(Bdata)
ncovariates  <- locpat - 7
assign("ncovariates",ncovariates,envir=.GlobalEnv)
covariates <- colnames(Bdata)[5:(locpat-3)]
ncmc_tr <-  (ncol(Bdata)-locpath(Bdata)) # max(nchar(Bdata$path))
assign("ncmc_tr",ncmc_tr,envir=.GlobalEnv)
maxtrans <- ncmc_tr
assign("maxtrans",maxtrans,envir=.GlobalEnv)
# global variables:
#cmc <<- cmc
if (test88==0)
 { print (paste("Number of states = ",numstates,sep=""),quote=FALSE)
   print (c("Names of the states = ",namstates),quote=FALSE)
   print ("To change the sequence of states, type: <namstates <- c(...)> after calling Parameters",quote=FALSE)
 }
# ---------  Flow table of transitions  -----------
 print ("Exploring types of transitions")
 zt <- transitions (Bdata)  

return (list (nsample = nsample,
              numstates=numstates,
              namstates = namstates,
              absorbstates=statespace$absorbstates,
              iagelow=iagelow,
              iagehigh=iagehigh,
              namage = namage,
              number_of_age_groups_nage=nage,
              number_of_transitions_ntrans = zt$ntrans,
              trans_possible = zt$trans_possible,
              tmat = zt$tmat,
              transitions = zt$transitions,
              nntrans = zt$nntrans,
              maximum_number_of_transitions_by_individual_maxtrans=maxtrans,
              number_of_covariates_ncov=ncovariates,
              covariates=covariates,
              format.date = attr(Bdata,"format.date")))
}
