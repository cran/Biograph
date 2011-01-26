p.SamplePath <-
function (data, timeunit,subjectsID,seq.ind,ages,path,file)
# Produces sample path for selection of subjects
#       The record numbers of the selected subjects are in vector "subjects"
#       e.g. subjects <- c(1,40,765,5320)
# NEEDS Parameters (first run Parameters)
# Output: prints sample paths to text file; no other output
{ locpat <- locpath (data)
  curpath <- getwd()
  setwd(path)
  sink(file) 
  nn <- length(subjectsID)
  # Remove IDs that are not in sample (in survey data)
  # Check whether subjectsID are in ID
   z<- subjectsID %in% data$ID
   if (TRUE %in% z==FALSE) 
      {print ("print.Samplepath: wrong subjectsID. Check ID numbers.")
       subjectsID <- data$ID[1]
      } 
  for (k in 1:nn) 
  { i <- which (data$ID == subjectsID[k])
    if (length(i) == 0) subjectsID[k] <- NA
  }
  subjects <- subset(subjectsID,!is.na(subjectsID))
  nn <- length(subjects)
  zc <- c(1:nrow(data))
  for (k in 1:nn) 
  { # i<- as.numeric(rownames(survey)[survey$ID==subjects[k]]) Replaced 7/5/2010
     i <- zc[data$ID==subjects[k]]
       # subjects = ID; get associated line number 
    nss <- data$ns[i]
    nss1 <- nss + 1 # = number of transitions + 2 
    nss2 <- nss-1 
    cmc_trans <- vector(mod="numeric",length=nss1)
    cmc_dur <- vector(mod="numeric",length=nss)
    n78 <- locpat+nss # gives cmc at last transition before censoring  
    cmc_trans[1] <- data$start[i] 
    if (nss == 1) cmc_trans[2] <- data$end[i] else cmc_trans[2:nss1] <- c(data[i,(locpat+1):(n78-1)],data$end[i])
    cmc_trans <- unlist (unname(cmc_trans))
    for (j in 1:nss) cmc_dur[j] <- (cmc_trans[j+1]-cmc_trans[j])
    cmc_dur[nss1] <- NA
    ntimeunit <- ifelse (timeunit=="year",1,12)
    age_start <- (data$start[i]-data$born[i])/ntimeunit
    age_cens <- (data$end[i]-data$born[i])/ntimeunit
    print (paste("ID ",data$ID[i], "  Date of birth ",round(data$born[i],2)),quote=FALSE)
    print (paste("Observation window: Start ",round(data$start[i],2),"  End ",round(data$end[i],2)),quote=FALSE)
    print (paste("Total duration of observation ",round(data$end[i]-data$start[i],2),
                 " ",timeunit,"s",sep=""),quote=FALSE)
    print (c("Number of episodes ",nss),quote=FALSE)
    if (nss ==1) age55 <- c(round(age_start,2),round(age_cens,2)) else 
             age55 <- c(round(age_start,2),c(round(ages[i,1:nss2],2),round(age_cens,2)))
    
    samplepath <- cbind(Episode=1:nss1,State=c(namstates[seq.ind[i,1:nss]],"Cens"),TS=c(round(cmc_trans,2)),
    Age=age55,Durat=round(cmc_dur[1:nss1],2),OR=c(0,seq.ind[i,1:nss]),DE=c(seq.ind[i,1:nss],0))
    rownames(samplepath)<- NULL
    TAB_samplepath <-noquote(format(samplepath,justify="right"))
    print (TAB_samplepath)
    print (" ",quote=FALSE)
    print (" ",quote=FALSE)
  }
sink()
print (paste("Selected individual sample paths in file ",file," in working directory ",getwd(),sep=""))
setwd(curpath)
}

