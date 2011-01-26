TransitionAB <-
function (data,transition)
{ if (!exists("date_in_monht_or_year"))
{  print ("TransitionAB: Biograph first runs Parameters",quote=FALSE)
z <- Parameters (data)
}
  locpat <- locpath(data)
	# Determine whether transition is from origin to destination state 
  # or from any origin to a destination state
  if (nchar(transition)!=2) 
   { print ("TransitionB: error in defining transition, use '*T' if you want destination T from any origin") 
     return
   }
  if (nchar(transition)==2 & substr(transition,1,1)=="*") ncasetrans <- 1 else ncasetrans <- 2  

# Determine, from data$path, for each subject the position of the transition 
 pos <- sapply(data$path,function(x) 
    { z1 <- grep(transition,x)
     pos <- ifelse (length(z1)==0,NA,
            ifelse (z1==1,nchar(unlist(strsplit(x,transition)[1]))+1,0)) } )
  pos <- unname(pos)
  
  print (paste ("Age profile: Number of individuals with transition ",transition," is ",length(na.omit(pos)),sep=""),quote=FALSE)
  # = position of transition given by cmeanhar in survey$path
  
  # aa <- ifelse (ncasetrans==1,locpat+as.numeric(x[1]),locpat+as.numeric(x[1])+1)
  dates <- apply(cbind(pos,data),1,function(x) 
             {z <- ifelse (ncasetrans==1,locpat+as.numeric(x[1]),locpat+as.numeric(x[1])+1)
              x[z]})
  dates <- as.numeric(dates)

# Ages at transition
  if (date_in_month_or_year == 1) z12 <- 12 else z12 <- 1
  ages <- (dates-data$born)/z12

# Calendar years at transition
  years <- 1900 + dates/z12  
  
  # select subjects who experienced the transition
  # datas <- data[!is.na(pos),]  # subset of survey
   
   cases <- c("From any origin to given destination","From given origin to given destination")
   cases <- paste ("Transition ",transition,": ",cases,sep="")
   return (list (case = cases[ncasetrans],
                 N = length(na.omit(ages)),
                 pos = pos,
                 date = dates,
                 age = ages,
                 year=years,
                 cohort=1900 + trunc(data$born/z12))) 
}#  meana = mean(ages,na.rm=T)

