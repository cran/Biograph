Remove.intrastate <-
function (survey)
{
# Check whether intrastate transitions have been removed. 
  if (is.null(attr(survey,"trans")))   # attribute trans is missing
    {  stop ("Attribute 'trans' is missing from Biograph object. First run Parameters and add attribute.",quote=FALSE)	
    }
  z<- !is.na(diag(attr(survey,"trans")))
  if (!TRUE %in%z) # all diagonal elements are NA
 {   print ("No intra-state transitions present in the data.",quote=FALSE)
 	 zz <- Parameters (survey)
     return (list(D=survey,
              par=zz))
  }  else
 { #  REMOVE INTRASTATE TRANSITIONS 
 	print (". . . . . .   Removes intrastate transitions . . . .",quote=FALSE) 
 	 locpat <- locpath(survey) 
 	 nsample <- nrow(survey)
     nn<- ncol(survey)-locpat
     z<- array(NA,dim=c(nsample,nn))
     for (k in 2:(nn+1))
     {z[,k-1]<- ifelse (substr(survey$path,k,k)!="" & substr(survey$path,k,k)==substr(survey$path,k-1,k-1),k-1,NA)
      }
      pp <- survey$path
      for(k in 2:(nn+1))
      { substr(pp,k,k) <- ifelse (substr(survey$path,k,k)!="" & substr(survey$path,k,k)==substr(survey$path,k-1,k-1)," ",substr(survey$path,k,k))
      }
      for (i in 1:nsample)
      { pp[i]<- string.blank.omit(pp[i])
      }
      dates <- survey[,(locpat+1):ncol(survey)]
      for (j in 1:nn)
      { dates[!is.na(z[,j]),j]<- NA}
      dd <- apply(dates,1,function(x) sort(x,na.last=TRUE))
      dates <- t(dd)
 print (". . . . Intrastate transitions removed. Running Parameters . . . . ",quote=FALSE)
 survey2 <- survey
 survey2$path <- pp
 survey2$ns <- nchar(survey2$path)
 survey2[,(locpat+1):ncol(survey2)] <- dates  
 z <- Parameters (survey2)
 attr(survey2,"trans") <- z$tmat
 print ("A new Biograph object without intrastate transitions is returned.",quote=FALSE)
 return (list(D=survey2,
              par=z))
 }
 }

