AgeTrans <-
function (survey) 
# Global variables: nsample, numstates, namstates
{  nsample <- nrow(survey)
	maxtrans <- (ncol(survey)-locpath(survey))
   ages <- array(0,c(nrow(survey),maxtrans))
   # date_in_month_or_year <- 1 else date_in_month_or_year <-2
   if (!exists("date_in_month_or_year")) 
   { print ("AgeTrans: date_in_month_or_year has not been defined. Biograph first runs Parameters.",quote=FALSE)
   	z<- Parameters (survey) 
   }
   if (date_in_month_or_year == 1) zz <- 12 else zz <- 1
   z12 <-rep(zz,nrow(survey))     # year = 12 months
   for (j in 1:maxtrans) 
     { j22 <- locpath(survey)+j ; ages[,j] <- (survey[,j22] - survey$born)/z12+0.0000001 }  # COMPARE FORTRAN
   ageentry <- vector (mode="numeric",length=nrow(survey))
   ageentry <- (survey$start-survey$born)/z12 +0.0000
   agecens <- vector(mode="numeric",length=nrow(survey))
   agecens <- (survey$end-survey$born)/z12 + 0.000001
   dimnames (ages) <- list (ID=survey$ID,paste("tr",1:maxtrans,sep=""))
  # ages <<- ages
  # ageentry <<- ageentry
  # agecens <<- agecens
   # stae occupied at censoring
  # st_censoring <<- substr(survey$path,survey$ns,survey$ns)
  # st_entry <<- substr(survey$path,1,1)
   return (list (ages =ages,
                 ageentry = ageentry,
                 agecens = agecens,
                 st_entry = substr(survey$path,1,1),
                 st_censoring = substr(survey$path,survey$ns,survey$ns)))
}

