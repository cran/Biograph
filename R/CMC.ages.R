CMC.ages <-
function (survey,covsCMC)
#
{  locpat <- locpath(survey)
   if (!exists("timeunit"))
     { print ("CMC.ages: Biograph first runs Parameters . . . ",quote=FALSE)
       z <- Parameters(survey)
     }
   maxtrans <- ncol(survey)-locpat
    if (timeunit == "month")
  { code88 <- 12 } else
  { code88 <- 1 
  }
   z12 <-rep(code88,nrow(survey))
   survey$start <- round((survey$start-survey$born)/z12,3)
   survey$end   <- round ((survey$end-survey$born)/z12,3)
   for (i in (locpat+1):(locpat+maxtrans))
    { survey[,i] <- round((survey[,i]-survey$born)/z12,3)
    }
    k <- exists("covsCMC") & !is.null(covsCMC)
    if (k==TRUE) 
    {dd <- (survey[colnames(survey) %in% covsCMC] - survey$born)/z12
    survey[colnames(survey) %in% covsCMC] <- round(dd,3)}
    survey$born  <- rep(0,nrow(survey))
    return(survey)
}

