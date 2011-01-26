CMC.years <-
function (survey,covsCMC)
#
{  #locpath <- which(survey[1,] == survey[1,"path"],arr.ind=TRUE)[2]
     if (!exists("timeunit"))
     { print ("CMC.years: Biograph first runs Parameters . . . ",quote=FALSE)
       z <- Parameters(survey)
     }
   locpat <- locpath(survey)
   maxtrans <- ncol(survey)-locpat
   if (timeunit == "month")
  { z1900 <- 1900
  	code88 <- 12 } else
  { z1900 <- 0
  	code88 <- 1 
  }
   zz <-rep(z1900,nrow(survey))
   z12 <-rep(code88,nrow(survey))
   survey$born  <- zz + survey$born/z12
   survey$start <- zz + survey$start/z12
   survey$end   <- zz + survey$end/z12
   for (i in (locpath(survey)+1):(locpath(survey)+maxtrans))
    { survey[,i] <- zz + survey[,i]/z12 
    }
    k <- exists("covsCMC") & !is.null(covsCMC)
    if (k==TRUE) 
    { dd <- zz + survey[colnames(survey) %in% covsCMC]/z12   
    survey[colnames(survey) %in% covsCMC] <- dd }
    return(survey)
}

