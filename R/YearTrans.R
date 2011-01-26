YearTrans <-
function (survey) 
# Global variables: nsample, numstates, namstates
{   nsample2 <- nsample <- nrow(survey)
     maxtrans <- ncol(survey)-locpath(survey)
    yeartrans <- array(0,c(nsample2,maxtrans))
    if (date_in_month_or_year == 1) {zz <- 12; zzz <- 1900} else {zz <- 1 ; zzz <- 0}
   z12 <-rep(zz,nsample2)
   for (j in 1:maxtrans) 
     { j22 <- locpath(survey)+j 
       yeartrans[,j] <- zzz + survey[,j22]/z12    # real value
       # year = trunc(yeartrans[,j]
       # month = survey[,j22]-  year*12
       #  or month = (yeartrans - year)*12
       }
   yearborn <- vector (mode="numeric",length=nsample2)
   yearborn <-zzz + survey$born/z12
   yearentry <- vector (mode="numeric",length=nsample2)
   yearentry <- zzz + survey$start/z12
   yearcens <- vector(mode="numeric",length=nsample2)
   yearcens <- zzz + survey$end/z12
   year_trans <- array(0,c(nsample2,18))
    year_trans <- cbind(survey$ID,yearborn,yearentry,yearcens,yeartrans)
   dimnames (year_trans) <- list (ID=survey$ID,transition=c("ID","born","entry","censored",paste("tr",1:maxtrans,sep="")))
  # year_trans <<- year_trans
  return (year_trans)
}

