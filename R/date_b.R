date_b <-
function (Bdata,format.in,selectday,format.out,covs)
#  e.g format.in = "CMC" and selectday = 1
{  # Determine position of path variable in Biograph object
#   if (is.null(attr(Bdata,"timeunit"))) 
#      {stop ("date.years: attribute timeunit missing from data. Please add attribute.") } else
#      {timeunit <- attr(Bdata,"timeunit")
#       if (timeunit=="year") 
#         {print ("Biograph message: Time unit is already year. Conversion skipped and no new object created.",quote=FALSE); return ()}}  
   if (missing(selectday)) selectday <- 1 
   if (missing(covs)) covs=NULL
   born <- Bdata$born
   Bdata$start <- date_convert(Bdata$start,format.in,selectday,format.out,born=born)
   Bdata$end   <- date_convert(Bdata$end,format.in,selectday,format.out,born=born)
 # Determine maximum number of transitions on Biograph object
   locpat <- locpath(Bdata)
   maxtrans <- ncol(Bdata)-locpat
   for (i in (locpat+1):(locpat+maxtrans))
    { Bdata[,i] <- date_convert(Bdata[,i],format.in,selectday,format.out,born=born) # MAKE SURE TO SET Bdata = GLHS
    }
    if (!is.null(covs)) 
    { zz <- Bdata[colnames(Bdata) %in% covs]
      for (j in 1:ncol(zz))
      { jj <- as.numeric(which(colnames(Bdata)==colnames(zz)[j]))
      	if (is.factor(Bdata[,jj])) stop ("The date-covariate is a factor variable, not a date.")
      	d <- as.numeric(Bdata[,jj])
      	d[d==0] <- NA
      	Bdata[,jj] <- date_convert (d=Bdata[,jj],format.in,selectday,format.out,born=born)
      }} 
      
  if (format.out!="age")  Bdata$born <- date_convert(Bdata$born,format.in,selectday,format.out=format.out,born=Bdata$born)
     #     Bdata$born=rep(0,nrow(Bdata)) else
    # assign("date_in_month_or_year", 2, env = .GlobalEnv)
     # assign("format.date","age", env = .GlobalEnv)
    attr(Bdata,"format.date") <- format.out
#    param <- Parameters (Bdata)
#    print (param)
#    attr(Bdata,"param") <- param
    return(date=Bdata)
    #    chron(z[10,2],out.format.in="d/m/y")
}
