Biograph.msm <-
function(survey) 
{# remove records where start > end
survey <- subset (survey,survey$start <= survey$end)
z2 <- Parameters(survey)
attr(survey,"trans") <- z2$tmat
locpat <- locpath(survey)
tmat <- attr(survey,"trans")
z88 <- ifelse (timeunit=="month",12,1)
print (". . . . .  running reshape  . . . . . ",quote=FALSE)
nn <- ncol(survey)

survey$start2 <- survey$start
zx <- reshape(survey, idvar = "ID", varying = list(c(3, 
        (locpat + 1):nn, 4)), v.names = "date", direction = "long", 
        drop = NULL)
print (" . . .  . Adjusting long format  . . . . ",quote=FALSE)
zx2 <- zx[!is.na(zx$date),]
D <- zx2[do.call(order,list(zx2$ID,zx2$date)),] # sort by 2 variables
D$age <- (D$date-D$born)/z88
D$time <- ifelse (D$time==max(D$time),D$ns+1,D$time)   # time = line number of episode in trajectory (first= from birth; last = open to censored)
D$OR <- ifelse (D$date==D$start2,"B",ifelse (D$time >D$ns, substr(D$path,D$ns,D$ns), substr(D$path,(D$time-1),(D$time-1))))  
D$DES <-   ifelse (D$time > D$ns, "cens",substr(D$path,(D$time),(D$time)))
D$trans <- apply(D,1,function (x) {ifelse (x[ncovariates+9+2]=="cens", 
           grep(x[ncovariates+9+1],namstates),
           tmat[grep(x[ncovariates+9+1],namstates),grep(x[ncovariates+9+2],namstates)])})
D$firstobs <- ifelse (is.na(D$trans),1,0)
D$stateN <- ifelse (D$DES=="cens",D$OR,D$DES)
D$state <- match (D$stateN,namstates)
attr(D,"trans") <- tmat
return (D)
}

