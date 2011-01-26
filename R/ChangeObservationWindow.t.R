ChangeObservationWindow.t <-
function (survey,starttime,endtime)
{ # see Parameters: time_in_month_or_year
   if (!exists("timeunit"))
     { print ("ChangeObservationWindow.t: Biograph first runs Parameters . . . ",quote=FALSE)
       z <- Parameters(survey)
     }
  tunit <- ifelse (timeunit == "month",12,ifelse (timeunit=="year",1,NA))
  survey2 <- survey
  za <- rep(starttime,nrow(survey2))
  zb <- rep(endtime,nrow(survey2))
  survey2$start <- ifelse (survey2$start < za, za,survey2$start)
  survey2$start <- ifelse (survey2$start < zb, survey2$start,NA)
  survey2$end <- ifelse (survey2$end > zb,zb,survey2$end)
  survey2$end <- ifelse (survey2$end > za,survey2$end,NA)
  survey2 <- subset (survey2,!is.na(survey2$start)&!is.na(survey2$end))
  survey2$marriage <- ifelse (survey2$marriage >= za & survey2$marriage <= zb,survey2$marriage,0)
  for (i in 1:nrow(survey2))
  { # state occupied at starttime
    if (survey2$ns[i] >1)
    {zx <- c(survey$start[i],survey2[i,(locpath(survey)+1):(locpath(survey)+survey2$ns[i]-1)],survey$end[i])
     zx <- unlist (zx)
     zy <- ifelse (zx >= rep(starttime,survey2$ns[i]+1) & zx <= rep(endtime,survey2$ns[i]+1),zx,0)
     # new transition dates
     survey2[i,(locpath(survey)+1):ncol(survey2)] <- NA
     if (sum(zy[zy>0]) > 0) # At least one transition in observation window
      { survey2[i,(locpath(survey)+1):(locpath(survey)+length(zy[zy>0]))] <- zy[zy>0]
      }
 ## Determine state occupied at onset of new observation window
      ii <- ifelse (starttime > zx[1], min(which(starttime-zx>0),na.rm=TRUE),NA) # starttime after birth
      if (is.na(ii)) ii <- 1 
      state1 <- substr(survey$path[i],ii,ii)
      pathn <- substr(survey$path[i],ii,ii+length(zy[zy>0]))
#     zpa <- zp                                                                                     
#     if (min(which(zx >= starttime),na.rm=TRUE) > 2) zpa[min(which(zpa==1),na.rm=TRUE)-1] <- 1
#     if (max(which(zpa==1),na.rm=TRUE) < survey2$ns[i]+1) zpa[max(which(zpa==1),na.rm=TRUE)+1] <- 1
  #   pathn <- substr(survey2$path[i],min(which(zpa==1),na.rm=TRUE),max(which(zpa==1),na.rm=TRUE)-1)
     survey2$path[i] <- pathn
     survey2$ns[i] <- nchar(pathn)
   #  survey2[i,(locpath(survey)+1):(locpath(survey)+survey2$ns[i]-1)] <- zx[zp==1]
    }    
  }
  return (survey =survey2)
 }

