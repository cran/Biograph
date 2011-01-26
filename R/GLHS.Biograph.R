GLHS.Biograph <-
function ()
# Create GLHS from rrdat1.RData
# GLHS.RData created by GLHS.txt_to_R.R
{
z <- file.exists("GLHS.RData")
if (!z) 
   { print ("GLHS.RData does not exist in current working directory",quote=FALSE)
   	return
   }
nsample <- length(unique(rrdat[,1]))
maxns <- 15
path <- array(" ",c(nsample))        # character string vector
irec <- c(1:nsample)
idim <- vector(mode="numeric",length=nsample)
idim <- rep(1,nsample)     
ID2 <- vector(mode="numeric",length=nsample)   
ID2 <- subset(rrdat[,1],diff(rrdat[,1]) > 0) # ID  
born <- vector(mode="numeric",length=nsample)
born <- subset(rrdat[,7],diff(rrdat[,1]) > 0) # TB
start <-subset(rrdat[,7],diff(rrdat[,1]) > 0)# TB
end <-subset(rrdat[,6],diff(rrdat[,1])   > 0)  # TI
end <- end + rep(1,nsample)
namcmc <- paste("cmc",1:maxns,sep="")
cmc <- matrix(NA,nrow=nsample,ncol=maxns,dimnames=list(c(1:nsample),namcmc))
ns <- vector(mode="numeric",length=nsample)  # number of states occupied before censoring = events - 1
jj <- 1
path[jj] <- "NJ"
cmc[jj,1] <- rrdat[1,3]  
ns[1] <- 1
if (rrdat[1,4] < rrdat[1,6])   # TF < TI
   { ns[1] <- ns[1] + 1 
     cmc[jj,ns[1]] <- rrdat[1,4] + 1 
     path[jj] <- paste (path[jj],"J",sep="")
   } 
for (i in 2:nrow(rrdat))  
 {  if (rrdat[i,3] == rrdat[i,8])    # first job
     {jj <- jj + 1
      ns[jj] <- 1
      cmc[jj,ns[jj]] <- rrdat[i,3]
      path[jj] <- "NJ"
      if (rrdat[i,4] < rrdat[i,6]) 
           { ns[jj] <- ns[jj] + 1 
             cmc[jj,ns[jj]] <- rrdat[i,4] + 1 
           } 
  } else {if (rrdat[i,3] - rrdat[i-1,4] > 1)   # job episode at least one month
       {path[jj] <- paste (path[jj],"N",sep="")
        ns[jj] <- ns[jj] + 1
        cmc[jj,ns[jj]] <- rrdat[i,3]
        path[jj] <- paste (path[jj],"J",sep="")
       }  else    path[jj] <- paste (path[jj],"J",sep="")
  if (rrdat[i,4] < rrdat[i,6])    # TF < TI
     {ns[jj] <- ns[jj] + 1 
      cmc[jj,ns[jj]] <- rrdat[i,4] + 1 
     }   
       }
 }
nsmax <- max(ns)         # max number of events
for (jj in 1:nsample) 
    { ns[jj] <- ns[jj] + 1 # number of states occupied
      if (nchar(path[jj]) < ns[jj]) path[jj] <- paste (path[jj],"N",sep="")
    }
namcov <- c("cov1","cov2","cov3","cov4")
cov1 <- subset(rrdat[,5],diff(rrdat[,1]) > 0) # Sex  (for different IDs)
# cov1 <- factor(cov1,levels=c(1,2),labels=c("Males","Females"))
# removed on 1  Aug 2010 because of problems (see survival.main.GLHS.r)
cov2 <- subset(rrdat[,12],diff(rrdat[,1]) > 0) # EDU
cov3 <- subset(rrdat[,9],diff(rrdat[,1])  > 0) # CMC at marriage
cov4 <- cmc[,1]
cohortbreaks <- c(0,468,504,3000)
cohortnames <- c("1929-31","1939-41","1949-51") 
cohort <- cut(born,breaks=cohortbreaks,labels=cohortnames,include.lowest=TRUE )

GLHS <- data.frame (ID2,born,start,end,cov1,cov2,cov3,cov4,cohort,idim,ns,path,cmc[,1:nsmax])  
if (!is.character(GLHS$path)) GLHS$path <- as.character(GLHS$path)
namcov <- c("sex","edu","marriage","TE","cohort")
colnames(GLHS) <- c("ID","born","start","end",namcov,"idim","ns","path",paste("Tr",1:nsmax,sep=""))
nsmax2 <- locpath(GLHS) + nsmax
GLHS$sex <- as.factor(GLHS$sex)
GLHS$edu <- as.numeric(GLHS$edu)
GLHS$start <- as.numeric(GLHS$start)
GLHS$born <- as.numeric(GLHS$born)
GLHS$marriage <- as.numeric(GLHS$marriage)
z <- Parameters (GLHS)
attr(GLHS,"trans") <- z$tmat
save (GLHS,file="GLHS.RData")

return (GLHS)
}

