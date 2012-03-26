
rm(list=ls())
library (Biograph)
# Define state space
namstates <- c("H","A","C","M")
# Specify subject identification numbers
id <- c(1,2,3)
# Dates of birth
born <- c("1986-04-05","1986-08-08","1986-11-28")
born <- as.Date(born) # converts character variable to Date variable
# Observation window
start <- born   
interview <- as.Date(rep("2019-05-09",3))
# Covariates
sex <- factor(c("F","M","F"))
educ <- factor(c("High","Medium","Medium"))

# Create a data frame of dates of transition
A <- c("2004-08-15","2011-09-15","2006-08-15")
C <- c("2011-12-15",NA,NA)
M <- c(NA,NA,"2012-03-16")
d <- data.frame(A=A,C=C,M=M,stringsAsFactors =FALSE)
# Convert character object d to object of class 'Date' (days elapsed since 1-1-1970 (Julian dates))
dd<- apply(d,2,function(x) y=as.Date(x))
dd <- data.frame(dd)  #  dd is numeric
dimnames(dd) <- dimnames(d)
# nsample = sample size
nsample <- nrow(d)
# Sort and rearrange the dates and obtain state sequence (path)
f <- Sequences.ind.0(dd,namstates)
# Convert Julian dates in calendar (Gregorian) dates
dates <- data.frame (f$d)
for (i in 1:3)
 {dates[,i] <- as.Date(dates[,i],origin="1970-01-01") 
 }
colnames(dates) <- paste ("Tr",1:(length(namstates)-1),sep="")
# State sequence (path)
path <- as.character(f$path)
# Number of states (ns)
ns <- f$ns
# Create Biograph object
bio  <- data.frame (ID=id,born=born,start=start,end=interview,sex=sex,educ=educ,idim=as.numeric(rep(1,length(id))),ns=as.numeric(ns),path=as.character(path),dates[,1:(max(ns)-1)],stringsAsFactors=FALSE)

attr(bio,"format.date") <- "%Y-%m-%d"
