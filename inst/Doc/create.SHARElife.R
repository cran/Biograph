rm(list=ls())
library(foreign)
# ---------------   Read data  ----------------
# ------------  PLEASE CHANGE THE PATHS -----------------
d.st <- data.frame(read.dta ("/Users/franswillekens/Documents/DATA/SHARE/Wave3Stata/sharew3_rel1_st.dta",convert.dates=TRUE,convert.underscore=TRUE))
d.rp <- data.frame(read.dta ("/Users/franswillekens/Documents/DATA/SHARE/Wave3Stata/sharew3_rel1_rp.dta",convert.dates=TRUE,convert.underscore=TRUE))
d.ac <- data.frame(read.dta ("/Users/franswillekens/Documents/DATA/SHARE/Wave3Stata/sharew3_rel1_ac.dta",convert.dates=TRUE,convert.underscore=TRUE))
d.re <- data.frame(read.dta ("/Users/franswillekens/Documents/DATA/SHARE/Wave3Stata/sharew3_rel1_re.dta",convert.dates=TRUE,convert.underscore=TRUE)) # retro employment + educ
d.rc <- data.frame(read.dta ("/Users/franswillekens/Documents/DATA/SHARE/Wave3Stata/sharew3_rel1_rc.dta",convert.dates=TRUE,convert.underscore=TRUE)) # retro children

# ---------------   Dates of birth and interview  -----------
nsample <- nrow(d.st)
IDc <- d.st$mergeid
Yborn <- d.st$sl.st007   # Year of birth 
Mborn <- d.st$sl.st006   # Month of birth
Mb <- unclass(Mborn)-2
Mb[Mb<=0] <- 6  # unknown or refused month of birth: month 6
Yborn <- ifelse (Yborn<0,NA,Yborn)
bb <- round(Yborn+(Mb-1)/12,3)  # CMC 
end <- 2008.5                   #date of interview

# --------------   Dates of transitions   ------------------
# Year of onset of n-th relationship
p.1.mstart <- d.rp$sl.rp004c.1 # when relationship started
p.2.mstart <- d.rp$sl.rp004c.2 # when relationship started
p.3.mstart <- d.rp$sl.rp004c.3 # when relationship started
p.4.mstart <- d.rp$sl.rp004c.4 # when relationship started
p.5.mstart <- d.rp$sl.rp004c.5 # when relationship started
p.6.mstart <- d.rp$sl.rp004c.6 # when relationship started
p.1.mstart[p.1.mstart<0] <- NA
p.2.mstart[p.2.mstart<0] <- NA
p.3.mstart[p.3.mstart<0] <- NA
p.4.mstart[p.4.mstart<0] <- NA
p.5.mstart[p.5.mstart<0] <- NA
p.6.mstart[p.6.mstart<0] <- NA

# Year of n-th marriage
nmarriages <- d.rp$sl.rp002e. # how often married
d.rp$sl.rp002e.[d.rp$sl.rp002e.==1955] <- 1 # is NL-615022-02
p.1.marriedY <- d.rp$sl.rp008.1 # year married
p.1.marriedY[p.1.marriedY==1900] <- 1957 # one male married in 1900
p.2.marriedY <- d.rp$sl.rp008.2 # year married
p.3.marriedY <- d.rp$sl.rp008.3 # year married
p.4.marriedY <- d.rp$sl.rp008.4 # year married
p.5.marriedY <- d.rp$sl.rp008.5 # year married
p.6.marriedY <- d.rp$sl.rp008.6 # year married
p.1.marriedY[p.1.marriedY<0] <- NA
p.2.marriedY[p.2.marriedY<0] <- NA
p.3.marriedY[p.3.marriedY<0] <- NA
p.4.marriedY[p.4.marriedY<0] <- NA
p.5.marriedY[p.5.marriedY<0] <- NA
p.6.marriedY[p.6.marriedY<0] <- NA

# year started living with partner who was later married (9997= never)
#    = cohabitation related to a marriage
d.rp$sl.rp004b.1[d.rp$mergeid=="FR-457171-01"] <- 1971 # was 9171 record 14020:  d.rp[!is.na(p.1.comY) & p.1.comY==9171,]
p.1.comY <- d.rp$sl.rp004b.1  
p.2.comY <- d.rp$sl.rp004b.2
p.3.comY <- d.rp$sl.rp004b.3 
p.4.comY <- d.rp$sl.rp004b.4 
p.5.comY <- d.rp$sl.rp004b.5 
p.6.comY <- d.rp$sl.rp004b.6
p.1.comY[p.1.comY<0 | p.1.comY ==9997] <- NA # 9997 = never cohabited
p.2.comY[p.2.comY<0 | p.2.comY ==9997] <- NA 
p.3.comY[p.3.comY<0 | p.3.comY ==9997] <- NA 
p.4.comY[p.4.comY<0 | p.4.comY ==9997] <- NA 
p.5.comY[p.5.comY<0 | p.5.comY ==9997] <- NA 
p.6.comY[p.6.comY<0 | p.6.comY ==9997] <- NA 
p.1.comnow <- d.rp$sl.rp009.1
p.2.comnow <- d.rp$sl.rp009.2
p.3.comnow <- d.rp$sl.rp009.3
p.4.comnow <- d.rp$sl.rp009.4
p.5.comnow <- d.rp$sl.rp009.5
p.6.comnow <- d.rp$sl.rp009.6
p.1.comnow[p.1.comnow=="refusal"|p.1.comnow=="don't know"] <- NA
p.2.comnow[p.2.comnow=="refusal"|p.2.comnow=="don't know"] <- NA
p.3.comnow[p.3.comnow=="refusal"|p.3.comnow=="don't know"] <- NA
p.4.comnow[p.4.comnow=="refusal"|p.4.comnow=="don't know"] <- NA
p.5.comnow[p.5.comnow=="refusal"|p.5.comnow=="don't know"] <- NA
p.6.comnow[p.6.comnow=="refusal"|p.6.comnow=="don't know"] <- NA
p.1.comnow.why <- d.rp$sl.rp010.1 # reason of end relationship
p.2.comnow.why <- d.rp$sl.rp010.2
p.3.comnow.why <- d.rp$sl.rp010.3
p.4.comnow.why <- d.rp$sl.rp010.4
# cohabitation ended
p.1.comendY <- d.rp$sl.rp012.1  # year in which cohabitation ended
p.2.comendY <- d.rp$sl.rp012.2
p.3.comendY <- d.rp$sl.rp012.3
p.4.comendY <- d.rp$sl.rp012.4
p.1.comendY[p.1.comendY<0] <- NA
p.2.comendY[p.2.comendY<0] <- NA
p.3.comendY[p.3.comendY<0] <- NA
p.4.comendY[p.4.comendY<0] <- NA
# Divorce
p.1.divorced <- d.rp$sl.rp013.1 # divorced Yes No
p.2.divorced <- d.rp$sl.rp013.2 # divorced Yes No
p.3.divorced <- d.rp$sl.rp013.3 # divorced Yes No
p.4.divorced <- d.rp$sl.rp013.4 # divorced Yes No
p.1.divorcedY <- d.rp$sl.rp014.1  # year of divorce
p.2.divorcedY <- d.rp$sl.rp014.2 
p.3.divorcedY <- d.rp$sl.rp014.3
p.4.divorcedY <- d.rp$sl.rp014.4
p.1.divorcedY[p.1.divorcedY<0] <- NA
p.2.divorcedY[p.2.divorcedY<0] <- NA
p.3.divorcedY[p.3.divorcedY<0] <- NA
p.4.divorcedY[p.4.divorcedY<0] <- NA

# ------  cohabitation NOT related to a marriage -----
#   Q: In which year did you start cohabiting (first cohabitation)?
# Note: other cohabitations: RP015a_
# if index >10
p.unmarried.ever <- d.rp$sl.rp002d# 1=yes; 5=no

p.11.cohabY <-d.rp$sl.rp003.11   # year starting cohabitation
                                 #  index from 11 to 18
p.12.cohabY <-d.rp$sl.rp003.12
p.13.cohabY <-d.rp$sl.rp003.13
p.14.cohabY <-d.rp$sl.rp003.14
p.15.cohabY <-d.rp$sl.rp003.15
p.16.cohabY <-d.rp$sl.rp003.16
p.17.cohabY <-d.rp$sl.rp003.17
p.18.cohabY <-d.rp$sl.rp003.18
p.11.cohabY[p.11.cohabY<0] <- NA
p.12.cohabY[p.12.cohabY<0] <- NA
p.13.cohabY[p.13.cohabY<0] <- NA
p.14.cohabY[p.14.cohabY<0] <- NA
p.15.cohabY[p.15.cohabY<0] <- NA
p.16.cohabY[p.16.cohabY<0] <- NA
p.17.cohabY[p.17.cohabY<0] <- NA
p.18.cohabY[p.18.cohabY<0] <- NA
 # when relationship started?
p.11.cohabsY <- d.rp$sl.rp004c.11
p.12.cohabsY <- d.rp$sl.rp004c.12
p.13.cohabsY <- d.rp$sl.rp004c.13
p.14.cohabsY <- d.rp$sl.rp004c.14
p.15.cohabsY <- d.rp$sl.rp004c.15
p.16.cohabsY <- d.rp$sl.rp004c.16
p.17.cohabsY <- d.rp$sl.rp004c.17
p.18.cohabsY <- d.rp$sl.rp004c.18
p.11.cohabsY[p.11.cohabsY<0] <- NA
p.12.cohabsY[p.12.cohabsY<0] <- NA
p.13.cohabsY[p.13.cohabsY<0] <- NA
p.14.cohabsY[p.14.cohabsY<0] <- NA
p.15.cohabsY[p.15.cohabsY<0] <- NA
p.16.cohabsY[p.16.cohabsY<0] <- NA
p.17.cohabsY[p.17.cohabsY<0] <- NA
p.18.cohabsY[p.18.cohabsY<0] <- NA
 # year in which cohabitation ended
p.11.comendY <- d.rp$sl.rp012.11 
p.12.comendY <- d.rp$sl.rp012.12
p.13.comendY <- d.rp$sl.rp012.13
p.14.comendY <- d.rp$sl.rp012.14
p.15.comendY <- d.rp$sl.rp012.15
p.16.comendY <- d.rp$sl.rp012.16
p.17.comendY <- d.rp$sl.rp012.17
p.18.comendY <- d.rp$sl.rp012.18
p.11.comendY[p.11.comendY<0] <- NA
p.12.comendY[p.12.comendY<0] <- NA
p.13.comendY[p.13.comendY<0] <- NA
p.14.comendY[p.14.comendY<0] <- NA
p.15.comendY[p.15.comendY<0] <- NA
p.16.comendY[p.16.comendY<0] <- NA
p.17.comendY[p.17.comendY<0] <- NA
p.18.comendY[p.18.comendY<0] <- NA
# LAT relation: RP016
LAT.1.start <- d.rp$sl.rp017.
LAT.1.end <- d.rp$sl.rp020

#Leaving parental home: start living independently
leavehome <- d.ac$sl.ac003. #9997 = nooit
leavehome[leavehome<0] <- NA
leavehome[leavehome==9997 | leavehome==2975] <- NA

leavehome <- ifelse (!is.na(p.1.marriedY) & leavehome==p.1.marriedY,NA,leavehome)
p.1.mstart <- ifelse (!is.na(p.1.marriedY) & p.1.mstart==p.1.marriedY,NA,p.1.mstart)
p.1.comY <-   ifelse (!is.na(p.1.marriedY) & p.1.comY==  p.1.marriedY,NA,p.1.comY)
p.2.mstart <- ifelse (!is.na(p.2.marriedY) & p.2.mstart==p.2.marriedY,NA,p.2.mstart)
p.2.comY <-   ifelse (!is.na(p.2.marriedY) & p.2.comY==  p.2.marriedY,NA,p.2.comY)
p.3.mstart <- ifelse (!is.na(p.3.marriedY) & p.3.mstart==p.3.marriedY,NA,p.3.mstart)
p.3.comY <-   ifelse (!is.na(p.3.marriedY) & p.3.comY==  p.3.marriedY,NA,p.3.comY)
p.4.mstart <- ifelse (!is.na(p.4.marriedY) & p.4.mstart==p.4.marriedY,NA,p.4.mstart)
p.4.comY <-   ifelse (!is.na(p.4.marriedY) & p.4.comY==  p.4.marriedY,NA,p.4.comY)
p.5.mstart <- ifelse (!is.na(p.5.marriedY) & p.5.mstart==p.5.marriedY,NA,p.5.mstart)
p.5.comY <-   ifelse (!is.na(p.5.marriedY) & p.5.comY==  p.5.marriedY,NA,p.5.comY)
p.6.mstart <- ifelse (!is.na(p.6.marriedY) & p.6.mstart==p.6.marriedY,NA,p.6.mstart)
p.6.comY <-   ifelse (!is.na(p.6.marriedY) & p.6.comY==  p.6.marriedY,NA,p.6.comY)
p.1.comendY <- ifelse (!is.na(p.1.divorcedY) & p.1.comendY==p.1.divorcedY,NA,p.1.comendY)
p.2.comendY <- ifelse (!is.na(p.2.divorcedY) &p.2.comendY==p.2.divorcedY,NA,p.2.comendY)
p.3.comendY <- ifelse (!is.na(p.3.divorcedY) &p.3.comendY==p.3.divorcedY,NA,p.3.comendY)
p.4.comendY <- ifelse (!is.na(p.4.divorcedY) &p.4.comendY==p.4.divorcedY,NA,p.4.comendY)

# Create an array of transition dates
kk <- cbind(leavehome,
#p.1.mstart,p.2.mstart,p.3.mstart,p.4.mstart,p.5.mstart,p.6.mstart,
p.1.marriedY,p.2.marriedY,p.3.marriedY,p.4.marriedY,p.5.marriedY,p.6.marriedY,
p.1.comY,p.2.comY,p.3.comY,p.4.comY,p.5.comY,p.6.comY,
p.1.comendY,p.2.comendY,p.3.comendY,p.4.comendY,
p.1.divorcedY,p.2.divorcedY,p.3.divorcedY,p.4.divorcedY,
p.11.cohabY,p.12.cohabY,p.13.cohabY,p.14.cohabY,p.15.cohabY,p.16.cohabY,p.17.cohabY,p.18.cohabY,
#p.11.cohabsY,p.12.cohabsY,p.13.cohabsY,p.14.cohabsY,p.#15.cohabsY,p.16.cohabsY,p.17.cohabsY,p.18.cohabsY,
p.11.comendY,p.12.comendY,p.13.comendY,p.14.comendY,p.15.comendY,p.16.comendY,p.17.comendY,p.18.comendY)
# variable names in SHARELIFE data files: colnames(kk)
names.full <- colnames(kk) 
# Replace SHARELIFE variable names by state labels to be used in Biograph (single character)
#  State labels in case of 6 states
names6 <- c("A","M","M","M","M","M","M","C","C","C","C","C","C","A","A","A","A","A","A","A","A","c","c","c","c","c","c","c","c","a","a","a","a","a","a","a","a")
#   State labels in case of 4 states
names4 <- c("A","M","M","M","M","M","M","C","C","C","C","C","C","A","A","A","A","A","A","A","A","C","C","C","C","C","C","C","C","A","A","A","A","A","A","A","A")
zk <- kk
colnames(zk) <- names4
# zk contains the dates of transitions (unsorted)
# ------------  transitions in chronological order  ----------
print ("Transitions are arranged in chronological order. This may take minutes.")
f<- Sequences.ind.0(zk,namstates=c("H","A","C","M"))
# f$d contains the dates of transition (sorted) 
# f$sequences and f$path contain the state sequences
path <- as.character(f$path)

# ------------  covariates  -------------------
sex <- d.st$sl.st011.
# birt cohort
bcohort <- ifelse (bb<1930,1,ifelse(bb>=1930&bb<1940,2,ifelse(bb>=1940&bb<1950,3,4)))
bcohort <- factor(bcohort,levels=c(1:4),labels=c("<1930","1930-39","1940-49","1950+"))
table(bcohort,useNA="always")
# Year finished full-time education
edu.f <- d.re$sl.re002.
edu.f <- ifelse(edu.f<0 | edu.f==9000 | edu.f == 9997,NA,edu.f)
# Year in which (s)he started first job
job.1.start <- d.re$sl.re011.1
job.1.start <- ifelse (job.1.start<0,NA,job.1.start)
sex2 <- ifelse(sex=="male","male",ifelse(sex=="female","female",NA))
#number of children
nchildren <- d.rc$sl.rc023.
options(stringsAsFactors=FALSE)

# ------------  Create Biograph object "SHARE"   -------------
path <- as.character(path)
maxns <- max (nchar(path))
SHARE<- data.frame(ID=c(1:nsample),born=as.numeric(bb),start=as.numeric(bb),end=as.numeric(end),country=as.factor(d.st$country),IDc=IDc,cohort=bcohort,sex=as.factor(sex2),eduf=as.numeric(edu.f),job1=as.numeric(job.1.start),children=nchildren,path=path,f$d[,1:(maxns-1)])
locpat <- locpath(SHARE)
colnames(SHARE)[(locpat+1):ncol(SHARE)] <- paste("Tr",1:(ncol(SHARE)-locpat),sep="")
SHARE <- subset(SHARE,!is.na(SHARE$born))
attr(SHARE,"format.date") <- "year"
attr(SHARE,"format.born") <- "year"
require (Biograph)
param <- Parameters(SHARE)
attr(SHARE,"param") <- param
# 210 cases without date of birth. These are removed.
table(trunc(SHARE$born),useNA="always")
zzd <-  "/Users/franswillekens/Documents/R/0 0 MAC/Package/TEST.Biograph/SHARE/"
setwd(zzd)
save (SHARE,file="SHARE.RData")
# ===========   BIOGRAPH OBJECT COMPLETE   ==============
