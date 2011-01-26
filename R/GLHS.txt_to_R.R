GLHS.txt_to_R <-
function (path,filename)
# called in Chapter 4
{
# =========================================================
# ==== Reads rrdat1.txt and creates the R data file =======
# ====   rrdat1.RData in the current working directory ====
# =========================================================

# Check existence
file2 <- paste(path,"/",filename,sep="")
z<- file.exists(file2)  # for info: ?file
                        #  see also  file.access("rrdat.txt")
if (!z) 
  {print (paste("ERROR in GLHS.txt_to_R : ",file2," does not exist.",sep=""),quote=FALSE)
   return
  }
# Read the data
rrdat <- as.matrix(read.table(file=file2),header=FALSE)
colnames(rrdat) <- c("ID","NOJ","TS","TF","SEX","TI","TB","TE","TM","PRES","PRES1","EDU")
rownames(rrdat) <-c(1:nrow(rrdat))
# save the data in original format
fileout <- "rrdat1.RData"
save (rrdat,file=fileout)
print (file2)
print (paste ("   saved as ",fileout," in the current working directory: ",getwd(),sep=""),quote=FALSE)
return (rrdat)
}

