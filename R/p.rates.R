p.rates <-
function(pc_ac,rates,path,filename)
 { # Print rates
  curpath <- getwd()
  setwd(path)
 if (pc_ac==1) namrates <- "Period-cohort rates" else
                namrates <- "Age-cohort rates"

   cat (paste("\n Occurrence-exposure rates (M-matrix: ",namrates,")",sep="" ),file=filename)   
   cat (" \n ",file=filename,append=TRUE)
   p.tableBiograph (rates,filename,5,append=TRUE)
   setwd(curpath) 
}

