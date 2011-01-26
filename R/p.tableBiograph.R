p.tableBiograph <-
function (table,filename,w,append)
{ zcolnam <- format (colnames(table),width=w,justity="right")
  niter <- ifelse (!is.na(dim(table)[3]),dim(table)[3],1)
  #unlink("dtest")
  for (iter in 1:niter)
  {  case5 <- ifelse (iter==1 & append==FALSE,"FALSE","TRUE")
     if (niter > 1) z <- format(table[,,iter],width=w,justity="right") else
       z <- format(table,width=w,justity="right") 
     zy <- format(rownames(table),width=6)
     write.table(z, file = filename, sep = "\t", qmethod = "double",col.names=NA,quote=FALSE,append=case5)
     cat (" \n ",file=filename,append=TRUE)
  }
}

