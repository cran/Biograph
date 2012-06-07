Sequences.ind <-
function(path,namstatesnew)
{ if (missing(namstatesnew))  if (exists ("namstates")) namstatesnew <- namstates else stop ("Sequences.ind: namstates is missing")
  path <- as.character(path)
  nsample<-length(path)
  maxns <- max(nchar(path))
  path.num <- array(NA,c(nsample,maxns))
  str_char <- array(" ",c(maxns))
  for (i in 1:nsample)
   { str_char <- stringf(path[i])
        for (k in 1:length(str_char))
        { path.num[i,k] <- grep(str_char[k],namstatesnew)
        }
   }
  return (seq.ind=path.num)
}
