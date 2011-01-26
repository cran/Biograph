statesequence.ind <-
function(survey)
{ survey$path <- as.character(survey$path)
  nsample<-length(survey$path)
  maxns <- max(nchar(survey$path))
  seq.ind <- array(0,c(nsample,maxns))
  # --------- Determine OR and DE and generate flow table ------------------------------------
  str_char <- array(" ",c(maxns))
  for (i in 1:nsample)
   { str_char <- stringf(survey$path[i])
        for (k in 1:(survey$ns[i]))
        { seq.ind[i,k] <- grep(str_char[k],namstates)
        }
   }
  dimnames (seq.ind) <- list(ID=survey$ID,Transition=c(1:ncol(seq.ind)))
  return (seq.ind)
}

