MSLT.S <-
function (cumrates)
{ require (msm)
  S<- apply(cumrates,1,function(x) 
	  MatrixExp(x,t=1,n=5,k=3,method="series"))
  # S has the transition probabilities from each origin state to the different
  # destination states in conseciutive rows.
  # Ages are in columns
  # The following code rearranges the probabilities 
  zz <- array (S,c(numstates,numstates,nrow(cumrates)))
  S2 <- aperm(zz,c(3,1,2))
 dimnames(S2) <- dimnames(cumrates)
  return(S2)
}

