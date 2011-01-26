MSLT.S.plot <-
function (S,e0,title)
 {# ======== Plot state occupancies  =============    
  # Graph the state probabilities
   namrates <- "Age-cohort rates"
   namst <- namstates
   for (i in 1:numstates)
     { namst[i] <- paste(namstates[i]," (e0=",round(e0[i,1],2),")",sep="")
     }
   title_sub <- paste("Life table (",namrates,")",sep="")
  StackGraph(S[,1:numstates,1],xlabel="Age",ylabel="State probability",xlegend="topright",
  ylegend="topright",title,title_sub,namst)
  abline(h=0.5,lty=2,col="darkgrey")
  }

