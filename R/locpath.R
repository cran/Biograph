locpath <-
function(survey) 
  {loc <- which(survey[1,] == survey[1,"path"],arr.ind=TRUE)[2]
  return (loc)}

