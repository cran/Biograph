pos.char <-
function(string,char)
{  zz <- stringf(as.character(string))
   if (char%in%zz)
     { z <- grep(char,zz)}  else
       { z <- NA }
    z1<- z[1] # returns first occurrence only
   return (z1)
}

