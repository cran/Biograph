StateSpace <-
function (survey) 
{  d <- unique (as.character(survey$path))
   namstates <- array (NA,50)      # character vector
   numstates <- 0
     for (i in 1:length(d)) 
    { str_char <- stringf (d[i]) 
     for (j in 1:length(str_char))
    {     if (str_char[j] %in% namstates) test22 <- 1 else
           { numstates <- numstates + 1 ;  namstates[numstates] <- str_char[j] }
    }
    }                          #  end of loop i
    namstates <- subset(namstates,!is.na(namstates))
    # Determine absorbing states (always last state in path): state namstates[j] is an
    # aborbing state if that state is always the last state occupied
     absorbstates <- namstates
     for (j in 1:numstates)
      { for (i in 1:length(d))
          { if (namstates[j]%in%stringf(d[i]) & namstates[j] != substr(d[i],nchar(d[i]),nchar(d[i])))  
                    {absorbstates[j] <- NA; break}
          #  substr(d[i],nchar(d[i]),nchar(d[i]))  = last character of string d[i]
          }
      }
     absorbstates <- subset(absorbstates,!is.na(absorbstates))
     namstates <<- namstates
     numstates <<- numstates
     return (list(absorbstates = absorbstates) )
}

