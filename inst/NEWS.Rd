\name{NEWS}
\title{NEWS file for the Biograph package}
\section{Changes in version 2.0.2 compared to version 1.*}{
  \itemize{
    \item Date conversion functions. In Biograph 1.0, calendar dates were in Century Month Code (CMC) or decimal year (calendar year and fraction of year). In Biograph 2.0, dates may be given as calendar date (year, month, day). Several function are added to convert one representation of date into another representation. The following representations are distinguished in Biograph 2.0.1 and higher: date of Gregorian calendar (year, month, day), Julian date (days since reference point in time), decimal year, CMC, and age. To compute the precise age in years, months and day in the Gregorian calendar, Biograph 2.0.2 distinguishes different classes of date objects (instant, interval, period and duration). The classes are implemented in functions of the lubridate package. These functions are used in Biograph 2.0. For details, see Willekens (2013) Chronological objects in demographic research. Submitted. 

    \item Occup . In Biograph 2.0.1 the function Occup is split in two functions: 
i.	state_age determines for each subject the state occupancy by age, and aggregates the result to produce (sample) population state occupancies by age. 
ii.	state_time determines for each individual state occupation times (sojourn times) by age and state and aggregates the result to produce (sample) population state occupation times by age.
The states displayed include the (a) the state space and (b) the fictitious states ‘not-yet-in-observation’  and ‘observation right censored’. The functions state_age and state_time are called by the function Occup. The state ‘not-yet-in-observation’ is dropped in objects returned by the function Occup.

    \item In Biograph object, the variables idem and ns are deleted. The variable idim denotes the domain of life. It has never been used (idim=1) and is not deleted. The variable ns (number of states a subject occupies during the observation period) can be determined from the path variable, e.g. nchar(GLHS$path)
The removal of idem and ns has consequences for several functions including Parameters, transitions, GLHS.Biograph, OverviewTransitions and OverviewEpisodes. The functions have been updated. 

    \item The graphics is improved using the ggplot2 package.

    \item A distinction is made between Biograph functions and utilities used by Biograph functions. Utilities are generic. The package includes about 20 utilities. 

    \ item ChangeObservationWindow.e  attr(Bdata3,"format.dat") <- attr(Bdata,"format.date")   replaced by attr(Bdata3,"format.date") <- attr(Bdata,"format.date")

    \item With the publication of Version 2.0, a utility is distributed to assist the production of a Biograph object (in the doc subdirectory of the package). The code that applies the utility to different data sets is published too.
    
    \item In Biograph 2.0.2, the date functions have been renamed: a . is replaced by _. The reason for renaming is the possible confusion with S3 methods in R that determine the dispatching methods based upon an attribute (class) of objects.
    
    \item In Version 2.0.2 global variables are not longer used. Instead, the attribute "param" is added to the Biograph object. The attribute consists of the object produced by the Parameters function. The attribute "trans"  is omitted because it is included in "param".
}}
