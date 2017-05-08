\name{addSpec.te}
\alias{addSpec.te}

\title{
	Add the specification widgets for time to event analysis
}
\description{
	This handles all the GUI widgets needed to specify time to event data.  
}
\usage{
addSpec.te()
}


\value{

  \item{\bold{Treatment}			}{Used to specify the name of the treatment variable.  This must be selected for any analysis to run. }
  \item{\bold{Replicate}			}{Used to specify the name of the replicate variable.}
  \item{\bold{Gender Variable}			}{Used to specify the name of the gender variable.}
\item{\bold{Gender Value}			}{The gender the selected analysis is performed on.}
\item{\bold{Generation Variable}			}{Used to specify the name of the generation variable.}
\item{\bold{Generation Value}			}{The generation the selected analysis is performed on.}
\item{\bold{Time Variable}			}{Used to specify the name of the time variable. This must be selected for any analysis to run.}
\item{\bold{Status Variable}			}{Used to specify the name of the event status variable. This must be selected for any analysis to run.}
\item{\bold{Event Value}			}{Used to specify the name of the value used to signify the event the analysis is for. This must be selected for any analysis to run.}
\item{\bold{Censored Value} 	}{Used to specify the name of the value used to signify a censored event.}

}


\author{
	Joe Swintek
}


\seealso{
\code{\link{Time2EventMain}} and \code{\link{analyseTime2Effect}}
}


\keyword{GUI Specification}

