\name{eventTimeData}
\alias{eventTimeData}
\docType{data}
\title{
	Time to event data
}
\description{
	An anonymized data set used in the example analysis for time to event data. 
	The data set is used during the call of \code{\link{generateExamples}} 
	which is called from the [\bold{Examples}] button in the StatCharrms main window. 
}
\usage{data("eventTimeData")}
\format{
  A data frame with 240 observations on the following 4 variables.
  \describe{
    \item{\code{Time}}{Time as a numeric vector}
    \item{\code{Status}}{The status variable; 0 for a censored event, 1 for the the measured event.}
    \item{\code{Trt}}{The treatment level.}
    \item{\code{Rep}}{The replicate variable.}
  }
}

\examples{
	data(eventTimeData)
	str(eventTimeData)	
}

\keyword{datasets}
