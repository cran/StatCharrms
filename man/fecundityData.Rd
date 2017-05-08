\name{fecundityData}
\alias{fecundityData}
\docType{data}
\title{
		Example fecundity data
}
\description{
	Example fecundity data taken in daily increments. The data set is used during the call of \code{\link{generateExamples}} 
	which is called from the [\bold{Examples}] button in the StatCharrms main window. 
}
\usage{data("fecundityData")}
\format{
  \describe{
    \item{\code{Date}}{The date the observation took place. }
    \item{\code{Treatment}}{The treatment level.}
    \item{\code{Rep}}{The replicate the observation belongs to.}
    \item{\code{Fecundity}}{The number of counted eggs for the pair.}
    \item{\code{Generation}}{The generation of the fish.}
  }
}

\examples{
	data(eventTimeData)
	str(eventTimeData)	
}


\references{
		TBD
}

\keyword{datasets}
