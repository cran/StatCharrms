\name{lengthWeightData}
\alias{lengthWeightData}
\docType{data}
\title{
  Example length and weight data
}
\description{
	Example length and weight data used in StatCharrms when \code{\link{generateExamples}} is 
	called from the [\bold{Examples}] button in the StatCharrms main window.
}
\usage{data("lengthWeightData")}
\format{

  \describe{
    \item{\code{Generation}}{Generation of the fish.}
    \item{\code{Treatment}}{Treatment level for an observation.}
    \item{\code{Replicate}}{Replicate the observation belongs to.}
    \item{\code{Number}}{The row number in the dataset.}
    \item{\code{SEX}}{The gender of the fish. }
    \item{\code{WEIGHT}}{The weight of the fish.}
    \item{\code{LENGTH}}{The length of the fish.}
    \item{\code{Concentration}}{The concentration of the chemical the fish were exposed to.}
    \item{\code{Age}}{The age of the fish.}
  }
}
\examples{
	data(lengthWeightData)
	str(lengthWeightData)	
}

\references{
		TBA
}

\keyword{datasets}
