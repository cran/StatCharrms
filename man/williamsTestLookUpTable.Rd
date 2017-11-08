\name{williamsTestLookUpTable}
\alias{williamsTestLookUpTable}
\docType{data}
\title{
	Look up table for the critical values used in the Williams test. 
}
\description{
	This is the look up table for the critical values provided in Green (2018). This table is automaticly called durring the the exicution of \code{\link{williamsTest}} function.   
}
\usage{data("williamsTestLookUpTable")}
\format{
  \describe{
    \item{\code{df}}{The degree of freedom for the test.}
    \item{\code{Q}}{}
    \item{\code{B}}{}
  }
}

\examples{
	data(williamsTestLookUpTable)
	str(williamsTestLookUpTable)	
}


\references{
	 Green J., Springer T.,  Holbeck H.   \emph{Statistical Analysis of Ecotoxicology Data} (Wiley in press)
}
\keyword{datasets}
