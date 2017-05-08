\name{Run.StatCharrms}
\alias{Run.StatCharrms}

\title{
	Main function call for StatCharrms
}
\description{
	Call this function to start StatCharrms.
}
\usage{
Run.StatCharrms()
}

\details{
	This is the GUI front end for (StatCharrms).  
}

\value{
\item{\bold{Read Me Button}				}{Calls \code{\link{ReadMeFile}} which brings up the read me file.}
\item{\bold{Examples Button}				}{Calls \code{\link{generateExamples}}, which populates a folder with example datasets and analyses.}
\item{\bold{Histology Analysis Button}		}{Calls \code{\link{Histopath}} which will run RSCABS.}
\item{\bold{Time to Event Analysis Button}}{Calls \code{\link{Time2EventMain}} which allows for a time to event analysis.  }
\item{\bold{Analysis of Other End Points Button}}{Calls \code{\link{StdEndMain}} which allows for statistical tests on numerical endpoints. }
\item{\bold{Quit Button}		}{Closes StatCharrms.}
}

\references{
	use \code{\link{Reference}} to see all the references.
}
\author{
	Joe Swintek
}



\keyword{GUI Main}

