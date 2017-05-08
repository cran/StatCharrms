\name{StdEndMain}
\alias{StdEndMain}

\title{
	GUI for Analysis of numerical endpoints
}
\description{
	Main function call used to produce the GUI for analysis of standard numerical endpoints. Example 
		numerical endpoints are length, weight, and fecundity.  
}
\usage{
StdEndMain()
}

\details{
	This is set up to follow the protocols for data analysis outlined in MEOGRT.  
}

\value{
 \item{\bold{Load Data} Button}{Load a data set from an csv file.}
 \item{\bold{Specify Data} Button}{Calls \code{\link{addMultiSpec.std}}.  This will produce a "Data specification" tab where the data can be specified.}
 \item{\bold{Run Analysis} Button}{Calls \code{\link{autoStdAnylsis}} or \code{\link{forceStdAnalysis}}.  Both functions will perform the analyses specified in the "Data specification" tab.
	After the analyses are ran the \code{\link{buildResultsWindow}} function will be called which will bring up a new window containing the results.}	
}

\references{
	OECD, 2015, Test No. 240: Medaka Extended One Generation Reproduction Test (MEOGRT), OECD	
	Publishing, Paris. DOI: http://dx.doi.org/10.1787/9789264242258-en
}
\author{
	Joe Swintek
}

\keyword{GUI Main}