\name{Time2EventMain}
\alias{Time2EventMain}


\title{
	GUI for time to event analysis
}
\description{
	Main function call that produces the GUI for a time to event analysis.   
}
\usage{
Time2EventMain()
}

\details{
	Used as the GUI front-end to perform the time to event analysis as outline in the LAGDA guidelines. 
}
\value{
 \item{\bold{Load Data} Button}{Load a data set from an csv file.}
 \item{\bold{Specify Data} Button}{Calls \code{\link{addSpec.te}}.  This will produce the widgets needed to specify the data.}
 \item{\bold{Run Analysis} Button}{Calls \code{\link{analyseTime2Effect}} and performs the time to event
	analysis. After which call the \code{\link{buildResultsWindow.te}} 
	function which displays the results from the analysis.}
}
\references{
	OECD, 2015, OECD Guidelines for the Testing of Chemicals, Section 2. Test No. 241: The Larval Amphibian Growth and Development Assay (LAGDA) 
 OECD Publishing, Paris. DOI:http://dx.doi.org/10.1787/9789264242340-en
}
\author{
	Joe Swintek
}

\keyword{GUI Main}

