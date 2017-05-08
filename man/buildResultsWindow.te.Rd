\name{buildResultsWindow.te}
\alias{buildResultsWindow.te}

\title{
	Display the results from time to event analyses
}
\description{
	This function builds a window that displays the results from a time to event analysis.
}
\usage{
buildResultsWindow.te(Results)
}

\arguments{
  \item{Results}{
	The results output from \code{\link{analyseTime2Effect}}.  
}
}

\value{
The window displays two objects; a 
 graph which is a kaplan-meier plot generated from the \code{\link{plot.survfit}} function and 
 Dunnett's table from the \code{\link[multcomp:glht]{glht}} function in the multcomp package. The Dunnett's table contains the 
 following items:

  \item{Comparison}{The levels of the treatment variable that are being compared.}
  \item{Estimate}{The estimated difference between the levels.}
  \item{Std. Error}{The standard error.}
  \item{z value}{The z value from the standard normal distribution.}
  \item{P Value}{The p-value corresponding to the z value.}
  \item{Sig}{The significance flag where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.} 

 The buttons on the window are:
  \item{\bold{Save Everything}}{Creates a folder where the graph is saved as a pdf. In addition, the data set and the Dunnett's table will also be contained in an HTLM file within the folder.  }
  \item{\bold{Save Results}			}{ Saves the data set used and the Dunnett's table as a HTML file.}
  \item{\bold{Save Graphs Only}}{Saves the graph as a pdf.}
  

}

\author{
	Joe Swintek
}


\keyword{GUI Results}

