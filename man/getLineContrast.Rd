\name{getLineContrast}
\alias{getLineContrast}
\alias{getQuadContrast}

\title{
	Generate contrasts
}
\description{
	Generates contrasts for testing monotonicity based on the OECD guidance for statistics.  
}
\usage{
getLineContrast(Data, Treatment)
getQuadContrast(Data, Treatment)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{Treatment}{
	The name (as a string) of the treatment variable. 
}
}

\value{
	Returns a numeric vector of contrasts. 
}

\references{
	OECD SERIES ON TESTING AND ASSESSMENT Number 54
}
\author{
	Joe Swintek
}

