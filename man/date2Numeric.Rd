\name{date2Numeric}
\alias{date2Numeric}

\title{
	Convert a date into a number
}
\description{
	Converts a vector of dates into a vector of numbers, where the first date is considered to be at time 0.  
}
\usage{
date2Numeric(DateVec, Format)
}

\arguments{
  \item{DateVec}{
	A vector of dates. 
}
  \item{Format}{
	The R-coded format (\code{\link{as.Date}}) the dates are in.
}
}

\value{
	\item{DateVec}{A numeric vector representing the date.}
}

\author{
	Joe Swintek
}

\note{
	This always sets the earliest date to 0. 
}
