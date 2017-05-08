\name{checkTime}
\alias{checkTime}

\title{
	Checks the format the date is in
}
\description{
	Checks the date to see if it is in the selected format.     
}
\usage{
checkTime(TimeTemp)
}

\arguments{
  \item{TimeTemp}{
	A vector of time converted to numerics by \code{\link{date2Numeric}}.
}
}


\value{
	Returns FALSE if all of the times were not in the correct format. Returns TRUE if \bold{any} of the times where in the correct format.   
}

\author{
	Joe Swintek
}

