\name{transformationWarning}
\alias{transformationWarning}

\title{
	Gives warnings about a transformation
}
\description{
	An internal function that warns the user when a transformation can not be done on a set of numbers.
}
\usage{
transformationWarning(Data, Trans, VecName)
}

\arguments{
  \item{Data			}{
	A data set.
}
  \item{Trans			}{
	The transformation being tested. Gives warning for the values 
	of: c('Log', Log1', Square_Root', 'Arcsin').
}
  \item{VecName			}{
	The name of the variable to be transformed. 
}
}
\details{
	This function is called by \code{\link{responseTransform}}. 
}
\value{
	Will display a warning message to the user, when appropriate.  
}

\author{
	Joe Swintek
}

