\name{cleanString}
\alias{cleanString}
	
\title{
	Cleans a string of illegal characters
}
\description{
	An internal function that converts any characters in a string that can not be in a windows file name to an underscore ("_").
}
\usage{
cleanString(String)
}

\arguments{
  \item{String}{
	A character string. 
}
}

\value{
	\item{String }{A character string where all of the illegal characters are converted to "_".}
}

\author{
	Joe Swintek
}
