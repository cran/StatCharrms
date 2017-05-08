\name{tranformTime}
\alias{tranformTime}

\title{
	Transforms the time variable   
}
\description{
	Transforms a variable that contains information about when an observation was taken into 
	a numeric vector, then averages the response variable across a specified interval of time. 
}
\usage{
tranformTime(Data, TimeVar, Time, RepVar, GroupVar, ResponVar)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{TimeVar}{
  The name (as a string) of the time variable.
}
  \item{Time}{
	The interval time the response is averaged across.
}
  \item{RepVar}{
	  The name (as a string) of the replicate variable.
}
  \item{GroupVar}{
	  The name (as a string) of the treatment variable.
}
  \item{ResponVar}{
	 The name (as a string) of response variable.
}
}
\details{
	This function just averages across time.
}

\value{
	Returns a data set where the time variable has been changed to a numeric time and the response variable has been averaged across time.  
}

\author{
	Joe Swintek
}

