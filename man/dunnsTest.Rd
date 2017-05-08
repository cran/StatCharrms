\name{dunnsTest}
\alias{dunnsTest}

\title{
	Perform a Dunns test
}
\description{
	This function performs a Dunns (non-parametric dunnett) test for multiple comparisons. 
	The code used in this function is a direct translation of the code in the SAS version of StatCharrms.
}
\usage{
dunnsTest(Data, Treatment, Response, TestDirection)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{Treatment}{
	The name (as a string) of the treatment variable. 
}
  \item{Response}{
	The name (as a string) of the response variable. 
}
  \item{TestDirection}{
	The direction the test statistic can be: c('Both', 'Descending', 'Ascending').
}
}

\value{
  Return a data frame containing: 
  \item{Treatment}{The level of the treatment variable.}
  \item{Count}{The Number of observation in that treatment.}
  \item{Rank}{The average rank of the response in the treatment level.}
  \item{Difference}{Difference in rank of the current level to the rank of the controls.}
  \item{P Value}{The p-value of the test statistic.}
  \item{Signif}{The significance flag where"." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.} 
}


\author{
	Joe Swintek
}

\references{
Dunn, O. J. 1964. Multiple comparisons using rank sums. \emph{Technometrics} \bold{6}:241-252.
}

\examples{
	#Data 
		data(lengthWeightData)
	#Subset the data
		SubData<-lengthWeightData[lengthWeightData$Age=='16 week', ] 
		SubData<-SubData[SubData$Generation=='F1', ]
		SubData<-SubData[SubData$SEX=='M', ]
	#Run 
		dunnsTest(Data=SubData, Treatment='Treatment',
			Response='WEIGHT', TestDirection='Both')
}

\keyword{Analysis}

