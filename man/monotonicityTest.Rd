\name{monotonicityTest}
\alias{monotonicityTest}

\title{
	A test for a monotonic trend
}
\description{
	Performs the test for monotonicity as per the the OECD guidance on statistics. 
}
\usage{
monotonicityTest(Data, Treatment, Response)
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
}

\details{
	calls both \code{\link{getLineContrast}} and \code{\link{getQuadContrast}} to attain the contrasts used in the monotonicity test.
}
\value{
	A table of test statistics for both the linear and quadratic trends.  If the quadratic trend is significant and the linear trend is not,
	then the response is \bold{not} monotonic, otherwise it is consider to be monotonic.   
}
\references{
	Current Approaches in the Statistical Analysis of Ecotoxicity Data A guidance to application DOI: 10.1787/9789264085275-en 
}
\author{
	Joe Swintek
}

\examples{
	#Data 
		data(lengthWeightData)
	#Subset the data
		SubData<-lengthWeightData[lengthWeightData$Age=='8 week', ] 
		SubData<-SubData[SubData$Generation=='F1', ]
		SubData<-SubData[SubData$SEX=='M', ]
	#Run 
		monotonicityTest(Data=SubData, Treatment='Treatment', 
			Response='WEIGHT')

}


\keyword{Analysis}

