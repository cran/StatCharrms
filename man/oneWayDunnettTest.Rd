\name{oneWayDunnettTest}
\alias{oneWayDunnettTest}

\title{
	One way Dunnett's test. 
}
\description{
	Performs the Dunnett's test for multiple comparisons.  This is a wrapper function for \code{\link{glht}} from the multcomp package.
}
\usage{
oneWayDunnettTest(Data, Treatment, Response, 
WeightList = NULL, TestDirection = "Decreasing")
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
  \item{WeightList}{
	A list of weights for a weighted ANOVA. 
}
  \item{TestDirection}{
	The direction the test statistic can be: c('Both', 'Decreasing', 'Increasing').
}
}

\value{
	Return a data frame containing: 
  \item{Treatment}{The name of the treatment variable.}
  \item{Levels}{The levels of the treatment variable that are being compared.}
  \item{Estimate}{The estimated difference between the levels.}
  \item{Std. Error}{The standard error.}
  \item{Df}{The degrees of freedom.}
  \item{t.value}{The value of the test statistic.}
  \item{p.value}{The p-value of the test statistic.}
  \item{Signif}{The significance flag where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.} 

  }

\references{
	Dunnett C. W. (1955) A multiple comparison procedure for comparing several treatments with a control, \emph{Journal of the American Statistical Association}, \bold{50}:1096-1121.
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
	oneWayDunnettTest(Data=SubData, Treatment='Treatment', Response='WEIGHT',
		WeightList = NULL, TestDirection = "Decreasing")	
}

\keyword{Analysis}