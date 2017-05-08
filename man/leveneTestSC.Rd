\name{leveneTestSC}
\alias{leveneTestSC}

\title{
	Levene test for homogeneity of variance 
}
\description{
	Test residuals from an  analysis for homogeneity of variance. This function is a wrapper function 
	for \code{\link[car:leveneTest]{leveneTest}} from the car package.
}
\usage{
leveneTestSC(Data, Treatment, Residuals)
}

\arguments{
 \item{Data}{
	A data set.
}
  \item{Treatment}{
	The name (as a string) of the treatment variable.
}
  \item{Residuals}{
	Residuals from a model fit.
}
}



\value{
 \item{Treatment}{The name of the treatment variable.}
 \item{Df}{The degrees of freedom.}
 \item{F value}{The value of the F-statistic.}
 \item{ Pr(>F)}{The p-value corresponding to the  F-statistic.}
}

\references{
Levene, Howard (1960) Robust tests for equality of variances In Ingram Olkin, Harold Hotelling, et al. 
Contributions to Probability and Statistics: Essays in Honor of Harold Hotelling. Stanford University Press. pp. 278-292. 
}
\author{
	Joe Swintek
}

\examples{
	#Data 
		data(lengthWeightData)
	#Subset the data
		SubData<-lengthWeightData[lengthWeightData$Age=='16 week', ] 
		SubData<-SubData[SubData$Generation=='F1', ]
		SubData<-SubData[SubData$SEX=='M', ]
	#Run 
		Residuals<-aov(WEIGHT~Treatment,SubData)$residuals
		wilksTest(Residuals)
}


\seealso{
	\code{\link{leveneTest}}
}

\keyword{Analysis}

