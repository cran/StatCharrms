\name{basicAnova}
\alias{basicAnova}

\title{
	Perform a standard one way ANOVA.
}
\description{
	A wrapper function for the \code{\link{aov}} function, which performs a standard ANOVA.  
}
\usage{
basicAnova(Data, Treatment, Response, WeightList)
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
	The name (as a string) of the variables containing the weight for ANOVA. Set this value to NULL to indicate that weights are not being used.
}
}

\value{
 \item{Sum Sq}{The within treatment sum of squares.}
 \item{Df}{The degrees of freedom.}
 \item{F value}{The value of the F-statistic.}
 \item{ Pr(>F)}{The p-value corresponding to the F statistic.}
 \item{Signif}{The significance flag for the where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.} 
}

\references{
Chambers, J. M., Freeny, A and Heiberger, R. M. (1992) emph{Analysis of variance; designed experiments} 
\cr Chapter 5 of Statistical Models in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole 
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
		basicAnova(Data=SubData, Treatment='Treatment',
			Response='WEIGHT',WeightList=NULL)
}

\seealso{
	\code{\link{aov}}
}

\keyword{Analysis}
