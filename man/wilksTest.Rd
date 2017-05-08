\name{wilksTest}
\alias{wilksTest}

\title{
	Performs the Shapiro-Wilks test.
}
\description{
	Performs the Shapiro-Wilks test for normality on a set of residuals from an analysis. This is a wrapper function for the \code{\link{shapiro.test}} function with additional evaluation statistics.      
}
\usage{
wilksTest(Residuals)
}

\arguments{
  \item{Residuals}{
	Residuals from an analysis. 
}
}

\value{
WilksTest, a data frame containing:
 \item{OBS}{Total number of observation.}
 \item{STD}{The standard error.}
 \item{SKEW}{The skew of the data set.}
 \item{KURT}{The measure of kurtosis (how heavy tailed the distribution is) of the data set.}
 \item{SW_STAT}{The Shapiro-Wilks test statistic.}
 \item{P_VALUE}{The p-Value for the test statistic.}
 \item{Signif}{The flag for p-values less then 0.05.}

}
\references{
 Patrick Royston (1982) An extension of Shapiro and Wilk's \emph{W} test for normality to large samples. \emph{Applied Statistics}, \bold{31}: 115-124.
\cr Patrick Royston (1982) Algorithm AS 181: The \emph{W} test for Normality. \emph{Applied Statistics}, \bold{31}: 176-180. 
\cr Patrick Royston (1995) Remark AS R94: A remark on Algorithm AS 181: The \emph{W} test for normality. \emph{Applied Statistics}, \bold{44}: 547-551. 
\cr Johnson, NL, Kotz, S, Balakrishnan N (1994) \emph{Continuous Univariate Distributions, Vol 1, 2nd Edition} Wiley ISBN 0-471-58495-9.
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



\keyword{Analysis}

