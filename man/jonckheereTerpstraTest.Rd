\name{jonckheereTerpstraTest}
\alias{jonckheereTerpstraTest}

\title{
	Perform the Jonckheere-Terpstra trend test 
}
\description{
	The Jonckheere-Terpstra is a non-parametric test for trend.  This function is a wrapper for \code{\link{jonckheere.test}} from the clinfun package. 
}
\usage{
jonckheereTerpstraTest(Data, Treatment, Response, TestDirection, AlphaLevel)
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
		The direction of the test statistic can be any of: c('Both', 'Descending', 'Ascending').
}
  \item{AlphaLevel}{
	The alpha level each step needs to pass before moving on to test a lower treatment level. 
}
}

\value{
 \item{JT Statistic				}{ The JT test statistic.}
 \item{Decreasing Trend P-Value}{The p-value for a decreasing trend.}
 \item{D.Sig					}{The significance flag for the decreasing tend where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.} 
 \item{Increasing Trend P-Value}{The p-value for a increasing trend.}
 \item{I.Sig					}{The significance flag for the increasing tend where "." is a p-value > 0.05, "*" is a 0.01 < p-value <=  0.05, "**" for 0.001 <  p-value <=  0.01, and "***" for p-value <= 0.001.} 
\item{Max Level of Treatment}{The treatment level the test statistic corresponds to.}		
}

\note{
A note when using the JT.  The JT estimates p-values by a random process thus after an initial run, subsequent runs of the JT will produce slightly different p-values. To produce exactly the same p-values between each run you will need to use the \code{\link{set.seed}} function in the R console before each run.
}


\references{
Jonckheere, A. R. (1954). A distribution-free k-sample test against ordered alternatives. \emph{Biometrika} \bold{41}:133-145. 
\cr Terpstra, T. J. (1952). The asymptotic normality and consistency of Kendall's test against trend, when ties are present in one ranking. emph{Indagationes Mathematicae} \bold{14}:327-333. 

}
\author{
	Joe Swintek
}


\seealso{
\code{\link{jonckheere.test}}
}

\examples{
	## Not run: 
	#Data 
		data(lengthWeightData)
	#Subset the data
		SubData<-lengthWeightData[lengthWeightData$Age=='16 week', ] 
		SubData<-SubData[SubData$Generation=='F1', ]
		SubData<-SubData[SubData$SEX=='M', ]
	#Not Run 
	\dontrun{
		jonckheereTerpstraTest(Data=SubData, Treatment='Treatment', 
			Response='WEIGHT', TestDirection='Both', AlphaLevel=0.05)
	}
	## End(Not run)		
}


\keyword{Analysis}