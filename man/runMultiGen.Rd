\name{runMultiGen}
\alias{runMultiGen}

\title{
	Runs a mixed effects ANOVA
}
\description{
	This function runs a mixed effect ANOVA. 
}
\usage{
runMultiGen(Data, TreatVar = "", ResponVar = "", 
RepVar = "Not Used", TimeVar = "Not Used", 
Path, TestDirection = "Descending")
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{TreatVar}{
	Name (as a string) of the treatment variable.
}
  \item{ResponVar}{
	Name (as a string) of the response variable.
}
  \item{RepVar}{
	Name (as a string) of the replicate variable.
}
  \item{TimeVar}{
	Name (as a string) of the time variable.
}
  \item{Path}{
	Path indicates the type of mixed effects analysis performed. Use Path = 2 for Time (Repeated Measures) and Path = 3 Group - Replicate (Mixed Effects).
}
  \item{TestDirection}{
	Indicates the test direction. This can be c('Both', 'Descending', 'Ascending').
}
}
\details{
	This is mostly a wrapper function for \code{\link[nlme:nlme]{nlme}} from the nlme package and \code{\link[multcomp:glht]{glht}} from the multcomp package.   
}

\value{
	Returns a list with the following items: 
	\item{Anova.Table}{An ANOVA table see \code{\link{Anova}}.}
	\item{MainEffects}{A table of main effect, see \code{\link{oneWayDunnettTest}} for more details. }
	\item{FreqTable}{A table of frequencies of occurrences for every sub group in the data set. }
	\item{Lmm}{A nlme class data structure from \code{\link[nlme:nlme]{<nlme>}} from the nlme package.}
	\item{ShapiroTest}{The results from a Shapiro-Wilks test. See \code{\link{wilksTest}} for more information. }
}
\references{
	Pinheiro, J.C., and Bates, D.M. (2000) Mixed-Effects Models in S and S-PLUS, Springer
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
		
	runMultiGen(Data=SubData, TreatVar = "Treatment", 
		ResponVar = "WEIGHT", RepVar = 'Replicate', TimeVar = "Not Used", 
		Path=3, TestDirection = "Descending")

}





\seealso{
\code{\link[nlme:nlme]{nlme}}, \code{\link[multcomp:glht]{glht}} 
}

\keyword{Analysis}