\name{williamsTest}
\alias{williamsTest}

\title{
	Performs Williams Test
}
\description{
	The Williams test is a parametric test for trend. It is used to test for a trend when normality assumption is met.
	
}
\usage{
	williamsTest(df,resp,trt,direction='decreasing',SeIn=NULL)
}

\arguments{
  \item{df}{
	A data set.
}

\item{resp}{
	The name (as a string) of the response variable.
}

 \item{trt}{
	The name (as a string) of the treatment variable.
}

\item{direction}{
		The direction of the test statistic which can either be 'increasing', 'decreasing').
			
}
  \item{SeIn}{
	This is used to specify a different standard error then the one calculated within the WilliamsTest function. 
}
}




\value{
	\item{trt}{The treatment level the test statistic corresponds to.}
	\item{Y.Tilde}{The amalgamated averages for the treatment level.}
	\item{Se Diff}{The standard error.} 
	\item{DF}{The degrees of freedom.}
	\item{Will}{The value of the Williams test statistic.} 
	\item{TCrit}{The critical value of the Williams test statistic, corresponding to a p-value of 0.05.}
}



\references{
Williams D.A. (1971).  A test for differences between treatment means when several dose levels are compared with a zero dose control. \emph{Biometrics} \bold{27}(1):103-117. 
\cr  Green J., Springer T.,  Holbeck H.   \emph{Statistical Analysis of Ecotoxicology Data} (Wiley in press)
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
		williamsTest(df=SubData, trt='Treatment', 
			resp='WEIGHT', direction='decreasing',SeIn=NULL)
		
}


\keyword{Analysis}



