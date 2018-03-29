\name{makeSummaryTable}
\alias{makeSummaryTable}

\title{
 Generate a summary table of a data set.
}
\description{
	Generates a table of standard summary statistical values for each treatment level of a response from a data set. 
	This function will always be called when standard numerical endpoints are analysed.  
}
\usage{
makeSummaryTable(Data, Treatment, Response, 
 alpha = 0.05, Replicate = NULL)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{Treatment}{
	The name (as a string) of the treatment variable. 
}
  \item{Response}{
	The name (as a string) of the endpoint. 
}
  \item{alpha}{
	The alpha level used to calculate confidence intervals.   
}
  \item{Replicate}{
	The name (as a string) of the replicate variable.   
}

}


\value{
	A data set containing the mean, median, standard deviation, standard error, and confidence intervals for each treatment level of the response.
	If a replicate variable is supplied, the output will also contain the mean of the replicate means and the number of replicates.  
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
		makeSummaryTable(Data=SubData, Treatment='Treatment', 
			Response='WEIGHT')	
}
\keyword{Analysis}









