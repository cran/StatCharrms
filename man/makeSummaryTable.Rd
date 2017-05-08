\name{makeSummaryTable}
\alias{makeSummaryTable}

\title{
 Generate a summary table of a data set.
}
\description{
	Generates a table of standard summary statistical values for each treatment level of a response from a data set. 
	This function will always be called when standard numerical endpoints are analyzed.  
}
\usage{
makeSummaryTable(Data, Treatment, Response)
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
}


\value{
	A data set containing the mean, median, standard deviation, and standard error for each treatment level of the response.
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









