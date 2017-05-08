 \name{medianData}
\alias{medianData}
\alias{averageData}

\title{
	Calculate the data median
}
\description{
	Calculates the mean or median value of a response within a replicate for every replicate within a treatment.
}
\usage{
medianData(Data, Treatment, Response, Replicate)
averageData(Data, Treatment, Response, Replicate)
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
  \item{Replicate}{
	The name (as a string) of the replicate variable. 
}
}

\value{
	Returns a data set containing mean or median values for every replicate and treatment. 
}


\examples{
	#Data 
		data(lengthWeightData)
	#Subset the data
		SubData<-lengthWeightData[lengthWeightData$Age=='8 week', ] 
		SubData<-SubData[SubData$Generation=='F1', ]
		SubData<-SubData[SubData$SEX=='M', ]
	#Run 	
	medianData(Data=SubData, Treatment='Treatment', Response='WEIGHT',
		Replicate='Replicate')
	averageData(Data=SubData, Treatment='Treatment', Response='WEIGHT',
		Replicate='Replicate')
}


\author{
	Joe Swintek
}
