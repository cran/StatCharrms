\name{rankTransform}
\alias{rankTransform}

\title{
	Rank transforms a response
}
\description{
	Transforms a response by rank into critical values of the standard normal distribution. In the case of ties this function will
	use the mean of the transformed response. 
}
\usage{
rankTransform(Data, VecName)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{VecName}{
	The name (as a string) of the response to be transformed.
}
}

\value{
 \item{Data}{The original data set with a new variable called 'TransformedResponse' which is the rank transform of the response.}
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
		RankData<-rankTransform(Data=SubData, VecName='WEIGHT')
		head(RankData)
}

