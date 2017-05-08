\name{responseTransform}
\alias{responseTransform}

\title{
	Transforms a response
}
\description{
	Transforms a response based on a selection from the GUI input. 
}
\usage{
responseTransform(Data, ResponVar, Trans)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{ResponVar}{
	The name (as a string) of the response variable. 
}
  \item{Trans}{
	The name (as a string) of the response variable. Can take on the values of 
	c('Transformation', 'None', 'Log',Log1', 'Square_Root', 'Arcsin', 'Rank'). 
}
}


\value{
 \item{OutData}{The original data set with a new variable called 'TransformedResponse' which is the rank transform of the response.}
}

\author{
	Joe Swintek
}

\note{
	The log transformation is the common log (base 10) transformation, while 'Arcsin' is the arcsin of the square root of the response. The 'Rank' transformation uses
	\code{\link{rankTransform}}.	
}


\examples{
	#Data 
		data(lengthWeightData)
	#Subset the data
		SubData<-lengthWeightData[lengthWeightData$Age=='16 week', ] 
		SubData<-SubData[SubData$Generation=='F1', ]
		SubData<-SubData[SubData$SEX=='M', ]
	#Run 
		RankData<-responseTransform(Data=SubData, ResponVar='WEIGHT', Trans='Log')
		head(RankData)
}


