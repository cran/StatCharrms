responseTransform <-function(Data,ResponVar,Trans){
#' @export
#This function will apply a transform to the response vector of a data set then add .
#it as a vector called TransformedResponse to the data set
#Data-Data set to be modified 
#ResponVar- The name of the response variable to be transformed 
#Trans- The transformation list is; None, Log, Log1, Square_Root, Arcsin
#Returns a Modified data set with the new vector TransformedResponse


OutData<-Data
if (Trans=='Log+1'){Trans<-'Log1'}
if (Trans=='Arcsin(Square_Root)'){Trans<-'Arcsin'} #2017-10-23



#Apply Transform
if (Trans != 'Rank'){
	TResponse<-switch(Trans,
		None =Data[ ,ResponVar],
		Log = log10(Data[ ,ResponVar]),
		Log1 = log10(Data[ ,ResponVar]+1),
		Square_Root=sqrt(Data[ ,ResponVar]),
		Arcsin=asin(sqrt(Data[ ,ResponVar]))
	)
OutData$TransformedResponse<-TResponse
}
if (Trans == 'Rank'){
	OutData<-rankTransform(Data,ResponVar)
}



return(OutData)
}
