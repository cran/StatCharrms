transformationWarning <-function(Data,Trans,VecName){
#' @export
#Warning messages if a transformation will not work	
	if (Trans=='Log' || Trans=='Log1'){
		if (length(which(Data[ ,VecName]<0))>0){
			Message<-''
			for (e in which(Data[ ,VecName]<0)){
				Message<-paste(Message,e,sep=', ')
			}
		
			Message<-paste('The log transformation can not be performed on negative numbers.\n The rows of:',Message,' will be removed from the analysis.')
			popMessage(Message)
		
		}
		if (Trans=='Log'){
			if (length(which(Data[ ,VecName]==0))>0){
				Message<-''
				for (e in which(Data[ ,VecName]==0)){
					Message<-paste(Message,e,sep=', ')
				}
				Message<-paste('The log transformation can not be performed on negative numbers.\n The rows of:',Message,' will be removed from the analysis.
				A Log + 1 transformation may correct this problem.')
				popMessage(Message)	
			}
		}
		if (Trans=='Log1'){
			TestVec<-Data[which(Data[ ,VecName]>0),VecName]
			if(min(TestVec<16)) {  
				popMessage('Log+1 is skewing the data by greater than 1% away from a standard log transformation. Results may be negatively effected!')
			}
		
		}
	}
	if (Trans=='Square_Root' || Trans=='Arcsin'){
		if (length(which(Data[ ,VecName]<0))>0){
			Message<-''
			for (e in which(Data[ ,VecName]<0)){
				Message<-paste(Message,e,sep=', ')
			}
		
			Message<-paste('The square root transformation can not be performed on negative numbers.\n The rows of:',Message,' will be removed from the analysis.')
			popMessage(Message)
		
		}
		if ( Trans=='Arcsin'){
			TestVec<-which(Data[ ,VecName]>1 | -1>Data[ ,VecName])
			if(length(TestVec)>0){
				Message<-''
				for (e in TestVec){
					Message<-paste(Message,e,sep=', ')
				}
				Message<-paste('Arcsin can only be used on numbers between -1 and 1. 
					The rows of:',Message,' will be removed from the analysis.\n Try a different transformation.')
				popMessage(Message)
			}
		}
	}		
}