makeSummaryTable <-
function(Data,Treatment,Response,alpha=0.05,Replicate=NULL){
#' @export
#Make a data summary table for the analysis
#Give this MainData so it uses the untransformed amounts


i=0 #varble that controls placment in summary table. 0 is used for the base case, 2 is used
#when there is a replicate 
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
Mean<-tapply(Data[ ,Response],Data[ ,Treatment],mean)
Count<-tapply(Data[ ,Response],Data[ ,Treatment],length)
Median<-tapply(Data[ ,Response],Data[ ,Treatment],median)
Var<-tapply(Data[ ,Response],Data[ ,Treatment],var)
Std_Dev<-sqrt(Var)
Std_Error<-sqrt(Var/Count)
LCI<-Mean+qt(alpha/2,Count-1)*Std_Error
UCI<-Mean-qt(alpha/2,Count-1)*Std_Error



SummaryTable<-cbind(levels(Data[ ,Treatment]),Count,signif(Mean,5),signif(Median,5),signif(Std_Dev,5),signif(Std_Error,5),signif(LCI,5),signif(UCI,5))
colnames(SummaryTable)[1]<-'Treatment'
colnames(SummaryTable)[3]<-'Mean'
colnames(SummaryTable)[4]<-'Median'
colnames(SummaryTable)[5]<-'Std_Dev'
colnames(SummaryTable)[6]<-'Std_Error'
colnames(SummaryTable)[7]<-paste0('Lower ', round(100-alpha*100,0) ,'% CI')
colnames(SummaryTable)[8]<-paste0('Upper ', round(100-alpha*100,0) ,'% CI')

#Filters for replicates that are not used
if (identical(Replicate,'Not Used')==TRUE){
	Replicate<-NULL
}

#Added 2018-2-26 This displays the by replicate mean
if (is.null(Replicate)==FALSE){
	colnames(SummaryTable)[3]<-'Mean (Ignores Replicates)'
	
	#Calculates the mean of replicate means and the number of replicates
	Data$Unit<-Data[ ,Treatment]:Data[ ,Replicate]
	Means<-tapply(Data[ ,Response],Data$Unit,mean)
	Tr<-tapply(Data[ ,Treatment],Data$Unit,function(X){X[1]})
	MeanR<-tapply(Means,Tr,mean,na.rm=TRUE)
	
	NR<-tapply(Tr,Tr,length)
	
	SummaryTable<-cbind(SummaryTable[ ,1],signif(MeanR,5),NR,SummaryTable[ ,2:8])
	
	colnames(SummaryTable)[1]<-'Treatment'
	colnames(SummaryTable)[2]<-'Mean (of Replicates)'
	colnames(SummaryTable)[3]<-'N (Replicate)'
}


return(SummaryTable)
}












