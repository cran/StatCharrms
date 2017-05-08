makeSummaryTable <-
function(Data,Treatment,Response){
#' @export
#Make a data summary table for the analysis
#Give this MainData so it uses the untransformed amounts

Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
Mean<-tapply(Data[ ,Response],Data[ ,Treatment],mean)
Count<-tapply(Data[ ,Response],Data[ ,Treatment],length)
Median<-tapply(Data[ ,Response],Data[ ,Treatment],median)
Var<-tapply(Data[ ,Response],Data[ ,Treatment],var)
Std_Dev<-sqrt(Var)
Std_Error<-sqrt(Var/Count)

SummaryTable<-cbind(levels(Data[ ,Treatment]),Count,signif(Mean,5),signif(Median,5),signif(Std_Dev,5),signif(Std_Error,5))
colnames(SummaryTable)[1]<-'Treatment'
colnames(SummaryTable)[3]<-'Mean'
colnames(SummaryTable)[4]<-'Median'
colnames(SummaryTable)[5]<-'Std_Dev'
colnames(SummaryTable)[6]<-'Std_Error'

return(SummaryTable)
}
