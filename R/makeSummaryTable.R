makeSummaryTable <-
function(Data,Treatment,Response,alpha=0.05){
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
LCI<-Mean+qt(alpha/2,Count-1)
UCI<-Mean-qt(alpha/2,Count-1)


SummaryTable<-cbind(levels(Data[ ,Treatment]),Count,signif(Mean,5),signif(Median,5),signif(Std_Dev,5),signif(Std_Error,5),signif(LCI,5),signif(UCI,5))
colnames(SummaryTable)[1]<-'Treatment'
colnames(SummaryTable)[3]<-'Mean'
colnames(SummaryTable)[4]<-'Median'
colnames(SummaryTable)[5]<-'Std_Dev'
colnames(SummaryTable)[6]<-'Std_Error'
colnames(SummaryTable)[7]<-paste0('Lower ', round(100-alpha*100,0) ,'% CI')
colnames(SummaryTable)[8]<-paste0('Upper ', round(100-alpha*100,0) ,'% CI')



return(SummaryTable)
}
