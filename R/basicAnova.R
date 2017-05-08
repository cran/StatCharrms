basicAnova <-
function(Data,Treatment,Response,WeightList){

#' @export
#basic ANOVA test with clean table output

Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
#Anova
AnovaTable<-aov(Data[ ,Response]~as.factor(Data[ ,Treatment]), weight=WeightList)

#Summary Table
AnovaTable<-Anova(AnovaTable)
AnovaTable<-as.data.frame(AnovaTable)
AnovaTable$Signif<-NA
AnovaTable$Signif[1]<-'.'
if(AnovaTable[1,4]<0.05){AnovaTable$Signif[1]<-'*'}
if(AnovaTable[1,4]<0.01){AnovaTable$Signif[1]<-'**'}
if(AnovaTable[1,4]<0.001){AnovaTable$Signif[1]<-'***'}
rownames(AnovaTable)[1]<-Treatment;

return(AnovaTable)
}
