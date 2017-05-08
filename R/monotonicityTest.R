monotonicityTest <-
function(Data,Treatment,Response){
#' @export
#This is the test for monotonicity as done in the SAS Version
#it sets up linear and Quadratic contrast for an ANOVA
#Uses .stdEndEnv$WeightVar 

#Transform data for rank response 
Data<-rankTransform(Data,Response)


#Form Contrasts
LineContrast<-getLineContrast(Data,Treatment)
QuadContrast<-getQuadContrast(Data,Treatment)
Contrasts <- cbind(LineContrast,QuadContrast)
colnames(Contrasts)<-c('Line','QuadContrast')

#ANOVA
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
contrasts(Data[ ,Treatment])<-Contrasts
AnovaTable<-aov(Data[ ,'TransformedResponse']~as.factor(Data[ ,Treatment]))
if (is.null(.stdEndEnv$WeightVar)==FALSE){  #if there is a weight and the data is not averaged 
if (length(.stdEndEnv$WeightVar)==length(Data[ ,Response])){
AnovaTable<-aov(Data[ ,'TransformedResponse']~as.factor(Data[ ,Treatment]), weight=.stdEndEnv$WeightVar )
}
}

#gather information and clean the table
CAnova<-summary.lm(AnovaTable)
MonocityTable<-as.data.frame(CAnova$coefficients[2:3,3:4])
rownames(MonocityTable)<-NULL
MonocityTable<-cbind(c('Linear','Quadratic'),MonocityTable,'.')
colnames(MonocityTable)[1]<-'Test'
colnames(MonocityTable)[4]<-'Significance'
MonocityTable$Significance<-as.character(MonocityTable$Significance)

MonocityTable$Significance[MonocityTable[ ,'Pr(>|t|)']<0.05]<-'*'
MonocityTable$Significance[MonocityTable[ ,'Pr(>|t|)']<0.01]<-'**'
MonocityTable$Significance[MonocityTable[ ,'Pr(>|t|)']<0.001]<-'***'

MonocityTable[ ,'Pr(>|t|)']<-round(MonocityTable[ ,'Pr(>|t|)'],4)
if (length(which(MonocityTable[ ,'Pr(>|t|)']<10^-4))>0){
MonocityTable[which(MonocityTable[ ,'Pr(>|t|)']<10^-4),'Pr(>|t|)']<-'<0.0001'
}
MonocityTable[ ,'t value']<-round(MonocityTable[ ,'t value'],2)

return(MonocityTable)
}
