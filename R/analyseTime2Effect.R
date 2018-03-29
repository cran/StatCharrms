analyseTime2Effect <-
function(Data,StatusVar,TimeVar,TreatmentVar,ReplicateVar){
#' @export
#Runs the CoxMe for time to effect 
#Data: data set for analysis 
#StatusVar: Name of status variable
#TimeVar: Name of time variable
#Treatmentvar: Name of treatment variable
#ReplicateVar : Name of Replicate variable
#Returns a table of results
#Added 2018-3-23
	#code to calculate and display median time to effect
	
#Prep Data
Data$TreatmentVarUse<-as.factor(Data[ ,TreatmentVar])  
Data$ReplicateVarUse<-as.factor(Data[ ,ReplicateVar])  
Data$Subject<-Data$TreatmentVarUse:Data$ReplicateVarUse  #This is each tank
Data$TimeVarUse<-Data[ ,TimeVar]
Data$StatusVarUse<-Data[ ,StatusVar]

#Used for analysis
FitMe <- withCallingHandlers(
coxme(Surv(TimeVarUse, StatusVarUse) ~ TreatmentVarUse + (1|Subject), data = Data),  #Use this one
warning = function(w){popMessage(paste('Warning!\n',w$message))}
)

#Used in Graphing
FitHP<-coxph(Surv(TimeVarUse, StatusVarUse)~TreatmentVarUse, data=Data)
FitS <- survfit(Surv(TimeVarUse, StatusVarUse) ~ TreatmentVarUse, data = Data)


options(warn=-1)
Effects<-summary(glht(FitMe,linfct=mcp(TreatmentVarUse='Dunnett'))) #to apply Dunnetts adjustment 
options(warn=0)

#Create Table of Results
EffectsTable<-as.data.frame(cbind(Effects$test$coefficients,Effects$test$sigma,Effects$test$tstat,Effects$test$pvalues))
colnames(EffectsTable)<-c('Estimate','Std. Error','z value','P Value')
Sig<-rep('.',dim(EffectsTable)[1])
Sig[which(EffectsTable[ ,4]<0.05)]<-'*'
Sig[which(EffectsTable[ ,4]<0.01)]<-'**'
Sig[which(EffectsTable[ ,4]<0.001)]<-'***'
EffectsTable<-round(EffectsTable,4)
if (length(which(EffectsTable[ ,4]==0))>0){
EffectsTable[which(EffectsTable[ ,4]==0),4]<-'<1e-4'
}
#Added 2018-3-23
#Create Table of median time to effects
Meds<-quantile(FitS,.50)
Treat<-levels(Data$TreatmentVarUse)
MedianTable<-data.frame(Treat,FitS$n,Meds$quantile,Meds$lower,Meds$upper)
colnames(MedianTable)<-c('Treatment','N','Median','Lower 95% CI','Upper 95% CI')
rownames(MedianTable)<-NULL


EffectsTable<-cbind(EffectsTable,Sig)
Results<-list(FitMe=FitMe,FitHP=FitHP,FitS=FitS,EffectsTable=EffectsTable,MedianTable=MedianTable)
return(Results)
}



















