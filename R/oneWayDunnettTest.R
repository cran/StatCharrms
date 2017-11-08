oneWayDunnettTest <-
function(Data,Treatment,Response,WeightList=NULL,TestDirection='Decreasing',alpha=0.05){
#patched in to allow for unadjusted p-values


#' @export
#This will perform a Dunnett Test and monotonicity test for the JT

#Change names to be used in functions
Alternative<-switch(TestDirection,
	Both="two.sided",
	Decreasing='less',
	Increasing="greater"
)

Data$TreatmentVar<-as.factor(Data[ ,Treatment])
Data$Response<-Data[ ,Response]

#Anova
AnovaTable<-aov(Response~TreatmentVar,data=Data, weight=WeightList)

#Dunnett
LT<-glht(AnovaTable, linfct=mcp(TreatmentVar="Dunnett"),alternative=Alternative)
CI<-confint(LT,level = 1-alpha)
DunTest<-summary(LT);
Df<-AnovaTable$df


tRes<-switch (TestDirection, 
		Decreasing=pt(DunTest$test$tstat,Df,lower.tail = TRUE),
		Increasing=pt(DunTest$test$tstat,Df,lower.tail = FALSE),
		Both=pt(abs(DunTest$test$tstat),Df,lower.tail = FALSE)*2
	)	

#Clean Output
DunTable<-as.data.frame(cbind(
	Treatment,
	rownames(DunTest$linfct),
	signif(DunTest$test$coefficients,4),
	signif(DunTest$test$sigma,4),
	Df,
	round(CI$confint[ ,2],4),
	round(CI$confint[ ,3],4),
	signif(DunTest$test$tstat,4),
	signif(tRes,4),	
	round(DunTest$test$pvalues,4)

))

DunTable$Signif<-'.'
colnames(DunTable)<-c('Treatment','Levels','Estimate', 'Std. Error','Df',paste0('Lower ', round(100-alpha*100,0) ,'% CI'),paste0('Upper ', round(100-alpha*100,0) ,'% CI'),'t.value', 'p.value','AdjP','Signif')

#Known warning
oldw <- getOption("warn")
options(warn=-1)  

DunTable$Signif[DunTest$test$pvalues<0.05]<-'*'
DunTable$Signif[DunTest$test$pvalues<0.01]<-'**'
DunTable$Signif[DunTest$test$pvalues<0.001]<-'***'
rownames(DunTable)<-NULL
options(warn = oldw) 
return(DunTable)
}



