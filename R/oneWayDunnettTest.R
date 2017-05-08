oneWayDunnettTest <-
function(Data,Treatment,Response,WeightList=NULL,TestDirection='Decreasing'){
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
DunTest<-summary(glht(AnovaTable, linfct=mcp(TreatmentVar="Dunnett"),alternative=Alternative));
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
	signif(DunTest$test$tstat,4),
	signif(tRes,4),	
	round(DunTest$test$pvalues,4)
))

DunTable$Signif<-'.'
colnames(DunTable)<-c('Treatment','Levels','Estimate', 'Std. Error','Df', 't.value', 'p.value','AdjP','Signif')

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



