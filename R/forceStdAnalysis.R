forceStdAnalysis <-
function(Data,Response,TreatmentVar,Transform,WeightsVar,TimeVar,TestDirection,ReplicateVar,Test,AlphaLevel){

#' @export
#This function run if Auto is not selected for TestType
#It will force the analysis to what ever the user picks

Results<-{}

#Initialize Starting Variables 
WilksResults<-{}
LeveneResults<-{}
AnovaResults<-{}
OneWayDunnetResults<-{}
JonckheereTerpstraResults<-{}
Comments<-{}
DunnsTable<-{}
MonocityMsg<-{}
MonocityTable<-{}
WilliamsTableUp<-{} #2017-10-17
WilliamsTableDown<-{} #2017-10-17


Alternative<-switch(TestDirection,
Decreasing='Descending',
Increasing='Ascending',
Both='Both'
)
TempData<-Data


message(paste('\n',Response,'is being analysed'))
#Clean numbers that are not finite
if (length(which(is.finite(TempData[ ,Response])==FALSE))>0){
TempData<-TempData[-{which(is.finite(TempData[ ,Response])==FALSE)}, ]
}

TransData<-responseTransform(TempData,Response,Transform) #Transform Data

################################################################################################################
#Summary Table No matter what
SummaryTable<-makeSummaryTable(TransData,TreatmentVar,Response,alpha = AlphaLevel,ReplicateVar)

if (Test== 'RM ANOVA'){
Path<-2
#Wilks-levens 
RMResponce<-runMultiGen(TransData,TreatVar=TreatmentVar,ResponVar='TransformedResponse',
RepVar=ReplicateVar,Path=2,TimeVar=TimeVar,Alternative,alpha=AlphaLevel)

AnovaResults<-RMResponce$Anova.Table
OneWayDunnetResults<-RMResponce$MainEffects
Residuals<- RMResponce$Lmm$residuals

if(is.null(dim(Residuals))==FALSE){ #This checks the Residuals for the Lme function
Residuals<-Residuals[ ,2]
}


LeveneResults<-as.data.frame(RMResponce$LeveneTest)
LeveneResults<-LeveneResults[-2,]


WilksResults<-wilksTest(Residuals)

}
################################################################################################################
if (Test== 'ME ANOVA'){
Path<-3
#Wilks-levens 
RMResponce<-runMultiGen(TransData,TreatVar=TreatmentVar,ResponVar='TransformedResponse',
RepVar=ReplicateVar,Path=3,TimeVar=TimeVar,Alternative,alpha=AlphaLevel)

AnovaResults<-RMResponce$Anova.Table
OneWayDunnetResults<-RMResponce$MainEffects
Residuals<- RMResponce$Lmm$residuals

if(is.null(dim(Residuals))==FALSE){ #This checks the Residuals for the Lme function
Residuals<-Residuals[ ,2]
}

LeveneResults<-RMResponce$LeveneTest
WilksResults<-wilksTest(Residuals)
}
################################################################################################################
if (Test== 'Simple ANOVA' | Test== 'Weighted ANOVA' ){

if (ReplicateVar=='Not Used'){  #Do nothing
AvgData<-TempData;}else{  
AvgData<-averageData(TempData,TreatmentVar,Response,ReplicateVar) 
}
AvgData[ ,Response]<-as.numeric(as.character(AvgData[ ,Response]))
AvgTransData<-responseTransform(AvgData,Response,Transform) #Transform Data of averages
WeightsVar=NULL

if (Test== 'Weighted ANOVA'){
WeightsVar=AvgData$N.WEIGHT

}
#JS 2018-1-4 fixed the issue with the simple ANOVA test not using transdata

AnovaResults<-basicAnova(AvgTransData,TreatmentVar,'TransformedResponse',WeightsVar)
OneWayDunnetResults<-oneWayDunnettTest(AvgTransData,TreatmentVar,'TransformedResponse',WeightsVar,TestDirection,alpha=AlphaLevel)
AOV<-aov(AvgTransData[ ,'TransformedResponse']~as.factor(AvgTransData[ ,TreatmentVar]))
Residuals<- AOV$residuals
LeveneResults<-leveneTestSC(AvgTransData,TreatmentVar,Residuals)
WilksResults<-wilksTest(Residuals)
}

if (Test== 'Jonckheere'){
#Check Replicate Structure and Take Median for each Replicate by OECD Standards 
if (ReplicateVar=='Not Used'){  #Do nothing
AvgData<-TempData;}else{  
AvgData<-medianData(TempData,TreatmentVar,Response,ReplicateVar) 
}
AvgData[ ,Response]<-as.numeric(as.character(AvgData[ ,Response]))

MonocityTable<-monotonicityTest(AvgData,TreatmentVar,Response) #Test for Monotonicity set in place by OECD p.44 #142
JonckheereTerpstraResults<-jonckheereTerpstraTest(AvgData,TreatmentVar,Response,TestDirection,AlphaLevel)
}

if (Test== 'Dunnett'){
if (ReplicateVar=='Not Used'){  #Do nothing
AvgData<-TempData;}else{  
AvgData<-averageData(TempData,TreatmentVar,Response,ReplicateVar) 
}
AvgData[ ,Response]<-as.numeric(as.character(AvgData[ ,Response]))
AvgTransData<-responseTransform(AvgData,Response,Transform) #Transform Data of average

#JS 2018-1-4 fixed the issue with the Dunnett test not using transdata

	
OneWayDunnetResults<-oneWayDunnettTest(AvgTransData,TreatmentVar,'TransformedResponse',WeightList=NULL,TestDirection,alpha=AlphaLevel)
AOV<-aov(AvgTransData[ ,'TransformedResponse']~as.factor(AvgTransData[ ,TreatmentVar]))
Residuals<- AOV$residuals
LeveneResults<-leveneTestSC(AvgTransData,TreatmentVar,Residuals)
WilksResults<-wilksTest(Residuals)
}


if (Test== 'Dunns'){
DunnsTable<-dunnsTest(TempData,TreatmentVar,Response,TestDirection)
}


#2017-10-17
if (Test== 'Williams'){
	
	#Test for normality needed for Williams
	if (ReplicateVar=='Not Used'){  #Do nothing
		AvgData<-TempData;}else{  
		AvgData<-averageData(TempData,TreatmentVar,Response,ReplicateVar) 
	}
	
	
	AvgData[ ,Response]<-as.numeric(as.character(AvgData[ ,Response]))
	AvgTransData<-responseTransform(AvgData,Response,Transform) #Transform Data of average
	AOV<-aov(AvgTransData[ ,Response]~as.factor(AvgTransData[ ,TreatmentVar]))
	Residuals<- AOV$residuals
	LeveneResults<-leveneTestSC(AvgTransData,TreatmentVar,Residuals)
	WilksResults<-wilksTest(Residuals)


	#Also display results for the test for 
	MonocityTable<-monotonicityTest(AvgData,TreatmentVar,Response) #Test for Monotonicity set in place by OECD p.44 #142
	
		
		
	if (TestDirection=='Both'){
		WilliamsTableUp<-williamsTest(TransData,'TransformedResponse',TreatmentVar,'increasing')
		WilliamsTableDown<-williamsTest(TransData,'TransformedResponse',TreatmentVar,'decreasing')
	}
	if (TestDirection=='Decreasing'){
		WilliamsTableDown<-williamsTest(TransData,'TransformedResponse',TreatmentVar,'decreasing')
	}
	if (TestDirection=='Increasing'){
		WilliamsTableUp<-williamsTest(TransData,'TransformedResponse',TreatmentVar,'increasing')
	}	
	
	
}


Results<-list(Response=Response,SummaryTable=SummaryTable,
WilksResults=WilksResults,LeveneResults=LeveneResults,AnovaResults=AnovaResults,OneWayDunnetResults=OneWayDunnetResults,
JonckheereTerpstraResults=JonckheereTerpstraResults,TransformationUsed=Transform,MonocityTable=MonocityTable,
Comments=Comments,MonocityMsg=MonocityMsg,DunnsTable=DunnsTable,WilliamsTableUp=WilliamsTableUp,WilliamsTableDown=WilliamsTableDown)







return(Results)
}
