autoStdAnylsis <-
function(Data,Response,TreatmentVar,Transform,WeightsVar,TimeVar,TestDirection,ReplicateVar,AlphaLevel){
#' @export
#This function run if Auto is selected for TestType
#It runs throw the MEOGRT guidelines for decisions 

Results<-{}

#Counts the number of endpoints 


#Initialize Starting Variables 
WilksResults<-{}
LeveneResults<-{}
AnovaResults<-{}
OneWayDunnetResults<-{}
JonckheereTerpstraResults<-{}
Comments<-{}
DunnsTable<-{}
MonocityMsg<-{}

#Changes the Direction name to something the functions use
Alternative<-switch(TestDirection,
Decreasing='Descending',
Increasing='Ascending',
Both='Both'
)


message(paste('\n',Response,'is being analysed'))

#Remove missing data from the data set
TempData<-Data
if (length(which(is.finite(TempData[ ,Response])==FALSE))>0){
	TempData<-TempData[-{which(is.finite(TempData[ ,Response])==FALSE)}, ]
}

#Check Replicate Structure and Take Median for each Replicate by OECD Standards 
if (ReplicateVar=='Not Used'){  #Do nothing
AvgData<-TempData;}else{  
AvgData<-medianData(TempData,TreatmentVar,Response,ReplicateVar) 
}
AvgData[ ,Response]<-as.numeric(as.character(AvgData[ ,Response]))


#Check for Multiple treatment levels V0.93
if (length(unique(Data[[TreatmentVar]])) == 2){
		UseJT<-FALSE
		MonocityTable<-NULL
	}else{
		#Step one Check Monotonicity 
		MonocityTable<-monotonicityTest(AvgData,TreatmentVar,Response) #Test for Monotonicity set in place by OECD p.44 #142

		UseJT<-TRUE
		if (MonocityTable[2,4] !='.' & MonocityTable[1,4] =='.' ){ #Fail test
			MonocityMsg<<-'Test for Monotonicity Failed: Will Not Compute JT'
			UseJT<-FALSE
		}
}

#Trend Test 
if (UseJT==TRUE){
	JonckheereTerpstraResults<-jonckheereTerpstraTest(AvgData,TreatmentVar,Response,TestDirection,AlphaLevel)
}

#Transform Data
TransData<-responseTransform(TempData,Response,Transform) 


#Clean numbers that are not finite
if (length(which(is.finite(TransData[ ,'TransformedResponse'])==FALSE))>0){
TransData<-TransData[-{which(is.finite(TransData[ ,'TransformedResponse'])==FALSE)}, ]
}

#Summary Table No matter what
SummaryTable<-makeSummaryTable(TransData,TreatmentVar,Response,alpha = AlphaLevel,ReplicateVar)

if (UseJT==TRUE){ #End the loop Do not for the Variable do not do any more analyses 
Results<-list(Response=Response,SummaryTable=SummaryTable,
WilksResults=WilksResults,LeveneResults=LeveneResults,AnovaResults=AnovaResults,OneWayDunnetResults=OneWayDunnetResults,
JonckheereTerpstraResults=JonckheereTerpstraResults,TransformationUsed=Transform,MonocityTable=MonocityTable,
Comments=Comments,MonocityMsg=MonocityMsg,DunnsTable=DunnsTable)  
}

if (UseJT==FALSE){


RMResponce<-runMultiGen(TransData,TreatVar=TreatmentVar,ResponVar='TransformedResponse',
RepVar=ReplicateVar,Path=3,TimeVar='Not Used',Alternative)


AnovaResults<-RMResponce$Anova.Table
OneWayDunnetResults<-RMResponce$MainEffects


Residuals<-RMResponce$Lmm$residuals

if(is.null(dim(Residuals))==FALSE){ #This checks the Residuals for the Lme function
Residuals<-Residuals[ ,2]
}

LeveneResults<-leveneTestSC(TransData,TreatmentVar,Residuals)
WilksResults<-wilksTest(Residuals)

#Check assumptions
NormalTestLevene=TRUE
NormalTestWiks=TRUE


if (is.finite(LeveneResults[ ,'Pr(>F)'])==FALSE){
LeveneResults[ ,'Pr(>F)']=0
}

if (as.numeric(WilksResults$P_VALUE)<0.01){
NormalTestWiks=FALSE
}
if (as.numeric(LeveneResults[ ,'Pr(>F)'])<0.01){
NormalTestLevene=FALSE
}
#Normality Checked

#If Normality  or Homogeneity Fails
if (NormalTestWiks==FALSE | NormalTestLevene==FALSE){    

	DunnsTable<-dunnsTest(AvgData,TreatmentVar,Response,TestDirection)
	OneWayDunnetResults<-{}
}

#Clean up LeveneResults and WilksResults
Results<-list(Response=Response,SummaryTable=SummaryTable,
WilksResults=WilksResults,LeveneResults=LeveneResults,AnovaResults=AnovaResults,OneWayDunnetResults=OneWayDunnetResults,
JonckheereTerpstraResults=JonckheereTerpstraResults,TransformationUsed=Transform,MonocityTable=MonocityTable,
Comments=Comments,MonocityMsg=MonocityMsg,DunnsTable=DunnsTable)
}

return(Results)
}
