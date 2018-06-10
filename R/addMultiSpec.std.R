addMultiSpec.std <-
function(Notebook){
#' @export
#Clear 
.stdEndEnv$EndpointObject<-list() #List of containers for each endpoint

OverGp<- ggroup(horizontal = FALSE, container=Notebook, label='Data specification')
SpecGp<- ggroup(horizontal = TRUE, container=OverGp, label='Data specification')
MustControlFrm<-gframe(horizontal = FALSE, container=SpecGp)


TreatmentVarFrm<-gframe('Treatment',horizontal = FALSE, container=MustControlFrm,fill=TRUE)
.stdEndEnv$TreatmentVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=TreatmentVarFrm, fill=TRUE)

ReplicateVarFrm<-gframe('Replicate',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$ReplicateVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=ReplicateVarFrm, fill=TRUE)

GenerationVarFrm<-gframe('Generation Variable',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$GenerationVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=GenerationVarFrm, fill=TRUE,
handler= function(h,...){
#Update Value CBox
.stdEndEnv$GenerationVar<-svalue(.stdEndEnv$GenerationVarCbx)
delete(.stdEndEnv$GenerationValFrm,.stdEndEnv$GenerationValCbx)
if (identical(svalue(.stdEndEnv$GenerationVarCbx),'Not Used')==FALSE){
.stdEndEnv$GenerationValCbx<-gcombobox(levels(as.factor((.stdEndEnv$MainData[ ,.stdEndEnv$GenerationVar]))), fill=TRUE)
}else{
.stdEndEnv$GenerationValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
}
add(.stdEndEnv$GenerationValFrm,.stdEndEnv$GenerationValCbx)
})

.stdEndEnv$GenerationValFrm<-gframe('Generation Value',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$GenerationValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
add(.stdEndEnv$GenerationValFrm,.stdEndEnv$GenerationValCbx)

AgeVarFrm<-gframe('Age Variable',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$AgeVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=AgeVarFrm, fill=TRUE,
handler= function(h,...){
#Update Value CBox
.stdEndEnv$AgeVar<-svalue(.stdEndEnv$AgeVarCbx)
delete(.stdEndEnv$AgeValFrm,.stdEndEnv$AgeValCbx)
if (identical(svalue(.stdEndEnv$AgeVarCbx),'Not Used')==FALSE){
.stdEndEnv$AgeValCbx<-gcombobox(levels(as.factor((.stdEndEnv$MainData[ ,.stdEndEnv$AgeVar]))), fill=TRUE)
}else{
.stdEndEnv$AgeValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
}
add(.stdEndEnv$AgeValFrm,.stdEndEnv$AgeValCbx)
})

.stdEndEnv$AgeValFrm<-gframe('Age Value',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$AgeValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
add(.stdEndEnv$AgeValFrm,.stdEndEnv$AgeValCbx)

GenderVarFrm<-gframe('Gender Variable',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$GenderVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=GenderVarFrm, fill=TRUE,
handler= function(h,...){
#Update Value CBox
.stdEndEnv$GenderVar<-svalue(.stdEndEnv$GenderVarCbx)
delete(.stdEndEnv$GenderValFrm,.stdEndEnv$GenderValCbx)
if (identical(svalue(.stdEndEnv$GenderVarCbx),'Not Used')==FALSE){
.stdEndEnv$GenderValCbx<-gcombobox(levels(as.factor((.stdEndEnv$MainData[ ,.stdEndEnv$GenderVar]))), fill=TRUE)
}else{
.stdEndEnv$GenderValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
}
add(.stdEndEnv$GenderValFrm,.stdEndEnv$GenderValCbx)
})

.stdEndEnv$GenderValFrm<-gframe('Gender Value',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$GenderValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
add(.stdEndEnv$GenderValFrm,.stdEndEnv$GenderValCbx)

WeightVarFrm<-gframe('Anova Weights',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$WeightVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=WeightVarFrm, fill=TRUE)

TestDirectionFrm<-gframe('Test Direction',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$TestDirectionCbx<-gcombobox(c('Both','Increasing','Decreasing'),selected=3,container=TestDirectionFrm, fill=TRUE)

AlphaLvFrm<-gframe('Alpha Level',horizontal = FALSE, container=MustControlFrm, fill=TRUE)
.stdEndEnv$AlphaLvCbx<-gcombobox(seq(0.01,0.2,0.01),selected=5,container=AlphaLvFrm, fill=TRUE,editable = TRUE)

EndPointBtn<-gbutton("Select Endpoints To Test",container=MustControlFrm,fill=TRUE,
handler= function(h,...){
.updateEnviroment()
if (identical(.stdEndEnv$TreatmentVar,'Not Used')==TRUE){
popMessage('Please Specify Treatment First')
return()
}
.stdEndEnv$EndPointVar<-NULL
selectPara('EndPointVar',NULL,'.stdEndEnv',NULL,TRUE,Display='End Points')
#Test for suitable endpoints
while(is.null(.stdEndEnv$EndPointVar)==TRUE){
Sys.sleep(0.01)
}
.TestEndPoints()
#Add testing selection
#Changed Arcsin to Arcsin(SquareRoot) 2017-10-23
for (e in .stdEndEnv$EndPointVar){
	if(is.null(.stdEndEnv$EndpointObject[[e]])==TRUE){
		.stdEndEnv$EndpointObject[[e]]<-list()
		.stdEndEnv$EndpointObject[[e]]$Frame<-gframe(e,container=.stdEndEnv$EndpointControlFrm,fill=TRUE,editable = TRUE)
		.stdEndEnv$EndpointObject[[e]]$TestTypeCbx<-gcombobox(c('Test Type','Auto','RM ANOVA','ME ANOVA','Simple ANOVA','Weighted ANOVA','Jonckheere','Dunns','Dunnett','Williams'),container=.stdEndEnv$EndpointObject[[e]]$Frame,fill=TRUE,editable = TRUE)
		.stdEndEnv$EndpointObject[[e]]$TransformationCbx<-gcombobox(c('Transformation','None','Log',
		'Log+1','Square_Root','Arcsin(Square_Root)'),container=.stdEndEnv$EndpointObject[[e]]$Frame,fill=TRUE,editable = TRUE)
	}
}
})
.stdEndEnv$EndpointControlFrm<-gframe(horizontal = FALSE, container=SpecGp, fill=TRUE,expand=TRUE)

TimeControlGrp<-ggroup(horizontal = TRUE, container=.stdEndEnv$EndpointControlFrm,fill=TRUE)
TimeVarFrm<-gframe('Time Variable',horizontal = FALSE, container=TimeControlGrp, fill=TRUE)
.stdEndEnv$TimeVarCbx<-gcombobox(c('Not Used',colnames(.stdEndEnv$MainData)),container=TimeVarFrm, fill=TRUE,
handler= function(h,...){
#Update Value CBox
.stdEndEnv$TimeVar<-svalue(.stdEndEnv$TimeVarCbx)
delete(.stdEndEnv$TimeValFrm,.stdEndEnv$TimeValCbx)
#Start code for time format
if (identical(svalue(.stdEndEnv$TimeVarCbx),'Not Used')==FALSE){

.stdEndEnv$TimeValCbx<-gcombobox(as.vector(.stdEndEnv$CurrentDateList),selected=1,fill=TRUE,
handler= function(h,...){
#Format box updates the data
.stdEndEnv$Format<-FindFormat(svalue(.stdEndEnv$TimeValCbx),.stdEndEnv$CurrentDate)
if (identical(.stdEndEnv$TimeVar,'Not Used')==FALSE){
.stdEndEnv$TimeTemp<-{}
try(.stdEndEnv$TimeTemp<-date2Numeric(.stdEndEnv$MainData[ ,.stdEndEnv$TimeVar],.stdEndEnv$Format))
#Tell the user it did not work.
if (length(.stdEndEnv$TimeTemp)==0){
popMessage('Incompatible date format please reselect date format. ')
return()
}
if (sum(is.na(.stdEndEnv$TimeTemp))==length(.stdEndEnv$TimeTemp)){
popMessage('Incompatible date format please reselect date format. ')
return()
}
if (sum(.stdEndEnv$TimeTemp, na.rm=TRUE)==0){
popMessage('Incompatible date format please reselect date format. ')
return()
}

}
#Tell the User the problematic rows
if (sum(is.na(.stdEndEnv$TimeTemp))>0){
RowList<-which(is.na(.stdEndEnv$TimeTemp)==TRUE)
RowMessage<-RowList[1]
if (length(RowList)>1){
for (item in 1:{length(RowList)-1}){
RowMessage<-paste(RowMessage,', ',RowList[item],sep='')
}
RowMessage<-paste(RowMessage,', and ',RowList[length(RowList)])
}
TimeErrorMessage<-paste('Time formatting has lead to missing or unreadable data in Rows:\n ',RowMessage,
'\nPlease correct or remove the rows from the data file and then reload the data set',sep='')
popMessage(TimeErrorMessage)
return ()
}
})
#End code for time format
}else{
.stdEndEnv$TimeValCbx<-gcombobox(c('Variable Not Selected'), fill=TRUE)
}
add(.stdEndEnv$TimeValFrm,.stdEndEnv$TimeValCbx)
})

.stdEndEnv$TimeValFrm<-gframe('Time Format',horizontal = FALSE, container=TimeControlGrp, fill=TRUE)
.stdEndEnv$TimeValCbx<-gcombobox(c('Variable Not Selected'), fill=TRUE)
add(.stdEndEnv$TimeValFrm,.stdEndEnv$TimeValCbx)

TimeIntFrm<-gframe('Analysis Interval',horizontal = FALSE, container=TimeControlGrp, fill=TRUE)
.stdEndEnv$TimeIntCbx<-gcombobox(1:40,selected=21,container=TimeIntFrm, fill=TRUE,editable = TRUE)

TimeIntGraphFrm<-gframe('Graph Interval',horizontal = FALSE, container=TimeControlGrp, fill=TRUE)
.stdEndEnv$TimeIntGraphCbx<-gcombobox(1:40,selected=7,container=TimeIntGraphFrm, fill=TRUE,editable = TRUE)

TimeExcludeGrp<-ggroup(horizontal = TRUE, container=.stdEndEnv$EndpointControlFrm)
TimeExcludeButton<-gbutton("Select Excluded Times",container=TimeExcludeGrp,
handler= function(h,.){
if (identical(.stdEndEnv$TimeVar,'Not Used')==TRUE){
popMessage('Please Select Date/Time Variable First')
return()
}


.stdEndEnv$TimeExcludeVal<-NULL
selectPara('TimeExcludeVal',NULL,'.stdEndEnv',levels(as.factor(.stdEndEnv$MainData[ ,.stdEndEnv$TimeVar])),TRUE,'Excluded Times')

while (is.null(.stdEndEnv$TimeExcludeVal) == TRUE){
	Sys.sleep(0.01)
}
Display<-.stdEndEnv$TimeExcludeVal[1]
	if (length(.stdEndEnv$TimeExcludeVal)>1){
		for (e in .stdEndEnv$TimeExcludeVal){
			Display<-paste(Display,e,sep=', ')
		}
	}
	svalue(.stdEndEnv$TimeExcludeLab)<-Display
})

.stdEndEnv$TimeExcludeLabelFrm<-gframe('Excluded Times',horizontal = TRUE, container=TimeExcludeGrp,fill=TRUE,expand=TRUE)
.stdEndEnv$TimeExcludeLab<-glabel('Nothing Excluded', container=.stdEndEnv$TimeExcludeLabelFrm)


##################################################################################################################
#													End Button 												     #
##################################################################################################################

#This function will prep the data with the select inputs
EndButton<-gbutton("Confirm Selected Values and Variables",container=OverGp,fill=TRUE,expand=TRUE,
	handler= function(h,...){
	#Check for a treatment and response variable 
	
	Message<-''
	if (svalue(.stdEndEnv$TreatmentVarCbx)=='Not Used'){
		Message<-c(Message,'A treatment variable still needs to be selected.\n')
	}
	if (is.null(.stdEndEnv$EndPointVar)==TRUE){
		Message<-c(Message,'At least one endpoint needs to be selected.\n')
	}
	if (identical(Message,'')==FALSE){
		popMessage(Message)
		return()
		}
	#Update the R Environment from the GUI, subset the data and  
	.stdEndEnv$CanRun<-.updateEnviroment()
	
	for (e in .stdEndEnv$EndPointVar){
		Transform<-svalue(.stdEndEnv$EndpointObject[[e]]$TransformationCbx)
		transformationWarning(.stdEndEnv$MainData,Transform,e)
	}
	
	if (.stdEndEnv$CanRun==FALSE){
		return()
	}
#Apply the time averaging to each endpoint
if (identical(.stdEndEnv$TimeVar, 'Not Used')==FALSE){
	.stdEndEnv$UseData<-.getTimeData(.stdEndEnv$UseData,.stdEndEnv$TimeVar,.stdEndEnv$Format,.stdEndEnv$TimeInt,.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)

	#Change colname to 'Time' to update graphing
	colnames(.stdEndEnv$DataSub)[which(colnames(.stdEndEnv$DataSub)==.stdEndEnv$TimeVar)]<-'Time'
	.stdEndEnv$TimeVar<-'Averaged_Numeric_Time'
}


popMessage('The analysis can be ran from the main tab.') 
add(.stdEndEnv$ButtonBox,.stdEndEnv$RunButton)

#Display the New Data Set
.stdEndEnv$DisplayData<-.stdEndEnv$UseData
delete(.stdEndEnv$DataBox,.stdEndEnv$DataGrid)
.stdEndEnv$DataGrid<-gtable(.stdEndEnv$DisplayData, expand=TRUE )
add(.stdEndEnv$DataBox,.stdEndEnv$DataGrid, expand=TRUE)
})
}
