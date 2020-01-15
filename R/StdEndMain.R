StdEndMain <-
function(){
#' @export
#Main Gui call for analysis of other endpoints

.stdEndEnv$TimeVar<-'Not Used'
.stdEndEnv$TimeInt<-21    #Time interval used 
.stdEndEnv$TimeIntGraph<-7 #Time interval used for graphing
.stdEndEnv$TimeExcludeVal<-{}
.stdEndEnv$GenerationVar<-'Not Used'
.stdEndEnv$GenerationVal<-''  #Can be a Character array
.stdEndEnv$AgeVar<-'Not Used'
.stdEndEnv$AgeVal<-'Not Used'
.stdEndEnv$ReplicateVar<-'Not Used'
.stdEndEnv$TreatmentVar<-'Not Used'
.stdEndEnv$Format<-"%m/%d/%Y"
.stdEndEnv$Results<-list()
.stdEndEnv$WeightsVar<-'Not Used'
.stdEndEnv$AlphaLevel<-0.05

.stdEndEnv$Message<-{}
.stdEndEnv$TestDirection<-'Decreasing'

.stdEndEnv$CanRun<-FALSE
.stdEndEnv$EndPointVar<-{}

CurrentDate<-rbind(format(Sys.Date(),'%Y-%m-%d'),format(Sys.Date(),'%m-%d-%Y'),format(Sys.Date(),'%d-%m-%Y'),
format(Sys.Date(),'%Y/%m/%d'),format(Sys.Date(),'%m/%d/%Y'),format(Sys.Date(),'%d/%m/%Y'),
format(Sys.Date(),'%Y-%b-%d'),format(Sys.Date(),'%b-%d-%Y'),format(Sys.Date(),'%d-%b-%Y'))
.stdEndEnv$CurrentDateList<-rbind(CurrentDate,'Numeric','                ')
.stdEndEnv$CurrentDate<-CurrentDate


.stdEndEnv$MultiWindow<-gwindow("Other Endpoints", visible=FALSE)
#size(.stdEndEnv$MultiWindow)<-c(784,536)
size(.stdEndEnv$MultiWindow)<-c(800,600)
.stdEndEnv$MultiNotebook <- gnotebook(container =.stdEndEnv$MultiWindow, expand=TRUE)
.stdEndEnv$Maingroup <- ggroup(horizontal = TRUE)
add(.stdEndEnv$MultiNotebook,.stdEndEnv$Maingroup, label='Main')
.stdEndEnv$ButtonBox<-gframe(horizontal = FALSE, container=.stdEndEnv$Maingroup)

LoadButton.mg<-gbutton("Load Data",container=.stdEndEnv$ButtonBox,handler= function(h,.){
temp<-gtkWindow(show=FALSE)
.stdEndEnv$MainData<- openCB(temp)
delete(.stdEndEnv$DataBox,.stdEndEnv$DataGrid)
.stdEndEnv$DataGrid<-gtable(.stdEndEnv$MainData)
add(.stdEndEnv$DataBox,.stdEndEnv$DataGrid,expand=TRUE)
add(.stdEndEnv$ButtonBox,.stdEndEnv$SpecButton)
}) 

.stdEndEnv$SpecButton<-gbutton("Specify Data",handler= function(h,.){
addMultiSpec.std(.stdEndEnv$MultiNotebook)
delete(.stdEndEnv$ButtonBox,.stdEndEnv$SpecButton)
})
.stdEndEnv$RunButton<-gbutton("Run Analysis",handler= function(h,.){
if (.stdEndEnv$CanRun==FALSE){
popMessage('please specify the data first.')
}

if (.stdEndEnv$CanRun==TRUE){
.stdEndEnv$WilkTests<-{}
.stdEndEnv$LevenesTests<-{}
.stdEndEnv$Results<-list()
.stdEndEnv$TestType<-list()

popMessage('The analysis is now running, this may take a few minutes.')
if (length(unique(.stdEndEnv$UseData[[.stdEndEnv$TreatmentVar]])) == 2){
		popMessage('There are only two treatment levels.\nStatCharrms will skip using the JT.')
}

Seed<- abs(sum(as.numeric(.Random.seed)))
Seed<- Seed-signif(Seed,6)
for (Response in .stdEndEnv$EndPointVar){
set.seed(Seed)
Transform<-svalue(.stdEndEnv$EndpointObject[[Response]]$TransformationCbx)
	if (identical(Transform,'Transformation')==TRUE){
	Transform<-'None'
}


TestType<-svalue(.stdEndEnv$EndpointObject[[Response]]$TestTypeCbx)
if (identical(TestType,'Test Type')==TRUE){
TestType<-'Auto'
}


if (TestType == 'Auto'){ 
#Check for Multiple treatment levels V0.93
.stdEndEnv$Results[[Response]]<-autoStdAnylsis(.stdEndEnv$UseData,Response,.stdEndEnv$TreatmentVar,
Transform,.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,.stdEndEnv$AlphaLevel)

}

if (TestType!= 'Auto'){

.stdEndEnv$Results[[Response]]<-forceStdAnalysis(.stdEndEnv$UseData,Response,.stdEndEnv$TreatmentVar, Transform,.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,
.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,TestType,.stdEndEnv$AlphaLevel)
}
.stdEndEnv$Results[[Response]]$TestType<-TestType
}

}
buildResultsWindow(.stdEndEnv$Results)
})

SaveButton.mg<-gbutton("Save Result",handler= function(h,.){
stemp<-gtkWindow(show=FALSE)
saveCB(stemp,.stdEndEnv$Results)
})
.stdEndEnv$DataBox<-gframe(horizontal = FALSE, container=.stdEndEnv$Maingroup, label='Histopath Main',expand=TRUE)
blankDF = data.frame(variables=character(0), stringsAsFactors=FALSE)
.stdEndEnv$DataGrid<-gtable(blankDF,  expand=TRUE )
add(.stdEndEnv$DataBox,.stdEndEnv$DataGrid, expand=TRUE)

visible(.stdEndEnv$MultiWindow)<-TRUE
}
