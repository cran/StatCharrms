Time2EventMain <-
function(){
#' @export
#This is the main GUI call for time to event data
#Initialize Variables  
.time2EventEnv$TreatmentVar<-'Not Used'
.time2EventEnv$ReplicateVar<-'Not Used'
.time2EventEnv$TimeVar<-'Not Used'
.time2EventEnv$StatusVar<-'Not Used'
.time2EventEnv$StatusEventVal<-'Not Used'
.time2EventEnv$StatusCenVal<-'Not Used'
.time2EventEnv$CanRun<-FALSE
.time2EventEnv$TimeTemp<-{}
.time2EventEnv$Added.StatusVal<-0  #Used to control the control boxes
.time2EventEnv$GenderVar<-'Not Used'
.time2EventEnv$GenerationVar<-'Not Used'

.time2EventEnv$MainWindow<-gwindow("Time to Event Analysis", visible=FALSE)
size(.time2EventEnv$MainWindow)<-c(800,600)
.time2EventEnv$T2ENotebook <- gnotebook(container =.time2EventEnv$MainWindow, expand=TRUE)
.time2EventEnv$Maingroup <- ggroup(horizontal = TRUE)
add(.time2EventEnv$T2ENotebook,.time2EventEnv$Maingroup, label='Main')
.time2EventEnv$ButtonBox<-gframe(horizontal = FALSE, container=.time2EventEnv$Maingroup, label='Time to Event Main')
.time2EventEnv$LoadButton<-gbutton("Load Data",container=.time2EventEnv$ButtonBox,handler= function(h,...){
temp<-gtkWindow(show=FALSE)
.time2EventEnv$MainData<-openCB(temp)
delete(.time2EventEnv$DataBox,.time2EventEnv$DataGrid)
.time2EventEnv$DataGrid<-gtable(.time2EventEnv$MainData)
add(.time2EventEnv$DataBox,.time2EventEnv$DataGrid,expand=TRUE)


if (.time2EventEnv$Added.StatusVal == 1){
delete(.time2EventEnv$ButtonBox,.time2EventEnv$SpecGroup)
}
addSpec.te()
.time2EventEnv$Added.StatusVal<-1
Sys.sleep(.1)
add(.time2EventEnv$ButtonBox,.time2EventEnv$RunButton)
}) 


.time2EventEnv$RunButton<-gbutton("Run Analysis",handler= function(h,...){

.time2EventEnv$CanRun<-.updateT2E() #Checks everything 

if (.time2EventEnv$CanRun==FALSE){
return()
}
if (.time2EventEnv$CanRun==TRUE){
.time2EventEnv$Results<-analyseTime2Effect(.time2EventEnv$UseData,.time2EventEnv$StatusVar,.time2EventEnv$TimeVar,.time2EventEnv$TreatmentVar,.time2EventEnv$ReplicateVar)

delete(.time2EventEnv$DataBox,.time2EventEnv$DataGrid)
.time2EventEnv$DataGrid<-gtable(.time2EventEnv$UseData)
add(.time2EventEnv$DataBox,.time2EventEnv$DataGrid,expand=TRUE)

buildResultsWindow.te(.time2EventEnv$Results)

}
})

.time2EventEnv$SaveButton<-gbutton("Save Result",handler= function(h,...){
stemp<-gtkWindow(show=FALSE)
saveCB(stemp,.time2EventEnv$Results)
})
.time2EventEnv$DataBox<-gframe(horizontal = FALSE, container=.time2EventEnv$Maingroup,expand=TRUE)
blankDF = data.frame(variables=character(0), stringsAsFactors=FALSE)
.time2EventEnv$DataGrid<-gtable(blankDF,  expand=TRUE )
add(.time2EventEnv$DataBox,.time2EventEnv$DataGrid, expand=TRUE)

visible(.time2EventEnv$MainWindow)<-TRUE
}
