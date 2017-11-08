buildResultsWindow <-
function(Results){
#' @export
#This window will display the Results of the One-Way Anova Test 
ResultsWindow<-gwindow("Results", visible=FALSE)
size(ResultsWindow)<-c(800,600) 
AnovaResultsNotebook <- gnotebook(container =ResultsWindow, expand=TRUE)
#Each Result will get a page
for (i in .stdEndEnv$EndPointVar){if (is.null(Results[[i]])==FALSE){  #Add is.null to ensure that the response is there
	CurrentRespose<-i

	ResultsMaingroup <- ggroup(horizontal = FALSE,container =AnovaResultsNotebook,label=paste(Results[[i]]$Response,'Results'))
	# Button to save these tables
	saveResultsButton<-gbutton("Save All Results",container=ResultsMaingroup,
	handler= function(h,...){ 
	stemp<-gtkWindow(show=FALSE)
	Dir<-getDir(stemp)
	if (Dir==' '){
	popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
	return()
	}
	.stdEndEnv$FileName<-' '
	stemp<-gtkWindow(show=FALSE)
	.stdEndEnv$FileName<-getFileName()
	if (.stdEndEnv$FileName==' '){
		popMessage('File name needed. Save aborted!')
		return()
	}
		for (j in 1:length(Results)){   #Save Every Result
			.saveResults(Results[[j]],Dir,.stdEndEnv$FileName)
		}
	})
	# Button to save everything
	SaveAllButton<-gbutton("Save All Results and Graphs",container=ResultsMaingroup,
	handler= function(h,...){ 
	stemp<-gtkWindow(show=FALSE)
	Dir<-getDir(stemp)
	if (Dir==' '){
	popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
	return()
	}
	.stdEndEnv$FileName<-' '
	stemp<-gtkWindow(show=FALSE)
	.stdEndEnv$FileName<-getFileName()
	if (.stdEndEnv$FileName==' '){
	popMessage('File name needed. Save aborted!')
	return()
	}
	for (j in 1:length(Results)){   #Save Every Result
	.saveResults(Results[[j]],Dir,.stdEndEnv$FileName)
	}
	.saveGraphs(Dir,.stdEndEnv$FileName)
	})
	#------------------------------------------------------------------------------------------------------------------------------------
	#Results Tables 
	#------------------------------------------------------------------------------------------------------------------------------------

	#Determines What Box is Expandable 
	DunnsExpand=TRUE
	DunnetExpand=FALSE
	JTExpand=FALSE
	WilliamsExpand=TRUE
	
	
	if (is.null(Results[[i]]$DunnsTable)==TRUE){
		DunnetExpand=TRUE
		if(is.null(Results[[i]]$OneWayDunnetResults)==TRUE){
			JTExpand=TRUE
		}
	}

	SummaryLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
	ResultsSummaryLab<-glabel('Summary Table ',container=SummaryLabBox,where='center') #Summary Table
	ResultsSummaryGrid<-gtable(Results[[i]]$SummaryTable, container=ResultsMaingroup,where='center')
	size(ResultsSummaryGrid)<-c(1,110)
	#Mono-Table
	if (is.null(Results[[i]]$MonocityTable)==FALSE){
		MonocityTableLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		MonocityTableLab<-glabel('Test For Monotonicity ',container=MonocityTableLabBox,where='center') 
		MonocityTableGrid<-gtable(Results[[i]]$MonocityTable, container=ResultsMaingroup,where='center')
		size(MonocityTableGrid)<-c(1,60)
	}
	#Jonckheere-Terpstra
	if (is.null(Results[[i]]$JonckheereTerpstraResults)==FALSE){
		JonckheereLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsJonckheereLab<-glabel('Jonckheere-Terpstra Table ',container=JonckheereLabBox,where='center') #Jonckheere-Terpstra Table
		ResultsJonckheereGrid<-gtable(Results[[i]]$JonckheereTerpstraResults, container=ResultsMaingroup,where='center',expand=JTExpand)
		size(ResultsJonckheereGrid)<-c(1,90)
	}

	#This will check for the existence of a table and print it if it exists
	if (is.null(Results[[i]]$WilksResults)==FALSE){
		WilksLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsWilksLab<-glabel('Wilks Table ',container=WilksLabBox,where='center') #Wilks Table
		ResultsWilksGrid<-gtable(Results[[i]]$WilksResults, container=ResultsMaingroup,where='center')
		size(ResultsWilksGrid)<-c(1,28)
	}
	if (is.null(Results[[i]]$LeveneResults)==FALSE){
		LeveneLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsLeveneLab<-glabel('Levene Table ',container=LeveneLabBox,where='center') #Leven Table
		ResultsLeveneGrid<-gtable(Results[[i]]$LeveneResults, container=ResultsMaingroup,where='center')
		size(ResultsLeveneGrid)<-c(1,28)
	}
	if (is.null(Results[[i]]$AnovaResults)==FALSE){
		AnovaLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsAnovaLab<-glabel('Anova Table ',container=AnovaLabBox,where='center') #Anova Table
		AnovaDisplay<-cbind(rownames(Results[[i]]$AnovaResults),Results[[i]]$AnovaResults)
		colnames(AnovaDisplay)[1]<-''
		ResultsAnovaGrid<-gtable(AnovaDisplay, container=ResultsMaingroup,where='center')
		size(ResultsAnovaGrid)<-c(1,60)
	}
	if (is.null(Results[[i]]$OneWayDunnetResults)==FALSE){
		DunnetLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsDunnetLab<-glabel('Dunnett Table ',container=DunnetLabBox,where='center') #Dunnet Table
		ResultsDunnetGrid<-gtable(Results[[i]]$OneWayDunnetResults, container=ResultsMaingroup,where='center',expand=DunnetExpand)
		size(ResultsDunnetGrid)<-c(1,90)
	}
	if (is.null(Results[[i]]$DunnsTable)==FALSE){
		DunnsLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsDunnstLab<-glabel('Dunns Table ',container=DunnsLabBox,where='center') #Dunns Table
		ResultsDunnsGrid<-gtable(Results[[i]]$DunnsTable, container=ResultsMaingroup,where='center',expand=DunnsExpand)
	}
	
	#size(ResultsDunnsGrid)<-c(1,90)
	#Added Williams Table 2017-10-17
	if (is.null(Results[[i]]$WilliamsTableUp)==FALSE){
		Expand=is.null(Results[[i]]$WilliamsTableDown)==TRUE
		
		WilliamsUpLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsWilliamsUpLab<-glabel('Williams Table for Increasing Trend ',container=WilliamsUpLabBox,where='center') #Williams Table
		ResultsWilliamsUpGrid<-gtable(Results[[i]]$WilliamsTableUp, container=ResultsMaingroup,where='center',expand=Expand)
	}
	if (is.null(Results[[i]]$WilliamsTableDown)==FALSE){
		WilliamsDownLabBox<-gframe(horizontal = FALSE,container=ResultsMaingroup)
		ResultsWilliamsDownLab<-glabel('Williams Table for Decreasing Trend',container=WilliamsDownLabBox,where='center') #Williams Table
		ResultsWilliamsDownGrid<-gtable(Results[[i]]$WilliamsTableDown, container=ResultsMaingroup,where='center',expand=TRUE)
	}
	
}}

#------------------------------------------------------------------------------------------------------------------------------------
#Graphs
#------------------------------------------------------------------------------------------------------------------------------------
visible(ResultsWindow)<-TRUE
#Because of how ggraphics works, I can only have one graphing window at a time
GraphTab <- ggroup(horizontal = TRUE, container=AnovaResultsNotebook, label='Graphs')
GraphControlBox<-gframe(horizontal = FALSE, container=GraphTab)
GraphAddBox<-gframe(horizontal = FALSE) #Contains the button to view graphs
add(GraphControlBox,GraphAddBox)

#Define the save buttons
#Define the save all button
AllSaveButton<-gbutton("Save All Results and Graphs",
handler= function(h,...){ 
stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.stdEndEnv$FileName<-' '
stemp<-gtkWindow(show=FALSE)
.stdEndEnv$FileName<-getFileName()
if (.stdEndEnv$FileName==' '){
popMessage('File name needed. Save aborted!')
return()
}
for (i in .stdEndEnv$EndPointVar){   #Save Every Result
.saveResults(Results[[i]],Dir,.stdEndEnv$FileName)
}
.saveGraphs(Dir,.stdEndEnv$FileName)
})

#Define the save all button
GraphSaveButton<-gbutton("Save Graph",
handler= function(h,...){ 
stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.stdEndEnv$FileName<-' '
stemp<-gtkWindow(show=FALSE)
.stdEndEnv$FileName<-getFileName()
if (.stdEndEnv$FileName==' '){
popMessage('File name needed. Save aborted!')
return()
}
.saveGraphs(Dir,.stdEndEnv$FileName)
})
ResultSaveButton<-gbutton("Save Results and Graphs for This Response",
handler= function(h,...){ 
stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.stdEndEnv$FileName<-' '
stemp<-gtkWindow(show=FALSE)
.stdEndEnv$FileName<-getFileName()
if (.stdEndEnv$FileName==' '){
popMessage('File name needed. Save aborted!')
return()
}
.saveResults(Results[[i]],Dir,.stdEndEnv$FileName)
.saveGraph(Dir,.stdEndEnv$FileName,.stdEndEnv$Response.graph)
})


#Add the buttons that save the results
GraphSaveBox<-gframe(horizontal = FALSE, container=GraphControlBox,expand=TRUE)
add(GraphSaveBox,GraphSaveButton)
add(GraphSaveBox,ResultSaveButton)
add(GraphSaveBox,AllSaveButton)






#Defaults and lists
.stdEndEnv$Response.graph<-.stdEndEnv$EndPointVar[1]
.stdEndEnv$Scale<-FALSE #If it is FALSE it plots in the original .stdEndEnv$Scale, if it is TRUE it plots in the transformed .stdEndEnv$Scale
.stdEndEnv$PlotType<-'Box'
.stdEndEnv$PlotTypeList<-c('Box','Quantile-Quantile','Violin')
.stdEndEnv$PlotData<-.stdEndEnv$UseData
.stdEndEnv$XVar<-.stdEndEnv$TreatmentVar
if (identical(svalue(.stdEndEnv$TimeVarCbx),'Not Used')==FALSE){
.stdEndEnv$PlotTypeList<-c(.stdEndEnv$PlotTypeList,'Interaction')
}
.stdEndEnv$ByVar=NULL

#Select Response
ResponseCBoxLabel<-glabel('Choose Response',container=GraphAddBox,where='center') #Wilks Table
.stdEndEnv$ResponseCBoxEnter<-gcombobox(.stdEndEnv$EndPointVar, selected = 1, editable = FALSE, container = GraphAddBox,
handler= function(h,...){
.stdEndEnv$Response.graph<-svalue(.stdEndEnv$ResponseCBoxEnter)
.makePlot(.stdEndEnv$PlotData,.stdEndEnv$Response.graph,Results,.stdEndEnv$EndPointVar,
.stdEndEnv$XVar,.stdEndEnv$Scale,.stdEndEnv$PlotType,.stdEndEnv$ByVar)
})
#Select Graph Type
GraphTypeCBoxLabel<-glabel('Choose Graph Type',container=GraphAddBox,where='center') #Wilks Table
.stdEndEnv$GraphTypeCBoxEnter<-gcombobox(.stdEndEnv$PlotTypeList, selected = 1, editable = FALSE, container = GraphAddBox,
handler= function(h,...){
.stdEndEnv$PlotType<-svalue(.stdEndEnv$GraphTypeCBoxEnter)
.makePlot(.stdEndEnv$PlotData,.stdEndEnv$Response.graph,Results,.stdEndEnv$EndPointVar,
.stdEndEnv$XVar,.stdEndEnv$Scale,.stdEndEnv$PlotType,.stdEndEnv$ByVar)
})
#Select Scale
.stdEndEnv$ScaleTypeCBoxLabel<-glabel('Use Transformed Scale',container=GraphAddBox,where='center') #Wilks Table
.stdEndEnv$ScaleTypeCBoxEnter<-gcombobox(c('No','Yes'), selected = 1, editable = FALSE, container = GraphAddBox,
handler= function(h,...){
if (svalue(.stdEndEnv$ScaleTypeCBoxEnter)=='No'){.stdEndEnv$Scale<-FALSE}
if (svalue(.stdEndEnv$ScaleTypeCBoxEnter)=='Yes'){.stdEndEnv$Scale<-TRUE}
.makePlot(.stdEndEnv$PlotData,.stdEndEnv$Response.graph,Results,.stdEndEnv$EndPointVar,
.stdEndEnv$XVar,.stdEndEnv$Scale,.stdEndEnv$PlotType,.stdEndEnv$ByVar)
})

#Select Time-Treatment Interaction
if (identical(svalue(.stdEndEnv$TimeVarCbx),'Not Used')==FALSE){ #Test to see if Time is being used 
.stdEndEnv$ByVar='Averaged_Numeric_Time'
XVarCBoxLabel<-glabel('Group Variable',container=GraphAddBox,where='center') #Wilks Table
.stdEndEnv$XVarCBoxEnter<-gcombobox(c(.stdEndEnv$TreatmentVar,'Time'), selected = 1, editable = FALSE, container = GraphAddBox,
handler= function(h,...){
.stdEndEnv$XVar<-svalue(.stdEndEnv$XVarCBoxEnter)
if (.stdEndEnv$XVar=='Time'){
.stdEndEnv$ByVar<-.stdEndEnv$TreatmentVar
.stdEndEnv$PlotData<-.getTimeData(.stdEndEnv$DataSub,'Time',.stdEndEnv$Format,.stdEndEnv$TimeIntGraph,.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)
colnames(.stdEndEnv$PlotData)[which(colnames(.stdEndEnv$PlotData)=='Averaged_Numeric_Time')]<-'Time'
.stdEndEnv$PlotData$Time<-as.factor(.stdEndEnv$PlotData$Time)
}else{
.stdEndEnv$ByVar<-'Time'
}
.makePlot(.stdEndEnv$PlotData,.stdEndEnv$Response.graph,Results,.stdEndEnv$EndPointVar,
.stdEndEnv$XVar,.stdEndEnv$Scale,.stdEndEnv$PlotType,.stdEndEnv$ByVar)
})

GraphTimeIntLabel<-glabel('Time Interval',container=GraphAddBox,where='center') #Wilks Table
N<-max(as.numeric(as.character(levels(.stdEndEnv$UseData[ ,.stdEndEnv$TimeVar]))))*.stdEndEnv$TimeInt+1
.stdEndEnv$GraphTimeIntValCbx<-gcombobox(1:N, selected = .stdEndEnv$TimeIntGraph, editable = FALSE, container = GraphAddBox,
handler= function(h,...){

.stdEndEnv$TimeIntGraph<-svalue(.stdEndEnv$GraphTimeIntValCbx)
.stdEndEnv$PlotData<-.getTimeData(.stdEndEnv$DataSub,'Time',.stdEndEnv$Format,.stdEndEnv$TimeIntGraph,.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)

colnames(.stdEndEnv$PlotData)[which(colnames(.stdEndEnv$PlotData)=='Averaged_Numeric_Time')]<-'Time'
.stdEndEnv$PlotData$Time<-as.factor(.stdEndEnv$PlotData$Time)

.makePlot(.stdEndEnv$PlotData,.stdEndEnv$Response.graph,Results,.stdEndEnv$EndPointVar,
.stdEndEnv$XVar,.stdEndEnv$Scale,.stdEndEnv$PlotType,.stdEndEnv$ByVar)

})

}

GraphMainBox<-gframe(horizontal = FALSE, container=GraphTab,expand=TRUE)
GG<-ggraphics(container=GraphMainBox)
#Graphs.... 
#Box plot in original form, Starting Graph
print(bwplot(.stdEndEnv$UseData[ ,.stdEndEnv$Response.graph[1]]~.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar],main=paste(.stdEndEnv$Response.graph[1],'for each',.stdEndEnv$TreatmentVar),
xlab=.stdEndEnv$TreatmentVar,ylab=.stdEndEnv$Response.graph[1], horizontal = FALSE))
}
