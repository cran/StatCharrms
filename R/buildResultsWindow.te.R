buildResultsWindow.te <-
function(Results){
#' @export
#This will add a Tab of results to the Cox-Me table
.time2EventEnv$ResultsWindow<-gwindow("Time to Event Results", visible=TRUE)
size(.time2EventEnv$ResultsWindow)<-c(800,600)
#Two Pages Results and Graph
#Tables Page
.time2EventEnv$ResultsTab <- ggroup(horizontal = FALSE, container=.time2EventEnv$ResultsWindow, label='Results')
.time2EventEnv$ResultsTabUp <- ggroup(horizontal = TRUE, container=.time2EventEnv$ResultsTab, label='Results')
.time2EventEnv$ResultsButtonBox<-gframe(horizontal = FALSE, container=.time2EventEnv$ResultsTabUp)
#Define buttons 
.time2EventEnv$ResultsSaveButton<-gbutton("Save Results",handler= function(h,...){
stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.time2EventEnv$FileName<-' '
stemp<-gtkWindow(show=FALSE)
.time2EventEnv$FileName<-getFileName()
if (.time2EventEnv$FileName==' '){
popMessage('File name needed. Save aborted!')
return()
}
.saveResults.te(Results) 
})
#Save Graphs
.time2EventEnv$GraphSaveButton<-gbutton("Save Graphs Only",handler= function(h,...){
  stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.time2EventEnv$FileName<-' '
stemp<-gtkWindow(show=FALSE)
.time2EventEnv$FileName<-getFileName()
if (.time2EventEnv$FileName==' '){
popMessage('File name needed. Save aborted!')
return()
}
.saveGraphs.te(Dir,.time2EventEnv$FileName)
})
#Save Graphs and Results
.time2EventEnv$AllSaveButton<-gbutton("Save Everything",handler= function(h,...){
stemp<-gtkWindow(show=FALSE)
Dir<-getDir(stemp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.time2EventEnv$FileName<-' '
stemp<-gtkWindow(show=FALSE)
.time2EventEnv$FileName<-getFileName()
if (.time2EventEnv$FileName==' '){
popMessage('File name needed. Save aborted!')
return()
}
.saveGraphs.te(Dir,.time2EventEnv$FileName)
.saveResults.te(Results,Dir,.time2EventEnv$FileName)
})

#Add Buttons
add(.time2EventEnv$ResultsButtonBox,.time2EventEnv$AllSaveButton)
add(.time2EventEnv$ResultsButtonBox,.time2EventEnv$ResultsSaveButton)
add(.time2EventEnv$ResultsButtonBox,.time2EventEnv$GraphSaveButton)

#Add Results Tables
#Blank DF for use
blankDF = data.frame(variables=character(0), stringsAsFactors=FALSE)

#Main Results Box
.time2EventEnv$GraphBox<-gframe(horizontal = FALSE, container=.time2EventEnv$ResultsTabUp,expand=TRUE)
.time2EventEnv$GG<-ggraphics(container=.time2EventEnv$GraphBox,expand=TRUE) #Plot container 


#Define color and line types
Colors=c("Black", "red", "blue", "orange","purple","green")

Lines = c(rep(1,ceiling(nlevels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]))/2)),
rep(2,floor(nlevels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]))/2)))


Sys.sleep(.1)
plot(Results$FitS, conf.int = FALSE, main='Plot of Raw Data By Treatment',xlab=.time2EventEnv$TimeVar,ylab='Percent In Original Status',
col = Colors,lty=Lines,lwd=1.5) 

legend('bottomleft',levels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar])),lty = Lines,
col =Colors) 

EffectsTable<-as.data.frame(Results$EffectsTable)
EffectsTable<-cbind(rownames(EffectsTable), EffectsTable)
colnames(EffectsTable)[1]<-'Comparison'

.time2EventEnv$ResultsTablebox<-gframe(horizontal = FALSE, container=.time2EventEnv$ResultsTab,expand=TRUE)
.time2EventEnv$ResultsDataGrid<-gtable(EffectsTable,container=.time2EventEnv$ResultsTablebox, expand=TRUE,fill=TRUE )
size(.time2EventEnv$ResultsDataGrid)<-c(1,90)

}
