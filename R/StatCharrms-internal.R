.changeStatusValues <-
function(){
#Changes the Cencerd Value to 0
#Changes the Event Value to 1
#' @export

	TempData<-.time2EventEnv$UseData
	#Change Event status to 1

	.time2EventEnv$UseData[which(TempData[ ,.time2EventEnv$StatusVar]==.time2EventEnv$StatusEventVal),.time2EventEnv$StatusVar]<-1
	if (length(which(TempData[ ,.time2EventEnv$StatusVar]==.time2EventEnv$StatusEventVal)) == 0){
	popMessage('There are no event. Analysis aborted.')
	return(FALSE)
	}

	if (identical(.time2EventEnv$StatusCenVal,'Not Used') == FALSE){
	#Change Censored status to 0
	.time2EventEnv$UseData[which(TempData[ ,.time2EventEnv$StatusVar]==.time2EventEnv$StatusCenVal),.time2EventEnv$StatusVar]<-0
	}
return(TRUE)
}
.getTimeData <-
function(Data,TimeVar,Format,TimeInt,ReplicateVar,TreatmentVar,EndPointVar){
#' @export
#Averages Time over multiple possible endpoints
Data[ ,TimeVar]<-date2Numeric(Data[ ,TimeVar],Format)
colnames(Data)[which(colnames(Data)==TimeVar)]<-'Numeric_Time'

TempData<-tranformTime(Data,'Numeric_Time',as.numeric(TimeInt),
ReplicateVar, TreatmentVar,EndPointVar[1])
if (length(EndPointVar)>1){
Remove<-which(is.element(colnames(TempData),EndPointVar[-1]))
TempData<-TempData[ -Remove]
for (i in 2:length(EndPointVar)){ 
TimeData<-tranformTime(Data,'Numeric_Time',as.numeric(TimeInt),
ReplicateVar, TreatmentVar,EndPointVar[i])

TimeData<-TimeData[ ,c('UniqueID.SC',EndPointVar[i])]
TempData<-merge(TempData,TimeData,by ='UniqueID.SC')
}
}
 TempData<-TempData[ ,-which(colnames(TempData)=='UniqueID.SC')]

return(TempData)
}
.lengthNa <-
function(Vec){
#' @export
#This is a length function that acts like a rm.na=TRUE option
if (sum(is.na(Vec))>0) Vec<-Vec[-which(is.na(Vec))]
return(length(Vec))
}
.makePlot <-
function(Data,Response,Results,EndPoints,TreatmentVar,Scale,PlotType,ByVar){
#' @export
#This function will make a plot
#uses 
DataUse<-Data

NameResponse<-Response
TrasnStr<-''
#Convert the response and names to indicate the transformation used
if (Scale==TRUE){  #Add Transform used to Title
	DataUse<-responseTransform(Data,NameResponse,Results[[NameResponse]]$TransformationUsed) #Transform Data
	#Filter out non-numbers
	if (length(which(is.na(DataUse$TransformedResponse)))>0){
		DataUse<-DataUse[-which(is.na(DataUse$TransformedResponse)), ]
	}
	if (length(which(is.finite(DataUse$TransformedResponse) == FALSE))>0){
		DataUse<-DataUse[-which(is.finite(DataUse$TransformedResponse) == FALSE), ]
	}
	TrasnStr<-paste(Results[[NameResponse]]$TransformationUsed,'Transformed')
	Response<-'TransformedResponse'
	if (Results[[NameResponse]]$TransformationUsed=='None'){ #If no transform was used tell user of that
		TrasnStr<-gsub('None','Not',TrasnStr)
		Response<-NameResponse
		DataUse<-Data
	}
}

#Box-Plot
if (PlotType=='Box'){
print(bwplot(DataUse[ ,Response]~DataUse[ ,TreatmentVar],
main=paste(NameResponse,'for each',TreatmentVar,'\n',TrasnStr),
xlab=TreatmentVar,ylab=paste(NameResponse,TrasnStr), horizontal = FALSE))
}
#Quantile-Quantile-Plot
if (PlotType=='Quantile-Quantile'){
Model<-aov(DataUse[ ,Response]~DataUse[ ,TreatmentVar])
qqnorm(Model$residuals,main=paste('Normal Q-Q Plot for\n',NameResponse,TrasnStr,'by',TreatmentVar))
qqline(Model$residuals)
}
#Violin-Plot
if (PlotType=='Violin'){
print(bwplot(DataUse[ ,Response]~DataUse[ ,TreatmentVar],
main=paste(NameResponse,'for each',TreatmentVar,'\n',TrasnStr),
xlab=TreatmentVar,ylab=paste(NameResponse,TrasnStr), horizontal = FALSE,panel=panel.violin))
}


if (PlotType=='Interaction'){
print(interaction.plot(DataUse[[ByVar]],DataUse[[TreatmentVar]],DataUse[[Response]],col=1:nlevels(DataUse[[TreatmentVar]])
,ylab=Response,xlab=ByVar,trace.label=TreatmentVar,
main=paste('Interaction plot for\n',Response,'by',ByVar)))
}

if (PlotType==''){
print(bwplot(DataUse[ ,Response]~DataUse[ ,TreatmentVar],
main=paste(NameResponse,'for each',TreatmentVar,'\n',TrasnStr),
xlab=TreatmentVar,ylab=paste(NameResponse,TrasnStr), horizontal = FALSE,panel=panel.violin))
}

}


.saveGraph <-
function(Dir,FileName,Response){ #One Response
#' @export
#This function saves graphs specificity from the GUI for the One-Way Anova Part
#It uses the global names for the data set and values

#Save graphs as PDF
options(warn=-1)
dir.create(Dir)
options(warn=0)

FileNameG<-paste(Dir,'\\',FileName,sep='')
#Start PDF Output
pdf(paste(FileNameG,Response,'-Graphs.pdf',sep=''))
for (Scale in c(FALSE,TRUE)){
for (PlotType in .stdEndEnv$PlotTypeList){  #PlotTypeList.sa is a global from the GUI
.makePlot(.stdEndEnv$PlotData,Response,.stdEndEnv$Results,.stdEndEnv$EndPointVar,
.stdEndEnv$TreatmentVar,Scale,PlotType,.stdEndEnv$ByVar)
}
}
#Ends PDF output
dev.off()
dev.set(dev.prev())
}
.saveGraphs <-
function(Dir,FileName){ #All Graphs 
#' @export
#This function saves graphs specificity from the GUI for the One-Way Anova Part
#It uses the global names for the data set and values

#Save graphs as PDF
dir.create(Dir , showWarnings = FALSE)



if (identical(.stdEndEnv$TimeVar,'Not Used')==FALSE){
.stdEndEnv$PlotData<-.getTimeData(.stdEndEnv$DataSub,.stdEndEnv$TimeVar,.stdEndEnv$Format,.stdEndEnv$TimeIntGraph,.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)
colnames(.stdEndEnv$PlotData)[which(colnames(.stdEndEnv$PlotData)=='Averaged_Numeric_Time')]<-'Time'
.stdEndEnv$PlotData$Time<-as.factor(.stdEndEnv$PlotData$Time)
}

FileNameG<-paste(Dir,'\\',FileName,sep='')

for (Response in .stdEndEnv$EndPointVar){
#Start PDF Output
pdf(paste(FileNameG,'-',Response,'-',.stdEndEnv$TreatmentVar,'-Graphs.pdf',sep=''))
for (Scale in c(FALSE,TRUE)){
for (PlotType in .stdEndEnv$PlotTypeList){  #PlotTypeList.sa is a global from the
.makePlot(.stdEndEnv$PlotData,Response,.stdEndEnv$Results,.stdEndEnv$EndPointVar,
.stdEndEnv$TreatmentVar,Scale,PlotType,'Time')
}
}
#Ends PDF output
dev.off()
dev.set(dev.prev())
}

#if time is used
if (identical(.stdEndEnv$TimeVar,'Not Used')==FALSE){
for (Response in .stdEndEnv$EndPointVar){
#Start PDF Output
pdf(paste(FileNameG,'-',Response,'-Time','-Graphs.pdf',sep=''))
for (Scale in c(FALSE,TRUE)){
for (PlotType in .stdEndEnv$PlotTypeList){  #PlotTypeList.sa is a global from the
.makePlot(.stdEndEnv$PlotData,Response,.stdEndEnv$Results,.stdEndEnv$EndPointVar,
'Time',Scale,PlotType,.stdEndEnv$TreatmentVar)
}

}
#Ends PDF output
dev.off()
dev.set(dev.prev())
}
}
}
.saveGraphs.te <-
function(Dir,FileName){ #One Response
#This function saves graphs specificity from the GUI for the Time to effect module 
#It uses the global names for the data set and values
#' @export

#Save graphs as PDF
options(warn=-1)
dir.create(Dir)
options(warn=0)

FileNameG<-paste(Dir,'\\',FileName,sep='')
#Start PDF Output
pdf(paste(FileNameG,'-Graphs.pdf',sep=''))
#Make the plot
Colors=c("Black", "red", "blue", "orange","purple","green")

Lines = c(rep(1,ceiling(nlevels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]))/2)),
rep(2,floor(nlevels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]))/2)))


Sys.sleep(.1)
plot(.time2EventEnv$Results$FitS, conf.int = FALSE, main='Plot of Raw Data By Treatment',xlab=.time2EventEnv$TimeVar,ylab='Percent In Original Status',
col = Colors,lty=Lines,lwd=1.5) 

legend('bottomleft',levels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar])),lty = Lines,
col =Colors) 
#Ends PDF output
dev.off()
dev.set(dev.prev())

}
.saveResults <-
function(Results,Dir,FileName){
#' @export
#This function will save the results of the of an mg analysis 
options(warn=-1)
dir.create(Dir)
options(warn=0)

#I want to put in a message to the user in the output
#Extract Results
Response<-Results$Response
SummaryTable<-Results$SummaryTable


WilksResults<-Results$WilksResults
LeveneResults<-Results$LeveneResults


AnovaResults<-Results$AnovaResults
OneWayDunnetResults<-Results$OneWayDunnetResults

JonckheereTerpstraResults<-Results$JonckheereTerpstraResults

MonocityTable<-Results$MonocityTable

DunnsTable<-Results$DunnsTable

WilliamsTableUp<-Results$WilliamsTableUp #2017-10-17
WilliamsTableDown<-Results$WilliamsTableDown #2017-10-17


Transform<-Results$TransformationUsed
TestType<-Results$TestType


FileNameUse<-paste(FileName,Response,'-')

#Start HTML OutPut
HTMLStart(outdir=Dir, filename= FileNameUse,
extension="html", echo=FALSE, HTMLframe=TRUE)  

Title<-paste('<center>Results from ',Response,'</center>',sep='')
HTML.title(Title, HR=1,CSSstyle='')

#HTML for Data Summary Table
if (is.null(SummaryTable)==FALSE){
HTML('<center><b>Data Summary Table for Untransformed/Unweighted Data </b></center>')
HTML(SummaryTable,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Monotonicity  Test
if (is.null(MonocityTable )==FALSE){
HTML('<center><b>Test for Monotonicity </b></center>')
HTML(MonocityTable ,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Jonckheere-Terpstra Test
if (is.null(JonckheereTerpstraResults )==FALSE){
HTML('<center><b>Jonckheere-Terpstra Test</b></center>')
HTML(JonckheereTerpstraResults ,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Shapiro Wilks Test Table
if (is.null(WilksResults)==FALSE){
HTML('<center><b>Shapiro Wilks Test for Normality</b></center>')
HTML(WilksResults,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Levene's Test Table
if (is.null(LeveneResults)==FALSE){
HTML("<center><b>Levene's test for equality of variances</b></center>")
HTML(LeveneResults,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Anova
if (is.null(AnovaResults)==FALSE){
HTML('<center><b>AnovaTable</b></center>')
HTML(AnovaResults,row.name=TRUE,innerBorder = 1,HR=1)}

#HTML for Dunnets Test
if (is.null(OneWayDunnetResults)==FALSE){
HTML('<center><b>Dunnets Test</b></center>')
HTML(OneWayDunnetResults,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Dunn Test
if (is.null(DunnsTable)==FALSE){
HTML('<center><b>Dunns Test</b></center>')
HTML(DunnsTable,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Williams Test 2017-10-18
if (is.null(WilliamsTableUp)==FALSE){
HTML('<center><b>Williams Test for Increasing Trend</b></center>')
HTML(WilliamsTableUp,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Williams Test 2017-10-18
if (is.null(WilliamsTableDown)==FALSE){
HTML('<center><b>Williams Test for Decreasing Trend</b></center>')
HTML(WilliamsTableDown,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#Stamp the output
.stdEndEnv$Message<-.stampOutput(Transform,TestType)

HTML(.stdEndEnv$Message)

HTMLStop() #End HTML OutPut
#Deletes Junk files
unlink(paste(Dir,'\\',FileNameUse,'.html',sep=''))
unlink(paste(Dir,'\\',FileNameUse,'_menu.html',sep='')) 
Sys.sleep(.01)
}
.saveResults.te <-
function(Results,Dir,FileName){
#' @export
#This function will save the results of the of an mg analysis 
options(warn=-1)
dir.create(Dir)
options(warn=0)


#I want to put in a message to the user in the output
#Extract Results

FileNameUse<-paste(FileName)

#Start HTML OutPut
HTMLStart(outdir=Dir, filename= FileNameUse,
extension="html", echo=FALSE, HTMLframe=TRUE)  

Title<-'<center>Results</center>'
HTML.title(Title, HR=1,CSSstyle='')

#Effects Table
EffectsTable<-as.data.frame(Results$EffectsTable)
EffectsTable<-cbind(rownames(EffectsTable), EffectsTable)
colnames(EffectsTable)[1]<-'Comparison'

.time2EventEnv$Results$FitMe$coefficients

#HTML for Data  Table
if (is.null(.time2EventEnv$UseData)==FALSE){
HTML('<center><b>Data Used in Analysis </b></center>')
HTML(.time2EventEnv$UseData,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Main Effects Results
if (is.null(EffectsTable)==FALSE){
HTML('<center><b>Main Effects Table</b></center>')
HTML(EffectsTable ,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Main Effects Results
MedianTable<-as.data.frame(Results$MedianTable)


if (is.null(MedianTable)==FALSE){
	if (sum(is.na(MedianTable)) > 0){
		NAs<-which(is.na(MedianTable)==TRUE,arr.ind = TRUE)
		MedianTable[NAs]<-'-'
	}
	HTML('<center><b>Median Time to Effect with 95% CI</b></center>')
	HTML(MedianTable ,row.name=FALSE,innerBorder = 1,CSSstyle='',nsmall=1)
}



#Stamp the output
.stampOutput.te()

HTMLStop() #End HTML OutPut
#Deletes Junk files
unlink(paste(Dir,'\\',FileNameUse,'.html',sep=''))
unlink(paste(Dir,'\\',FileNameUse,'_menu.html',sep='')) 
Sys.sleep(.01)
}
.stampOutput <-
function(Transform,TestType){
#' @export
#This function writes the user's selection to the output file created.
#This function assumes that it is called from the StdAnylsis saving function
.stdEndEnv$TempList<-strsplit(.stdEndEnv$FileName,'\\\\')  
.stdEndEnv$File<-.stdEndEnv$TempList[[1]][length(.stdEndEnv$TempList[[1]])]

Message1<-paste("<br>Use Test Type of: <b>",TestType,'</b>',"<br>Transformation of: <b>",Transform,'</b>',sep='')
Message2<-paste("<br>Using Generation variable: <b>",.stdEndEnv$GenerationVar,'</b>'," and Generation value: <b>",.stdEndEnv$GenerationVal,'</b>', sep='')
Message3<-paste("<br>Treatment Variable: <b>",.stdEndEnv$TreatmentVar,'</b>',"<br>Replicate Variable: <b>",.stdEndEnv$ReplicateVar,'</b>',sep='')
Message4<-paste("<br>Using Gender variable: <b>",.stdEndEnv$GenderVar,'</b>'," and Gender value: <b>",.stdEndEnv$GenderVal,'</b>', sep='')
Message5<-paste("<br>Using Age variable: <b>",.stdEndEnv$AgeVar,'</b>'," and Age value: <b>",.stdEndEnv$AgeVal,'</b>', sep='')
Message6<-paste("<br>Using Time variable: <b>",.stdEndEnv$TimeVar,'</b>'," and Time Increment: <b>",.stdEndEnv$TimeInt,'</b>', sep='')
Message7<-paste("<br>Using as weights: <b>",.stdEndEnv$WeightList,'</b>', sep='')
Message8<-paste("<br>Using Test Direction: <b>",.stdEndEnv$TestDirection,'</b>',"<br>Using Alpha Level: <b>",.stdEndEnv$AlphaLevel,'</b>',  sep='')
Message9<-"<br> Alpha Level only applies to calculations for Confidence Intervals and the Jonckheere-Terpstra trend test"  
Message<-paste(Message1,Message2,Message3,Message4,Message5,Message6,Message7,Message8,Message9,'</b><br>',sep='')

return(Message)
}
.stampOutput.te <-
function(){
#' @export
#This function writes the user's selection to the output file created.
#This function assumes that it is called from the time to event saving function

Message1<-paste("<br>Treatment Variable: <b>",.time2EventEnv$TreatmentVar,'</b>',"<br>Replicate Variable: <b>",.time2EventEnv$ReplicateVar,'</b>',sep='')
Message2<-paste("<br>Time Variable: <b>",.time2EventEnv$TimeVar,'</b>', sep='')
Message3<-paste("<br>Status Variable: <b>",.time2EventEnv$StatusVar,'</b>',sep='')
Message4<-paste("<br>Event Status: <b>",.time2EventEnv$StatusEventVal,'</b>',"<br>Censored Status: <b>",.time2EventEnv$StatusCenVal,'</b>', sep='')
Message<-paste(Message1,Message2,Message3,Message4,'</b><br>',sep='')
#HTML('</div></center>')

HTML(Message)
return(Message)
}

.TestEndPoints <-
function(){
#' @export
#Tests Endpoints to see if they can be ran
#Turn off warnings
oldw <- getOption("warn")
options(warn=-1)  

#This tests to see if the end point can be tested on
for (Response in .stdEndEnv$EndPointVar){
RemoveMessage<-NULL
Remove<-FALSE
#Checks to see if more then 1 Treatment has a response
MeansTest<-MeansTest<-tapply(.stdEndEnv$UseData[ ,Response],.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar],mean,na.rm=TRUE)
if (var(MeansTest,na.rm=TRUE)==0 | is.na(var(MeansTest,na.rm=TRUE))==TRUE ){
RemoveMessage<-paste(RemoveMessage,Response, 'has a value for only one treatment: removing it from the analysis.\n')
Remove<-TRUE
}
#Checks to see if the vector is a constant vector
if (var(.stdEndEnv$UseData[ ,Response],na.rm=TRUE)==0 |is.na(var(.stdEndEnv$UseData[ ,Response],na.rm=TRUE))==TRUE ){
RemoveMessage<-paste(RemoveMessage,Response, 'is a consent vector: removing it from the analysis.\n')
Remove<-TRUE
}
if (is.factor(.stdEndEnv$UseData[ ,Response])==TRUE ){
RemoveMessage<-paste(RemoveMessage,Response, 'is not a number: removing it from the analysis.\n')
Remove<-TRUE
}
if (Response==.stdEndEnv$TreatmentVar){
RemoveMessage<-paste(RemoveMessage,Response, 'can not be used as a treatment and a response: removing it from the analysis.\n')
Remove<-TRUE
}
if (Remove==TRUE){
.stdEndEnv$EndPointVar<-.stdEndEnv$EndPointVar[-which(.stdEndEnv$EndPointVar==Response)]
popMessage(RemoveMessage)
}
}
#Revert Warnings
options(warn = oldw) 

}

.updateEnviroment <-
function(){
#' @export
#Updates the environment .stdEndEnv from the values in the GUI

.stdEndEnv$UseData<-.stdEndEnv$MainData
#Gather values from the GUI  
.stdEndEnv$TreatmentVar<-svalue(.stdEndEnv$TreatmentVarCbx)
.stdEndEnv$ReplicateVar<-svalue(.stdEndEnv$ReplicateVarCbx)
.stdEndEnv$ReplicateVar<-svalue(.stdEndEnv$ReplicateVarCbx)
.stdEndEnv$GenerationVar<-svalue(.stdEndEnv$GenerationVarCbx)
.stdEndEnv$GenerationVal<-svalue(.stdEndEnv$GenerationValCbx)
.stdEndEnv$AgeVar<-svalue(.stdEndEnv$AgeVarCbx)
.stdEndEnv$AgeVal<-svalue(.stdEndEnv$AgeValCbx)
.stdEndEnv$GenderVar<-svalue(.stdEndEnv$GenderVarCbx)
.stdEndEnv$GenderVal<-svalue(.stdEndEnv$GenderValCbx)
.stdEndEnv$WeightVar<-svalue(.stdEndEnv$WeightVarCbx)
.stdEndEnv$TestDirection<-svalue(.stdEndEnv$TestDirectionCbx)
.stdEndEnv$AlphaLevel<-as.numeric(svalue(.stdEndEnv$AlphaLvCbx))
.stdEndEnv$TimeVar<-svalue(.stdEndEnv$TimeVarCbx)
.stdEndEnv$Format<-FindFormat(svalue(.stdEndEnv$TimeValCbx),.stdEndEnv$CurrentDate)
.stdEndEnv$TimeInt<-as.numeric(svalue(.stdEndEnv$TimeIntCbx))
.stdEndEnv$TimeIntGraph<-as.numeric(svalue(.stdEndEnv$TimeIntGraphCbx))

#Convert to factors
if (identical(.stdEndEnv$TreatmentVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar])
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
}


#Apply Subsets
if(identical(.stdEndEnv$GenerationVar,'Not Used')==FALSE){ 
	.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenerationVar] == .stdEndEnv$GenerationVal)
}

if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenderVar] == .stdEndEnv$GenderVal)
}

if(identical(.stdEndEnv$AgeVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$AgeVar] == .stdEndEnv$AgeVal)
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]) == dim(.stdEndEnv$UseData)[1]){
.stdEndEnv$ReplicateVar<-'Not Used'
}
}

#Exclude times 2018-3-20
if (length(.stdEndEnv$TimeExcludeVal)>0 & identical(.stdEndEnv$TimeExcludeVal, 'Not Used')==FALSE){
	.stdEndEnv$UseData<-.stdEndEnv$UseData[is.element(.stdEndEnv$UseData[ ,.stdEndEnv$TimeVar],.stdEndEnv$TimeExcludeVal)==FALSE, ]	#only times that are not excluded
}

.stdEndEnv$DataSub<-.stdEndEnv$UseData #DataSub is used to dynamically change the time interval for graphing 

#Test to see if there is still data
if (dim(.stdEndEnv$UseData)[1]==0){
popMessage('The Selection of Age, Generation, or Gender has caused
the data to become an empty set.  Please Reselect one of the above groups') 
return(FALSE)
}

if (is.na(.stdEndEnv$AlphaLevel)==TRUE){
popMessage('Please select a number between 0 and 1 for the Alpha Level ')
return(FALSE)
}
if (.stdEndEnv$AlphaLevel < 0  || .stdEndEnv$AlphaLevel > 1){
popMessage('Please select a number between 0 and 1 for the Alpha Level ')
return(FALSE)
}
return(TRUE)
}
.updateT2E <-
function(){
#' @export
#Updates the environment for Time2Event
#Also checks to see if the analysis can be ran


.time2EventEnv$UseData<-.time2EventEnv$MainData
#Gather values from the GUI  
.time2EventEnv$TreatmentVar<-svalue(.time2EventEnv$TreatmentVarCbx)
.time2EventEnv$ReplicateVar<-svalue(.time2EventEnv$ReplicateVarCbx)
.time2EventEnv$GenerationVar<-svalue(.time2EventEnv$GenerationVarCbx)
.time2EventEnv$GenerationVal<-svalue(.time2EventEnv$GenerationValCbx)
.time2EventEnv$GenderVar<-svalue(.time2EventEnv$GenderVarCbx)
.time2EventEnv$GenderVal<-svalue(.time2EventEnv$GenderValCbx)
.time2EventEnv$TimeVar<-svalue(.time2EventEnv$TimeVarCbx)
.time2EventEnv$Format<-FindFormat(svalue(.time2EventEnv$TimeValCbx),.time2EventEnv$CurrentDate)
.time2EventEnv$StatusVar<-svalue(.time2EventEnv$StatusVarCbx)
.time2EventEnv$StatusEventVal<-svalue(.time2EventEnv$StatusEventValCbx)
.time2EventEnv$StatusCenVal<-svalue(.time2EventEnv$StatusCenValCbx)
.time2EventEnv$CanRun<-FALSE



#Convert to factors
if (identical(.time2EventEnv$TreatmentVar, 'Not Used')==FALSE){
.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]<-as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar])
}else{
popMessage("A treatment variable needs to be defined.")
return(FALSE)
}

if (identical(.time2EventEnv$StatusVar, 'Not Used')==TRUE){
popMessage("A statues variable needs to be defined.")
return(FALSE)
}

if (identical(.time2EventEnv$StatusEventVal, 'Not Used')==TRUE){
popMessage("A value representing events needs to be defined")
return(FALSE)
}

if (identical(.time2EventEnv$StatusEventVal, 'Not Used')==TRUE){
popMessage("A value representing censored events needs to be defined")
return(FALSE)
}


if (identical(.time2EventEnv$TimeVar, 'Not Used')==TRUE){
popMessage("A time variable needs to be defined.")
return(FALSE)
}


if (identical(.time2EventEnv$ReplicateVar, 'Not Used')==FALSE){
.time2EventEnv$UseData[ ,.time2EventEnv$ReplicateVar]<-as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.time2EventEnv$UseData[ ,.time2EventEnv$ReplicateVar]) == dim(.time2EventEnv$UseData)[1]){
.time2EventEnv$ReplicateVar<-'Not Used'
}
}


#Apply Subsets
if(identical(.time2EventEnv$GenderVar,'Not Used')==FALSE){
.time2EventEnv$UseData<-subset(.time2EventEnv$UseData,.time2EventEnv$UseData[ ,.time2EventEnv$GenerationVar] == .time2EventEnv$GenerationVal)
}

if(identical(.time2EventEnv$GenderVar,'Not Used')==FALSE){
.time2EventEnv$UseData<-subset(.time2EventEnv$UseData,.time2EventEnv$UseData[ ,.time2EventEnv$GenderVar] == .time2EventEnv$GenderVal)
}

#Test to see if there is still data
if (dim(.time2EventEnv$UseData)[1]==0){
popMessage('The Selection of Generation, or Gender has caused
the data to become an empty set.  Please Reselect one of the above groups') 
return(FALSE)
}

#Change time
if (is.null(.time2EventEnv$TimeTemp)==TRUE){
try(.time2EventEnv$TimeTemp<-date2Numeric(.time2EventEnv$UseData[ ,.time2EventEnv$TimeVar],.time2EventEnv$Format))
UseTime<-checkTime(.time2EventEnv$TimeTemp) & .time2EventEnv$CanRun  #Update CanRun
if (UseTime == FALSE){
return(FALSE)
}else{
UseData[ ,.time2EventEnv$TimeVar]<-.time2EventEnv$TimeTemp
}
}

#Change Status Vars

Out<-.changeStatusValues()
return(Out)
}
.writeExamples <-
function(Folder){
#' @export
popMessage('This may take a moment')
dir.create(Folder, showWarnings = FALSE)
dir.create(paste(Folder,'\\Histology Example',sep=''), showWarnings = FALSE)
dir.create(paste(Folder,'\\Fecundity Example',sep=''), showWarnings = FALSE)
dir.create(paste(Folder,'\\Length - Weight Example',sep=''), showWarnings = FALSE)
dir.create(paste(Folder,'\\Time to Event Example',sep=''), showWarnings = FALSE)
Sys.sleep(.5)


fecundityData<-StatCharrms::fecundityData
lengthWeightData<-StatCharrms::lengthWeightData
eventTimeData<-StatCharrms::eventTimeData
exampleHistData<-RSCABS::exampleHistData


#Write Data
write.table(exampleHistData,paste(Folder,'\\Histology Example','\\Histology Example Data.csv',sep=''),
row.names=FALSE,sep=',' )
write.table(fecundityData,paste(Folder,'\\Fecundity Example','\\Fecundity Example Data.csv',sep=''),
row.names=FALSE,sep=',' )
write.table(lengthWeightData,paste(Folder,'\\Length - Weight Example','\\Length - Weight Data.csv',sep=''),
row.names=FALSE,sep=',' )
write.table(eventTimeData,paste(Folder,'\\Time to Event Example','\\Time to Event Example Data.csv',sep=''),
row.names=FALSE,sep=',' )


##################################################################################
#RSCABS

#Take the subset corresponding to F0-females of 16 weeks of age
exampleHistData.sub<-exampleHistData[which(exampleHistData$Generation=='F2' & 
exampleHistData$Genotypic_Sex=='Male' & exampleHistData$Age=='16_wk' ),  ]
#Run RSCABS
exampleResults<-runRSCABS(exampleHistData.sub,'Treatment','Replicate',test.type='RS')

#Create Dir
HistoDir<-paste(Folder,'\\Histology Example\\Results',sep='')
dir.create(HistoDir, showWarnings = FALSE)

write.table(exampleResults,paste(HistoDir,'\\Histology Example Results.csv',sep=''),row.names=FALSE,sep=',')

#Find endpoints that can be graphed
Names<-strsplit(as.character(exampleResults$Effect),split='')
Names<-lapply(Names,function(X){paste0(X[-length(X)],collapse = '')})
Names<-unique(Names)
Files<-lapply(Names,function(X){paste(HistoDir,'\\',X,sep='')})

#Graph all endpoints
CantPrint<-''
for (i in 1:length(Files)){
Msg<-try(plotRSCABS(exampleHistData,Names[[i]],'Treatment','Percent',
'Remove',NULL,'png',File=Files[[i]]))
if (is(Msg)[1]=='try-error'){
CantPrint<-paste(CantPrint,Names[[i]],sep=' \n ')
print(CantPrint)
dev.off()
}
}


##################################################################################
#Length Weight
LWDir=paste(Folder,'\\Length - Weight Example\\Results',sep='')
dir.create(LWDir, showWarnings = FALSE)
FileName<-'Length-Weight Example Results'



#Initializes variables
.stdEndEnv$TimeVar<-'Not Used'
.stdEndEnv$TimeInt<-21    #Time interval used 
.stdEndEnv$TimeIntGraph<-7 #Time interval used for graphing
.stdEndEnv$TimeExcludeVal<-{}
.stdEndEnv$GenerationVar<-'Generation'
.stdEndEnv$GenerationVal<-'F1'  #Can be a Character array
.stdEndEnv$GenderVar<-'SEX'
.stdEndEnv$GenderVal<-'M'  #Can be a Character array
.stdEndEnv$AgeVar<-'Age'
.stdEndEnv$AgeVal<-'16 week'
.stdEndEnv$ReplicateVar<-'Replicate'
.stdEndEnv$TreatmentVar<-'Treatment'
.stdEndEnv$Format<-"%m/%d/%Y"
.stdEndEnv$Results<-list()
.stdEndEnv$WeightsVar<-'Not Used'
.stdEndEnv$TestDirection<-'Both'
.stdEndEnv$AlphaLevel<-0.05
.stdEndEnv$UseData<-lengthWeightData

#manually upset data

if (identical(.stdEndEnv$TreatmentVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar])
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
}


#Apply Subsets
if(identical(.stdEndEnv$GenerationVal,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenerationVar] == .stdEndEnv$GenerationVal)
}

if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenderVar] == .stdEndEnv$GenderVal)
}

if(identical(.stdEndEnv$AgeVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$AgeVar] == .stdEndEnv$AgeVal)
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]) == dim(.stdEndEnv$UseData)[1]){
.stdEndEnv$ReplicateVar<-'Not Used'
}
}

.stdEndEnv$Results[['LENGTH']]<-autoStdAnylsis(.stdEndEnv$UseData,'LENGTH',.stdEndEnv$TreatmentVar,
'None',.stdEndEnv$WeightsVar.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,
.stdEndEnv$AlphaLevel)
.stdEndEnv$Results[['WEIGHT']]<-autoStdAnylsis(.stdEndEnv$UseData,'WEIGHT',.stdEndEnv$TreatmentVar,
'Log',.stdEndEnv$WeightsVar.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,
.stdEndEnv$AlphaLevel)
.stdEndEnv$Results[['WEIGHT']]$TestType<-'Auto'
.stdEndEnv$Results[['LENGTH']]$TestType<-'Auto'

#Save Results
.stdEndEnv$FileName<-'Example'
.saveResults(.stdEndEnv$Results[['WEIGHT']],LWDir,FileName)
.saveResults(.stdEndEnv$Results[['LENGTH']],LWDir,FileName)

#plots
.stdEndEnv$PlotTypeList<-c('Box','Quantile-Quantile','Violin')
.stdEndEnv$EndPointVar<-c('LENGTH','WEIGHT')
.stdEndEnv$PlotData<-.stdEndEnv$UseData
.saveGraphs(LWDir,FileName)


#Remove vairables

rm(list=ls(.stdEndEnv) ,envir =.stdEndEnv)

##################################################################################
#fecundity
FDir=paste(Folder,'\\Fecundity Example\\Results',sep='')
dir.create(LWDir, showWarnings = FALSE)
FileName<-'Fecundity Example Results'

.stdEndEnv$TimeVar<-'Date'
.stdEndEnv$TimeInt<-21    #Time interval used 
.stdEndEnv$TimeIntGraph<-7 #Time interval used for graphing
.stdEndEnv$TimeExcludeVal<-{}
.stdEndEnv$GenerationVar<-'Generation'
.stdEndEnv$GenerationVal<-'F1'  #Can be a Character array
.stdEndEnv$GenderVar<-'Not Used'
.stdEndEnv$GenderVal<-'Not Used'  #Can be a Character array
.stdEndEnv$AgeVar<-'Not Used'
.stdEndEnv$AgeVal<-'Not Used'
.stdEndEnv$ReplicateVar<-'Rep'
.stdEndEnv$TreatmentVar<-'Treatment'
.stdEndEnv$Format<-"%m/%d/%Y"
.stdEndEnv$Results<-list()
.stdEndEnv$WeightsVar<-'Not Used'
.stdEndEnv$TestDirection<-'Both'
.stdEndEnv$AlphaLevel<-0.05
.stdEndEnv$UseData<-fecundityData

#manually upset data

if (identical(.stdEndEnv$TreatmentVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar])
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
}


#Apply Subsets
if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenerationVar] == .stdEndEnv$GenerationVal)
}

if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenderVar] == .stdEndEnv$GenderVal)
}

if(identical(.stdEndEnv$AgeVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$AgeVar] == .stdEndEnv$AgeVal)
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]) == dim(.stdEndEnv$UseData)[1]){
.stdEndEnv$ReplicateVar<-'Not Used'
}
}

.stdEndEnv$DataSub<-.stdEndEnv$UseData #DataSub is used to dynamically change the time interval for graphing 

.stdEndEnv$Results[['Fecundity']]<-autoStdAnylsis(.stdEndEnv$UseData,'Fecundity',.stdEndEnv$TreatmentVar,
'Square_Root',.stdEndEnv$WeightsVar.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,
.stdEndEnv$AlphaLevel)
.stdEndEnv$Results[['Fecundity']]$TestType<-'Auto'


.stdEndEnv$FileName<-'Example'
.saveResults(.stdEndEnv$Results[['Fecundity']],FDir,FileName)


#plots
.stdEndEnv$PlotTypeList<-c('Box','Quantile-Quantile','Violin','Interaction')
.stdEndEnv$EndPointVar<-c('Fecundity')

.stdEndEnv$PlotData<-.getTimeData(.stdEndEnv$DataSub,'Date',.stdEndEnv$Format,.stdEndEnv$TimeIntGraph,
.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)
colnames(.stdEndEnv$PlotData)[which(colnames(.stdEndEnv$PlotData)=='Averaged_Numeric_Time')]<-'Time'
.stdEndEnv$PlotData$Time<-as.factor(.stdEndEnv$PlotData$Time)
.stdEndEnv$TimeVar<-'Date'


.saveGraphs(FDir,FileName)

rm(list=ls(.stdEndEnv) ,envir =.stdEndEnv)
##################################################################################
#Time to Effect
Dir=paste(Folder,'\\Time to Event Example\\Results',sep='')
FileName<-'Time to Effect Sample Results'
dir.create(Dir, showWarnings = FALSE)


#Set globals that the function are reliant on  
.time2EventEnv$TreatmentVar<-'Trt'
.time2EventEnv$ReplicateVar<-'Rep'
.time2EventEnv$TimeVar<-'Time'
.time2EventEnv$StatusVar<-'Status'
.time2EventEnv$StatusEventVal<-'1'
.time2EventEnv$StatusCenVal<-'0'
.time2EventEnv$CanRun<-FALSE
.time2EventEnv$TimeTemp<-{}
.time2EventEnv$Added.StatusVal<-0  #Used to control the control boxes
.time2EventEnv$GenderVar<-'Not Used'
.time2EventEnv$GenerationVar<-'Not Used'
.time2EventEnv$UseData<-eventTimeData


.time2EventEnv$Results<-analyseTime2Effect(.time2EventEnv$UseData,.time2EventEnv$StatusVar,.time2EventEnv$TimeVar,
.time2EventEnv$TreatmentVar,.time2EventEnv$ReplicateVar)

#Save Results
.saveResults.te(.time2EventEnv$Results,Dir,FileName)
.saveGraphs.te(Dir,FileName)

#Remove globals
rm(list=ls(.time2EventEnv) ,envir =.stdEndEnv)


}

selectPara<-function (VarName, LabelName = NULL, Enviro, What = NULL, Mult = FALSE, 
    Display = NULL) 
{
    if (is.null(What) == TRUE) {
        Word <- strsplit(VarName, split = "")[[1]]
        Type <- paste0(Word[{
            length(Word) - 2
        }:length(Word)], collapse = "")
        if (identical(Type, "Var") == TRUE) {
            Varaibles <- c("Not Used", colnames(get("MainData", 
                envir = get(Enviro))))
            if (is.null(Display) == TRUE) {
                Display <- paste(paste0(Word[1:{
                  length(Word) - 3
                }], collapse = ""), "Variable")
            }
        }
        if (identical(Type, "Val") == TRUE) {
            Word[length(Word)] <- "r"
            From <- paste0(Word, collapse = "")
            Choices <- levels(as.factor(get("MainData", envir = get(Enviro))[, 
                get(From, get(Enviro))]))
            Varaibles <- c("Not Used", Choices)
            if (is.null(Display) == TRUE) {
                Display <- paste(paste0(Word[1:{
                  length(Word) - 3
                }], collapse = ""), "Value")
            }
        }
    }
    else {
        Varaibles <- c("Not Used", What)
    }
    SelectWindow <- gwindow(paste("Please select the", Display), 
        visible = FALSE)
    group <- ggroup(horizontal = FALSE, container = SelectWindow, 
        spacing = 20)
    SubSetSelect <- gtable(Varaibles, container = group, expand = TRUE, 
        multiple = Mult)
    SelectButton <- gbutton("Select", container = group, handler = function(h, 
        ...) {
        assign(VarName, SubSetSelect[svalue(SubSetSelect, index = TRUE), 
            ], envir = get(Enviro))
        if (Mult == TRUE) {
            if (is.null(LabelName) == FALSE) {
                try(temp <- get(LabelName, envir = get(Enviro)))
                try(svalue(temp) <- "Multiple Values")
                LabelName <- NULL
            }
        }
        if (is.null(LabelName) == FALSE) {
            try(tempVar <- get(VarName, envir = get(Enviro)), 
                silent = TRUE)
            try(tempWig <- get(LabelName, envir = get(Enviro)), 
                silent = TRUE)
            try(svalue(tempWig) <- tempVar)
        }
        dispose(SelectWindow)
    })
    addHandlerUnrealize(SelectWindow, handler = function(h, ...) {
        assign(VarName, "Not Used", envir = get(Enviro))
    })
    visible(SelectWindow) <- TRUE
    return()
}
