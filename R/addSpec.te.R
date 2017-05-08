addSpec.te <-
function(){
#' @export
#This tab is where the user specifies the data set

#Main tab for specifying data
.time2EventEnv$CurrentDate<-rbind(format(Sys.Date(),'%Y-%m-%d'),format(Sys.Date(),'%m-%d-%Y'),format(Sys.Date(),'%d-%m-%Y'),
format(Sys.Date(),'%Y/%m/%d'),format(Sys.Date(),'%m/%d/%Y'),format(Sys.Date(),'%d/%m/%Y'),
format(Sys.Date(),'%Y-%b-%d'),format(Sys.Date(),'%b-%d-%Y'),format(Sys.Date(),'%d-%b-%Y'))
.time2EventEnv$CurrentDateList<-rbind(.time2EventEnv$CurrentDate,'Numeric','                ')
#Main Box
.time2EventEnv$SpecGroup <- ggroup(horizontal = FALSE, label='Data specification')
add(.time2EventEnv$ButtonBox,.time2EventEnv$SpecGroup)
#size(.time2EventEnv$SpecGroup)<-c(760,502)

#First Box, Treatment and Replicate
#Treatment
TreatmentVarFrm<-gframe('Treatment',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$TreatmentVarCbx<-gcombobox(c('Not Used',colnames(.time2EventEnv$MainData)),container=TreatmentVarFrm, fill=TRUE)

#Replicate
ReplicateVarFrm<-gframe('Replicate',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$ReplicateVarCbx<-gcombobox(c('Not Used',colnames(.time2EventEnv$MainData)),container=ReplicateVarFrm, fill=TRUE)
#Gender
GenderVarFrm<-gframe('Gender Variable',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$GenderVarCbx<-gcombobox(c('Not Used',colnames(.time2EventEnv$MainData)),container=GenderVarFrm, fill=TRUE,
handler= function(h,...){
#Update Value CBox
.time2EventEnv$GenderVar<-svalue(.time2EventEnv$GenderVarCbx)
delete(.time2EventEnv$GenderValFrm,.time2EventEnv$GenderValCbx)
if (identical(svalue(.time2EventEnv$GenderVarCbx),'Not Used')==FALSE){
.time2EventEnv$GenderValCbx<-gcombobox(levels(as.factor((.time2EventEnv$MainData[ ,.time2EventEnv$GenderVar]))), fill=TRUE)
}else{
.time2EventEnv$GenderValCbx<-gcombobox(unique('Variable Not Selected'))
}
add(.time2EventEnv$GenderValFrm,.time2EventEnv$GenderValCbx)
})

.time2EventEnv$GenderValFrm<-gframe('Gender Value',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$GenderValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
add(.time2EventEnv$GenderValFrm,.time2EventEnv$GenderValCbx)

 #Generation
GenerationVarFrm<-gframe('Generation Variable',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$GenerationVarCbx<-gcombobox(c('Not Used',colnames(.time2EventEnv$MainData)),container=GenerationVarFrm, fill=TRUE,
handler= function(h,...){
#Update Value CBox
.time2EventEnv$GenerationVar<-svalue(.time2EventEnv$GenerationVarCbx)
delete(.time2EventEnv$GenerationValFrm,.time2EventEnv$GenerationValCbx)
if (identical(svalue(.time2EventEnv$GenerationVarCbx),'Not Used')==FALSE){
.time2EventEnv$GenerationValCbx<-gcombobox(levels(as.factor((.time2EventEnv$MainData[ ,.time2EventEnv$GenerationVar]))), fill=TRUE)
}else{
.time2EventEnv$GenerationValCbx<-gcombobox(unique('Variable Not Selected'))
}
add(.time2EventEnv$GenerationValFrm,.time2EventEnv$GenerationValCbx)
})

.time2EventEnv$GenerationValFrm<-gframe('Generation Value',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$GenerationValCbx<-gcombobox(unique('Variable Not Selected'), fill=TRUE)
add(.time2EventEnv$GenerationValFrm,.time2EventEnv$GenerationValCbx)


#Time 
TimeVarFrm<-gframe('Time Variable',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$TimeVarCbx<-gcombobox(c('Not Used',colnames(.time2EventEnv$MainData)),container=TimeVarFrm, 
handler= function(h,...){
#Update Value CBox
.time2EventEnv$TimeVar<-svalue(.time2EventEnv$TimeVarCbx)
delete(.time2EventEnv$TimeValFrm,.time2EventEnv$TimeValCbx)
#Start code for time format
if (identical(svalue(.time2EventEnv$TimeVarCbx),'Not Used')==FALSE){

.time2EventEnv$TimeValCbx<-gcombobox(as.vector(.time2EventEnv$CurrentDateList),selected=1,fill=TRUE,
handler= function(h,...){
#Format box updates the data
.time2EventEnv$Format<-FindFormat(svalue(.time2EventEnv$TimeValCbx),.time2EventEnv$CurrentDate)
if (identical(.time2EventEnv$TimeVar,'Not Used')==FALSE){
.time2EventEnv$TimeTemp<-{}
try(.time2EventEnv$TimeTemp<-date2Numeric(.time2EventEnv$MainData[ ,.time2EventEnv$TimeVar],.time2EventEnv$Format))
#Tell the user it did not work.
if (length(.time2EventEnv$TimeTemp)==0){
popMessage('Incompatible date format please reselect date format. ')
return()
}
if (sum(is.na(.time2EventEnv$TimeTemp))==length(.time2EventEnv$TimeTemp)){
popMessage('Incompatible date format please reselect date format. ')
return()
}
if (sum(.time2EventEnv$TimeTemp, na.rm=TRUE)==0){
popMessage('Incompatible date format please reselect date format. ')
return()
}

}
#Tell the User the problematic rows
if (sum(is.na(.time2EventEnv$TimeTemp))>0){
RowList<-which(is.na(.time2EventEnv$TimeTemp)==TRUE)
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
.time2EventEnv$TimeValCbx<-gcombobox(c('Variable Not Selected'))
}
add(.time2EventEnv$TimeValFrm,.time2EventEnv$TimeValCbx)
})

.time2EventEnv$TimeValFrm<-gframe('Time Format',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$TimeValCbx<-gcombobox(c('Variable Not Selected'))
add(.time2EventEnv$TimeValFrm,.time2EventEnv$TimeValCbx)


StatusVarFrm<-gframe('Status Variable',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$StatusVarCbx<-gcombobox(c('Not Used',colnames(.time2EventEnv$MainData)),container=StatusVarFrm,
handler= function(h,...){
#Update Value CBox
.time2EventEnv$StatusVar<-svalue(.time2EventEnv$StatusVarCbx)
delete(.time2EventEnv$StatusEventValFrm,.time2EventEnv$StatusEventValCbx)
delete(.time2EventEnv$StatusCenValFrm,.time2EventEnv$StatusCenValCbx)

if (identical(svalue(.time2EventEnv$StatusVarCbx),'Not Used')==FALSE){
 Values<-c('Not Used',levels(as.factor((.time2EventEnv$MainData[ ,.time2EventEnv$StatusVar]))))
.time2EventEnv$StatusEventValCbx<-gcombobox(Values)
    .time2EventEnv$StatusCenValCbx<-gcombobox(Values)
}else{
.time2EventEnv$StatusEventValCbx<-gcombobox(unique('Variable Not Selected'))
.time2EventEnv$StatusCenValCbx<-gcombobox(unique('Variable Not Selected'))
}
add(.time2EventEnv$StatusEventValFrm,.time2EventEnv$StatusEventValCbx)
add(.time2EventEnv$StatusCenValFrm,.time2EventEnv$StatusCenValCbx)
})

.time2EventEnv$StatusEventValFrm<-gframe('Event Value',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$StatusEventValCbx<-gcombobox(unique('Variable Not Selected'))
add(.time2EventEnv$StatusEventValFrm,.time2EventEnv$StatusEventValCbx)

.time2EventEnv$StatusCenValFrm<-gframe('Censored Value',horizontal = FALSE, container=.time2EventEnv$SpecGroup)
.time2EventEnv$StatusCenValCbx<-gcombobox(unique('Variable Not Selected'))
add(.time2EventEnv$StatusCenValFrm,.time2EventEnv$StatusCenValCbx)

}
