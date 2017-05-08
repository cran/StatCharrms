checkTime <-
function(TimeTemp){
#' @export
#Tell the user it did not work
if (length(TimeTemp)==0){
popMessage('Incompatible date format please reselect date format. ')
return(FALSE)
}
if (sum(is.na(TimeTemp))==length(TimeTemp)){
popMessage('Incompatible date format please reselect date format. ')
return(FALSE)
}
if (sum(TimeTemp, na.rm=TRUE)==0){
popMessage('Incompatible date format please reselect date format. ')
return(FALSE)
}
if (sum(is.na(TimeTemp))>0){
RowList<-which(is.na(TimeTemp)==TRUE)
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
return(FALSE)
}
return(TRUE)
}
