tranformTime <-
function(Data,TimeVar,Time,RepVar,GroupVar,ResponVar){
#' @export
#This function averages the response variable a across "Time" units in time

#TimeVar: The variable time information is stored in.  
#Time is the new time Interval
#GroupVar is the grouping variable in this case it's treatment
#RepVar is the replicate variable in this case it's tank
#ResponVar is the name of the response being tested

#Returns a new data set with the averaged variable 
Data[ ,TimeVar]<-as.numeric(as.character(Data[ ,TimeVar]))
Data$Tinv<-floor(Data[ ,TimeVar]/Time)+1  #new variable for new unit time, the +1 starts counting at 1
Data[ ,GroupVar]<-as.factor(Data[ ,GroupVar])  #Convert to factor
Data$Tinv<-as.factor(Data$Tinv)  #Convert to factor

if (is.element(RepVar,colnames(Data))==TRUE){
Data[ ,RepVar]<-as.factor(Data[ ,RepVar])    #Convert to factor
Data$UniqueID.SC<-Data[ ,RepVar]:Data[ ,GroupVar]:Data[ ,'Tinv']
}else{
Data$UniqueID.SC<-Data[ ,GroupVar]:Data[ ,'Tinv']
}

MeanRes<-as.vector(by(Data[ ,ResponVar],Data$UniqueID.SC,mean,na.rm=TRUE))
if (length(which(is.na(MeanRes)==TRUE))>0){
MeanRes<-MeanRes[-which(is.na(MeanRes)==TRUE)]
}
Row<-by(Data,Data$UniqueID.SC,function(X){X[1, ]})

NewData<-as.data.frame(do.call(rbind,Row))
rownames(NewData)<-NULL

NewData[ ,TimeVar]<-NewData[ ,'Tinv']
NewData<-NewData[ ,-which(is.element(colnames(NewData),c('Tinv')))]
colnames(NewData)[which(colnames(NewData)==TimeVar)]<-paste('Averaged',TimeVar,sep='_')

NewData[ ,ResponVar]<-MeanRes
#Remove Unique ID
return(NewData)
}
