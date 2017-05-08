rankTransform <-
function(Data,VecName){
#' @export
#This function will rank transform the data the same way the SAS code below does

#proc rank data=sorted out=assumtst normal=blom 
#ties=mean;
#var Counts;
#by generation;   
#ranks rank_EGGS;
#run;

#Data- data set to be modified 
#VecName Name of Variable  to be modified
#returns a data set with TransformedResponse as the rank transformed responce


Vector<-Data[ ,VecName];

#This handles the rank transform 
n<-length(Vector)
r.i<-rank(Vector, na.last = TRUE, ties.method = c("random"))
y.i<-(r.i-3/8)/(n+1/4) 
Data$y.i.norm<-qnorm(y.i)  #this will be for output
Data<-Data[with(Data, order(Data$y.i.norm)), ]


Repeats<-xtabs(~as.factor(Data[ ,VecName]))
#This Block of code is for finding ties and will break if no ties exist 
if(max(Repeats)>1){ #Check for ties
Repeats<-Repeats[-which(Repeats==1)] #Remove non-ties
Names<-dimnames(Repeats)
for (e in Names[[1]]){  #For every number that has a tie
e<-as.numeric(e)
Data[which(Data[ ,VecName]==e),'y.i.norm']<-mean(Data[which(Data[ ,VecName]==e),'y.i.norm']) #Average
}
}
colnames(Data)[which(colnames(Data)=='y.i.norm')]<-'TransformedResponse' #rename column

return(Data)
}
