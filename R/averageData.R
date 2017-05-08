averageData <-
function(Data,Treatment,Response,Replicate){
#' @export
#This function is called when replicate is given 
#It Builds a new dataset by averaging over Replicate:Treatment
Data$UIDforAVG<-as.factor(Data[ ,Treatment]):as.factor(Data[ ,Replicate])
Means<-tapply(Data[ ,Response],Data$UIDforAVG,mean,na.rm=TRUE)  #Mean over Replicate:Treatment
Lengths<-tapply(Data[ ,Response],Data$UIDforAVG,.lengthNa) #Length over Replicate:Treatment
OutData<-as.data.frame(cbind(Means,Lengths,rownames(Means)))
OutData<-OutData[complete.cases(OutData), ]  #Remove rows with NA in them
A<-unlist(strsplit(as.character(OutData[ ,3]),':'))
n<-1:{length(A)/2}
ReplicateVec<-A[2*n]
TreatmentVec<-A[2*n-1]
OutData[ ,3]<-TreatmentVec
OutData<-cbind(OutData,ReplicateVec)
colnames(OutData)<-c(Response,paste('N.',Response,sep=''),
Treatment,Replicate)
return(OutData)
}
