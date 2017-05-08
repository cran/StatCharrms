dunnsTest <-
function(Data,Treatment,Response,TestDirection){
#' @export
#Dunns test with clean table output
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])

#For Dunns test
Sub<- subset(Data, is.na(Data[ ,Response]) == FALSE)  #This is why it's done one response at a time
Sub[ ,Response] <- rank(Sub[ ,Response])

as.vector(by(Sub[ ,Response], Sub[ ,Treatment],sum,na.rm=TRUE))

means <- as.vector(by(Sub[ ,Response], Sub[ ,Treatment],sum,na.rm=TRUE))#Sum Rank
NEachTreatment <- as.vector(by(Sub[ ,Response], Sub[ ,Treatment],.lengthNa))   #of Samples per treatment 
Means <- data.frame(means, N = NEachTreatment)
Means[, 1] <- Means[, 1]/Means[, 2]  #Average Rank


#Attain correction for Ties (TC)
RN<-table(Sub[ ,Response])
TC<-sum(RN^3-RN)

N<-dim(Sub)[1]  
SD<-sqrt({ N^3-N-TC}/(12*N-12)*(1/Means[1,2]+1/Means[ ,2]))  #Standard Deviation, form SAS StatCharrms

#P-Values, test from SAS StatCharrms
Nlevels<-nlevels(Sub[ ,Treatment])
if (TestDirection=='Both'){    
	PValue<-pmin({1-pnorm(abs(Means[ ,1]-Means[1,1])/SD)}*{Nlevels-1},1)} #Correct for multiple levels
if (TestDirection=='Increasing'){
	PValue<-pmin({1-pnorm({Means[ ,1]-Means[1,1]}/SD)}*{Nlevels-1},1)}
if (TestDirection=='Decreasing'){
	PValue<-pmin({pnorm({Means[ ,1]-Means[1,1]}/SD)}*{Nlevels-1},1)}

LessThanP<-which(PValue<0.0001)
PValue<-round(PValue,4)
Signif<-'.'

#Combine and clean up tables
DunnsTable<-as.data.frame(cbind(Means[ ,1],Means[ ,2],Means[ ,1],Means[ ,1]-Means[1,1],PValue))
DunnsTable<-cbind(DunnsTable,Signif)

#Significance Markers 
DunnsTable$Signif<-factor(Signif,levels=c('.','*','**','***'))
DunnsTable$Signif[DunnsTable[ ,'PValue']<0.05]<-'*'
DunnsTable$Signif[DunnsTable[ ,'PValue']<0.01]<-'**'
DunnsTable$Signif[DunnsTable[ ,'PValue']<0.001]<-'***'

#Change <10^-4 to <10^-4
if (length(LessThanP)>0){
DunnsTable$PValue[LessThanP]<-'<10^-4'}
DunnsTable[ ,1]<-levels(Data[ ,Treatment])


#Change Names
colnames(DunnsTable)<-c('Treatment','Count','Rank','Difference','P Value','Signif')
return(DunnsTable)
}
