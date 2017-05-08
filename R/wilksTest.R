wilksTest <-
function(Residuals){
#' @export
#This will create a table for the Wilks test for normality 

#Test for norm
ShapiroTest<-shapiro.test(Residuals) 
#Kurtosis over the normal
Kurt<-sum((Residuals-mean(Residuals))^4)/(var(Residuals)^2)/(length(Residuals)-1)
Kurt<-Kurt*(length(Residuals)/(length(Residuals)-2))
Kurt<-Kurt-3

#Skewness
Skew<-sum((Residuals-mean(Residuals))^3)/(var(Residuals)^(3/2))/(length(Residuals)-1)
Skew<-Skew*(length(Residuals)/(length(Residuals)-2))

#Residual standard error
StdError<-sqrt(var(Residuals))

#Clean and make output Table
WilksTest<-cbind(length(Residuals),StdError,Skew,Kurt,ShapiroTest$statistic,ShapiroTest$p.value)
colnames(WilksTest)<-c('OBS','STD','SKEW','KURT','SW_STAT','P_VALUE')
WilksTest<-as.data.frame(WilksTest)
WilksTest$P_VALUE<-round(WilksTest$P_VALUE,4)
WilksTest$STD<-round(WilksTest$STD,5)
WilksTest$SKEW<-round(WilksTest$SKEW,5)
WilksTest$KURT<-round(WilksTest$KURT,5)
WilksTest$SW_STAT<-round(WilksTest$SW_STAT,5)

if (WilksTest$P_VALUE<0.05){
WilksTest$Signif<-'*'
}
if (WilksTest$P_VALUE>=0.05){
WilksTest$Signif<-'.'
}
return(WilksTest)
}
