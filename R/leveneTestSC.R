leveneTestSC <-
function(Data,Treatment,Residuals){
#' @export
#Creates a table for the Levene test test of variance homogeneity 
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
LeveneTable<-cbind(Treatment,leveneTest(Residuals,Data[ ,Treatment])) 
LeveneTable<-LeveneTable[-2, ]#Removes a junk row from the table 
#Round the numbers
LeveneTable[3]<-round(LeveneTable[3],2)
LeveneTable[4]<-round(LeveneTable[4],4)

rownames(LeveneTable)<-NULL
return(LeveneTable)
}
