leveneTestSC <-
function(Data,Treatment,Residuals){
#' @export
#Creates a table for the Levene test test of variance homogeneity 
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
LeveneTable<-cbind(Treatment,leveneTest(Residuals,Data[ ,Treatment])) 
LeveneTable<-LeveneTable[-2, ]#Removes a junk row from the table 
rownames(LeveneTable)<-NULL
return(LeveneTable)
}
