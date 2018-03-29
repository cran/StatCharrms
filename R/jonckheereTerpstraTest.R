jonckheereTerpstraTest <-
function(Data,Treatment,Response,TestDirection,AlphaLevel){
#' @export
#performs the JonckheereTerpstraTest
#Outputs a clean data table of responses 
JTTable<-{}
Test=TRUE #tells the function to keep stepping down
while (Test == TRUE){
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
#Basic Test 
#Initialize variables for testing     
JTInc<-{};JTDec<-{} ;JTInc$p.value<-1; JTDec$p.value<-1


if (TestDirection=='Increasing' | TestDirection=='Both'){  #Increasing Test
	JTInc<-jonckheere.test(Data[ ,Response],as.ordered(Data[ ,Treatment]),nperm=50000,alternative='increasing')
}
if (TestDirection=='Decreasing' | TestDirection=='Both'){ #Decreasing Test
	JTDec<-jonckheere.test(Data[ ,Response],as.ordered(Data[ ,Treatment]),nperm=50000,alternative='decreasing')
	JTInc$statistic<-JTDec$statistic
}


JTEntry<-cbind(JTInc$statistic,JTDec$p.value,JTInc$p.value,length(levels(Data[ ,Treatment])))
#Step down
Data<-Data[Data[ ,Treatment] != levels(Data[ ,Treatment])[length(levels(Data[ ,Treatment]))], ] #Remove the highest level of treatment
Data[ ,Treatment]<-factor(Data[ ,Treatment])  #Removes the highest level of from treatment from the data frame
JTTable<-rbind(JTTable,JTEntry)
if (length(levels(Data[ ,Treatment]))==1){
Test = FALSE
}
if (JTDec$p.value>AlphaLevel & JTInc$p.value>AlphaLevel){ 
Test = FALSE
}
}
#Clean table
JTTable<-cbind(JTTable[ ,1],JTTable[ ,2],rep('.',dim(JTTable)[1]),JTTable[ ,3],rep('.',dim(JTTable)[1]),JTTable[ ,4])


#Add Signif flags
JTTable[JTTable[ ,2]<0.05,3]<-'*'
JTTable[JTTable[ ,2]<0.01,3]<-'**'
JTTable[as.numeric(JTTable[ ,2])<0.001,3]<-'***'

JTTable[JTTable[ ,4]<0.05,5]<-'*'
JTTable[JTTable[ ,4]<0.01,5]<-'**'
JTTable[as.numeric(JTTable[ ,4])<0.001,5]<-'***'


colnames(JTTable)<-c('JT Statistic','Decreasing Trend P-Value','D.Sig','Increasing Trend P-Value',
'I.Sig','Max Level of Treatment')

rownames(JTTable)=NULL

if (TestDirection=='Increasing'){
	JTTable[,2]<-NA
	JTTable[,3]<-NA
}
if (TestDirection=='Decreasing'){
	JTTable[,4]<-NA
	JTTable[,5]<-NA
}


return(JTTable)
}
