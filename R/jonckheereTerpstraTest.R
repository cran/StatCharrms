
jonckheereTerpstraTest <-
function(Data,Treatment,Response,TestDirection = 'Both',AlphaLevel = 0.05){
#' @export
#performs the JonckheereTerpstraTest
#Outputs a clean data table of responses 
JTTableInc<-{}
JTTableDec<-{}
JTTable<-{}
Data[ ,Treatment]<-as.factor(Data[ ,Treatment])

JTInc<-{};JTDec<-{} ;JTInc$p.value<-1; JTDec$p.value<-1

#Fix the random seed
Seed<- abs(sum(as.numeric(.Random.seed)))
Seed<- Seed-signif(Seed,6)

set.seed(Seed)
if (TestDirection=='Increasing' | TestDirection=='Both'){  #Increasing Test
	DataU<-Data
	Test = TRUE
	while (Test == TRUE){
		JTInc<-jonckheere.test(DataU[ ,Response],as.ordered(DataU[ ,Treatment]),nperm=50000,alternative='increasing')
		JTEntryInc<-cbind(JTInc$statistic,JTInc$p.value,length(levels(DataU[ ,Treatment])))
		JTTableInc<-rbind(JTTableInc,JTEntryInc)
		DataU<-DataU[DataU[ ,Treatment] != levels(DataU[ ,Treatment])[length(levels(DataU[ ,Treatment]))], ] #Remove the highest level of treatment
		DataU[ ,Treatment]<-factor(DataU[ ,Treatment])  #Removes the highest level of from treatment from the data frame
		
		if (length(levels(Data[ ,Treatment]))==1){
			Test = FALSE
		}
		if (JTInc$p.value>AlphaLevel){ 
			Test = FALSE
		}
	}
	JTTableInc[ ,2]<-round(JTTableInc[ ,2],4)
}

set.seed(Seed)
if (TestDirection=='Decreasing' | TestDirection=='Both'){ #Decreasing Test
	DataU<-Data
	Test = TRUE
	while (Test == TRUE){
		JTDec<-jonckheere.test(DataU[ ,Response],as.ordered(DataU[ ,Treatment]),nperm=50000,alternative='decreasing')
		
		JTEntryDec<-cbind(JTDec$statistic,JTDec$p.value,length(levels(DataU[ ,Treatment])))
		JTTableDec<-rbind(JTTableDec,JTEntryDec)
		DataU<-DataU[DataU[ ,Treatment] != levels(DataU[ ,Treatment])[length(levels(DataU[ ,Treatment]))], ] #Remove the highest level of treatment
		DataU[ ,Treatment]<-factor(DataU[ ,Treatment])  #Removes the highest level of from treatment from the data frame
		
		if (length(levels(Data[ ,Treatment]))==1){
			Test = FALSE
		}
		if (JTDec$p.value>AlphaLevel){ 
			Test = FALSE
		}
	}
	JTTableDec[ ,2]<-round(JTTableDec[ ,2],4)
}



	    
if (TestDirection=='Increasing'){
	JTTable<-JTTableInc
	Sig<-rep('.',dim(JTTable)[1])
	Sig[JTTable[ ,2]<0.05]<-'*'
	Sig[JTTable[ ,2]<0.01]<-'**'
	Sig[JTTable[ ,2]<0.001]<-'***'
	JTTable<-cbind(JTTable,Sig)
	colnames(JTTable)<-c('JT Statistic','Increasing Trend P-Value','Sig','Max Level of Treatment')	
}

if (TestDirection=='Decreasing'){
	JTTable<-JTTableDec
	Sig<-rep('.',dim(JTTable)[1])
	Sig[JTTable[ ,2]<0.05]<-'*'
	Sig[JTTable[ ,2]<0.01]<-'**'
	Sig[JTTable[ ,2]<0.001]<-'***'
	JTTable<-cbind(JTTable,Sig)
	colnames(JTTable)<-c('JT Statistic','Decreasing Trend P-Value','Sig','Max Level of Treatment')
}


	
if (TestDirection=='Both'){
	DI<-dim(JTTableInc)[1]
	DD<-dim(JTTableDec)[1]
	D<-max(DI,DD)
	
	SigI<-rep('.',dim(JTTableInc)[1])
	SigI[JTTableInc[ ,2]<0.05]<-'*'
	SigI[JTTableInc[ ,2]<0.01]<-'**'
	SigI[JTTableInc[ ,2]<0.001]<-'***'
	
	SigD<-rep('.',dim(JTTableDec)[1])
	SigD[JTTableDec[ ,2]<0.05]<-'*'
	SigD[JTTableDec[ ,2]<0.01]<-'**'
	SigD[JTTableDec[ ,2]<0.001]<-'***'
	
	JTTableDec<-cbind(JTTableDec,SigD)
	JTTableInc<-cbind(JTTableInc,SigI)
	
	JTTable<-cbind(rbind(JTTableDec,mat.or.vec(D-DD,dim(JTTableDec)[2])*NA),
		rbind(JTTableInc,mat.or.vec(D-DI,dim(JTTableInc)[2])*NA)
	)
	
	JTTable<-cbind(JTTable[ ,c(1,2,4,6,8)], JTTable[ ,c(3,7)[{DI==DD}+1]])
	
	
	colnames(JTTable)<-c('JT Statistic','Decreasing Trend P-Value','D.Sig',
		'Increasing Trend P-Value','I.Sig','Max Level of Treatment')
}



return(JTTable)
}
