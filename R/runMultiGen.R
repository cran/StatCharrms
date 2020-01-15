runMultiGen <-
function(Data,TreatVar='',ResponVar='',RepVar='Not Used',TimeVar='Not Used',Path,TestDirection='Descending',alpha=0.05){
#' @export
#TreatVar is the Treatment Variable
#ResponVar is the Response Variable
#RepVar is the Replicate Variable 
#GenVaris the Generation Variable 
#Path is a Global defined in CheckRun.mg
#Path indicates what analysis covariates to use 
#2 Time
#3 Group only
#Recheck for Variable Inclusion
#TestDirection is the direct of the statistical test it can be  Descending, Ascending, or Both 

#Converts the direction of the t-test into a string multcomp needs 

Alternative<-switch(TestDirection,
	Both="two.sided",
	Descending='less',
	Ascending="greater"
)

#Test to make sure the Variables are defined
if (TreatVar==''){
	popMessage('Treatment variable is not properly assigned.\n Please reassign the variable and rerun the analysis.')
	return
}
if (ResponVar==''){
	popMessage('Response variable is not properly assigned.\n Please reassign the variable and rerun the analysis.')
return
}
if (RepVar==''){
	popMessage('Replicate variable is not properly assigned.\n Please reassign the variable and rerun the analysis.')
return
}

#Make Group and Time ordered Variables  
#makes numbers >10 in alphabetical order
#makes numbers >10 in alphabetical order
Data$Group<-Data[ ,TreatVar]
Data$Count<-Data[ ,ResponVar]

if (identical(RepVar,'Not Used')==FALSE){
	Data$Replicate<-Data[ ,RepVar]
	Data$Subject<-Data$Group:Data$Replicate
}

if (identical(TimeVar,'Not Used')==FALSE){
	Data$Time<-Data[ ,TimeVar]
	Data$Time<-as.ordered(as.factor(Data$Time))
}

Data$Group<-as.ordered(as.factor(Data$Group))
Data$Count<-as.numeric(as.character(Data$Count))

#Remove missing data
if (length(which(is.na(Data$Count))>0)){
	Data<-Data[-which(is.na(Data$Count)), ]
}

#Remove non-numbers
if (length(which(is.finite(Data$Count)==FALSE))>0){
	Data<-Data[-which(is.finite(Data$Count)==FALSE), ]
}



#Make sure there still is more then one level of time
if (Path==2){
	#Time and Group at multiple levels
	if (nlevels(Data$Time)<2){
		Path=Path+1
	}
}

#Analysis for path two
if (Path==2){
##############################################################################################
#										Only Time and Group									 #
##############################################################################################


	#Frequency Table
	FreqTable<-xtabs(~Time+Group,data=Data)
	FreqTable<-cbind(FreqTable,colSums(t(FreqTable)))
	FreqTable<-rbind(FreqTable,colSums(FreqTable))
	rownames(FreqTable)[length(rownames(FreqTable))]<-'Total'
	colnames(FreqTable)[length(colnames(FreqTable))]<-'Total'


	Lmm<-lme(Count~Group*Time,
	random = ~ 1|Subject,  #Time is a random effect
	data = Data,na.action='na.omit', method = "REML")

	AnovaTable<-anova(Lmm)

	#Anova
	StdAov<-aov(Count~Time*Group,data=Data)



	##############################################################################################
	#										Main Effects  									     #
	##############################################################################################
	#Group
	ConstGroup<-{}                #Dunnett's Contrasts for Groups
	GLevels<-nlevels(Data$Group)
	for (i in 2:GLevels){
		Row<-rep(0,GLevels)
		Row[1]<--1
		Row[i]<-1
		ConstGroup<-rbind(ConstGroup,Row) 
		row.names(ConstGroup)[i-1]<-paste(levels(Data$Group)[i],'-',levels(Data$Group)[1])
	} 
	 
	Group.mcp<-mcp('Group'=ConstGroup,interaction_average = FALSE, covariate_average = FALSE)
	
	LT1<-glht(Lmm,linfct=Group.mcp,alternative=Alternative)
	GroupComp<-summary(LT1)
	GCI<-confint(LT1,level = 1-alpha)
	
	LT2<-glht(Lmm,linfct=mcp(Group='Dunnett'),alternative=Alternative)
	GroupCom<-summary(LT2)
	Df= anova(Lmm)[2,2]

	p.value<-switch (TestDirection, 
		Descending=pt(GroupComp$test$tstat,Df,lower.tail = TRUE),
		Ascending=pt(GroupComp$test$tstat,Df,lower.tail = FALSE),
		Both=pt(abs(GroupComp$test$tstat),Df,lower.tail = FALSE)*2
	)
	GroupEffects<-cbind(
		TreatVar,
		rownames(GroupCom$linfct),
		round(GroupComp$test$coefficients,4),
		round(GroupComp$test$sigma,4),
		Df,
		round(GCI$confint[ ,2],4),
	    round(GCI$confint[ ,3],4),
		round(GroupComp$test$tstat,4),
		round(p.value,4),
		round(GroupComp$test$pvalues,4),
		'.'
	)



	#if (length(which(p.value<=0.05))>0){
	#GroupEffects[which(GroupComp$test$pvalues<=0.05),7]='**'
	#}
	if (length(which(GroupComp$test$pvalues<=0.05))>0){
		GroupEffects[which(GroupComp$test$pvalues<=0.05),11]='*'
	}
	if (length(which(GroupComp$test$pvalues<=0.05))>0){
		GroupEffects[which(GroupComp$test$pvalues<=0.001),11]='**'
	}
	if (length(which(GroupComp$test$pvalues<=0.05))>0){
		GroupEffects[which(GroupComp$test$pvalues<=0.0001),11]='***'
	}



	##############################################################################################
	#Time

	ConstTime<-{}
	TLevels<-nlevels(Data$Time)
	count=0;
	for (i in 1:{TLevels-1}){
		for (j in {i+1}:TLevels){
			count=count+1
			Row<-rep(0,TLevels)
			Row[i]<-1
			Row[j]<--1
			ConstTime<-rbind(ConstTime,Row) 
			row.names(ConstTime)[count]<-paste(levels(Data$Time)[i],'-',levels(Data$Time)[j])
		}
	} 

	Time.mcp<-mcp('Time'=ConstTime,interaction_average = FALSE, covariate_average = FALSE)
	
	TLT<-glht(Lmm,linfct =Time.mcp)
	TCI<-confint(TLT,level = 1-alpha)
	
	TimeComp<-summary(TLT) 
	
	
	p.value<-switch (TestDirection, 
		Descending=pt(TimeComp$test$tstat,Df,lower.tail = TRUE),
		 Ascending=pt(TimeComp$test$tstat,Df,lower.tail = FALSE),
		      Both=pt(abs(TimeComp$test$tstat),Df,lower.tail = FALSE)*2
	)
	Df<-anova(Lmm)[3,2]
	TimeEffects<-cbind(
		TimeVar,
		rownames(TimeComp$linfct),
		round(TimeComp$test$coefficients,4),
		round(TimeComp$test$sigma,4),
		Df,
		round(TCI$confint[ ,2],4),
	    round(TCI$confint[ ,3],4),
		round(TimeComp$test$tstat,4),
		round(p.value,4),
		'.',
		'.'
	)


	if (length(which(p.value<=0.05))>0){
		TimeEffects[which(p.value<=0.05),11]='**'
	}


	MainEffects<-rbind(TimeEffects,GroupEffects)


	colnames(MainEffects)[1]<-'Effect'
	colnames(MainEffects)[2]<-'Levels'
	colnames(MainEffects)[3]<-'Estimate'
	colnames(MainEffects)[4]<-'StdErr'
	colnames(MainEffects)[5]<-'DF'
	colnames(MainEffects)[6]<-paste0('Lower ', round(100-alpha*100,0) ,'% CI')
	colnames(MainEffects)[7]<-paste0('Upper ', round(100-alpha*100,0) ,'% CI')
	colnames(MainEffects)[8]<-'T-Value'
	colnames(MainEffects)[9]<-'P-Value'
	colnames(MainEffects)[10]<-'AdjP'
	colnames(MainEffects)[11]<-'Sig'



		#Clean Output
	if(length(which(as.numeric(MainEffects[ ,9])>=1)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,9])>=1),9]<-'1'
	}
	if(length(which(as.numeric(MainEffects[ ,10])>=1)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,10])>=1),10]<-'1'
	}
	if(length(which(as.numeric(MainEffects[ ,9])<0.0001)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,9])<0.0001),9]<-'<1e-4'
	}
	if(length(which(as.numeric(MainEffects[ ,10])<0.0001)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,10])<0.0001),10]<-'<1e-4'
	}


	AnovaTable<-cbind(row.names(AnovaTable), AnovaTable)
		colnames(AnovaTable)[1]<-"Effect" 
		AnovaTable[ ,4]<-round(AnovaTable[ ,4],4)
		AnovaTable[ ,5]<-round(AnovaTable[ ,5],4)
		if(length(which(as.numeric(AnovaTable[ ,5])<0.0001)>0)){
		AnovaTable[which(as.numeric(AnovaTable[ ,5])<0.0001),5]<-'<1e-4'
	}

	ShapiroTest<-shapiro.test(Lmm$residuals[,1]) #Test for norm

	
	LeveneTest<-leveneTestSC(Data,'Group',Lmm$residuals[,1]) #Test for equal Variance
	
	

	
	
	
	Out=list('Anova.Table'=AnovaTable,'MainEffects'=MainEffects,
	'LeveneTest'=LeveneTest,'FreqTable'=FreqTable,'Lmm'=Lmm,'ShapiroTest'=ShapiroTest)
}

if (Path==3){

##############################################################################################
#										Only Group											 #
##############################################################################################


	# Only The Treatment Variable at multiple levels 
	FreqTable<-xtabs(~Group,data=Data)
	FreqTable<-c(FreqTable,sum(FreqTable))
	names(FreqTable)[length(names(FreqTable))]='Total'
	StdAov<-aov(Count~Group, data=Data)

	TestData<-Data
	if (RepVar != 'Not Used'){
		if (nlevels(Data$Subject)<dim(Data)[1]){
			Lmm<-lme(Count~Group, random = ~ 1|Subject, data = Data, method = "REML") 
			AnovaTable<-anova(Lmm)
			Df<-AnovaTable$denDF[2]
		} 
		if (nlevels(Data$Subject)>=dim(Data)[1]){
			Lmm<-StdAov
			AnovaTable<-anova(Lmm)
			Df<-AnovaTable[2,1]
		}
	}
	if (RepVar == 'Not Used'){
		Lmm<-StdAov
		AnovaTable<-anova(Lmm)
		Df<-AnovaTable[2,1]
	}


##############################################################################################
#										Main Effects 									     #
##############################################################################################
	#Group
	ConstGroup<-{}                #Dunnett's Contrasts for Groups
	GLevels<-nlevels(Data$Group)
	for (i in 2:GLevels){
		Row<-rep(0,GLevels)
		Row[1]<--1
		Row[i]<-1
		ConstGroup<-rbind(ConstGroup,Row) 
		row.names(ConstGroup)[i-1]<-paste(levels(Data$Group)[i],'-',levels(Data$Group)[1])
	} 
	 
	 
	Group.mcp<-mcp('Group'=ConstGroup,interaction_average = FALSE, covariate_average = FALSE)
	GLT<-glht(Lmm,linfct=Group.mcp,alternative=Alternative)
	GroupComp<-summary(GLT)
	GCI<-confint(GLT,level = 1-alpha)

	GroupCom<-summary(glht(Lmm,linfct=mcp(Group='Dunnett'),alternative=Alternative))
		p.value<-switch (TestDirection, 
		Descending=pt(GroupComp$test$tstat,Df,lower.tail = TRUE),
		Ascending=pt(GroupComp$test$tstat,Df,lower.tail = FALSE),
		Both=pt(abs(GroupComp$test$tstat),Df,lower.tail = FALSE)*2
	)


	GroupEffects<-cbind(TreatVar,
		rownames(GroupCom$linfct),
		round(GroupComp$test$coefficients,4),
		round(GroupComp$test$sigma,4),
		Df,
		round(GCI$confint[ ,2],4),
		round(GCI$confint[ ,3],4),
		round(GroupComp$test$tstat,4),
		round(p.value,4),
		round(GroupComp$test$pvalues,4),
		'.'
	)


	if (length(which(p.value<=0.05))>0){
		GroupEffects[which(GroupComp$test$pvalues<=0.05),11]='*'
	}
	if (length(which(p.value<=0.05))>0){
		GroupEffects[which(GroupComp$test$pvalues<=0.001),11]='**'
	}
	if (length(which(p.value<=0.05))>0){
		GroupEffects[which(GroupComp$test$pvalues<=0.0001),11]='***'
	}

	MainEffects<-GroupEffects
	colnames(MainEffects)[1]<-'Effect'
	colnames(MainEffects)[2]<-'Levels'
	colnames(MainEffects)[3]<-'Estimate'
	colnames(MainEffects)[4]<-'StdErr'
	colnames(MainEffects)[5]<-'DF'
	colnames(MainEffects)[6]<-paste0('Lower ', round(100-alpha*100,0) ,'% CI')
	colnames(MainEffects)[7]<-paste0('Upper ', round(100-alpha*100,0) ,'% CI')
	colnames(MainEffects)[8]<-'T-Value'
	colnames(MainEffects)[9]<-'P-Value'
	colnames(MainEffects)[10]<-'AdjP'
	colnames(MainEffects)[11]<-'Sig'




	#Clean Output
	if(length(which(as.numeric(MainEffects[ ,9])>=1)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,9])>=1),9]<-'1'
	}
	if(length(which(as.numeric(MainEffects[ ,10])>=1)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,10])>=1),10]<-'1'
	}
	if(length(which(as.numeric(MainEffects[ ,9])<0.0001)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,9])<0.0001),9]<-'<1e-4'
	}
	if(length(which(as.numeric(MainEffects[ ,10])<0.0001)>0)){
		MainEffects[which(as.numeric(MainEffects[ ,10])<0.0001),10]<-'<1e-4'
	}

	AnovaTable<-cbind(row.names(AnovaTable), AnovaTable)
	colnames(AnovaTable)[1]<-"Effect" 
	AnovaTable[ ,4]<-round(AnovaTable[ ,4],4)
	AnovaTable[ ,5]<-round(AnovaTable[ ,5],4)
	if(length(which(as.numeric(AnovaTable[ ,5])<0.0001)>0)){
		AnovaTable[which(as.numeric(AnovaTable[ ,5])<0.0001),5]<-'<1e-4'
	}

	Residuals<-Lmm$residuals
	if (is.matrix(Residuals)==TRUE){
		Residuals<-Residuals[ ,1]
	}
	ShapiroTest<-shapiro.test(Residuals) #Test for norm
	LeveneTest<-leveneTestSC(Data,'Group',Residuals) #Test for equal Variance
	
	


	
	
	Out=list('Anova.Table'=AnovaTable,'MainEffects'=MainEffects,
	'LeveneTest'=LeveneTest,'FreqTable'=FreqTable,'Lmm'=Lmm,'ShapiroTest'=ShapiroTest)
	}
return(Out)
}
