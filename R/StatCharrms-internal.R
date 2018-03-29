.changeStatusValues <-
function(){
#Changes the Cencerd Value to 0
#Changes the Event Value to 1
#' @export

	TempData<-.time2EventEnv$UseData
	#Change Event status to 1

	.time2EventEnv$UseData[which(TempData[ ,.time2EventEnv$StatusVar]==.time2EventEnv$StatusEventVal),.time2EventEnv$StatusVar]<-1
	if (length(which(TempData[ ,.time2EventEnv$StatusVar]==.time2EventEnv$StatusEventVal)) == 0){
	popMessage('There are no event. Analysis aborted.')
	return(FALSE)
	}

	if (identical(.time2EventEnv$StatusCenVal,'Not Used') == FALSE){
	#Change Censored status to 0
	.time2EventEnv$UseData[which(TempData[ ,.time2EventEnv$StatusVar]==.time2EventEnv$StatusCenVal),.time2EventEnv$StatusVar]<-0
	}
return(TRUE)
}
.getTimeData <-
function(Data,TimeVar,Format,TimeInt,ReplicateVar,TreatmentVar,EndPointVar){
#' @export
#Averages Time over multiple possible endpoints
Data[ ,TimeVar]<-date2Numeric(Data[ ,TimeVar],Format)
colnames(Data)[which(colnames(Data)==TimeVar)]<-'Numeric_Time'

TempData<-tranformTime(Data,'Numeric_Time',as.numeric(TimeInt),
ReplicateVar, TreatmentVar,EndPointVar[1])
if (length(EndPointVar)>1){
Remove<-which(is.element(colnames(TempData),EndPointVar[-1]))
TempData<-TempData[ -Remove]
for (i in 2:length(EndPointVar)){ 
TimeData<-tranformTime(Data,'Numeric_Time',as.numeric(TimeInt),
ReplicateVar, TreatmentVar,EndPointVar[i])

TimeData<-TimeData[ ,c('UniqueID.SC',EndPointVar[i])]
TempData<-merge(TempData,TimeData,by ='UniqueID.SC')
}
}
 TempData<-TempData[ ,-which(colnames(TempData)=='UniqueID.SC')]

return(TempData)
}
.lengthNa <-
function(Vec){
#' @export
#This is a length function that acts like a rm.na=TRUE option
if (sum(is.na(Vec))>0) Vec<-Vec[-which(is.na(Vec))]
return(length(Vec))
}
.makePlot <-
function(Data,Response,Results,EndPoints,TreatmentVar,Scale,PlotType,ByVar){
#' @export
#This function will make a plot
#uses 
DataUse<-Data

NameResponse<-Response
TrasnStr<-''
#Convert the response and names to indicate the transformation used
if (Scale==TRUE){  #Add Transform used to Title
	DataUse<-responseTransform(Data,NameResponse,Results[[NameResponse]]$TransformationUsed) #Transform Data
	#Filter out non-numbers
	if (length(which(is.na(DataUse$TransformedResponse)))>0){
		DataUse<-DataUse[-which(is.na(DataUse$TransformedResponse)), ]
	}
	if (length(which(is.finite(DataUse$TransformedResponse) == FALSE))>0){
		DataUse<-DataUse[-which(is.finite(DataUse$TransformedResponse) == FALSE), ]
	}
	TrasnStr<-paste(Results[[NameResponse]]$TransformationUsed,'Transformed')
	Response<-'TransformedResponse'
	if (Results[[NameResponse]]$TransformationUsed=='None'){ #If no transform was used tell user of that
		TrasnStr<-gsub('None','Not',TrasnStr)
		Response<-NameResponse
		DataUse<-Data
	}
}

#Box-Plot
if (PlotType=='Box'){
print(bwplot(DataUse[ ,Response]~DataUse[ ,TreatmentVar],
main=paste(NameResponse,'for each',TreatmentVar,'\n',TrasnStr),
xlab=TreatmentVar,ylab=paste(NameResponse,TrasnStr), horizontal = FALSE))
}
#Quantile-Quantile-Plot
if (PlotType=='Quantile-Quantile'){
Model<-aov(DataUse[ ,Response]~DataUse[ ,TreatmentVar])
qqnorm(Model$residuals,main=paste('Normal Q-Q Plot for\n',NameResponse,TrasnStr,'by',TreatmentVar))
qqline(Model$residuals)
}
#Violin-Plot
if (PlotType=='Violin'){
print(bwplot(DataUse[ ,Response]~DataUse[ ,TreatmentVar],
main=paste(NameResponse,'for each',TreatmentVar,'\n',TrasnStr),
xlab=TreatmentVar,ylab=paste(NameResponse,TrasnStr), horizontal = FALSE,panel=panel.violin))
}


if (PlotType=='Interaction'){
print(interaction.plot(DataUse[[ByVar]],DataUse[[TreatmentVar]],DataUse[[Response]],col=1:nlevels(DataUse[[TreatmentVar]])
,ylab=Response,xlab=ByVar,trace.label=TreatmentVar,
main=paste('Interaction plot for\n',Response,'by',ByVar)))
}

if (PlotType==''){
print(bwplot(DataUse[ ,Response]~DataUse[ ,TreatmentVar],
main=paste(NameResponse,'for each',TreatmentVar,'\n',TrasnStr),
xlab=TreatmentVar,ylab=paste(NameResponse,TrasnStr), horizontal = FALSE,panel=panel.violin))
}

}
.Random.seed <-
c(403L, 282L, 1162133435L, -678811484L, -1593745541L, 2106825706L, 
-1678737836L, 1633203179L, -178222632L, -280844829L, -504858409L, 
-1019680754L, -205248251L, 622865580L, -636933109L, 2070308299L, 
1785834012L, -1889448849L, 857695142L, 1922890729L, 2074082809L, 
-1918552742L, 1782288988L, -1877740848L, 1812705903L, 1568310097L, 
958635801L, -2104666161L, -1890052541L, 1225410922L, -457955346L, 
1261700976L, -1050527673L, 788643394L, 1743462805L, -1983764406L, 
-739351685L, 1082545014L, 1886878320L, 1664695152L, -244735773L, 
1120199621L, -1813426959L, 655847875L, -1594734210L, 146214903L, 
-1536937992L, -487966224L, -811928753L, -372861730L, -1442372922L, 
820545090L, -1827433373L, -377223378L, -1782364834L, -1895365642L, 
490729850L, 1021693743L, -2058185580L, 2012767611L, 415656057L, 
1256776554L, 1523553563L, 174562481L, 384260713L, -1279217361L, 
140342715L, -432283109L, 752717893L, 190418328L, 638220211L, 
-1014217614L, -241239724L, 558900487L, -806440591L, 591004430L, 
1123659807L, 1566725783L, 1668197378L, 1782292927L, -230253327L, 
-921656255L, 250215752L, -346350154L, -1446599642L, -1545942360L, 
-467111404L, -260071525L, -904308751L, 715110066L, 27820359L, 
-1505676965L, -669211734L, 55877703L, 1516406641L, -179362094L, 
1182757848L, -87993505L, -443573777L, -848529479L, 1410639063L, 
1733733990L, 1552427635L, -1410558840L, -923231239L, 1778263216L, 
-88621814L, 944181719L, -908402477L, -1964230458L, -68260451L, 
370210474L, -203330636L, -110527764L, -306830062L, -1844164942L, 
356934239L, 1153862192L, 1827130701L, -1023924432L, -1325208562L, 
446256585L, -1511579674L, -401276224L, 558732826L, 1211097982L, 
-7108707L, 1081505079L, -556570576L, -2102306880L, 515600534L, 
1783476971L, 461368234L, -2124695521L, 634193785L, 419000795L, 
1704301114L, 260683547L, -2135494210L, 1612963945L, -1957241876L, 
1637200155L, 615364106L, 205113521L, 2039953261L, 680458464L, 
-1320332590L, -160041677L, -1269798435L, -1482175343L, 497382740L, 
-1808146897L, -1629870346L, -636607368L, -115989528L, -205759044L, 
143021243L, -1064240833L, -1790107806L, -764385255L, -1868417218L, 
-547430889L, 1806279311L, -309645209L, -2048280616L, -1945328917L, 
444953471L, 1113475168L, 75009294L, -2054457965L, -1582116747L, 
-2063498591L, -2138450706L, 1528430716L, 1205642502L, -510803022L, 
2071053186L, 1754168550L, -1294388622L, 1259845265L, 1589038911L, 
-368699262L, 980256190L, 1019682878L, -1862270137L, -2008816426L, 
908225093L, -679139679L, -1566519870L, -405135640L, 1170431759L, 
-5759967L, -1695764139L, 1972591832L, 1438000531L, -172793616L, 
2029385177L, -334679731L, -367761608L, -94542792L, 1686288779L, 
-188586744L, -1016190536L, 1669778645L, -1247747524L, 1627351245L, 
1701915586L, 838870517L, 2012471539L, -75795345L, -1439816594L, 
1650694195L, 1869676921L, 94190937L, 177487894L, 1032132993L, 
1580753355L, 740132587L, 8806165L, -1151339185L, -720198889L, 
-802543905L, -1729029599L, 1761439035L, 964042058L, -501370125L, 
57028940L, -2068693360L, -1375760215L, 127909631L, -1249828033L, 
1787329797L, -800727495L, 1888439524L, 2056668880L, 1433258465L, 
457148438L, -1629544342L, -1297854474L, 563316568L, 239141524L, 
-1572792673L, -266314278L, -569817898L, 1254417333L, -627113968L, 
-481618736L, -111297827L, 433030640L, -1347890334L, 2125356571L, 
-724137378L, -433662223L, 873046402L, 4593955L, 385290537L, -940303747L, 
-1461034719L, 230269722L, 871943627L, -1454548662L, 960165776L, 
776835566L, -910488301L, -1820463273L, -1152556172L, 879300460L, 
-548604069L, 798144010L, 1279980320L, 1443718913L, 686929769L, 
1967196537L, -757044886L, -1549739762L, -444428159L, -17785943L, 
-1715443148L, -1706817268L, 920743226L, 1050827671L, -124362190L, 
-906927247L, 1287781056L, 1620192221L, -1016343542L, 909085360L, 
1702526930L, -1271785758L, 1855162874L, -1830366261L, -522463503L, 
76392688L, -370317003L, 674173338L, -1944545668L, -909920018L, 
-2072559843L, 1797311981L, -1473929550L, -1585023036L, -809761489L, 
12896123L, 920408505L, 825209791L, 842790912L, 1128087117L, 1744152299L, 
518021485L, 2011345563L, -892558946L, -410874927L, 526823333L, 
1487481681L, -228036726L, 573159059L, 277857711L, 1443510418L, 
687474517L, 1057324266L, -343032461L, 866879474L, 1501606780L, 
836851613L, 1832820660L, -569919054L, 1516696156L, -1385677050L, 
-620363606L, -2057771040L, 189301237L, -992924602L, 1547346055L, 
-540550677L, 1828396432L, -924280122L, -2025963647L, 711128340L, 
-1957122083L, -1993766836L, 770455017L, 1617469818L, -1850216766L, 
-1380110491L, -62875635L, -581066695L, 1551102353L, 822965885L, 
1581911369L, -228424782L, -836867838L, -1892161174L, 1654924970L, 
1486728224L, 1039118640L, -677174811L, 37891313L, 2092240392L, 
179439152L, -2024395008L, -1153309733L, -324112116L, -2022836353L, 
1364775643L, -661267391L, -644077858L, -1628475148L, 2135351374L, 
-404300907L, 431966341L, 744795696L, 1160708350L, 251166947L, 
-454879234L, -2075099609L, 211576601L, -1252845013L, -1953153030L, 
1248804188L, -365254523L, 1630711509L, 877097527L, 287760291L, 
896461951L, 218314457L, 449438339L, 1646027099L, 1982114252L, 
-933323933L, 872527703L, 2093672667L, 1654133965L, -923107805L, 
-1100371240L, -1953760114L, 1785795084L, -1262053850L, -1579718771L, 
-1438282129L, 954870104L, -1500869748L, -1439490733L, 201434211L, 
-1755473355L, 1932004561L, 1938640571L, -100049067L, 1848026076L, 
1580613060L, -1603748553L, -610213474L, 1977747527L, -1791216647L, 
439994909L, 641337947L, -1279890861L, -296067617L, 351155024L, 
-964902773L, 1464907900L, 1768671306L, 1742961614L, -1727145793L, 
-850690234L, 160665382L, 1007106842L, -782747785L, 190310585L, 
-444739791L, 2043437113L, 792706977L, 517835517L, 180239403L, 
-1686921815L, -1689686983L, 863732342L, 2120693036L, 63199838L, 
1368103336L, 218290319L, 709094221L, -1092194413L, 920267795L, 
-1103155669L, -1828411155L, -1747377191L, -1598919629L, 811144341L, 
1304133046L, 669019091L, -618440663L, -1273053303L, -980496530L, 
1548122683L, -1500935511L, -66279750L, 316845629L, 1251668550L, 
1600426526L, -2089687935L, -221541194L, 2115350639L, 1506837741L, 
760105634L, 901610792L, -1386855948L, 979962081L, 1058327003L, 
-650351128L, 1473705109L, 1368671555L, -1332189733L, 722393049L, 
-1447376846L, 64180222L, 624755165L, -557804635L, -815264047L, 
-540439224L, -1452352822L, 815693272L, 921211028L, -1287929916L, 
1894787878L, 947347578L, 1970380440L, 481000236L, 1994182966L, 
-1167752614L, 413235821L, 1698396772L, 1215614980L, 57094304L, 
-464510674L, -141570436L, -1184479999L, 1677473790L, 40473374L, 
1426159891L, 1151137317L, -2069548746L, -1027241591L, -138969472L, 
1723628378L, 1449241510L, -1215816388L, 1880264454L, 1204072705L, 
523639348L, -92355678L, 1220049893L, 725755810L, -1969891730L, 
-549969326L, 727192462L, -2028955162L, -1904002133L, -701258952L, 
-458885622L, -558661528L, -695606958L, -329369790L, 1687931532L, 
-1687109393L, 249949317L, -815100187L, -1359190104L, -997574851L, 
529021189L, -1249962315L, -1287707913L, -110312119L, 858713982L, 
652777897L, 764632784L, -849385676L, 1261239308L, 1166261027L, 
112459976L, -165547535L, -1648356183L, -2100203412L, 25990888L, 
1885877522L, -1295954470L, -444924266L, -1724970947L, -1383339594L, 
808943297L, -1668288298L, 1718417723L, 1939282489L, -408633664L, 
-1831894543L, 1857974256L, -1374536488L, -1625056786L, 1130806768L, 
-2019333642L, 518155237L, -1542294598L, -432978505L, 59095710L, 
1361588652L, -1540049188L, 1375936391L, -2035081738L, -1835437866L, 
-1031847736L, -370913946L, 1457756373L, -1735710327L, 1099161836L, 
-986657773L, -1058272751L, -1524375190L, 56699864L, -1182278994L, 
-1711032888L, 493356832L, -1062210400L, 537949249L, -1696757075L, 
302869586L, 1190779852L, 1998899032L, -1193151450L, 1500622802L, 
843758549L, 1766678578L, -532991053L, -1198614715L, 1029174974L, 
394221393L, 1290101002L, -2062287277L, 851220537L, -999014578L, 
1023575866L, -1190408884L, 1973582043L, -2007698456L, 1340786922L, 
-1106398623L, -2065254085L, -103828140L, -1833693719L, -150982777L, 
438212440L, 450090037L, 1654107702L, -222631722L, -530281056L, 
-614587639L, -607890640L, 1079415192L, 147247995L, 165499753L, 
-1649964388L, 1603449989L, 1154637966L, 678878811L, 1083684733L, 
416073391L, 383395771L, 300900717L)
.saveGraph <-
function(Dir,FileName,Response){ #One Response
#' @export
#This function saves graphs specificity from the GUI for the One-Way Anova Part
#It uses the global names for the data set and values

#Save graphs as PDF
options(warn=-1)
dir.create(Dir)
options(warn=0)

FileNameG<-paste(Dir,'\\',FileName,sep='')
#Start PDF Output
pdf(paste(FileNameG,Response,'-Graphs.pdf',sep=''))
for (Scale in c(FALSE,TRUE)){
for (PlotType in .stdEndEnv$PlotTypeList){  #PlotTypeList.sa is a global from the GUI
.makePlot(.stdEndEnv$PlotData,Response,.stdEndEnv$Results,.stdEndEnv$EndPointVar,
.stdEndEnv$TreatmentVar,Scale,PlotType,.stdEndEnv$ByVar)
}
}
#Ends PDF output
dev.off()
dev.set(dev.prev())
}
.saveGraphs <-
function(Dir,FileName){ #All Graphs 
#' @export
#This function saves graphs specificity from the GUI for the One-Way Anova Part
#It uses the global names for the data set and values

#Save graphs as PDF
dir.create(Dir , showWarnings = FALSE)



if (identical(.stdEndEnv$TimeVar,'Not Used')==FALSE){
.stdEndEnv$PlotData<-.getTimeData(.stdEndEnv$DataSub,.stdEndEnv$TimeVar,.stdEndEnv$Format,.stdEndEnv$TimeIntGraph,.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)
colnames(.stdEndEnv$PlotData)[which(colnames(.stdEndEnv$PlotData)=='Averaged_Numeric_Time')]<-'Time'
.stdEndEnv$PlotData$Time<-as.factor(.stdEndEnv$PlotData$Time)
}

FileNameG<-paste(Dir,'\\',FileName,sep='')

for (Response in .stdEndEnv$EndPointVar){
#Start PDF Output
pdf(paste(FileNameG,'-',Response,'-',.stdEndEnv$TreatmentVar,'-Graphs.pdf',sep=''))
for (Scale in c(FALSE,TRUE)){
for (PlotType in .stdEndEnv$PlotTypeList){  #PlotTypeList.sa is a global from the
.makePlot(.stdEndEnv$PlotData,Response,.stdEndEnv$Results,.stdEndEnv$EndPointVar,
.stdEndEnv$TreatmentVar,Scale,PlotType,'Time')
}
}
#Ends PDF output
dev.off()
dev.set(dev.prev())
}

#if time is used
if (identical(.stdEndEnv$TimeVar,'Not Used')==FALSE){
for (Response in .stdEndEnv$EndPointVar){
#Start PDF Output
pdf(paste(FileNameG,'-',Response,'-Time','-Graphs.pdf',sep=''))
for (Scale in c(FALSE,TRUE)){
for (PlotType in .stdEndEnv$PlotTypeList){  #PlotTypeList.sa is a global from the
.makePlot(.stdEndEnv$PlotData,Response,.stdEndEnv$Results,.stdEndEnv$EndPointVar,
'Time',Scale,PlotType,.stdEndEnv$TreatmentVar)
}

}
#Ends PDF output
dev.off()
dev.set(dev.prev())
}
}
}
.saveGraphs.te <-
function(Dir,FileName){ #One Response
#This function saves graphs specificity from the GUI for the Time to effect module 
#It uses the global names for the data set and values
#' @export

#Save graphs as PDF
options(warn=-1)
dir.create(Dir)
options(warn=0)

FileNameG<-paste(Dir,'\\',FileName,sep='')
#Start PDF Output
pdf(paste(FileNameG,'-Graphs.pdf',sep=''))
#Make the plot
Colors=c("Black", "red", "blue", "orange","purple","green")

Lines = c(rep(1,ceiling(nlevels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]))/2)),
rep(2,floor(nlevels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]))/2)))


Sys.sleep(.1)
plot(.time2EventEnv$Results$FitS, conf.int = FALSE, main='Plot of Raw Data By Treatment',xlab=.time2EventEnv$TimeVar,ylab='Percent In Original Status',
col = Colors,lty=Lines,lwd=1.5) 

legend('bottomleft',levels(as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar])),lty = Lines,
col =Colors) 
#Ends PDF output
dev.off()
dev.set(dev.prev())

}
.saveResults <-
function(Results,Dir,FileName){
#' @export
#This function will save the results of the of an mg analysis 
options(warn=-1)
dir.create(Dir)
options(warn=0)

#I want to put in a message to the user in the output
#Extract Results
Response<-Results$Response
SummaryTable<-Results$SummaryTable


WilksResults<-Results$WilksResults
LeveneResults<-Results$LeveneResults


AnovaResults<-Results$AnovaResults
OneWayDunnetResults<-Results$OneWayDunnetResults

JonckheereTerpstraResults<-Results$JonckheereTerpstraResults

MonocityTable<-Results$MonocityTable

DunnsTable<-Results$DunnsTable

WilliamsTableUp<-Results$WilliamsTableUp #2017-10-17
WilliamsTableDown<-Results$WilliamsTableDown #2017-10-17


Transform<-Results$TransformationUsed
TestType<-Results$TestType


FileNameUse<-paste(FileName,Response,'-')

#Start HTML OutPut
HTMLStart(outdir=Dir, filename= FileNameUse,
extension="html", echo=FALSE, HTMLframe=TRUE)  

Title<-paste('<center>Results from ',Response,'</center>',sep='')
HTML.title(Title, HR=1,CSSstyle='')

#HTML for Data Summary Table
if (is.null(SummaryTable)==FALSE){
HTML('<center><b>Data Summary Table for Untransformed/Unweighted Data </b></center>')
HTML(SummaryTable,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Monotonicity  Test
if (is.null(MonocityTable )==FALSE){
HTML('<center><b>Test for Monotonicity </b></center>')
HTML(MonocityTable ,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Jonckheere-Terpstra Test
if (is.null(JonckheereTerpstraResults )==FALSE){
HTML('<center><b>Jonckheere-Terpstra Test</b></center>')
HTML(JonckheereTerpstraResults ,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Shapiro Wilks Test Table
if (is.null(WilksResults)==FALSE){
HTML('<center><b>Shapiro Wilks Test for Normality</b></center>')
HTML(WilksResults,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Levene's Test Table
if (is.null(LeveneResults)==FALSE){
HTML("<center><b>Levene's test for equality of variances</b></center>")
HTML(LeveneResults,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Anova
if (is.null(AnovaResults)==FALSE){
HTML('<center><b>AnovaTable</b></center>')
HTML(AnovaResults,row.name=TRUE,innerBorder = 1,HR=1)}

#HTML for Dunnets Test
if (is.null(OneWayDunnetResults)==FALSE){
HTML('<center><b>Dunnets Test</b></center>')
HTML(OneWayDunnetResults,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Dunn Test
if (is.null(DunnsTable)==FALSE){
HTML('<center><b>Dunns Test</b></center>')
HTML(DunnsTable,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Williams Test 2017-10-18
if (is.null(WilliamsTableUp)==FALSE){
HTML('<center><b>Williams Test for Increasing Trend</b></center>')
HTML(WilliamsTableUp,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Williams Test 2017-10-18
if (is.null(WilliamsTableDown)==FALSE){
HTML('<center><b>Williams Test for Decreasing Trend</b></center>')
HTML(WilliamsTableDown,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#Stamp the output
.stdEndEnv$Message<-.stampOutput(Transform,TestType)

HTML(.stdEndEnv$Message)

HTMLStop() #End HTML OutPut
#Deletes Junk files
unlink(paste(Dir,'\\',FileNameUse,'.html',sep=''))
unlink(paste(Dir,'\\',FileNameUse,'_menu.html',sep='')) 
Sys.sleep(.01)
}
.saveResults.te <-
function(Results,Dir,FileName){
#' @export
#This function will save the results of the of an mg analysis 
options(warn=-1)
dir.create(Dir)
options(warn=0)


#I want to put in a message to the user in the output
#Extract Results

FileNameUse<-paste(FileName)

#Start HTML OutPut
HTMLStart(outdir=Dir, filename= FileNameUse,
extension="html", echo=FALSE, HTMLframe=TRUE)  

Title<-'<center>Results</center>'
HTML.title(Title, HR=1,CSSstyle='')

#Effects Table
EffectsTable<-as.data.frame(Results$EffectsTable)
EffectsTable<-cbind(rownames(EffectsTable), EffectsTable)
colnames(EffectsTable)[1]<-'Comparison'

.time2EventEnv$Results$FitMe$coefficients

#HTML for Data  Table
if (is.null(.time2EventEnv$UseData)==FALSE){
HTML('<center><b>Data Used in Analysis </b></center>')
HTML(.time2EventEnv$UseData,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Main Effects Results
if (is.null(EffectsTable)==FALSE){
HTML('<center><b>Main Effects Table</b></center>')
HTML(EffectsTable ,row.name=FALSE,innerBorder = 1,CSSstyle='')}

#HTML for Main Effects Results
MedianTable<-as.data.frame(Results$MedianTable)


if (is.null(MedianTable)==FALSE){
	if (sum(is.na(MedianTable)) > 0){
		NAs<-which(is.na(MedianTable)==TRUE,arr.ind = TRUE)
		MedianTable[NAs]<-'-'
	}
	HTML('<center><b>Median Time to Effect with 95% CI</b></center>')
	HTML(MedianTable ,row.name=FALSE,innerBorder = 1,CSSstyle='')
}



#Stamp the output
.stampOutput.te()

HTMLStop() #End HTML OutPut
#Deletes Junk files
unlink(paste(Dir,'\\',FileNameUse,'.html',sep=''))
unlink(paste(Dir,'\\',FileNameUse,'_menu.html',sep='')) 
Sys.sleep(.01)
}
.stampOutput <-
function(Transform,TestType){
#' @export
#This function writes the user's selection to the output file created.
#This function assumes that it is called from the StdAnylsis saving function
.stdEndEnv$TempList<-strsplit(.stdEndEnv$FileName,'\\\\')  
.stdEndEnv$File<-.stdEndEnv$TempList[[1]][length(.stdEndEnv$TempList[[1]])]

Message1<-paste("<br>Use Test Type of: <b>",TestType,'</b>',"<br>Transformation of: <b>",Transform,'</b>',sep='')
Message2<-paste("<br>Using Generation variable: <b>",.stdEndEnv$GenerationVar,'</b>'," and Generation value: <b>",.stdEndEnv$GenerationVal,'</b>', sep='')
Message3<-paste("<br>Treatment Variable: <b>",.stdEndEnv$TreatmentVar,'</b>',"<br>Replicate Variable: <b>",.stdEndEnv$ReplicateVar,'</b>',sep='')
Message4<-paste("<br>Using Gender variable: <b>",.stdEndEnv$GenderVar,'</b>'," and Gender value: <b>",.stdEndEnv$GenderVal,'</b>', sep='')
Message5<-paste("<br>Using Age variable: <b>",.stdEndEnv$AgeVar,'</b>'," and Age value: <b>",.stdEndEnv$AgeVal,'</b>', sep='')
Message6<-paste("<br>Using Time variable: <b>",.stdEndEnv$TimeVar,'</b>'," and Time Increment: <b>",.stdEndEnv$TimeInt,'</b>', sep='')
Message7<-paste("<br>Using as weights: <b>",.stdEndEnv$WeightList,'</b>', sep='')
Message8<-paste("<br>Using Test Direction: <b>",.stdEndEnv$TestDirection,'</b>',"<br>Using Alpha Level: <b>",.stdEndEnv$AlphaLevel,'</b>',  sep='')
Message<-paste(Message1,Message2,Message3,Message4,Message5,Message6,Message7,Message8,'</b><br>',sep='')

return(Message)
}
.stampOutput.te <-
function(){
#' @export
#This function writes the user's selection to the output file created.
#This function assumes that it is called from the time to event saving function

Message1<-paste("<br>Treatment Variable: <b>",.time2EventEnv$TreatmentVar,'</b>',"<br>Replicate Variable: <b>",.time2EventEnv$ReplicateVar,'</b>',sep='')
Message2<-paste("<br>Time Variable: <b>",.time2EventEnv$TimeVar,'</b>', sep='')
Message3<-paste("<br>Status Variable: <b>",.time2EventEnv$StatusVar,'</b>',sep='')
Message4<-paste("<br>Event Status: <b>",.time2EventEnv$StatusEventVal,'</b>',"<br>Censored Status: <b>",.time2EventEnv$StatusCenVal,'</b>', sep='')
Message<-paste(Message1,Message2,Message3,Message4,'</b><br>',sep='')
#HTML('</div></center>')

HTML(Message)
return(Message)
}

.TestEndPoints <-
function(){
#' @export
#Tests Endpoints to see if they can be ran
#Turn off warnings
oldw <- getOption("warn")
options(warn=-1)  

#This tests to see if the end point can be tested on
for (Response in .stdEndEnv$EndPointVar){
RemoveMessage<-NULL
Remove<-FALSE
#Checks to see if more then 1 Treatment has a response
MeansTest<-MeansTest<-tapply(.stdEndEnv$UseData[ ,Response],.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar],mean,na.rm=TRUE)
if (var(MeansTest,na.rm=TRUE)==0 | is.na(var(MeansTest,na.rm=TRUE))==TRUE ){
RemoveMessage<-paste(RemoveMessage,Response, 'has a value for only one treatment: removing it from the analysis.\n')
Remove<-TRUE
}
#Checks to see if the vector is a constant vector
if (var(.stdEndEnv$UseData[ ,Response],na.rm=TRUE)==0 |is.na(var(.stdEndEnv$UseData[ ,Response],na.rm=TRUE))==TRUE ){
RemoveMessage<-paste(RemoveMessage,Response, 'is a consent vector: removing it from the analysis.\n')
Remove<-TRUE
}
if (is.factor(.stdEndEnv$UseData[ ,Response])==TRUE ){
RemoveMessage<-paste(RemoveMessage,Response, 'is not a number: removing it from the analysis.\n')
Remove<-TRUE
}
if (Response==.stdEndEnv$TreatmentVar){
RemoveMessage<-paste(RemoveMessage,Response, 'can not be used as a treatment and a response: removing it from the analysis.\n')
Remove<-TRUE
}
if (Remove==TRUE){
.stdEndEnv$EndPointVar<-.stdEndEnv$EndPointVar[-which(.stdEndEnv$EndPointVar==Response)]
popMessage(RemoveMessage)
}
}
#Revert Warnings
options(warn = oldw) 

}

.updateEnviroment <-
function(){
#' @export
#Updates the environment .stdEndEnv from the values in the GUI

.stdEndEnv$UseData<-.stdEndEnv$MainData
#Gather values from the GUI  
.stdEndEnv$TreatmentVar<-svalue(.stdEndEnv$TreatmentVarCbx)
.stdEndEnv$ReplicateVar<-svalue(.stdEndEnv$ReplicateVarCbx)
.stdEndEnv$ReplicateVar<-svalue(.stdEndEnv$ReplicateVarCbx)
.stdEndEnv$GenerationVar<-svalue(.stdEndEnv$GenerationVarCbx)
.stdEndEnv$GenerationVal<-svalue(.stdEndEnv$GenerationValCbx)
.stdEndEnv$AgeVar<-svalue(.stdEndEnv$AgeVarCbx)
.stdEndEnv$AgeVal<-svalue(.stdEndEnv$AgeValCbx)
.stdEndEnv$GenderVar<-svalue(.stdEndEnv$GenderVarCbx)
.stdEndEnv$GenderVal<-svalue(.stdEndEnv$GenderValCbx)
.stdEndEnv$WeightVar<-svalue(.stdEndEnv$WeightVarCbx)
.stdEndEnv$TestDirection<-svalue(.stdEndEnv$TestDirectionCbx)
.stdEndEnv$AlphaLevel<-as.numeric(svalue(.stdEndEnv$AlphaLvCbx))
.stdEndEnv$TimeVar<-svalue(.stdEndEnv$TimeVarCbx)
.stdEndEnv$Format<-FindFormat(svalue(.stdEndEnv$TimeValCbx),.stdEndEnv$CurrentDate)
.stdEndEnv$TimeInt<-as.numeric(svalue(.stdEndEnv$TimeIntCbx))
.stdEndEnv$TimeIntGraph<-as.numeric(svalue(.stdEndEnv$TimeIntGraphCbx))

#Convert to factors
if (identical(.stdEndEnv$TreatmentVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar])
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
}


#Apply Subsets
if(identical(.stdEndEnv$GenerationVal,'Not Used')==FALSE){ #
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenerationVar] == .stdEndEnv$GenerationVal)
}

if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenderVar] == .stdEndEnv$GenderVal)
}

if(identical(.stdEndEnv$AgeVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$AgeVar] == .stdEndEnv$AgeVal)
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]) == dim(.stdEndEnv$UseData)[1]){
.stdEndEnv$ReplicateVar<-'Not Used'
}
}

#Exclude times 2018-3-20
if (length(.stdEndEnv$TimeExcludeVal)>0 & identical(.stdEndEnv$TimeExcludeVal, 'Not Used')==FALSE){
	.stdEndEnv$UseData<-.stdEndEnv$UseData[is.element(.stdEndEnv$UseData[ ,.stdEndEnv$TimeVar],.stdEndEnv$TimeExcludeVal)==FALSE, ]	#only times that are not excluded
}

.stdEndEnv$DataSub<-.stdEndEnv$UseData #DataSub is used to dynamically change the time interval for graphing 

#Test to see if there is still data
if (dim(.stdEndEnv$UseData)[1]==0){
popMessage('The Selection of Age, Generation, or Gender has caused
the data to become an empty set.  Please Reselect one of the above groups') 
return(FALSE)
}

if (is.na(.stdEndEnv$AlphaLevel)==TRUE){
popMessage('Please select a number between 0 and 1 for the Alpha Level ')
return(FALSE)
}
if (.stdEndEnv$AlphaLevel < 0  || .stdEndEnv$AlphaLevel > 1){
popMessage('Please select a number between 0 and 1 for the Alpha Level ')
return(FALSE)
}
return(TRUE)
}
.updateT2E <-
function(){
#' @export
#Updates the environment for Time2Event
#Also checks to see if the analysis can be ran


.time2EventEnv$UseData<-.time2EventEnv$MainData
#Gather values from the GUI  
.time2EventEnv$TreatmentVar<-svalue(.time2EventEnv$TreatmentVarCbx)
.time2EventEnv$ReplicateVar<-svalue(.time2EventEnv$ReplicateVarCbx)
.time2EventEnv$GenerationVar<-svalue(.time2EventEnv$GenerationVarCbx)
.time2EventEnv$GenerationVal<-svalue(.time2EventEnv$GenerationValCbx)
.time2EventEnv$GenderVar<-svalue(.time2EventEnv$GenderVarCbx)
.time2EventEnv$GenderVal<-svalue(.time2EventEnv$GenderValCbx)
.time2EventEnv$TimeVar<-svalue(.time2EventEnv$TimeVarCbx)
.time2EventEnv$Format<-FindFormat(svalue(.time2EventEnv$TimeValCbx),.time2EventEnv$CurrentDate)
.time2EventEnv$StatusVar<-svalue(.time2EventEnv$StatusVarCbx)
.time2EventEnv$StatusEventVal<-svalue(.time2EventEnv$StatusEventValCbx)
.time2EventEnv$StatusCenVal<-svalue(.time2EventEnv$StatusCenValCbx)
.time2EventEnv$CanRun<-FALSE



#Convert to factors
if (identical(.time2EventEnv$TreatmentVar, 'Not Used')==FALSE){
.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar]<-as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$TreatmentVar])
}else{
popMessage("A treatment variable needs to be defined.")
return(FALSE)
}

if (identical(.time2EventEnv$StatusVar, 'Not Used')==TRUE){
popMessage("A statues variable needs to be defined.")
return(FALSE)
}

if (identical(.time2EventEnv$StatusEventVal, 'Not Used')==TRUE){
popMessage("A value representing events needs to be defined")
return(FALSE)
}

if (identical(.time2EventEnv$StatusEventVal, 'Not Used')==TRUE){
popMessage("A value representing censored events needs to be defined")
return(FALSE)
}


if (identical(.time2EventEnv$TimeVar, 'Not Used')==TRUE){
popMessage("A time variable needs to be defined.")
return(FALSE)
}


if (identical(.time2EventEnv$ReplicateVar, 'Not Used')==FALSE){
.time2EventEnv$UseData[ ,.time2EventEnv$ReplicateVar]<-as.factor(.time2EventEnv$UseData[ ,.time2EventEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.time2EventEnv$UseData[ ,.time2EventEnv$ReplicateVar]) == dim(.time2EventEnv$UseData)[1]){
.time2EventEnv$ReplicateVar<-'Not Used'
}
}


#Apply Subsets
if(identical(.time2EventEnv$GenderVar,'Not Used')==FALSE){
.time2EventEnv$UseData<-subset(.time2EventEnv$UseData,.time2EventEnv$UseData[ ,.time2EventEnv$GenerationVar] == .time2EventEnv$GenerationVal)
}

if(identical(.time2EventEnv$GenderVar,'Not Used')==FALSE){
.time2EventEnv$UseData<-subset(.time2EventEnv$UseData,.time2EventEnv$UseData[ ,.time2EventEnv$GenderVar] == .time2EventEnv$GenderVal)
}

#Test to see if there is still data
if (dim(.time2EventEnv$UseData)[1]==0){
popMessage('The Selection of Generation, or Gender has caused
the data to become an empty set.  Please Reselect one of the above groups') 
return(FALSE)
}

#Change time
if (is.null(.time2EventEnv$TimeTemp)==TRUE){
try(.time2EventEnv$TimeTemp<-date2Numeric(.time2EventEnv$UseData[ ,.time2EventEnv$TimeVar],.time2EventEnv$Format))
UseTime<-checkTime(.time2EventEnv$TimeTemp) & .time2EventEnv$CanRun  #Update CanRun
if (UseTime == FALSE){
return(FALSE)
}else{
UseData[ ,.time2EventEnv$TimeVar]<-.time2EventEnv$TimeTemp
}
}

#Change Status Vars

Out<-.changeStatusValues()
return(Out)
}
.writeExamples <-
function(Folder){
#' @export
popMessage('This may take a moment')
dir.create(Folder, showWarnings = FALSE)
dir.create(paste(Folder,'\\Histology Example',sep=''), showWarnings = FALSE)
dir.create(paste(Folder,'\\Fecundity Example',sep=''), showWarnings = FALSE)
dir.create(paste(Folder,'\\Length - Weight Example',sep=''), showWarnings = FALSE)
dir.create(paste(Folder,'\\Time to Event Example',sep=''), showWarnings = FALSE)
Sys.sleep(.5)


fecundityData<-StatCharrms::fecundityData
lengthWeightData<-StatCharrms::lengthWeightData
eventTimeData<-StatCharrms::eventTimeData
exampleHistData<-RSCABS::exampleHistData


#Write Data
write.table(exampleHistData,paste(Folder,'\\Histology Example','\\Histology Example Data.csv',sep=''),
row.names=FALSE,sep=',' )
write.table(fecundityData,paste(Folder,'\\Fecundity Example','\\Fecundity Example Data.csv',sep=''),
row.names=FALSE,sep=',' )
write.table(lengthWeightData,paste(Folder,'\\Length - Weight Example','\\Length - Weight Data.csv',sep=''),
row.names=FALSE,sep=',' )
write.table(eventTimeData,paste(Folder,'\\Time to Event Example','\\Time to Event Example Data.csv',sep=''),
row.names=FALSE,sep=',' )


##################################################################################
#RSCABS

#Take the subset corresponding to F0-females of 16 weeks of age
exampleHistData.sub<-exampleHistData[which(exampleHistData$Generation=='F2' & 
exampleHistData$Genotypic_Sex=='Male' & exampleHistData$Age=='16_wk' ),  ]
#Run RSCABS
exampleResults<-runRSCABS(exampleHistData.sub,'Treatment','Replicate',test.type='RS')

#Create Dir
HistoDir<-paste(Folder,'\\Histology Example\\Results',sep='')
dir.create(HistoDir, showWarnings = FALSE)

write.table(exampleResults,paste(HistoDir,'\\Histology Example Results.csv',sep=''),row.names=FALSE,sep=',')

#Find endpoints that can be graphed
Names<-strsplit(as.character(exampleResults$Effect),split='')
Names<-lapply(Names,function(X){paste0(X[-length(X)],collapse = '')})
Names<-unique(Names)
Files<-lapply(Names,function(X){paste(HistoDir,'\\',X,sep='')})

#Graph all endpoints
CantPrint<-''
for (i in 1:length(Files)){

Msg<-try(plotRSCABS(exampleHistData,Names[[i]],'Treatment','Percent',
'Remove',NULL,'png',File=Files[[i]]))
if (is(Msg)[1]=='try-error'){
CantPrint<-paste(CantPrint,Names[[i]],sep=' \n ')
print(CantPrint)
dev.off()
}
}


##################################################################################
#Length Weight
LWDir=paste(Folder,'\\Length - Weight Example\\Results',sep='')
dir.create(LWDir, showWarnings = FALSE)
FileName<-'Length-Weight Example Results'



#Initializes variables
.stdEndEnv$TimeVar<-'Not Used'
.stdEndEnv$TimeInt<-21    #Time interval used 
.stdEndEnv$TimeIntGraph<-7 #Time interval used for graphing
.stdEndEnv$TimeExcludeVal<-{}
.stdEndEnv$GenerationVar<-'Generation'
.stdEndEnv$GenerationVal<-'F1'  #Can be a Character array
.stdEndEnv$GenderVar<-'SEX'
.stdEndEnv$GenderVal<-'M'  #Can be a Character array
.stdEndEnv$AgeVar<-'Age'
.stdEndEnv$AgeVal<-'16 week'
.stdEndEnv$ReplicateVar<-'Replicate'
.stdEndEnv$TreatmentVar<-'Treatment'
.stdEndEnv$Format<-"%m/%d/%Y"
.stdEndEnv$Results<-list()
.stdEndEnv$WeightsVar<-'Not Used'
.stdEndEnv$TestDirection<-'Both'
.stdEndEnv$AlphaLevel<-0.05
.stdEndEnv$UseData<-lengthWeightData

#manually upset data

if (identical(.stdEndEnv$TreatmentVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar])
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
}


#Apply Subsets
if(identical(.stdEndEnv$GenerationVal,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenerationVar] == .stdEndEnv$GenerationVal)
}

if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenderVar] == .stdEndEnv$GenderVal)
}

if(identical(.stdEndEnv$AgeVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$AgeVar] == .stdEndEnv$AgeVal)
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]) == dim(.stdEndEnv$UseData)[1]){
.stdEndEnv$ReplicateVar<-'Not Used'
}
}

.stdEndEnv$Results[['LENGTH']]<-autoStdAnylsis(.stdEndEnv$UseData,'LENGTH',.stdEndEnv$TreatmentVar,
'None',.stdEndEnv$WeightsVar.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,
.stdEndEnv$AlphaLevel)
.stdEndEnv$Results[['WEIGHT']]<-autoStdAnylsis(.stdEndEnv$UseData,'WEIGHT',.stdEndEnv$TreatmentVar,
'Log',.stdEndEnv$WeightsVar.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,
.stdEndEnv$AlphaLevel)
.stdEndEnv$Results[['WEIGHT']]$TestType<-'Auto'
.stdEndEnv$Results[['LENGTH']]$TestType<-'Auto'

#Save Results
.stdEndEnv$FileName<-'Example'
.saveResults(.stdEndEnv$Results[['WEIGHT']],LWDir,FileName)
.saveResults(.stdEndEnv$Results[['LENGTH']],LWDir,FileName)

#plots
.stdEndEnv$PlotTypeList<-c('Box','Quantile-Quantile','Violin')
.stdEndEnv$EndPointVar<-c('LENGTH','WEIGHT')
.stdEndEnv$PlotData<-.stdEndEnv$UseData
.saveGraphs(LWDir,FileName)


#Remove vairables

rm(list=ls(.stdEndEnv) ,envir =.stdEndEnv)

##################################################################################
#fecundity
FDir=paste(Folder,'\\Fecundity Example\\Results',sep='')
dir.create(LWDir, showWarnings = FALSE)
FileName<-'Fecundity Example Results'

.stdEndEnv$TimeVar<-'Date'
.stdEndEnv$TimeInt<-21    #Time interval used 
.stdEndEnv$TimeIntGraph<-7 #Time interval used for graphing
.stdEndEnv$TimeExcludeVal<-{}
.stdEndEnv$GenerationVar<-'Generation'
.stdEndEnv$GenerationVal<-'F1'  #Can be a Character array
.stdEndEnv$GenderVar<-'Not Used'
.stdEndEnv$GenderVal<-'Not Used'  #Can be a Character array
.stdEndEnv$AgeVar<-'Not Used'
.stdEndEnv$AgeVal<-'Not Used'
.stdEndEnv$ReplicateVar<-'Rep'
.stdEndEnv$TreatmentVar<-'Treatment'
.stdEndEnv$Format<-"%m/%d/%Y"
.stdEndEnv$Results<-list()
.stdEndEnv$WeightsVar<-'Not Used'
.stdEndEnv$TestDirection<-'Both'
.stdEndEnv$AlphaLevel<-0.05
.stdEndEnv$UseData<-fecundityData

#manually upset data

if (identical(.stdEndEnv$TreatmentVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$TreatmentVar])
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
}


#Apply Subsets
if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenerationVar] == .stdEndEnv$GenerationVal)
}

if(identical(.stdEndEnv$GenderVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$GenderVar] == .stdEndEnv$GenderVal)
}

if(identical(.stdEndEnv$AgeVar,'Not Used')==FALSE){
.stdEndEnv$UseData<-subset(.stdEndEnv$UseData,.stdEndEnv$UseData[ ,.stdEndEnv$AgeVar] == .stdEndEnv$AgeVal)
}

if (identical(.stdEndEnv$ReplicateVar, 'Not Used')==FALSE){
.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]<-as.factor(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar])
#If there is 1 unit per replicate
if (nlevels(.stdEndEnv$UseData[ ,.stdEndEnv$ReplicateVar]) == dim(.stdEndEnv$UseData)[1]){
.stdEndEnv$ReplicateVar<-'Not Used'
}
}

.stdEndEnv$DataSub<-.stdEndEnv$UseData #DataSub is used to dynamically change the time interval for graphing 

.stdEndEnv$Results[['Fecundity']]<-autoStdAnylsis(.stdEndEnv$UseData,'Fecundity',.stdEndEnv$TreatmentVar,
'Square_Root',.stdEndEnv$WeightsVar.stdEndEnv$WeightsVar,.stdEndEnv$TimeVar,.stdEndEnv$TestDirection,.stdEndEnv$ReplicateVar,
.stdEndEnv$AlphaLevel)
.stdEndEnv$Results[['Fecundity']]$TestType<-'Auto'


.stdEndEnv$FileName<-'Example'
.saveResults(.stdEndEnv$Results[['Fecundity']],FDir,FileName)


#plots
.stdEndEnv$PlotTypeList<-c('Box','Quantile-Quantile','Violin','Interaction')
.stdEndEnv$EndPointVar<-c('Fecundity')

.stdEndEnv$PlotData<-.getTimeData(.stdEndEnv$DataSub,'Date',.stdEndEnv$Format,.stdEndEnv$TimeIntGraph,
.stdEndEnv$ReplicateVar,.stdEndEnv$TreatmentVar,.stdEndEnv$EndPointVar)
colnames(.stdEndEnv$PlotData)[which(colnames(.stdEndEnv$PlotData)=='Averaged_Numeric_Time')]<-'Time'
.stdEndEnv$PlotData$Time<-as.factor(.stdEndEnv$PlotData$Time)
.stdEndEnv$TimeVar<-'Date'


.saveGraphs(FDir,FileName)

rm(list=ls(.stdEndEnv) ,envir =.stdEndEnv)
##################################################################################
#Time to Effect
Dir=paste(Folder,'\\Time to Event Example\\Results',sep='')
FileName<-'Time to Effect Sample Results'
dir.create(Dir, showWarnings = FALSE)


#Set globals that the function are reliant on  
.time2EventEnv$TreatmentVar<-'Trt'
.time2EventEnv$ReplicateVar<-'Rep'
.time2EventEnv$TimeVar<-'Time'
.time2EventEnv$StatusVar<-'Status'
.time2EventEnv$StatusEventVal<-'1'
.time2EventEnv$StatusCenVal<-'0'
.time2EventEnv$CanRun<-FALSE
.time2EventEnv$TimeTemp<-{}
.time2EventEnv$Added.StatusVal<-0  #Used to control the control boxes
.time2EventEnv$GenderVar<-'Not Used'
.time2EventEnv$GenerationVar<-'Not Used'
.time2EventEnv$UseData<-eventTimeData


.time2EventEnv$Results<-analyseTime2Effect(.time2EventEnv$UseData,.time2EventEnv$StatusVar,.time2EventEnv$TimeVar,
.time2EventEnv$TreatmentVar,.time2EventEnv$ReplicateVar)

#Save Results
.saveResults.te(.time2EventEnv$Results,Dir,FileName)
.saveGraphs.te(Dir,FileName)

#Remove globals
rm(list=ls(.time2EventEnv) ,envir =.stdEndEnv)


}

selectPara<-function (VarName, LabelName = NULL, Enviro, What = NULL, Mult = FALSE, 
    Display = NULL) 
{
    if (is.null(What) == TRUE) {
        Word <- strsplit(VarName, split = "")[[1]]
        Type <- paste0(Word[{
            length(Word) - 2
        }:length(Word)], collapse = "")
        if (identical(Type, "Var") == TRUE) {
            Varaibles <- c("Not Used", colnames(get("MainData", 
                envir = get(Enviro))))
            if (is.null(Display) == TRUE) {
                Display <- paste(paste0(Word[1:{
                  length(Word) - 3
                }], collapse = ""), "Variable")
            }
        }
        if (identical(Type, "Val") == TRUE) {
            Word[length(Word)] <- "r"
            From <- paste0(Word, collapse = "")
            Choices <- levels(as.factor(get("MainData", envir = get(Enviro))[, 
                get(From, get(Enviro))]))
            Varaibles <- c("Not Used", Choices)
            if (is.null(Display) == TRUE) {
                Display <- paste(paste0(Word[1:{
                  length(Word) - 3
                }], collapse = ""), "Value")
            }
        }
    }
    else {
        Varaibles <- c("Not Used", What)
    }
    SelectWindow <- gwindow(paste("Please select the", Display), 
        visible = FALSE)
    group <- ggroup(horizontal = FALSE, container = SelectWindow, 
        spacing = 20)
    SubSetSelect <- gtable(Varaibles, container = group, expand = TRUE, 
        multiple = Mult)
    SelectButton <- gbutton("Select", container = group, handler = function(h, 
        ...) {
        assign(VarName, SubSetSelect[svalue(SubSetSelect, index = TRUE), 
            ], envir = get(Enviro))
        if (Mult == TRUE) {
            if (is.null(LabelName) == FALSE) {
                try(temp <- get(LabelName, envir = get(Enviro)))
                try(svalue(temp) <- "Multiple Values")
                LabelName <- NULL
            }
        }
        if (is.null(LabelName) == FALSE) {
            try(tempVar <- get(VarName, envir = get(Enviro)), 
                silent = TRUE)
            try(tempWig <- get(LabelName, envir = get(Enviro)), 
                silent = TRUE)
            try(svalue(tempWig) <- tempVar)
        }
        dispose(SelectWindow)
    })
    addHandlerUnrealize(SelectWindow, handler = function(h, ...) {
        assign(VarName, "Not Used", envir = get(Enviro))
    })
    visible(SelectWindow) <- TRUE
    return()
}
