Reference <-
function(){
#' @export
#This contain the list of References for the RSCABS Modular 

MessageA1<-'------------------------------------------------'
MessageA2<-'-   General Use, Graphic, and I/O Packages     -'
MessageA3<-'------------------------------------------------'


MessageA4<-"GTK+
 http://www.gtk.org/ maintained by the GNOME foundation"

MessageA5<-paste("RGtk2 Package \n",citation('RGtk2')$textVersion,sep='')
  
MessageA6<-paste("gWidgetsRGtk2 Package \n", citation('gWidgetsRGtk2')$textVersion,sep='')
  
MessageA7<-paste("R2HTML Package \n", citation('R2HTML')$textVersion,sep='')

MessageA8<-paste("lattice Package \n", citation('lattice')$textVersion,sep='')
 
MessageA9<-paste("ggplot2 Package \n", citation('ggplot2')$textVersion,sep='')
 
MessageA10<-paste("cairoDevice Package \n", citation('cairoDevice')$textVersion ,sep='')

MessageA11<-paste("multcomp Package \n", citation('multcomp')$textVersion,sep='')

MessageB1<-'------------------------------------------------'
MessageB2<-'-     HistoPath       -'
MessageB3<-'------------------------------------------------'



#John Greens Paper
MessageB4<-paste("RSCABS Package \n", citation('RSCABS')$textVersion,sep='')


MessageB5<-"J. N. K. Rao and A. J. Scott
A simple method for analysing overdispersion in clustered Poisson data
Statistics in Medicine
Volume 18, Issue 11, pages 1373-1385, 15 June 1999"


MessageB6<-"J. N. K. Rao and A. J. Scott
A Simple Method for the Analysis of Clustered Binary Data
Biometrics
Vol. 48, No. 2 (Jun., 1992), pp. 577-585"


MessageC1<-'------------------------------------------------'
MessageC2<-'-     Fecundity       -'
MessageC3<-'------------------------------------------------'

MessageC4<-paste("nlme Package \n", citation('nlme')$textVersion,sep='')
 

MessageD1<-'------------------------------------------------'
MessageD2<-'-       Analysis of Other Endpoints   -'
MessageD3<-'------------------------------------------------'

MessageD4<-paste("Zoo Package \n", citation('zoo')$textVersion ,sep='')
MessageD5<-paste("car Package \n", citation('car')$textVersion ,sep='')
MessageD6<-paste("clinfun Package \n", citation('clinfun')$textVersion ,sep='')
 
MessageE1<-'------------------------------------------------'
MessageE2<-'-       Time to Event Analysis   -'
MessageE3<-'------------------------------------------------' 
MessageE4<-paste("Coxme Package \n", citation('coxme')$textVersion ,sep='')
 
 
ReferenceMessageA<-paste(MessageA1,'\n\n',MessageA2,'\n\n',MessageA3,'\n\n',
MessageA4,'\n\n',MessageA5,'\n\n',MessageA6,'\n\n',MessageA7,'\n\n',MessageA8,'\n\n',
MessageA9,'\n\n',MessageA10,'\n\n',MessageA11,'\n\n')

ReferenceMessageB<-paste(MessageB1,'\n\n',MessageB2,'\n\n',MessageB3,'\n\n',
MessageB4,'\n\n',MessageB5,'\n\n',MessageB6,'\n\n')


ReferenceMessageC<-paste(MessageC1,'\n\n',MessageC2,'\n\n',MessageC3,'\n\n',
MessageC4,'\n\n')

ReferenceMessageD<-paste(MessageD1,'\n\n',MessageD2,'\n\n',MessageD3,'\n\n',
MessageD4,'\n\n',MessageD5,'\n\n',MessageD6,'\n\n')

 
ReferenceMessageE<-paste(MessageE1,'\n\n',MessageE2,'\n\n',MessageE3,'\n\n',
MessageE4,'\n\n')
 
ReferenceMessage<-paste(ReferenceMessageA,ReferenceMessageB,ReferenceMessageC,
ReferenceMessageD,ReferenceMessageE)
 
ReferenceWindow<-gwindow(horizontal = FALSE,"StatCharrms R-Version Readme File", visible=FALSE)
ReferenceGroup<-ggroup(horizontal = FALSE,container=ReferenceWindow)
ReferenceLabel<-gtext( ReferenceMessage,horizontal = FALSE,container=ReferenceGroup,expand=TRUE,fill=TRUE)

size(ReferenceWindow)<-c(600,420) 
visible(ReferenceWindow)<-TRUE
}
