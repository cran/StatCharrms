Run.StatCharrms <-
function(){
#' @export
#Main function call for StatCharrms and brings up into screen 
IntroWindow<-gwindow(horizontal = FALSE,"StatCharrms ", visible=FALSE)
size(IntroWindow)<-c(800,600)
IntroTextframe<-gframe(horizontal = FALSE, container=IntroWindow,where="center")
IntroTextMessage1<-glabel('Welcome to StatCharrms',container=IntroTextframe,where="center") #Title1
font(IntroTextMessage1)<-list(size=34)
IntroTextMessage2<-glabel('v0.90 April 18, 2017',container=IntroTextframe,where="center") #Title2  #Update This
font(IntroTextMessage2)<-list(size=16)
IntroTextMessage3<-glabel('Beta -Developmental-',container=IntroTextframe,where="center") #Title2
font(IntroTextMessage3)<-list(size=16)
BlankSpace<-gframe(container=IntroTextframe,where="center") #Blank space for formats
size(BlankSpace)<-c(1,80)

ButtonBox1<-ggroup(container=IntroTextframe,horizontal = FALSE) #Read me file  
ReadMeButton<-gbutton("Read Me",container=ButtonBox1,where='center',
handler= function(h,...){ReadMeFile()
})
font(ReadMeButton)<-list(size=16)
ExampleButton<-gbutton("Examples ",container=ButtonBox1,where='center',   
handler= function(h,...){generateExamples()})
font(ExampleButton)<-list(size=16)
ButtonBox2<-gframe(horizontal = FALSE,container=ButtonBox1,where="center")
HistoButton<-gbutton("Histology Analysis",container=ButtonBox2,where='center',   #
handler= function(h,...){Histopath()})
font(HistoButton)<-list(size=16)
HistoButton<-gbutton("Time to Event  Analysis",container=ButtonBox2,where='center',   
handler= function(h,...){Time2EventMain()})
font(HistoButton)<-list(size=16)

OneWayAnovaButton<-gbutton("Analysis of Other Endpoints  ",container=ButtonBox2,where='center',   
handler= function(h,...){StdEndMain()})
font(OneWayAnovaButton)<-list(size=16)
QuitButton<-gbutton("quit",container=ButtonBox1,where='center',  #Quits the program
handler= function(h,...){dispose(IntroWindow)
quit("no")
})
font(QuitButton)<-list(size=16)

visible(IntroWindow)<-TRUE
}
