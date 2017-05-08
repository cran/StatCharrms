ReadMeFile <-
function(){
#' @export
#This contains the current notes and changes to the program.
ReadMeWindow<-gwindow(horizontal = FALSE,"StatCharrms R-Version Readme File", visible=FALSE)
ReadMeGroup<-ggroup(horizontal = FALSE,container=ReadMeWindow)
size(ReadMeWindow)<-c(784,536)
AuthorMessage<-'Written and Tested By Joe Swintek
Based on StatCharrms SAS-Version Developed by Dr. John Green
Additional Testing By Kevin Flynn and John Haselman'
AuthorLab<-glabel('Authors', container=ReadMeGroup,where='center')
font(AuthorLab)<-list(size=16)

IntroTextframe<-gtext(AuthorMessage,horizontal = FALSE, container=ReadMeGroup,expand=TRUE,fill=TRUE)
ChangeLogButton<-gbutton('changes', container=ReadMeGroup,handler= function(h,...){
ShowChangeLog()
})
ReferenceButton<-gbutton('References', container=ReadMeGroup,handler= function(h,...){
Reference()
})


visible(ReadMeWindow)<-TRUE
}
