ShowChangeLog <-
function(){
#' @export
Changes<-"---------------------------------V0.90--------------------------------------

"


ChangeLogWindow<-gwindow(horizontal = FALSE,"StatCharrms R-Version Readme File", visible=FALSE)
ChangeLogGroup<-ggroup(horizontal = FALSE,container=ChangeLogWindow)

ChangeLogLabel<-gtext(Changes,horizontal = FALSE,container=ChangeLogGroup,expand=TRUE,fill=TRUE)

size(ChangeLogWindow)<-c(450,280) 
visible(ChangeLogWindow)<-TRUE

}
