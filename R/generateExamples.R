generateExamples <-
function(){
#' @export
temp<-gtkWindow(show=FALSE)
Dir<-getDir(temp)
if (Dir==' '){
popMessage('A folder was not selected\nPlease Select a folder\nSave aborted!')
return()
}
.writeExamples(Dir)
}
