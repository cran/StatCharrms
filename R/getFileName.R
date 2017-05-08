getFileName <-
function(){
#' @export
FileNameWindow<-ginput("Enter File Name",icon='question', handler = function(h,.){
FileName<-h$input
FileName<-cleanString(FileName)
return(FileName)
})
}
