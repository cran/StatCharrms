cleanString <-
function(String){
#' @export
#This function cleans a string 
PString<-String
String<-gsub('[\\?/<>|*:."]','_',String)
if (PString != String){
Message<-'Contained illegal characters converting them to "_"' 
popMessage(Message)
}

return (String)
}
