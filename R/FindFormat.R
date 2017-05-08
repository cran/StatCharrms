FindFormat <-
function(string,CurrentDate){
#' @export
#This function will find the formate of the date from user selection
Formats<-rbind('%Y-%m-%d','%m-%d-%Y','%d-%m-%Y','%Y/%m/%d','%m/%d/%Y','%d/%m/%Y',
'%Y-%b-%d','%b-%d-%Y','%d-%b-%Y')
pick<-which(CurrentDate==string)  #Check for proper format 
if (length(pick)>0){
return(Formats[pick, ])};
return(string);
}
