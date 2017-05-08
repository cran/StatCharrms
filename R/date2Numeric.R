date2Numeric <-
function(DateVec,Format){
#' @export
if (is.numeric(DateVec)==TRUE){ #already an Ordinance end function
return(DateVec-min(DateVec))}  #Start Counting at zero

#Format
#DateVec<-apply(DateVec,1,as.Date) #old works for columns
DateVec<-as.Date(DateVec,Format)   #works for data input
DateVec<-DateVec-min(DateVec,na.rm=TRUE);
DateVec<-as.numeric(DateVec)

return(DateVec)
}
