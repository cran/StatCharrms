ShowChangeLog <-
function(){
#' @export


Changes<-"---------------------------------V0.90.6--------------------------------------
Change the display of the Arcsin transformation to display properly display Arcsin(Square_Root)
Added confidence intervals to both the Dunnett test and summary table
Added Williams test
Add warring about the use of RGtk2 version 2.20.33 
Increased the number of iterations on the JT from 1000 to 10000
"
	ChangeLogWindow<-gwindow(horizontal = FALSE,"StatCharrms R-Version Readme File", visible=FALSE)
	ChangeLogGroup<-ggroup(horizontal = FALSE,container=ChangeLogWindow)

	ChangeLogLabel<-gtext(Changes,horizontal = FALSE,container=ChangeLogGroup,expand=TRUE,fill=TRUE)

	size(ChangeLogWindow)<-c(450,280) 
	visible(ChangeLogWindow)<-TRUE
}
