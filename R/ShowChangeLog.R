ShowChangeLog <-
function(){
#' @export


Changes<-"---------------------------------V0.90.91--------------------------------------
Fixed coding typos in the vignette.
Allowed set.seed to work with the JT.
Fixed a bug that caused the table for the Levene's test to break.
Clarified use of the p-value for both the Levene's and Shapiro-Wilks test.
Clarified and the output what test alpha level was used for. 
Fixed bug that forced the specification of the generation variable.
Fixed a bug that prevented 'numeric' times to be properly excluded when the 'Select Excluded Times' was used.


---------------------------------V0.90.9--------------------------------------
Fixed coding typos in the vignette 

---------------------------------V0.90.8--------------------------------------
Fixed bug that would cause StatCharrms to sometimes ignore sub setting on generation.
StatCharrms will now properly excluded the excluded times.
Added output that now shows the estimated median time to effect.
Added the mean (of replicate means) to the summary table.

---------------------------------V0.90.7--------------------------------------
Fixed a bug that ignored the data transformation when the Dunnett test or a simple ANOVA was 
explicitly selected as the test type.

---------------------------------V0.90.6--------------------------------------
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
