.stdEndEnv<-new.env()
.time2EventEnv<-new.env()




.onLoad<-function(...){
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import RGtk2
#' @import R2HTML
#' @import multcomp	
#' @import nlme	
#' @import lattice	
#' @import cairoDevice	
#' @import clinfun
#' @import survival
#' @import coxme
#' @import RSCABS
#' @import car
#' @importFrom stats var aov qqnorm qqline interaction.plot complete.cases pnorm median pt qnorm xtabs anova shapiro.test summary.lm contrasts<-
#' @importFrom grDevices pdf dev.off dev.set dev.prev
#' @importFrom graphics plot legend 
#' @importFrom methods is
#' @importFrom utils write.table citation data
}



.onAttach <- function(...) {
	packageStartupMessage('Type Run.StatCharrms() to begin.')
}






