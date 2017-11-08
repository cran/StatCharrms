\name{forceStdAnalysis}
\alias{forceStdAnalysis}

\title{
	Run a chosen analysis
}
\description{
	This function runs an analysis chosen through the GUI.
}
\usage{
	forceStdAnalysis(Data, Response, TreatmentVar, Transform, WeightsVar, 
	TimeVar, TestDirection, ReplicateVar, Test, AlphaLevel)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{Response}{
	The name (as a string) of the response variable. 
}
  \item{TreatmentVar}{
	The name (as a string) of the treatment variable. 
}
  \item{Transform}{
	The Transformation for the response. Can be any of the following:  c('Transformation', 
	'None', 'Log', log+1', 'Square_Root', 'Arcsin', 'Rank'). 
}
  \item{WeightsVar}{
	The name (as a string) of the column containing the ANOVA weights. 
}
  \item{TimeVar}{
	The name (as a string) of the time variable. 
}
  \item{TestDirection}{
	The direction of the hypothesis test.  Can take on any of the values of: 
	c('Decreasing', 'Increasing', 'Both').
}
  \item{ReplicateVar}{
	The name (as a string) of the replicate variable. 
}
  \item{Test}{
	The test being ran. Any of the following: 
	c('RM ANOVA', 'ME ANOVA', 'Simple ANOVA', 'Weighted ANOVA', 'Jonckheere', 'Dunns', 'Dunnett','Williams') can be used.
}
  \item{AlphaLevel}{
	The alpha level used in the Jonckheere-Terpstra trend test. See \code{\link{jonckheereTerpstraTest}} for more details.
}
}

\value{
See the individual functions for information about each test. 
   \item{Jonckheere}{Jonckheere-Terpstra trend test see \code{\link{jonckheereTerpstraTest}}.}
   \item{RM ANOVA }{Repeated measures ANOVA see \code{\link{runMultiGen}}.}
   \item{ME ANOVA}{Mixed effects ANOVA see \code{\link{runMultiGen}}.}
   \item{Simple ANOVA}{One way ANOVA see \code{\link{basicAnova}}.}
   \item{Weighted ANOVA}{A weighted one way ANOVA see \code{\link{basicAnova}}.}
   \item{Dunns}{A Dunns test see \code{\link{dunnsTest}}.}
   \item{Dunnet}{A Dunnett's test see \code{\link{oneWayDunnettTest}}.}
   \item{WilliamsTable}{The results from the Williams test \code{\link{williamsTest}}.}
}

\author{
	Joe Swintek
}



