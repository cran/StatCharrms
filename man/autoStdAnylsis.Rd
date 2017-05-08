\name{autoStdAnylsis}
\alias{autoStdAnylsis}

\title{
	Run statistical tests according to OECD guidelines
}
\description{
	Runs tests following the flow chart outlined in the OECD guidelines for the tier II tests for endocrine disruptors.
	The basic outline of the flow chart is as follows: 1) if the data is monotonic run a jonckheere Terpstra trend test,
	2) is the data is not monotonic but is still normally distributed, run a Dunnett's test, 
	3) if the data is not monotonic and not normally distributed run a Dunns test. 
}
\usage{
autoStdAnylsis(Data, Response, TreatmentVar, Transform, WeightsVar, TimeVar, 
TestDirection, ReplicateVar, AlphaLevel)
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
	The name (as a string) of the transformation of the response variable. Can take on the values of 
	c('Transformation', 'None', 'Log', log+1', 'Square_Root', 'Arcsin', 'Rank'). 
}
  \item{WeightsVar}{
	The name (as a string) of the column containing the ANOVA weights. 
}
  \item{TimeVar}{
	The name (as a string) of the Time variable. 
}
  \item{TestDirection}{
	The direction of the hypothesis test.  Can take on any of the values of: 
	c('Decreasing', 'Increasing', 'Both').
}
 \item{ReplicateVar}{
	The name (as a string) of the replicate variable. 
}
  \item{AlphaLevel}{
	The alpha level used in the Jonckheere-Terpstra test for trend. See (\code{\link{jonckheereTerpstraTest}}).
}
}


\value{
 See the following function for details on the analysis.   
   \item{Jonckheere}{Jonckheere-Terpstratest test see \code{\link{jonckheereTerpstraTest}}.}
   \item{Dunns}{A Dunns test see \code{\link{dunnsTest}}.}
   \item{Dunnet}{A Dunnet test see \code{\link{oneWayDunnettTest}}.}
}

\references{
	OECD, 2015, Test No. 240: Medaka Extended One Generation Reproduction Test (MEOGRT), OECD	
	Publishing, Paris. \\  DOI: http://dx.doi.org/10.1787/9789264242258-en 
	}
\author{
	Joe Swintek
}


