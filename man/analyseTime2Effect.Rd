\name{analyseTime2Effect}
\alias{analyseTime2Effect}

\title{
	Mixed effect time to event analysis
}
\description{
	A wrapper function for the \code{\link[coxme:coxme]{coxme}} function from the coxme package.  
	Coxme is a mixed effects version of cox proportional models for comparison 
	of time to event data taking into account censoring events.  
}
\usage{
analyseTime2Effect(Data, StatusVar, TimeVar, TreatmentVar, ReplicateVar)
}

\arguments{
  \item{Data}{
	A data set.
}
  \item{StatusVar}{
	The name (as a string) of the status variable. 
}
  \item{TimeVar}{
	The name (as a string) of the time variable. 
}
  \item{TreatmentVar}{
	The name (as a string) of the treatment variable. 
}
  \item{ReplicateVar}{
	The name (as a string) of the replicate variable. 
}
}

\value{
	Returns a list with the following values:
	\item{FitME}{The results from \code{\link[coxme:coxme]{coxme}}.}
	\item{FitHP }{The results from \code{\link{coxph}}. It is used in graphing.}
	\item{FitS }{The results from \code{\link{survfit}}. It is used in graphing.}
	\item{EffectsTable}{A dunnett table from \code{\link{oneWayDunnettTest}}. }
	\item{MedianTable}{A table showing the median time to effect along with the median's 95\% confidence intervals. See \code{\link{quantile.survfit}} for more details.}
	
}


\references{
	Ripatti and J Palmgren, Estimation of multivariate frailty models using penalized partial likelihood, \emph{Biometrics} \bold{56}:1016-1022, 2000. 
	\cr T Therneau, P Grambsch and VS Pankratz, Penalized survival models and frailty, \emph{J Computational and Graphical Statistics} \bold{12}:156-175, 2003. 

}
\author{
	Joe Swintek
}


\examples{
	#Data 
		data(eventTimeData)
	#Run 
		analyseTime2Effect(Data=eventTimeData, StatusVar='Status', 
			TimeVar='Time', TreatmentVar='Trt', ReplicateVar='Rep')
}


\seealso{
\code{\link[coxme:coxme]{coxme}}
}


\keyword{Analysis}

