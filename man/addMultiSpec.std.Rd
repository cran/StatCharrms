\name{addMultiSpec.std}
\alias{addMultiSpec.std}

\title{
	Add a data specification tab
}
\description{
	This function adds a tab to the analysis main window where details about the data can be specified.  
}
\usage{
addMultiSpec.std(Notebook)
}

\arguments{
  \item{Notebook}{
 The notebook the specification tab is added to. 
}
}

\value{
These items are available from the start of the data specification tab. 
\item{\bold{Treatment}			}{Used to specify the name of the treatment variable.  This must be selected for any analysis to run. }
\item{\bold{Replicate}			}{Used to specify the name of the replicate variable.}
\item{\bold{Generation Variable}			}{Used to specify the name of the generation variable.}
\item{\bold{Generation Value}			}{The generation the selected analysis is performed on.}
\item{\bold{Age Variable}			}{Used to specify the name of the age variable.}
\item{\bold{Age Value}			}{The age the selected analysis is performed on.}
\item{\bold{Gender Variable}			}{Used to specify the name of the gender variable.}
\item{\bold{Gender Value}			}{The gender the selected analysis is performed on.}
\item{\bold{Anova Weights} 			}{The name of the variable that contains weights for a weighted ANOVA.}
\item{\bold{Test Direction}			}{The direction of the hypothesis test.}
\item{\bold{Alpha Level}			}{The alpha level used in the Jonckheere-Terpstra test from the \code{\link{jonckheereTerpstraTest}} function.}
\item{\bold{Select Endpoints To Test}			}{Brings up a window that allows for the selection of each endpoint to be tested.}

These item are relevant to repeated measures taken on the same subject at different times throughout the study: 
\item{\bold{Time Variable}			}{Used to specify the time variable.}
\item{\bold{Time Format}			}{Used to specify the format the time variable is in.}
\item{\bold{Analysis Interval}			}{This is used to specify the length of time a response is averaged over for an analysis.}
\item{\bold{Graph Interval}			}{This is used to specify the length of time a response is averaged over for the purpose of graphing.}
\item{\bold{Select Excluded Times}			}{Used to select dates or times to exclude from the analysis.}
\item{\bold{Excluded Times}			}{Displays all the times selected to be excluded from the anylsis.}

After the Select [\bold{Endpoints To Test}] button is pressed a new frame for each selected endpoint will appear.  
\item{\bold{Test Type}			}{This will select the test type.  If nothing is selected the test type defaults to 'Auto'.  See \code{\link{autoStdAnylsis}} and \code{\link{forceStdAnalysis}} for further details. }
\item{\bold{Transformation}			}{This will select the type of transformation the specific endpoint in the data set undergoes. If nothing is selected the test type defaults to 'None'. }

}

\author{
	Joe Swintek
}



\seealso{
 \code{\link{autoStdAnylsis}} and \code{\link{forceStdAnalysis}}.
}


\keyword{GUI Specification}

