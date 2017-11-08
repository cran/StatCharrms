\name{buildResultsWindow}
\alias{buildResultsWindow}

\title{
	Build the results window for the analysis of numerical endpoints
}
\description{
	This window is used to both display results from, and graph the data used in; the analysis of numerical endpoints.  
}
\usage{
buildResultsWindow(Results)
}

\arguments{
  \item{Results}{
	The results data structure from \code{\link{autoStdAnylsis}} or \code{\link{forceStdAnalysis}}.
}
}

\details{
	The results window has the potential to display the results from every possible type of analysis.  
	See the help section of each analysis for more details.  
}
\value{
Every analyzed endpoint will have a tab associated with it.  Every tab will contain these 2 buttons: 	 
  \item{\bold{Save All Results}}{Creates a folder and saves all the results from every endpoint in the folder as an HTML document.}
  \item{\bold{Save All Results and Graphs}}{Creates a folder and saves all the results from every endpoint 
  in the folder as an HTML document. Also saves all possible graphs for every endpoint as pdfs in the same folder.}

Each tab produces a table for every possible analysis.  See each individual function for details about each table.	
   \item{\bold{Summary table}}{See \code{\link{makeSummaryTable}}.}
   \item{\bold{Test For Monotonicity}}{See \code{\link{monotonicityTest}}.}
   \item{\bold{Jonckheere-Terpstra table}}{See \code{\link{jonckheereTerpstraTest}}.}
   \item{\bold{Wilks table}}{See \code{\link{wilksTest}}.}
   \item{\bold{Levene table}}{See \code{\link{leveneTestSC}}.}
   \item{\bold{Anova table}}{See \code{\link{basicAnova}}.}
   \item{\bold{Dunnet table}}{See \code{\link{oneWayDunnettTest}}.}
   \item{\bold{Dunns table}}{See \code{\link{dunnsTest}}.}
   \item{\bold{Williams Table}}{The results from the Williams test \code{\link{williamsTest}}.}
  
  
 In addition to the two save buttons shared by every tab, the graph tab has the additional button of:
  \item{\bold{Save  Graph}}{Saves the current graph as a pdf.}


The graph tab will always have the following controls:  
    \item{\bold{Choose Response}}{This allows for the selection of the endpoint to be graphed. 
		This is can be any of the endpoints used in the analysis.
	}
   
   \item{\bold{Choose Graph Type}}{This allows for the selection of graph type.  
		It can be a boxplot from the \code{\link{boxplot}} function, a quantile-quantile plot from  the \code{\link{qqnorm}} function,
		or a violin plot from the \code{\link[lattice:bwplot]{bwplot}} function in the lattice package.  
		If a time variable was specified, "Interaction" will be included as an additional option.
		Selecting "Interaction" produces an interaction plot using the \code{\link{interaction.plot}} function. 
	}
	\item{\bold{Use Transformed Scale}}{
		This controls the scale the data is graphed in.  
		The scale can be unmodified which uses the original, unmodified data or the scale can be modified which use the
		data that has been transformed by the transformation selected in the analysis.
	}
  	
If the time variable is specified, the graph will have these additional controls:  	
	\item{\bold{Group Variable}}{This controls what the x-axis is. It can be either the treatment variable used in the analysis or the time variable.}
  	\item{\bold{Time Interval}}{This controls the amount of time the response variable is averaged over. 
		For example if the data was 
		recorded every day selecting an interval of "7" would yield an average weekly response. }
}


\author{
	Joe Swintek
}


\seealso{
	\code{\link{autoStdAnylsis}} and \code{\link{forceStdAnalysis}}
}


\keyword{GUI Results}

