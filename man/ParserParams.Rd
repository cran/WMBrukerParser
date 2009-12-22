\name{ParserParams}
\alias{ParserParams}
\docType{data}
\title{Input Parameter List}
\description{ParserParams is a list of seven parameters which identify 
  the data to be parsed, and specify whether or not data from multiple 
  bioprocessor runs are to be concatenated and if memory use information  
  is to be printed during parsing.  For convenience, it is stored in a 
  file named OptionsAndParameters.txt file which can be edited and sourced 
  prior to execution of ParseAndSave.
}
\format{
ParserParams\$dataSource    : character string (e.g. \dQuote{Lab1})\cr
ParserParams\$multipleRuns  : character string (\dQuote{yes} or \dQuote{no} )\cr
ParserParams\$concatLists   : character string (\dQuote{yes} or \dQuote{no} )\cr
ParserParams\$runIndices    : numeric array (e.g. 1 or c(1,3,2) )\cr
ParserParams\$dataDirLeft   : character string (e.g. \dQuote{scr/MyRun})\cr
ParserParams\$dataDirRight  : character string (e.g. \dQuote{QC})\cr
ParserParams\$printMemoryUse: character string (\dQuote{yes} or \dQuote{no} )
}
\details{
Data directories contain the spot directories with names such as 0\_A1 or 
  0\_B17. ParserParams\$dataDirLeft, ParserParams\$runIndices and 
  ParserParams\$dataDirRight are defined so that pasting them together generates 
  the data directory paths desired.  If multiple runs are to be parsed, the 
  ParserParams\$runIndices need not be in order but must appear in the paths.  
  The "run branch" can occur at the level of the data directory or above.  
  If it occurs at the data directory level,ParserParams\$dataDirRight <- "". 
  If a single run is to be parsed, ParserParams\$dataDirLeft is assigned 
  the complete data directory path, ParserParams\$runIndices <- 1 and 
  ParserParams\$dataDirRight <- "".
}

\examples{
## Example1: Parse data from a single bioprocessor run:

ParserParams<-list()
ParserParams$dataSource <- "Lab1"
ParserParams$multipleRuns <- "no" 
ParserParams$concatLists <- "no" 
ParserParams$runIndices <- 1

directory = system.file("Examples", package = "WMBrukerParser")
ParserParams$dataDirLeft <- 
paste(directory,"/C3ValidationExtractSmall/RobotRun1/2-100kDa",sep="")

ParserParams$dataDirRight <- "" 
ParserParams$printMemoryUse <- "no"

ParseAndSave(ParserParams)

## Example2: Parse and concatenate data from two bioprocessor
## two runs with memory information printed:

ParserParams<-list()
ParserParams$dataSource <- "Lab1"
ParserParams$multipleRuns <- "yes" 
ParserParams$concatLists <- "yes" 
ParserParams$runIndices <- c(1,2)

directory = system.file("Examples", package = "WMBrukerParser")
ParserParams$dataDirLeft <- 
paste(directory,"/C3ValidationExtractSmall/RobotRun",sep="")

ParserParams$dataDirRight <- "/2-100kDa" 
ParserParams$printMemoryUse <- "yes"

ParseAndSave(ParserParams)

}
\seealso{
\code{\link{WMBrukerParser}}, \code{\link{BrukerParser}},
	\code{\link{ParserParams}}, \code{\link{ParseAndSave}}, 
	\code{\link{tofListMetaData}}
}
\keyword{misc}
