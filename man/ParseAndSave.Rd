\name{ParseAndSave}
\alias{ParseAndSave}
\title{
Parse and Save Bruker-Ultraflex TOF Mass Spectrometry Data
}
\description{
This routine calls the BrukerParser to parse data from individual
	or multiple bioprocessor runs.  It provides the option for
	concatenating data and meta-datastructures for multiple runs.  
	It saves individual and concatenated tofList and 
	tofListMetaData files as .Rdat files
}
\usage{
ParseAndSave(ParserParams)
}
\arguments{
  \item{ParserParams}{A list of seven parameters which 
	identify the data to be parsed, and specify whether or
	not data from multiple bioprocessor runs are to be 
	concatenated and if memory use information  is to be
	printed during parsing.  For convenience, it is stored
	in a file named OptionsAndParameters.txt file which 
	can be edited and sourced prior to execution of 
	ParseAndSave. It contains the parameters: dataSource, 
	multipleRuns, concatLists, runIndices, dataDirLeft,
	dataDirRight and printMemoryUse
}
}
\details{
Data directories contain spot directories with names such as
	 0\_A1 or 0\_B17.  The code constructs the data directory 
	paths using ParserParams\$dataDirLeft, 
	ParserParams\$runIdices and ParserParams\$dataDirRight. 
	If multiple runs are to be parsed, the 
	ParserParams\$runIdices MUST appear in the paths. The 
	"run branch" can occur at the level of the data directory
	 or above.  If it occurs at the data directory level, 
	ParserParams\$dataDirRight <- "".  If a single run is to 
	be parsed, ParserParams\$dataDirLeft is assigned the 
	complete data directory path, ParserParams\$runIdices <- 1
	 and ParserParams\$dataDirRight <- "".

tofListRun\#.Rdat and tofListMetaDataRun\#.Rdat files are saved in
	 dataDirectory (the directory containing the spot 
	subdirectories) for "Run\#". Concatenated files are saved 
	in the directory of the final data parsed. For instance if 
	runs 1, 3, and 2 were selected for concatenation 
	(ParserParams\$runIndices <- c(1,3,2)), the files 
	tofListCat\_1\_3\_2.Rdat and tofListMetaDataCat\_1\_3\_2.Rdat 
	would be found in the dataDirectory for the Run 2.
}
\value{
  \item{tofList }{A list of time-of-flight mass spectrum vectors
	 addressable by spectrumName.}
  \item{tofListMetaData}{A data.frame containing string values 
	for experimental meta-data, with rows and columns addres
	sable by spectrumName and attributeName.}
}
\author{Maureen Tracy, College of William and Mary, mbtrac@wm.edu
	}

\section{Warning }{
Since the data files are large, it may not be desirable or possible
	 to concatenate data parsed from all runs in an experiment.
	 The option to print memory usage through the parsing
	 of multiple runs allows the user to consider which files
	 to select for concatenation.  Prior to selecting the option
	 to concatenate data, it would be wise to parse all the
	 data of interest, examine the memory usage throughout 
	parsing in R and size of the resulting .Rdat files on the 
	disk, keeping in mind the doubling of memory required for
	 the further processing of this data. 
}
\seealso{
\code{\link{WMBrukerParser}}, \code{\link{BrukerParser}},
	\code{\link{ParserParams}}, \code{\link{tofList}}, 
	\code{\link{tofListMetaData}}
}
\examples{
## Example 1: Parse data from single run.

directory = system.file("Examples", package = "WMBrukerParser")
source(paste(directory,"/OptionsAndParametersParse1Run.txt",sep=""))

ParseAndSave(ParserParams)

## Example 2: Parse and concatenate data from two runs 
## with memory use printed during parsing.

directory = system.file("Examples", package = "WMBrukerParser")
source(paste(directory,"/OptionsAndParametersParseAndCat2Runs.txt",sep=""))

ParseAndSave(ParserParams)
}
\keyword{IO}