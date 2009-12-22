\name{BrukerParser}
\alias{BrukerParser}
\title{Parser for Bruker-Ultraflex TOF Mass Spectrometry Data}
\description{
This routine reads binary fid Bruker files from a single 
	bioprocessor run, along with associated experimental 
	files and creates R structures, tofList and 
	tofListMetaData.
}
\usage{
BrukerParser(dataSource, dataDirectory)
}
\arguments{
  \item{dataSource}{Typically laboratory where data was obtained}
  \item{dataDirectory}{Data directory path.}
}
\details{
The BrukerParser routine is called by the ParseAndSave routine 
	using ParserParams\$dataSource and the 
	path built from ParserParams\$dataDirLeft, 
	ParserParams\$runIndices and ParserParams\$dataDirRight.

This routine looks for the following contents in the dataDirectory:\cr
1.  a subdirectory with \dQuote{calibration} in the name (optional)\cr
2.  a file with the extension .par (optional)\cr
3.  a file with the extension .axe (optional)\cr
4.  a file with the extension .isset (optional)\cr
5.  a file named sample.xml (optional)\cr
6.  spot subdirectories with names like 0\_A1 and 0\_L5 (required), 
	each containing:\cr
EITHER (for Linear TOF data):\cr
/1/1SLin/fid  - the binary data file (required)\cr
/1/1SLin/acqu - the acquisition information file (required)\cr
/1/1SLin/pdata/1/proc - the processing file (required)\cr
OR (for Reflector TOF data):\cr
/1/1SRef/fid  - the binary data file (required)\cr
/1/1SRef/acqu - the acquisition information file (required)\cr
/1/1SRef/pdata/1/proc - the processing file (required)\cr
}

\value{
  \item{tofList }{A list of time-of-flight mass spectrum vectors 
	addressable by spectrumName.}
  \item{tofListMetaData}{A data.frame containing string values 
	for experimental meta-data, with rows and columns 
	addressable by spectrumName and attributeName.}
}
\author{William E. Cooke, College of William and Mary, 
	wecook@wm.edu}
\note{
If any of the optional files are missing, the user has the 
	option updating meta-data fields in the parsed meta-data
	 structure, tofListMetaData. If the sample.xml file is 
	missing, a field used in naming data and meta-data 
	structures is missing and placeholders will be used.  
	In this case, the user may wish to update structure 
	names. 

If the final line of the .axe file is not blank, R will 
	generate a warning message such as:  
	"In readLines(qq, n = -1) :  incomplete final line found 
	in 0-20KDaLin030308.axe"
This has no effect on the reader and can be ignored.
}
\seealso{
\code{\link{WMBrukerParser}}, \code{\link{ParserParams}}, 
	\code{\link{ParseAndSave}}, \code{\link{tofList}}, 
	\code{\link{tofListMetaData}}
}
\examples{
dataSource <- "Lab1"

directory = system.file("Examples", package = "WMBrukerParser")
dataDirectory <- 
paste(directory,"/C3ValidationExtractSmall/RobotRun1/2-100kDa",sep="")

BrukerParser(dataSource, dataDirectory)
}

\keyword{IO}
