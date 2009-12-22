\name{WMBrukerParser-package}
\alias{WMBrukerParser-package}
\alias{WMBrukerParser}
\docType{package}
\title{William and Mary Parser for Bruker-Ultraflex TOF-MS Data
}
\description{
The package parses Bruker-Ultraflex TOF mass spectrometry data 
	and experimental parameter files into R structures for 
	further signal processing and statistical analysis.
}
\details{
\tabular{ll}{
Package: \tab WMBrukerParser\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2009-12-22\cr
License: \tab GPL(version 2 or later)\cr
}
The package contains two routines. The core routine, 
	BrukerParser, reads binary TOF data (obtained using 
	either Linear or Reflector mode) and associated 
	experimental parameter files, and parses data and 
	meta-data into R structures, tofList and tofListMetaData.
	  This involves accessing files in different locations 
	within a data directory tree and reading different file 
	formats.  The second routine, ParseAndSave calls the 
	BrukerParser routine as directed by the parameter list, 
	ParserParams (provided in an OptionsAndParameters.txt 
	file), and saves the tofList and tofListMetaData 
	structures that are returned in .Rdat files.  Parameters
	in OptionsAndParameters can be edited to select options 
	for parsing data from multiple bioprocessor runs, 
	concatenating parsed data and printing memory usage 
	information. 
}
\author{William Cooke, Maureen Tracy and Dariya Malyarenko, 
	College of William and Mary

Maintainer: William Cooke <wecook@wm.edu>
}

\seealso{
\code{\link{BrukerParser}},\code{\link{ParserParams}}, 
\code{\link{ParseAndSave}}, \code{\link{tofList}}, 
\code{\link{tofListMetaData}}
}
\keyword{IO}
\keyword{classes}