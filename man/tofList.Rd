\name{tofList}
\alias{tofList}
\docType{data}
\title{Parsed TOF Spectrum List}
\description{
 A list of time-of-flight mass spectrum vectors addressable by 
	spectrumName.
}
\details{Spectrum vectors can also be addressed by spectrum index. 
	This option should be used with caution since data and 
	meta-data can be reordered independently during subsequent 
	signal processing or statistical analysis. 

In the event that default 
	assignments were used for spectrumNames (due to missing
	sampleInfo.sampleNames meta-data) they can be updated.
}

\examples{


## To access a spectrum vector:

data(tofListRun2)

spectrum <- tofList[[ "Pool 8_1"]];

## or:

spectrum <- tofList[[1]];  # Use with caution!

## To update a spectrum's vector:

 tofList[ "Pool 8_2"] <- list(spectrum)

## To update the tofList names:

newSpecNames<-list();
numSpec<-length(tofList);
for (i in 1:numSpec) {newSpecNames[i]<-paste("spec",i, sep="")};
names(tofList)<-newSpecNames;
}


\seealso{
\code{\link{WMBrukerParser}}, \code{\link{BrukerParser}},
	\code{\link{ParserParams}}, \code{\link{ParseAndSave}}, 
	\code{\link{tofListMetaData}}
}
\keyword{classes}