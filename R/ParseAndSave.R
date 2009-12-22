ParseAndSave <-
function(ParserParams) {

# By Maureen Tracy
# December 22, 2009

##############################################################
################ PARSE AND SAVE BRUKER MS DATA ###############
##############################################################
##
##  This routine calls the BrukerParser to parse data from 
##  single runs. It provides the option of parsing data from 
##  multiple runs and of concatenating the parsed 
##  structures. Individual and concatenated tofList and 
##  tofListMetaData files are saved as .Rdat files. 
##
##  INPUT:
##
##    OptionAndParamsDir - Directory path for 
##                       OptionsAndParametersBrukerParser.txt
##                       which contains the following:
##    
##      ParserParams$dataSource - laboratory where data was obtained
##
##      ParserParams$multipleRuns - number of runs to parse
##
##      ParserParams$concatLists - whether or not to concatenate tofLists
##                                
##      ParserParams$runIndices - indices of the runs to parse (must appear
##                   somewhere in the data directory path)
##                               
##      ParserParams$dataDirLeft - Portion of the data directory path to
##                   the left of the run index (or full path
##                   name if a single run is being parsed)
##                                
##      ParserParams$dataDirRight - Portion of the data directory path to
##                   the right of the run index (if it exits)
##                                 
##	ParserParams$printMemoryUse - if "yes" memory usage and date stamps
##                   throughout parsing will be printed
##                                 
## 
##    EXPECTED CONTENTS OF INPUT DATA DIRECTORY:
##
##      spot subdirectories with names like "0_A1" and "0_L5",
##        each containing:
##	  EITHER:
##	    /1/1SLin/fid  # the binary data file (required)
##	    /1/1SLin/acqu # the acquisition information file (required)
##	    /1/1SLin/pdata/1/proc # the processing file (required)
##        OR:    
##	    /1/1SRef/fid  # the binary data file (required)
##	    /1/1SRef/acqu # the acquisition information file (required)
##	    /1/1SRef/pdata/1/proc # the processing file (required) 
##      a subdirectory with “calibration” in the name  (optional)
##      a file with the extension .par  (optional)
##      a file with the extension .axe  (optional)
##      a file with the extension .isset  (optional)
##      a file named “sample.xml”  (optional)
##		
##  OUTPUT: 
##
##    tofList - list of vectors containing tof data
##             addressable by spectrum id
##    tofListMetaData - data.frame in which spectra meta data 
##             (string values) are addressed by spectrum id 
##             (row name) and metaData attribute (column name).
##
##  CALLS:
##
##    BrukerParser
##
##############################################################

## Memory Usage Check

	memoryL<-memory.limit()
	startMemoryUsed<-memory.size(max=FALSE)
	dateStamp<-date()

### Print Memory Limit and Initial Usage if desired

	print("Memory Limit")
	print(memoryL)
	print("Memory used before loading data")
	print(startMemoryUsed)
	print(dateStamp)

## End Memory Usage Check


### setup and parameters

concatLists<-ParserParams$concatLists
dataDirLeft<-ParserParams$dataDirLeft
dataDirRight<-ParserParams$dataDirRight
dataSource<-ParserParams$dataSource
multipleRuns<-ParserParams$multipleRuns
printMemoryUse<-ParserParams$printMemoryUse
runIndices<-ParserParams$runIndices

	tofListAll<-list()
	catTofFileName<-paste("tofListCat")
	catTofMetaFileName<-paste("tofListMetaDataCat")

	if(multipleRuns=="no") {
	numRR=1	
	} else {numRR=length(runIndices)}

	for (iRR in 1:numRR) {
          runIndex<-runIndices[iRR]

### Build name of working directory


	if(multipleRuns=="no") {
	dataDirectory<-dataDirLeft
	} else {
#	    runIndex<-runIndices[iRR]
	    if(!exists("dataDirRight")) {
	        dataDirectory<-paste(dataDirLeft,runIndex,sep="")
	    } else {
	    dataDirectory<-paste(dataDirLeft,runIndex,dataDirRight,sep="")
	    }
	}

### Parse data ##

	parsedData<-BrukerParser(dataSource,dataDirectory)
	tofList<- parsedData$tofList
	tofListMetaData<-parsedData$tofListMetaData

## Memory Usage Check

	memoryUsedAfterData<-memory.size(max=FALSE)
	dateStamp<-date()
	if(printMemoryUse=="yes") {
	printString<-paste("Memory used after parsing data for Run",runIndex)
	print(printString)
	print(memoryUsedAfterData)
	print(dateStamp)
	}

## End Memory Usage Check

### Save tofList and tofListMetaData files as R binary files

	tofFileName<-paste("tofListRun",runIndex,".Rdat",sep='')
	save(tofList,file=tofFileName)

	tofMetaFileName<-paste("tofListMetaDataRun",runIndex,".Rdat",sep='')
	save(tofListMetaData,file=tofMetaFileName)


### Concatenate files if desired

if (concatLists=="yes") {

	tofListAll<-c(tofListAll,tofList)
        }

## File Clean up and Memory Usage Check
	
	rm(tofList)
	gc()

	memoryUsedAfterCleanUp<-memory.size(max=FALSE)
	dateStamp<-date()
	if(printMemoryUse=="yes") {
	printString<-paste("Memory used after removing tofList data for Run",runIndex)
	print(printString)
	print(memoryUsedAfterCleanUp)
	print(dateStamp)
	}

## End Memory Usage Check

# Create array with all tof metaData 

if (concatLists=="yes") {

if(iRR==1) {tofListMetaDataAll<-tofListMetaData}

if(iRR>1) {
	tofListMetaDataPrior<-tofListMetaDataAll
	tofListMetaDataNew<-tofListMetaData

	spectraNamesPrior<-rownames(tofListMetaDataPrior)
	spectraNumberPrior<-length(spectraNamesPrior)

	spectraNamesNew<-rownames(tofListMetaDataNew)
	spectraNumberNew<-length(spectraNamesNew)

	spectraNamesAll<-c(spectraNamesPrior,spectraNamesNew)
	spectraNumberAll<-length(spectraNamesAll)

	metaNames<-colnames(tofListMetaData)
	metaNumber<-length(metaNames)

	tofListMetaDataAll<-array(0,c(spectraNumberAll,metaNumber))
	rownames(tofListMetaDataAll)<- spectraNamesAll
	colnames(tofListMetaDataAll)<- metaNames

	for (i in 1:spectraNumberPrior) {
	tofListMetaDataTemp <-as.matrix(tofListMetaDataPrior[i, ])
	tofListMetaDataAll[i, ]<- tofListMetaDataTemp
	}

	for (j in 1:spectraNumberNew) {
	jj=j+spectraNumberPrior
	tofListMetaDataTemp <-as.matrix(tofListMetaDataNew[j, ])
	tofListMetaDataAll[jj, ]<- tofListMetaDataTemp
	}

	rm(tofListMetaDataPrior)
	rm(tofListMetaDataNew)
      }
     }
## File Clean up and Memory Usage Check
	
	rm(tofListMetaData)
	gc()
	
	memoryUsedAfterCleanUp<-memory.size(max=FALSE)
	dateStamp<-date()
	if(printMemoryUse=="yes") {
	printString<-paste("Memory used after removing tofListMetaData for Run",runIndex)
	print(printString)
	print(memoryUsedAfterCleanUp)
	print(dateStamp)
	}

## End Memory Usage Check

       if (concatLists=="yes") {
	catTofFileName<-paste(catTofFileName,"_",runIndex,sep="")
	catTofMetaFileName<-paste(catTofMetaFileName,"_",runIndex,sep="")
       }
	
# end list concatenation

} # end run loop

### Save concatenated lists if desired

if (concatLists=="yes") {

	tofList<-tofListAll
	tofListMetaData<- tofListMetaDataAll

	catTofFileName<-paste(catTofFileName,".Rdat",sep="")
	save(tofList,file=catTofFileName)

	catTofMetaFileName<-paste(catTofMetaFileName,".Rdat",sep="")
	save(tofListMetaData,file=catTofMetaFileName) 
	}

	rm(list=ls())
	gc()
	memoryUsedAfterCleanUp<-memory.size(max=FALSE)
	dateStamp<-date()

	print("Memory used after final clean up")
	print(memoryUsedAfterCleanUp)
	print(dateStamp)
	
}

