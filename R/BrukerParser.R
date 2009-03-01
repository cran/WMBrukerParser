BrukerParser <-
function(dataSource,dataDirectory) {

	setwd(dataDirectory)

# Brucker Data Reader
# By Bill Cooke
# November 26, 2008
# updated December 22, 2009 MBT


if (!exists("tofWarnings")) tofWarnings <- TRUE
### Get the data source
if (!exists("dataSource")) dataSource <- readline(prompt = "Enter source of samples (e.g. EVMS) ")
# Create the array
metaDataNames = c("acquisitionInfo.arrayBarcode", 
"acquisitionInfo.ionPolarity", "acquisitionInfo.refId", "acquisitionInfo.spotIndex", 
"acquisitionInfo.spotName", "acquisitionInfo.sourceFile", "array.affinityType", "experiment.id", 
"instrumentSpecificSettings.adcBandwidth", "adcGain.high", "adcGain.low", 
"instrumentSpecificSettings.adcOffset", "instrumentSpecificSettings.adcScale","instrumentSpecificSettings.TOFmode", 
"mass.units", "mass.value", "deflector.mode", "instrumentSpecificSettings.detectorBaseVoltage", 
"laserIntensity.units", "laserIntensity.value", "instrumentSpecificSettings.laserIntensityBaseRange", 
"laserIntensityWarming.units", "laserIntensityWarming.value", 
"instrumentSpecificSettings.laserShots", "massCalibration.calibrationSpectrumFile", 
"massCalibration.dateCalibrated", "massCalibration.equation", 
"massCalibration.equationInverse", "massCalibration.equationValid", 
"param.Mode", "param.T0", "param.TDelta", "param.U", "param.c0", 
"param.c1", "param.c2", "massEnd.units", "massEnd.value", "massStart.units", 
"massStart.value", "instrumentSpecificSettings.timeDelta", "instrumentSpecificSettings.timeZero", 
"instrumentSpecificSettings.warmingShots", "instrument.instrumentType", 
"instrument.instrumentVendor", "instrument.serial", "software.version", 
"spottingInfo.spotProtocol", "replicateNumber", "sampleInfo.groupName", 
"sampleInfo.sampleDescription", "sampleInfo.sampleName", "sampleInfo.sampleSource", 
"timeOfFlightData.dateCreated", "timeOfFlightData.domain", "timeOfFlightData.encoding", 
"timeOfFlightData.end", "timeOfFlightData.offset", "timeOfFlightData.pairOrder", 
"timeOfFlightData.scale", "timeOfFlightData.start")

### Check for specialty files:
# Calibration folder
calPath <- dir(pattern="calibration")
## par file
parFile <- dir(pattern=".par")
# axe file
axeFile <- dir(pattern=".axe")
# isset file
issetFile <- dir(pattern=".isset")
# sample File
sampleFile <- dir(pattern="sample.xml")

### Error messages for missing or multiple files
if (tofWarnings){
#### Calibration path
	if (length(parFile) > 1){
		print("Error too many *.par files; Using first")	
		parFile <- parFile[1]
	}
	if (length(calPath) == 0){
		print("No calibration data directory! NA for the following:")
		print("massCalibration.calibrationSpectrumFile")
	}
### par file
	if (length(parFile) > 1){
		print("Error too many *.isset files; Using first")
		issetFile <- parFile[1]
	}
	if (length(parFile) == 0){
		print("Cannot find *.par file.  NA for the following:")
		print(c("massStart.value", "mass.value", "massEnd.value", "adcGain.low", "adcGain.high", "instrumentSpecificSettings.adcBandwidth", "deflector.mode", "param.Mode"))

		print("further, without adcGain.high value for the following can not be determined and remain NA:")
		print(c("instrumentSpecificSettings.detectorBaseVoltage", "instrumentSpecificSettings.adcOffset","instrumentSpecificSettings.adcScale"))

	}
#### axe file
	if (length(axeFile) > 1){
		print("Error too many *.par files; Using first")	
		axeFile <- axeFile[1]
	}
	if (length(axeFile) == 0){
		print("Cannot find *.axe file.  NA for the following:")
		print(c("laserIntensityWarming.value", "instrumentSpecificSettings.warmingShots", "laserIntensity.value", "spottingInfo.spotProtocol"))
	}
### isset file
	if (length(issetFile) > 1){
		print("Error too many *.isset files; Using first")
		issetFile <- issetFile[1]
	}
	if (length(issetFile) == 0){
		print("Cannot find *.isset file.  NA for the following:")
		print(c("instrumentSpecificSettings.detectorBaseVoltage", "instrumentSpecificSettings.adcScale", "instrumentSpecificSettings.adcOffset", "instrumentSpecificSettings.laserIntensityBaseRange"))
	}

### sample file
	if (length(sampleFile) > 1){
		print("Error too many sample.xml files; Using first")
		sampleFile <- sampleFile[1]
	}
	if (length(sampleFile) == 0){
		print("Cannot find sample.xml file.NA for following:")
		print(c("acquisitionInfo.refId","array.affinityType","experiment.id","sampleInfo.groupName","sampleInfo.sampleDescription"))
		print("sampleinfo.sampleName replaced with directory name, reps created")
		print("Edit metaData file to update values if needed.")
	}
}

tofList <- list()
# Get the available spots
spotNamesTemp1 <- dir(pattern="0_")
### Restrict to directories only
	spotNamesTemp2 <- spotNamesTemp1[which(file.info(spotNamesTemp1)$isdir)]

### Remove calibration directories (if name happens to include "0_")
	calNames <- dir(pattern="cal")
	if (length(calNames) > 0){
	spotNamesTemp3 <- spotNamesTemp2[which(spotNamesTemp2!=calNames)]
	spotNames <- spotNamesTemp3
	rm(spotNamesTemp3,calNames)
	}

	spotNames <- spotNamesTemp2
	rm(spotNamesTemp1,spotNamesTemp2)

numberOfSpots <- length(spotNames)
imageData <- FALSE
if (nchar(spotNames[1])==13) imageData <- TRUE
### Construct metaData
tofListMetaData <- array( NA, dim=c(numberOfSpots, length(metaDataNames) ) )
colnames(tofListMetaData) <- metaDataNames;
### look for directory that contains "fid file (1SLin or 1SRef)
setwd(paste(spotNames[1],"/1",sep=""))
fidDir<-dir(pattern="1S")
setwd(dataDirectory)
if(fidDir=="1SLin") {tofListMetaData[,"instrumentSpecificSettings.TOFmode"] <- "LINEAR"}
if(fidDir=="1SRef") {tofListMetaData[,"instrumentSpecificSettings.TOFmode"] <- "REFLECTOR"}
### Default spectrum names
row.names(tofListMetaData) <- spotNames
### Calibration file
if (length(calPath)>0) tofListMetaData[,"massCalibration.calibrationSpectrumFile"] <- paste(getwd(),"/",calPath,sep="")


### Default sample names and rep numbers
tofListMetaData[,"sampleInfo.sampleName"] <- getwd()
sampleNames <- unique(tofListMetaData[,"sampleInfo.sampleName"])
for (k in 1:length(sampleNames)){
	sampleReps <- which(tofListMetaData[,"sampleInfo.sampleName"] == sampleNames[k])
	repCt <- 1
	for (kSample in sampleReps){
		tofListMetaData[kSample,"replicateNumber"] <- repCt
		repCt <- repCt +1
	}
}

#### Calibration date from a proc file
# Define the file
qq <- file(paste(spotNames[1],"/1/",fidDir,"/pdata/1/proc",sep=""),"rb")
temp <- readLines(qq,n=-1)
close(qq)
index <- grep("##[[:punct:]]CLDATE=",temp)
tempSt <- substr(temp[index],12,100)
tempSt <- unlist(strsplit(tempSt,"<"))
tempSt <- unlist(strsplit(tempSt[2],">"))
### Format conversion for date
tofListMetaData[,"massCalibration.dateCalibrated"] <- format(as.POSIXct(paste(gsub("T"," ",substr(tempSt[1],1,19))," EDT",sep="")), "%a %b %d %H:%M:%S EST %Y")

#### This captures data that differs for each sample
for (kSample in 1:numberOfSpots) {
## Populate the metaData
### Creation Date
### Use this because it gets the EDT -> EST conversion correctly
	tofListMetaData[kSample,"timeOfFlightData.dateCreated"] <- format(as.POSIXlt(file.info(paste(spotNames[kSample],"/1/",fidDir,"/fid",sep=""))$mtime,tz="EST"), "%a %b %d %H:%M:%S EST %Y")
### Read metadata from the aqu file
# Define the file
	qq <- file(paste(spotNames[kSample],"/1/",fidDir,"/acqu",sep=""),"rb")
	temp <- readLines(qq,n=-1)
	close(qq)
### acqu items
	if (!imageData){
		tofListMetaData[kSample,"acquisitionInfo.spotName"] <- substr(spotNames[kSample],3,3)
		tofListMetaData[kSample,"acquisitionInfo.spotIndex"] <- substr(spotNames[kSample],4,5)
	}
	if (imageData){
		tofListMetaData[kSample,"acquisitionInfo.spotName"] <- substr(spotNames[kSample],7,9)
		tofListMetaData[kSample,"acquisitionInfo.spotIndex"] <- substr(spotNames[kSample],11,13)
	}
### Data file name
	tofListMetaData[kSample,"acquisitionInfo.sourceFile"] <- paste(getwd(),"/",spotNames[kSample],sep="")
}

#### This captures data that is the same for each sample
### Some attributes are hard coded
tofListMetaData[,"instrument.instrumentType"] <- "Ultraflex Series"
tofListMetaData[,"timeOfFlightData.scale"] <- 1
tofListMetaData[,"timeOfFlightData.offset"] <- 0
tofListMetaData[,"timeOfFlightData.start"] <- 1
tofListMetaData[,"timeOfFlightData.encoding"] <- "base64"
tofListMetaData[,"timeOfFlightData.domain"] <- "time"
tofListMetaData[,"timeOfFlightData.pairOrder"] <- "t-int"
tofListMetaData[,"massCalibration.equation"] <- "c1*((T0+(X-1)*TDelta)/U)^2+c0*((T0+(X-1)*TDelta)/U)+c2"
tofListMetaData[,"param.U"] <- "1e6"
tofListMetaData[,"mass.units"] <- "Da"
tofListMetaData[,"massEnd.units"] <- "Da"
tofListMetaData[,"massStart.units"] <- "Da"
tofListMetaData[,"massCalibration.equationInverse"] <- "X+1"
tofListMetaData[,"massCalibration.equationValid"] <- "X-1"
tofListMetaData[,"sampleInfo.sampleSource"] <- dataSource
tofListMetaData[,"laserIntensityWarming.units"] <- "%"
tofListMetaData[,"laserIntensity.units"] <- "%"

# Use the .par file if it exists
if (length(parFile) == 1){
	qq <- file(parFile,"rb")
	temp <- readLines(qq,n=-1)
	close(qq)
### MassStart
	index <- grep("<MassStart>",temp)
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"massStart.value"] <- tempSt[1]
### Cutoff mass
	index <- grep("<CutOffMass>",temp)
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"mass.value"] <- tempSt[1]
### MassEnd
	index <- grep("<MassEnd>",temp)
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"massEnd.value"] <- tempSt[1]
### LinearDetector
	index <- grep("<LinearDetector>",temp)
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"adcGain.low"] <- tempSt[1]
### BandWidthLimit
	index <- grep("<BandWidthLimit>",temp)
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"instrumentSpecificSettings.adcBandwidth"] <- tempSt[1]
### ElectronicGainLevel
	index <- grep("<ElectronicGainLevel>",temp)
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"adcGain.high"] <- tempSt[1]
### Deflector Mode
### Find Matrix suppression region
	indexBlock <- grep("<MatrixSuppression>",temp)
	index <- grep("Mode",temp)
	index <- index[which(index>indexBlock[1])]
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"deflector.mode"] <- tempSt[1]
### Mode in Calibration
	indexBlock <- grep("<Calibration ",temp)
	innerBlock <- indexBlock[which(grep("<MassRange>",temp)>indexBlock[1])]
	index <- grep("<Mode>",temp)
	index <- index[which(index>innerBlock[1])]
	tempSt <- unlist(strsplit(temp[index],">"))
	tempSt <- unlist(strsplit(tempSt[2],"<"))
	tofListMetaData[,"param.Mode"] <- tempSt[1]
### TOF Mode can be LINEAR, REFLECTOR, NEUTRAL, FAST, LIFT
	if (length(grep("<Mode>LINEAR</Mode>",temp))>0) parTOFmode <- "LINEAR"
	if (length(grep("<Mode>FAST</Mode>",temp))>0) parTOFmode <- "FAST"
	if (length(grep("<Mode>REFLECTOR</Mode>",temp))>0) parTOFmode <- "REFLECTOR"
	if (length(grep("<Mode>NEUTRAL</Mode>",temp))>0) parTOFmode <- "NEUTRAL"
	if (length(grep("<Mode>LIFT</Mode>",temp))>0) parTOFmode <- "LIFT"
	if (tofListMetaData[1,"instrumentSpecificSettings.TOFmode"]!=parTOFmode) {
		print("Check TOF mode: Name of folder containing fid files not consistent with TOF mode in .par file. Can only parse LINEAR or REFLECTOR data")
	}
}

# Use .axe file if it exists
if (length(axeFile)==1) {
	qq <- file(axeFile,"rb")
	temp <- readLines(qq,n=-1)
	close(qq)

##  laserIntensityWarming.value
### matrix_blaster_laser_power
### This gets the correct line
	index <- grep("matrix_blaster_laser_power",temp)
### Isolate tokens
	tempSt <- strsplit(temp[index]," ")
### This get the right token
	tempSt <- tempSt[[1]][grep("matrix_blaster_laser_power",tempSt[[1]])]
### Divide identifier from value
	tempSt <- unlist(strsplit(tempSt,"="))
### Extract the value and clean up quotes 
	tofListMetaData[,"laserIntensityWarming.value"] <- gsub("\"","",tempSt[2])

### matrix_blaster_shots
	index <- grep("matrix_blaster_shots",temp)
	tempSt <- strsplit(temp[index]," ")
	tempSt <- tempSt[[1]][grep("matrix_blaster_shots",tempSt[[1]])]
	tempSt <- unlist(strsplit(tempSt,"="))
	tofListMetaData[,"instrumentSpecificSettings.warmingShots"] <- gsub("\"","",tempSt[2])

### maximal_laser_power
	index <- grep("maximal_laser_power",temp)
	tempSt <- strsplit(temp[index]," ")
	tempSt <- tempSt[[1]][grep("maximal_laser_power",tempSt[[1]])]
	tempSt <- unlist(strsplit(tempSt,"="))
	tofListMetaData[,"laserIntensity.value"] <- gsub("\"","",tempSt[2])

### movement_pattern
	index <- grep("movement_pattern",temp)
	tempSt <- strsplit(temp[index]," ")
	tempSt <- tempSt[[1]][grep("movement_pattern",tempSt[[1]])]
	tempSt <- unlist(strsplit(tempSt,"="))
	tofListMetaData[,"spottingInfo.spotProtocol"] <- gsub("\"","",tempSt[2])
}


# Use the .isset file if it exists
if (length(issetFile) == 1){
	qq <- file(issetFile,"rb")
	temp <- readLines(qq,n=-1)
	close(qq)

### If spectrum type is LINEAR
### instrumentSpecificSettings.detectorBaseVoltage = DetectorGainBaseLin
### If adcGain.high is 1, instrumentSpecificSettings.adcOffset = AnalogOffsetLinMin
### If adcGain.high is 2, instrumentSpecificSettings.adcOffset = AnalogOffsetLinMedium
### If adcGain.high is 3 or any other value, instrumentSpecificSettings.adcOffset = AnalogOffsetLinMax

### If spectrum type is not LINEAR
### instrumentSpecificSettings.detectorBaseVoltage= DetectorGainBaseRef
### instrumentSpecificSettings.adcOffset = AnalogOffsetRefMin


##  PowerMin 
	index <- grep("<PowerMin>",temp)
	tempSt <- substr(temp[index],11,100)
	tempSt <- unlist(strsplit(tempSt,"<"))
	tofListMetaData[,"instrumentSpecificSettings.laserIntensityBaseRange"] <- tempSt[[1]]



	if(!is.na(tofListMetaData[1,"instrumentSpecificSettings.TOFmode"])) {  

##  instrumentSpecificSettings.adcOffset
	if (tofListMetaData[1,"instrumentSpecificSettings.TOFmode"] == "LINEAR"){
### LINEAR mode
### instrumentSpecificSettings.detectorBaseVoltage
		index <- grep("<DetectorGainBaseLin>",temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.detectorBaseVoltage"] <- tempSt[1]
### instrumentSpecificSettings.adcOffset
		testString <- c("AnalogOffsetLinMin","AnalogOffsetLinMedium","AnalogOffsetLinMax")
		testIndex <- min(3,as.integer(tofListMetaData[1,"adcGain.high"]))
		index <- grep(testString[testIndex],temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.adcOffset"] <- tempSt[1]
	} else {
### not LINEAR mode
### instrumentSpecificSettings.detectorBaseVoltage
		index <- grep("<DetectorGainBaseRef>",temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.detectorBaseVoltage"] <- tempSt[1]
### instrumentSpecificSettings.adcOffset
		index <- grep("<AnalogOffsetRefMin>",temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.adcOffset"] <- tempSt[1]
	}

	} 
	if(!is.na(tofListMetaData[1,"adcGain.high"])) {  

##  instrumentSpecificSettings.adcScale
	if (tofListMetaData[1,"adcGain.high"] == "1"){
		index <- grep("<SensitivityMin>",temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.adcScale"] <- tempSt[1]
	} else if (tofListMetaData[1,"adcGain.high"] == "2"){
		index <- grep("<SensitivityMedium>",temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.adcScale"] <- tempSt[1]
	} else {
		index <- grep("<SensitivityMax>",temp)
		tempSt <- substr(temp[index],1,100)
		tempSt <- unlist(strsplit(tempSt,">"))
		tempSt <- unlist(strsplit(tempSt[2],"<"))
		tofListMetaData[,"instrumentSpecificSettings.adcScale"] <- tempSt[1]
	}
	} 
}
# Use the first acqu file
qq <- file(paste(spotNames[1],"/1/",fidDir,"/acqu",sep=""),"rb")
temp <- readLines(qq,n=-1)
close(qq)
### Barcode
index <- grep("##[[:punct:]]TgIDS=",temp)
tempSt <- substr(temp[index],10,100)
tempSt <- unlist(strsplit(tempSt,"<"))
tempSt <- unlist(strsplit(tempSt[2],">"))
tofListMetaData[,"acquisitionInfo.arrayBarcode"] <- tempSt[1]
### Number of data points
index <- grep("##[[:punct:]]TD=",temp)
tempSt <- substr(temp[index],7,100)
tempSt <- parse(text=tempSt)
tofListMetaData[,"timeOfFlightData.end"] <- as.character(c(tempSt[[1]]))
### Software Version
index <- grep("##[[:punct:]]XACQVS=",temp)
tempSt <- substr(temp[index],11,100)
tempSt <- unlist(strsplit(tempSt,"<"))
tempSt <- unlist(strsplit(tempSt[2],">"))
tofListMetaData[,"software.version"] <- tempSt[1]
### Instrument Vendor
index <- grep("##ORIGIN=",temp)
tofListMetaData[,"instrument.instrumentVendor"] <- substr(temp[index],11,100)
### Instrument ID
index <- grep("##[[:punct:]]InstrID=",temp)
tempSt <- substr(temp[index],12,100)
tempSt <- unlist(strsplit(tempSt,"<"))
tempSt <- unlist(strsplit(tempSt[2],">"))
tofListMetaData[,"instrument.serial"] <- tempSt[1]



### Calibration parameters
index <- grep("##[[:punct:]]ML2=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
tempSt <- parse(text=tempSt[2])
tofListMetaData[,"param.c0"] <- as.character(c(tempSt[[1]]))

index <- grep("##[[:punct:]]ML1=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
tempSt <- parse(text=tempSt[2])
tofListMetaData[,"param.c1"] <- as.character(c(tempSt[[1]]))



index <- grep("##[[:punct:]]ML3=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
tempSt <- parse(text=tempSt[2])
tofListMetaData[,"param.c2"] <- as.character(c(tempSt[[1]]))

### Dwell Time in ns
index <- grep("##[[:punct:]]DW=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
tempSt <- parse(text=tempSt[2])
tofListMetaData[,"param.TDelta"] <- as.character(c(tempSt[[1]]))
tofListMetaData[,"instrumentSpecificSettings.timeDelta"] <- tofListMetaData[,"param.TDelta"]

### Delay Time in ns
index <- grep("##[[:punct:]]DELAY=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
tempSt <- parse(text=tempSt[2])
tofListMetaData[,"param.T0"] <- as.character(c(tempSt[[1]]))

### Time Offset in clock cycles
cDelay <- as.double(tofListMetaData[kSample,"param.T0"])/as.double(tofListMetaData[kSample,"param.TDelta"])
tofListMetaData[,"instrumentSpecificSettings.timeZero"] <- cDelay

### Laser Shots
index <- grep("##[[:punct:]]NoSHOTS=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
tempSt <- parse(text=tempSt[2])
tofListMetaData[,"instrumentSpecificSettings.laserShots"] <- as.character(c(tempSt[[1]]))

### Ion polarity
tofListMetaData[,"acquisitionInfo.ionPolarity"] <- "negative"
index <- grep(".IONIZATION MODE=",temp)
tempSt <- unlist(strsplit(temp[index],"="))
if (length(grep("LD+",tempSt[2]))>0) tofListMetaData[,"acquisitionInfo.ionPolarity"] <- "positive"

### Now look for a "sample.xml" file
if (length(sampleFile)== 1){
	qq <- file(sampleFile,"rb")
	temp <- readLines(qq,n=-1)
	close(qq)

### Get info for each spot
	for (kSample in 1:numberOfSpots){
		spotID <- paste(tofListMetaData[kSample,"acquisitionInfo.spotName"],":",tofListMetaData[kSample,"acquisitionInfo.spotIndex"],sep="")
		index <- grep(spotID,temp)
		sampleInfo <- strsplit(temp[index],split="'")[[1]]
		tofListMetaData[kSample,"array.affinityType"] <- sampleInfo[18]
		tofListMetaData[kSample,"sampleInfo.sampleName"] <- sampleInfo[2]
		tofListMetaData[kSample,"acquisitionInfo.refId"] <- sampleInfo[2]
		tofListMetaData[kSample,"sampleInfo.sampleDescription"] <- sampleInfo[4]
		tofListMetaData[kSample,"sampleInfo.groupName"] <- sampleInfo[6]
	}
	} 
	replaceThese <- which(tofListMetaData[,"sampleInfo.sampleDescription"] =="")
	if(length(replaceThese > 0)) tofListMetaData[replaceThese,"sampleInfo.sampleDescription"] <- tofListMetaData[replaceThese,"sampleInfo.sampleName"]
	sampleNames <- unique(tofListMetaData[,"sampleInfo.sampleName"])
	for (k in 1:length(sampleNames)){
		sampleReps <- which(tofListMetaData[,"sampleInfo.sampleName"] == sampleNames[k])
		repCt <- 1
		for (kSample in sampleReps){
			tofListMetaData[kSample,"replicateNumber"] <- repCt
### This version replaces all spaces
###			tofListMetaData[kSample,"sampleInfo.sampleName"] <- paste(gsub(" ","_",tofListMetaData[1,"sampleInfo.sampleName"]),"_",repCt,sep="")
### This version just appends _rep
			tofListMetaData[kSample,"acquisitionInfo.refId"] <- paste(tofListMetaData[kSample,"sampleInfo.sampleName"],"_",repCt,sep="")
			tofListMetaData[kSample,"experiment.id"] <- tofListMetaData[kSample,"acquisitionInfo.refId"]
			repCt <- repCt +1
		}	
}

# Populate tofList
for (kSample in 1:numberOfSpots)
{
	numTofPts <- as.integer(tofListMetaData[kSample,"timeOfFlightData.end"])
	qq <- file(paste(spotNames[kSample],"/1/",fidDir,"/fid",sep=""),"rb")
	temp <- readBin(qq,integer(),n=numTofPts,size=4,endian="little")
	tofList[[spotNames[kSample]]] <- temp
	close(qq)
}

### Now rename the spectra
spectraNames <- tofListMetaData[,"acquisitionInfo.refId"]
row.names(tofListMetaData) <- spectraNames
names(tofList) <- spectraNames

### Clean up extra objects:
clearList <- c("calPath","sampleNames","sampleReps","spotNames","spectraNames",
"issetFile","kSample","metaDataNames","numberOfSpots","parFile","qq",
"cDelay","dataSource","index","repCt","replaceThese","sampleInfo","spotID",
"indexBlock","innerBlock","temp","tempSt","testIndex","testString")

for (k in 1:length(clearList)){
	temp <- ls(pattern=clearList[k])
	if (length(temp)>0) rm(list = temp)
}
rm(clearList,temp,k)
return(list(tofList=tofList,tofListMetaData=tofListMetaData))
}

