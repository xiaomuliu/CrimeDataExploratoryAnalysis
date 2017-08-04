# clean up burglary data
setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/ExploratoryAnalysis/BurglaryAnalysis/")
filePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/ExploratoryAnalysis/DataPortal/"
fileName.load <- "Crimes_2001_to_present.csv"
file <- paste(filePath,fileName.load,sep="")
CrimeData <- read.csv(file)

colnames(CrimeData) <- c("DATEOCC","CURR_IUCR","BEAT","DISTRICT","FBI_CD","X_COORD","Y_COORD","YEAR","LAT","LONG")
# remove year 2015 data
CrimeData <- subset(CrimeData,YEAR<2015)

# filter data by IUCR so that only burglaries remain
CrimeData <- subset(CrimeData,CURR_IUCR %in% c('0610','0620','0630','0650'))

# convert dateocc to date class
CrimeData$DATEOCC <- as.Date(strptime(CrimeData$DATEOCC,"%m/%d/%Y %I:%M:%S %p"))

# add attribute 'Month','Day' and 'day of week'
Month <- as.numeric(format(CrimeData$DATEOCC, "%m"))
Day <- as.numeric(format(CrimeData$DATEOCC,"%d"))
# DOW <- weekdays(CrimeData$DATEOCC,abbreviate=T)

# change date format
CrimeData$DATEOCC <- format(CrimeData$DATEOCC,'%d-%B-%y')

CrimeData <- cbind(CrimeData[,c("DATEOCC","YEAR")],MONTH=Month,DAY=Day,
                   CrimeData[,!names(CrimeData) %in% c("DATEOCC","YEAR")])


CrimeData$BEAT <- factor(CrimeData$BEAT)
CrimeData$DISTRICT <- factor(CrimeData$DISTRICT)  
# CrimeData$DOW <- factor(CrimeData$DOW, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
CrimeData$CURR_IUCR <- factor(CrimeData$CURR_IUCR)
CrimeData$FBI_CD <- factor(CrimeData$FBI_CD)

# remove rows that have NA location info
CrimeData <- subset(CrimeData,!(is.na(X_COORD)|is.na(Y_COORD)) )

CrimeData$INC_CNT <- rep(1,nrow(CrimeData))

# save as a csv file
fileName.save <- "BURGLARY_01_14.csv"
write.csv(CrimeData,paste(filePath,fileName.save,sep=""),row.names=FALSE)

# compare with CPD data warehouse data
source("importCrimeData.R")
filePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/ExploratoryAnalysis/CPD_DWH/"
fileName <- "X_BURGLARY_POINTS_08_14.csv"
CrimeData.cpd <- importCrimeData(filePath,fileName)
row.names(CrimeData.cpd) <- NULL

CrimeData.08_14 <- subset(CrimeData,YEAR>=2008&YEAR<=2014)

# CPD data has 161738 observations while data portal one has 160442 (~=0.8% discrepancy)