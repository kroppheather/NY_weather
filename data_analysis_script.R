#updated 12/17

library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(sp)
library(ggplot2)



### Set up directories   -----

# Creating user numbers for each person
Users = c(1, # Abby
          2, # Professor Kropp
          3) # Rachel

# Creating a directory with all of our file paths 
diru = c("/Users/abby/Documents/NYweather",
         "/Users/hkropp/Google Drive/research/students/NYweather/data",
         "/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/data")

# Choosing the user number - CHANGE THIS VALUE 
usernumber = 3

### Read in data   -----
#csv with weather data
# Reading in prcp data from google drive
PrcpData <- read.csv(paste0(diru[usernumber], "/prcp_all.csv"), na.strings=c(""," ","NA"))

# Reading in tmax data from google drive
TmaxData <- read.csv(paste0(diru[usernumber], "/Tmax_all.csv"), na.strings=c(""," ","NA"))

# Reading in tmin data from google drive
TminData <- read.csv(paste0(diru[usernumber],"/Tmin_all.csv"), na.strings=c(""," ","NA"))

# Read in station info data
StationInfo <- read.csv(paste0(diru[usernumber],"/station_info.csv"), na.strings=c(""," ","NA"))

#NY spatial data
ez <- readOGR(paste0(diru[usernumber],"/ecozone/dfw_ecozone.shp"))

### Organize weather data  -----
# Omitting na values from the data sets
PrcpData <- PrcpData %>% drop_na(prcp)
TmaxData <- TmaxData %>% drop_na(tmax)
TminData <- TminData %>% drop_na(tmin)

# Keeping only rows without a quality flag
PrcpData <- subset(PrcpData, is.na(PrcpData$qflag))
TmaxData <- subset(TmaxData, is.na(TmaxData$qflag))
TminData <- subset(TminData, is.na(TminData$qflag))

# Formatting date columns as dates
PrcpData$date <- as.Date(PrcpData$date, "%Y-%m-%d")
TmaxData$date <- as.Date(TmaxData$date, "%Y-%m-%d")
TminData$date <- as.Date(TminData$date, "%Y-%m-%d")

# Adding a year column to each data set
PrcpData$year <- year(PrcpData$date)
TmaxData$year <- year(TmaxData$date)
TminData$year <- year(TminData$date)

# Making a day of year column
PrcpData$DOY <- yday(PrcpData$date)
TmaxData$DOY <- yday(TmaxData$date)
TminData$DOY <- yday(TminData$date)

# Subsetting data to only include first half of the year
PrcpData <- subset(PrcpData, PrcpData$DOY < 182)
TmaxData <- subset(TmaxData, TmaxData$DOY < 182)
TminData <- subset(TminData, TminData$DOY < 182)

# Converting temperature data to degrees Celsius
TmaxData$tmax <- TmaxData$tmax/10
TminData$tmin <- TminData$tmin/10
# Converting precipitation data to millimeters
PrcpData$prcp <- PrcpData$prcp/10

### Narrow down to good stations ----
# Counting observations per year for the tmax data
# Make new data frame with just the id, tmax value, and year
TmaxDataYear <- aggregate(TmaxData$tmax, by=list(TmaxData$id,TmaxData$year), FUN="length")

# Changing column names
colnames(TmaxDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
TmaxDataYear <- subset(TmaxDataYear, TmaxDataYear$ncount >= 171)

# Counting observations per year for the tmin data
# Make new data frame with just the id, tmin value, and year
TminDataYear <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="length")

# Changing column names
colnames(TminDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
TminDataYear <- subset(TminDataYear, TminDataYear$ncount >= 171)

# Counting observations per year for the prcp data
# Make new data frame with just the id, prcp value, and year
PrcpDataYear <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="length")

# Changing column names
colnames(PrcpDataYear) <- c("station", "year", "ncount")

PrcpDataYear <- subset(PrcpDataYear, PrcpDataYear$ncount >= 171)

# Counting number of years per station for tmax
TmaxStn <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="length")

# Renaming columns
colnames(TmaxStn) <- c("station", "ycount")

# Adding a min year column
TmaxStn$min <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="min")$x

# Adding a max year column
TmaxStn$max <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="max")$x

# Adding a column for the range of years covered by the station
TmaxStn$range <- (TmaxStn$max + 1) - TmaxStn$min

# Getting rid of rows with less than 50 years
TmaxStn <- subset(TmaxStn, TmaxStn$ycount >= 50)

# Adding a column for percent of years in range (measure of continuity)
TmaxStn$pctcont <- TmaxStn$ycount/TmaxStn$range

# Getting rid of rows with less than 75% of years in their range
TmaxStn <- subset(TmaxStn, TmaxStn$pctcont >= .75)

# Narrow down to rows with max year = 2019
TmaxStn <- subset(TmaxStn, TmaxStn$max == 2019)

# Counting number of years per station for tmin
TminStn <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="length")

# Renaming columns
colnames(TminStn) <- c("station", "ycount")

# Adding a min year column
TminStn$min <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="min")$x

# Adding a max year column
TminStn$max <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="max")$x

# Adding a column for the range of years covered by the station
TminStn$range <- (TminStn$max + 1) - TminStn$min

# Getting rid of rows with less than 50 years
TminStn <- subset(TminStn, TminStn$ycount >= 50)

# Adding a column for percent of years in range (measure of continuity)
TminStn$pctcont <- TminStn$ycount/TminStn$range

# Getting rid of rows with less than 75% of years in their range
TminStn <- subset(TminStn, TminStn$pctcont >= .75)

# Narrow down to rows with max year = 2019
TminStn <-subset(TminStn, TminStn$max == 2019)

# Counting number of years per station for prcp
PrcpStn <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="length")

# Renaming columns
colnames(PrcpStn) <- c("station", "ycount")

# Adding a min year column
PrcpStn$min <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="min")$x

# Adding a max year column
PrcpStn$max <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="max")$x

# Adding a column for the range of years covered by the station
PrcpStn$range <- (PrcpStn$max + 1) - PrcpStn$min

# Getting rid of rows with less than 50 years
PrcpStn <- subset(PrcpStn, PrcpStn$ycount >= 50)

# Adding a column for percent of years in range (measure of continuity)
PrcpStn$pctcont <- PrcpStn$ycount/PrcpStn$range

# Getting rid of rows with less than 75% of years in their range
PrcpStn <- subset(PrcpStn, PrcpStn$pctcont >= .75)

# narrow down to rows with max year = 2019
PrcpStn <- subset(PrcpStn, PrcpStn$max == 2019)

### Map stations ----
#start by mapping all stations
#assume coordinates are in WGS 84
#epsg 4326
#turn stations into spatial points
siteLL <- SpatialPoints(matrix(c(StationInfo$long,StationInfo$lat), ncol=2,byrow=FALSE),
                        CRS( "+init=epsg:4326") )
#reproject points into the ez coordinate system (utm)
siteP <- spTransform(siteLL,ez@proj4string)
ez@data$MINOR_DESC <- as.factor(ez@data$MINOR_DESC )
ez@data$MAJOR<- as.factor(ez@data$MAJOR )
#look at weather stations
plot(siteP, pch=19)
#set up colors based on major zone
MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
#colors
MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
                    "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
                    "#576356","#31474f" )
#add colors to plot back in
ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
#make a map of all weather sites
plot(ez, col=ez@data$col, border=NA)
legend("topleft", paste(MajorZones$MAJOR),fill=MajorZones$col, bty="n", cex=0.35)
plot(siteP, add=TRUE, pch=19, col=rgb(0.5,0.5,0.5,0.45), cex=0.5)
#look at Tmax
TmaxStn$station_id <- TmaxStn$station
sitesMax <- left_join(TmaxStn,StationInfo, by ="station_id")
maxPoints <- SpatialPoints(matrix(c(sitesMax $long,sitesMax $lat), ncol=2,byrow=FALSE),
                           CRS( "+init=epsg:4326") )
#reproject points into the ez coordinate system (utm)
maxP <- spTransform(maxPoints ,ez@proj4string)
plot(maxP, col="grey25",pch=19, add=TRUE)
title(main= "Map of TMax Stations")


##now look at Tmin
#look at weather stations
plot(siteP, pch=19)
#set up colors based on major zone
MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
#colors
MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
                    "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
                    "#576356","#31474f" )
#add colors to plot back in
ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
#make a map of all weather sites
plot(ez, col=ez@data$col, border=NA)
legend("topleft", paste(MajorZones$MAJOR),fill=MajorZones$col, bty="n", cex=0.35)
plot(siteP, add=TRUE, pch=19, col=rgb(0.5,0.5,0.5,0.45), cex=0.5)
#look at Tmin
TminStn$station_id <- TminStn$station
sitesMin <- left_join(TminStn,StationInfo, by ="station_id")
minPoints <- SpatialPoints(matrix(c(sitesMin $long,sitesMin $lat), ncol=2,byrow=FALSE),
                           CRS( "+init=epsg:4326") )
#reproject points into the ez coordinate system (utm)
minP <- spTransform(minPoints ,ez@proj4string)
plot(maxP, col="grey25",pch=19, add=TRUE)
title(main= "Map of TMin Stations")


#Now look at PRCP
#look at weather stations
plot(siteP, pch=19)
#set up colors based on major zone
MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
#colors
MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
                    "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
                    "#576356","#31474f" )
#add colors to plot back in
ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
#make a map of all weather sites
plot(ez, col=ez@data$col, border=NA)
legend("topleft", paste(MajorZones$MAJOR),fill=MajorZones$col, bty="n", cex=0.35)
plot(siteP, add=TRUE, pch=19, col=rgb(0.5,0.5,0.5,0.45), cex=0.5)
#look at Prcp
PrcpStn$station_id <- PrcpStn$station
sitesPrcp <- left_join(PrcpStn,StationInfo, by ="station_id")
prcpPoints <- SpatialPoints(matrix(c(sitesPrcp $long,sitesPrcp $lat), ncol=2,byrow=FALSE),
                            CRS( "+init=epsg:4326") )
#reproject points into the ez coordinate system (utm)
prcpP <- spTransform(prcpPoints ,ez@proj4string)
plot(prcpP, col="grey25",pch=19, add=TRUE)
title(main= "Map of Precip Stations")

### identify sites with all data types ----
AllStn <- data.frame(station_id = sitesMax$station_id, lat = sitesMax$lat, long = sitesMax$long, name = sitesMax$name) 
AllStn <- inner_join(AllStn, sitesMin, by="station_id")
AllStn <- inner_join(AllStn, sitesPrcp, by="station_id")

# map the stations that have all data
#look at weather stations
plot(siteP, pch=19)
#set up colors based on major zone
MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
#colors
MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
                    "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
                    "#576356","#31474f" )
#add colors to plot back in
ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
#make a map of all weather sites
plot(ez, col=ez@data$col, border=NA)
legend("topleft", paste(MajorZones$MAJOR),fill=MajorZones$col, bty="n", cex=0.35)
plot(siteP, add=TRUE, pch=19, col=rgb(0.5,0.5,0.5,0.45), cex=0.5)
#look at Stations
allPoints <- SpatialPoints(matrix(c(AllStn $long,AllStn $lat), ncol=2,byrow=FALSE),
                           CRS( "+init=epsg:4326") )
#reproject points into the ez coordinate system (utm)
allP <- spTransform(allPoints ,ez@proj4string)
plot(allP, col="grey25",pch=19, add=TRUE)
title(main= "Map of Stations with Tmax, Tmin, and Prcp")



### Creating one large data frame ----
# join tmax, tmin, and prcp data
AllData <- full_join(TmaxData, TminData, by = c("id"="id", "year"="year", "DOY" = "DOY"), copy = FALSE)
AllData <- full_join(AllData, PrcpData, by = c("id"="id", "year"="year", "DOY" = "DOY"), copy = FALSE)

# add station names of the 12 good stations
AllData <- inner_join(AllData, AllStn, by = c("id"="station_id"))

# add month column
AllData$Month <- month(AllData$date, label = TRUE)

# add decade column
AllData$Decade <- AllData$year - (AllData$year %% 10)

# Subset to just keep id, tmin, tmax, year, doy
AllData <- data.frame(StationID = AllData$id, 
                      StationName = AllData$name.x, 
                      DOY = AllData$DOY, 
                      Month = AllData$Month,
                      Year = AllData$year, 
                      Decade = AllData$Decade,
                      prcp = AllData$prcp, 
                      tmax = AllData$tmax, 
                      tmin = AllData$tmin)

# Adding average temperature column
AllData$tav <- ((AllData$tmax - AllData$tmin)/2) + AllData$tmin 

# Adding freeze-thaw flag (less than -2.2 degrees and higher than 0 degrees in the same day)
# Maybe change these numbers?
AllData$FreezeThaw <- ifelse(AllData$tmin<(-2.2) & AllData$tmax>0, 1 , NA)

# Adding range of freeze thaw column
AllData$FTrange <- ifelse(AllData$FreezeThaw == 1, AllData$tmax - AllData$tmin, NA)

# Making station column a factor
AllData$StationID <- as.factor(AllData$StationID)


# Extreme values (occur <5% of the time) 
## make table of extreme values for each station, then can join into AllData, then can highlight tmax higher than extreme value
## by decade 
# highest 5% tmax
ExtRef <- aggregate(AllData$tmax, by = list(AllData$StationID, AllData$Month), FUN = "quantile", prob = 0.95, na.rm = TRUE)
# lowest 5% tmin
ExtRef$tmin <- aggregate(AllData$tmin, by = list(AllData$StationID, AllData$Month), FUN = "quantile", prob = 0.05, na.rm = TRUE)$x
# max tmax
ExtRef$max <- aggregate(AllData$tmax, by = list(AllData$StationID, AllData$Month), max, na.rm = TRUE)$x
# min tmin
ExtRef$min <- aggregate(AllData$tmin, by = list(AllData$StationID, AllData$Month), min, na.rm = TRUE)$x

# clean up data table 
ExtRef <- data.frame(StationID = ExtRef$Group.1,
                     Month = ExtRef$Group.2,
                     RefMin = ExtRef$min,
                     RefLo = ExtRef$tmin,
                     RefHi = ExtRef$x,
                     RefMax = ExtRef$max)

# join to AllData
AllData <- left_join(AllData, ExtRef, by = c("StationID", "Month"))

# add flag for extreme high tmax
AllData$ExtrHi <- ifelse(AllData$tmax>=AllData$RefHi, 1, NA)

# add flag for extreme low tmin
AllData$ExtrLo <- ifelse(AllData$tmin<=AllData$RefLo,1,NA)


# make extreme values dataframe
ExtVals <- aggregate(AllData$tmax, by = list(AllData$StationID, AllData$Month, AllData$Year), FUN = "quantile", prob = 0.95, na.rm = TRUE)
colnames(ExtVals) <- c("StationID", "Month", "Year", "HiTmax")
ExtVals$LoTmin <- aggregate(AllData$tmin, by = list(AllData$StationID, AllData$Month, AllData$Year), FUN = "quantile", prob = 0.05, na.rm = TRUE)$x

# join extreme values to alldata
AllData <- left_join(AllData, ExtVals, by = c("StationID", "Month","Year"))

## subset to spring data frame
SpringData <- subset(AllData, AllData$Month %in% c("Mar","Apr","May"))
SpringData$DayID <- ifelse(leap_year(SpringData$Year), SpringData$DOY - 60, SpringData$DOY - 59)

# spring anomalies
SpringAnomaly <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName, SpringData$DOY), FUN = "mean", na.rm = TRUE)
colnames(SpringAnomaly) <- c("StationID", "StationName", "DOY", "DayTav")
SpringAnomaly$sd <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName, SpringData$DOY), FUN = "sd", na.rm = TRUE)$x

# joining spring anomalies to spring data
SpringData <- left_join(SpringData, SpringAnomaly, by = c("StationID", "StationName", "DOY"))

# adding anomaly column to spring data
SpringData$AnRaw <- (SpringData$tav - SpringData$DayTav)
SpringData$AnStd <- (SpringData$tav - SpringData$DayTav) / SpringData$sd

# make a data frame of monthly averages
SpringMonths <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID, SpringData$StationName, SpringData$Month), FUN="mean", na.rm = TRUE)
colnames(SpringMonths) <- c("year","StationID","StationName", "month","tmax")
SpringMonths$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Month), FUN="mean", na.rm = TRUE)$x
SpringMonths$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Month), FUN="mean", na.rm = TRUE)$x
# add columns of extreme hi and lo temperature values
SpringMonths$ExtHi <- aggregate(SpringData$HiTmax, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringMonths$ExtLo <- aggregate(SpringData$LoTmin, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN = "mean", na.rm = TRUE)$x
# add columns counting extreme temp flags
SpringMonths$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month) , FUN = "sum", na.rm = TRUE)$x
SpringMonths$ExLoCount <- aggregate(SpringData$ExtrLo, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN = "sum", na.rm = TRUE)$x
# add columns with freeze thaw flags and range
SpringMonths$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringMonths$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN="mean", na.rm = TRUE)$x

# data frame with yearly averages - spring months averaged together
SpringYear <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID, SpringData$StationName), FUN="mean", na.rm = TRUE)
colnames(SpringYear) <- c("year","StationID","StationName", "tmax")
SpringYear$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName), FUN="mean", na.rm = TRUE)$x
SpringYear$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName), FUN="mean", na.rm = TRUE)$x
# add columns of extreme hi and lo temperature values
SpringYear$ExtHi <- aggregate(SpringData$HiTmax, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName), FUN = "mean", na.rm = TRUE)$x
SpringYear$ExtLo <- aggregate(SpringData$LoTmin, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName), FUN = "mean", na.rm = TRUE)$x
# add columns counting extreme temp flags
SpringYear$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName) , FUN = "sum", na.rm = TRUE)$x
SpringYear$ExLoCount <- aggregate(SpringData$ExtrLo, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName), FUN = "sum", na.rm = TRUE)$x
# add columns with freeze thaw flags and range
SpringYear$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName), FUN="sum", na.rm = TRUE)$x
SpringYear$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName), FUN="mean", na.rm = TRUE)$x

# decade averages by month
SpringDecade<- aggregate(SpringData$tmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)
colnames(SpringDecade) <- c("StationID", "StationName", "Decade", "Month", "tmax")
SpringDecade$tmin <- aggregate(SpringData$tmin, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$tav <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExtHi <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringDecade$ExLoCount <- aggregate(SpringData$ExtrLo,by=list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringDecade$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecade$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x

# decade averages with spring months averaged together
SpringDecadeAv <- aggregate(SpringData$tmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)
colnames(SpringDecadeAv) <- c("StationID", "StationName", "Decade", "tmax")
SpringDecadeAv$tmin <- aggregate(SpringData$tmin, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$tav <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExtHi <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$StationID, SpringData$Decade), FUN="sum", na.rm = TRUE)$x
SpringDecadeAv$ExLoCount <- aggregate(SpringData$ExtrLo,by=list(SpringData$StationID, SpringData$Decade), FUN="sum", na.rm = TRUE)$x
SpringDecadeAv$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecadeAv$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x

# subset to specific months
# all march data
MarData <- subset(AllData, AllData$Month == "Mar")
# march yearly averages
MarYear <- subset(SpringMonths, SpringMonths$month == "Mar")
# march decade averages
MarDecade <- subset(SpringDecade, SpringDecade$Month == "Mar")

# all april data
AprData <- subset(AllData, AllData$Month == "Apr")
# april yearly averages
AprYear <- subset(SpringMonths, SpringMonths$month == "Apr")
# april decade averages
AprDecade <- subset(SpringDecade, SpringDecade$Month == "Apr")

# all may data
MayData <- subset(AllData, AllData$Month == "May")
# may yearly averages
MayYear <- subset(SpringMonths, SpringMonths$month == "May")
# may decade averages
MayDecade <- subset(SpringDecade, SpringDecade$Month == "May")

# subset to specific stations
# yearly averages
stn1 <- subset(SpringYear, SpringYear$StationID == "USC00300785")
stn2 <- subset(SpringYear, SpringYear$StationID == "USC00301752")
stn3 <- subset(SpringYear, SpringYear$StationID == "USC00304102")
stn4 <- subset(SpringYear, SpringYear$StationID == "USC00304912")
stn5 <- subset(SpringYear, SpringYear$StationID == "USC00306085")
stn6 <- subset(SpringYear, SpringYear$StationID == "USC00306314")
stn7 <- subset(SpringYear, SpringYear$StationID == "USC00309000")
stn8 <- subset(SpringYear, SpringYear$StationID == "USW00014735")
stn9 <- subset(SpringYear, SpringYear$StationID == "USW00014750")
stn10 <- subset(SpringYear, SpringYear$StationID == "USW00014771")
stn11 <- subset(SpringYear, SpringYear$StationID == "USW00094725")
stn12 <- subset(SpringYear, SpringYear$StationID == "USW00094790")

#decadal averages

stn1dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00300785")
stn2dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00301752")
stn3dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00304102")
stn4dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00304912")
stn5dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00306085")
stn6dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00306314")
stn7dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USC00309000")
stn8dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USW00014735")
stn9dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USW00014750")
stn10dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USW00014771")
stn11dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USW00094725")
stn12dc <- subset(SpringDecadeAv, SpringDecadeAv$StationID == "USW00094790")

### general temperature trends ----

# line graphs of tmax, tmin, tav over time
# station 1 - Boonville 
ggplot(data = stn1, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Boonville, NY")

# station 2 - Cooperstown
ggplot(data = stn2, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Cooperstown, NY")

# station 3 - Indian Lake
ggplot(data = stn3, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Indian Lake, NY")

# station 4 - Lowville
ggplot(data = stn4, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Lowville, NY")

# station 5 - Norwich
ggplot(data = stn5, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Norwich, NY")

# station 6  - Oswego
ggplot(data = stn6, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Oswego, NY")

# station 7 - Watertown
ggplot(data = stn7, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Watertown, NY")

# station 8 - Albany
ggplot(data = stn8, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Albany, NY")

# station 9 - Glens Falls
ggplot(data = stn9, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Glens Falls, NY")

# station 10 - Syracuse
ggplot(data = stn10, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Syracuse, NY")

# station 11 - Massena
ggplot(data = stn11, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Massena, NY")

# station 12 - Watertown Airport
ggplot(data = stn12, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Watertown Airport, NY")


# average temperatures by decade for all stations
ggplot(data = SpringDecadeAv, aes(x = Decade, y = tav, color = StationName))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  theme_classic()+
  xlim(1950,2020)+
  labs(x = "Decade", y = "Temperature (celsius)", title = "Average Spring Temperatures")


## linear regressions for tav ----
# by year 
# station 1 model
stn1.mod <- lm(stn1$tav ~ stn1$year)
# assumptions
stn1.res <- rstandard(stn1.mod)
qqnorm(stn1.res)
qqline(stn1.res)
plot(stn1$year, stn1.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn1.mod)
plot(stn1$year, stn1$tav,
     ylab = "average temp",
     xlab = "year")
abline(stn1.mod)

# station 2 model
stn2.mod <- lm(stn2$tav ~ stn2$year)
# assumptions
stn2.res <- rstandard(stn2.mod)
qqnorm(stn2.res)
qqline(stn2.res)
plot(stn2$year, stn2.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn2.mod)
plot(stn2$year, stn2$tav,
     ylab = "average temp",
     xlab = "year")
abline(stn2.mod)

# station 3 model
stn3.mod <- lm(stn3$tav ~ stn3$year)
# assumptions
stn3.res <- rstandard(stn3.mod)
qqnorm(stn3.res)
qqline(stn3.res)
plot(stn3$year, stn3.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn3.mod)
plot(stn3$year, stn3$tav,
     ylab = "average temp",
     xlab = "year")
abline(stn3.mod)

# station 4 model
stn4.mod <- lm(stn4$tav ~ stn4$year)
# assumptions
stn4.res <- rstandard(stn4.mod)
qqnorm(stn4.res)
qqline(stn4.res)
plot(stn4$year, stn4.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn4.mod)
plot(stn4$year, stn4$tav,
     ylab = "average temp",
     xlab = "year")
abline(stn4.mod)

# station 5 model
stn5.mod <- lm(stn5$tav ~ stn5$year)
# assumptions
stn5.res <- rstandard(stn5.mod)
qqnorm(stn5.res)
qqline(stn5.res)
plot(stn5$year, stn5.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn5.mod)
plot(stn5$year, stn5$tav,
     ylab = "average temp",
     xlab = "year")
abline(stn5.mod)

# station 6 model -- SIGNIFICANT!
stn6.mod <- lm(stn6$tav ~ stn6$year)
# assumptions
stn6.res <- rstandard(stn6.mod)
qqnorm(stn6.res)
qqline(stn6.res)
plot(stn6$year, stn6.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn6.mod)
plot(stn6$year, stn6$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Oswego, NY")
abline(stn6.mod)

# station 7 model
stn7.mod <- lm(stn7$tav ~ stn7$year)
# assumptions
stn7.res <- rstandard(stn7.mod)
qqnorm(stn7.res)
qqline(stn7.res)
plot(stn7$year, stn7.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn7.mod)
plot(stn7$year, stn7$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Watertown, NY")
abline(stn7.mod)

# station 8 model -- SIGNIFICANT!
stn8.mod <- lm(stn8$tav ~ stn8$year)
# assumptions
stn8.res <- rstandard(stn8.mod)
qqnorm(stn8.res)
qqline(stn8.res)
plot(stn8$year, stn8.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn8.mod)
plot(stn8$year, stn8$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Albany, NY")
abline(stn8.mod)

# station 9 model
stn9.mod <- lm(stn9$tav ~ stn9$year)
# assumptions
stn9.res <- rstandard(stn9.mod)
qqnorm(stn9.res)
qqline(stn9.res)
plot(stn9$year, stn9.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn9.mod)
plot(stn9$year, stn9$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Glens Falls, NY")
abline(stn9.mod)

# station 10 model
stn10.mod <- lm(stn10$tav ~ stn10$year)
# assumptions
stn10.res <- rstandard(stn10.mod)
qqnorm(stn10.res)
qqline(stn10.res)
plot(stn10$year, stn10.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn10.mod)
plot(stn10$year, stn10$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Syracuse, NY")
abline(stn10.mod)

# station 11 model -- SIGNIFICANT (just slope ?)
stn11.mod <- lm(stn11$tav ~ stn11$year)
# assumptions
stn11.res <- rstandard(stn11.mod)
qqnorm(stn11.res)
qqline(stn11.res)
plot(stn11$year, stn11.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn11.mod)
plot(stn11$year, stn11$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Massena, NY")
abline(stn11.mod)

# station 12 model
stn12.mod <- lm(stn12$tav ~ stn12$year)
# assumptions
stn12.res <- rstandard(stn12.mod)
qqnorm(stn12.res)
qqline(stn12.res)
plot(stn12$year, stn12.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn12.mod)
plot(stn12$year, stn12$tav,
     pch = 19,
     ylab = "average temp",
     xlab = "year",
     main = "Average Spring Temperatures: Albany, NY")
abline(stn12.mod)

# by decade
# station 1 model
stn1dc.mod <- lm(stn1dc$tav ~ stn1dc$Decade)
# assumptions
stn1dc.res <- rstandard(stn1dc.mod)
qqnorm(stn1dc.res)
qqline(stn1dc.res)
plot(stn1dc$Decade, stn1dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn1dc.mod)
plot(stn1dc$Decade, stn1dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn1dc.mod)

# station 2 model
stn2dc.mod <- lm(stn2dc$tav ~ stn2dc$Decade)
# assumptions
stn2dc.res <- rstandard(stn2dc.mod)
qqnorm(stn2dc.res)
qqline(stn2dc.res)
plot(stn2dc$Decade, stn2dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn2dc.mod)
plot(stn2dc$Decade, stn2dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn2dc.mod)

# station 3 model
stn3dc.mod <- lm(stn3dc$tav ~ stn3dc$Decade)
# assumptions
stn3dc.res <- rstandard(stn3dc.mod)
qqnorm(stn3dc.res)
qqline(stn3dc.res)
plot(stn3dc$Decade, stn3dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn3dc.mod)
plot(stn3dc$Decade, stn3dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn3dc.mod)

# station 4 model
stn4dc.mod <- lm(stn4dc$tav ~ stn4dc$Decade)
# assumptions
stn4dc.res <- rstandard(stn4dc.mod)
qqnorm(stn4dc.res)
qqline(stn4dc.res)
plot(stn4dc$Decade, stn4dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn4dc.mod)
plot(stn4dc$Decade, stn4dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn4dc.mod)

# station 5 model
stn5dc.mod <- lm(stn5dc$tav ~ stn5dc$Decade)
# assumptions
stn5dc.res <- rstandard(stn5dc.mod)
qqnorm(stn5dc.res)
qqline(stn5dc.res)
plot(stn5dc$Decade, stn5dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn5dc.mod)
plot(stn5dc$Decade, stn5dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn5dc.mod)

# station 6 model
stn6dc.mod <- lm(stn6dc$tav ~ stn6dc$Decade)
# assumptions
stn6dc.res <- rstandard(stn6dc.mod)
qqnorm(stn6dc.res)
qqline(stn6dc.res)
plot(stn6dc$Decade, stn6dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn6dc.mod)
plot(stn6dc$Decade, stn6dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn6dc.mod)

# station 7 model
stn7dc.mod <- lm(stn7dc$tav ~ stn7dc$Decade)
# assumptions
stn7dc.res <- rstandard(stn7dc.mod)
qqnorm(stn7dc.res)
qqline(stn7dc.res)
plot(stn7dc$Decade, stn7dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn7dc.mod)
plot(stn7dc$Decade, stn7dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn7dc.mod)

# station 8 model -- SIGNIFICANT!
stn8dc.mod <- lm(stn8dc$tav ~ stn8dc$Decade)
# assumptions
stn8dc.res <- rstandard(stn8dc.mod)
qqnorm(stn8dc.res)
qqline(stn8dc.res)
plot(stn8dc$Decade, stn8dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn8dc.mod)
plot(stn8dc$Decade, stn8dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn8dc.mod)

# station 9 model
stn9dc.mod <- lm(stn9dc$tav ~ stn9dc$Decade)
# assumptions
stn9dc.res <- rstandard(stn9dc.mod)
qqnorm(stn9dc.res)
qqline(stn9dc.res)
plot(stn9dc$Decade, stn9dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn9dc.mod)
plot(stn9dc$Decade, stn9dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn9dc.mod)

# station 10 model
stn10dc.mod <- lm(stn10dc$tav ~ stn10dc$Decade)
# assumptions
stn10dc.res <- rstandard(stn10dc.mod)
qqnorm(stn10dc.res)
qqline(stn10dc.res)
plot(stn10dc$Decade, stn10dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn10dc.mod)
plot(stn10dc$Decade, stn10dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn10dc.mod)

# station 11 model
stn11dc.mod <- lm(stn11dc$tav ~ stn11dc$Decade)
# assumptions
stn11dc.res <- rstandard(stn11dc.mod)
qqnorm(stn11dc.res)
qqline(stn11dc.res)
plot(stn11dc$Decade, stn11dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn11dc.mod)
plot(stn11dc$Decade, stn11dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn11dc.mod)

# station 12 model
stn12dc.mod <- lm(stn12dc$tav ~ stn12dc$Decade)
# assumptions
stn12dc.res <- rstandard(stn12dc.mod)
qqnorm(stn12dc.res)
qqline(stn12dc.res)
plot(stn12dc$Decade, stn12dc.res,
     xlab = "average temp",
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)
# regression
summary(stn12dc.mod)
plot(stn12dc$Decade, stn12dc$tav,
     ylab = "average temp",
     xlab = "decade")
abline(stn12dc.mod)

### EXTREME TEMPERATURES ----

# extreme temperatures by year ----
# MARCH 
# station 1 - Boonville 
plot(MarYear$year[MarYear$StationID=="USC00300785"], MarYear$ExtHi[MarYear$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Boonville, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00300785"], MarYear$ExtLo[MarYear$StationID=="USC00300785"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(MarYear$year[MarYear$StationID=="USC00301752"], MarYear$ExtHi[MarYear$StationID=="USC00301752"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Cooperstown, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00301752"], MarYear$ExtLo[MarYear$StationID=="USC00301752"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(MarYear$year[MarYear$StationID=="USC00304102"], MarYear$ExtHi[MarYear$StationID=="USC00304102"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Indian Lake, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00304102"], MarYear$ExtLo[MarYear$StationID=="USC00304102"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(MarYear$year[MarYear$StationID=="USC00304912"], MarYear$ExtHi[MarYear$StationID=="USC00304912"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Lowville, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00304912"], MarYear$ExtLo[MarYear$StationID=="USC00304912"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(MarYear$year[MarYear$StationID=="USC00306085"], MarYear$ExtHi[MarYear$StationID=="USC00306085"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Norwich, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00306085"], MarYear$ExtLo[MarYear$StationID=="USC00306085"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(MarYear$year[MarYear$StationID=="USC00306314"], MarYear$ExtHi[MarYear$StationID=="USC00306314"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Oswego, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00306314"], MarYear$ExtLo[MarYear$StationID=="USC00306314"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(MarYear$year[MarYear$StationID=="USC00309000"], MarYear$ExtHi[MarYear$StationID=="USC00309000"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Watertown, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USC00309000"], MarYear$ExtLo[MarYear$StationID=="USC00309000"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(MarYear$year[MarYear$StationID=="USW00014735"], MarYear$ExtHi[MarYear$StationID=="USW00014735"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Albany, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USW00014735"], MarYear$ExtLo[MarYear$StationID=="USW00014735"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(MarYear$year[MarYear$StationID=="USW00014750"], MarYear$ExtHi[MarYear$StationID=="USW00014750"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Glens Falls, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USW00014750"], MarYear$ExtLo[MarYear$StationID=="USW00014750"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(MarYear$year[MarYear$StationID=="USW00014771"], MarYear$ExtHi[MarYear$StationID=="USW00014771"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Syracuse, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USW00014771"], MarYear$ExtLo[MarYear$StationID=="USW00014771"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(MarYear$year[MarYear$StationID=="USW00094725"], MarYear$ExtHi[MarYear$StationID=="USW00094725"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Massena, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USW00094725"], MarYear$ExtLo[MarYear$StationID=="USW00094725"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(MarYear$year[MarYear$StationID=="USW00094790"], MarYear$ExtHi[MarYear$StationID=="USW00094790"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Watertown Airport, NY",
     ylim = c(-30, 30))
lines(MarYear$year[MarYear$StationID=="USW00094790"], MarYear$ExtLo[MarYear$StationID=="USW00094790"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# APRIL
# station 1 - Boonville 
plot(AprYear$year[AprYear$StationID=="USC00300785"], AprYear$ExtHi[AprYear$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Boonville, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00300785"], AprYear$ExtLo[AprYear$StationID=="USC00300785"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(AprYear$year[AprYear$StationID=="USC00301752"], AprYear$ExtHi[AprYear$StationID=="USC00301752"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Cooperstown, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00301752"], AprYear$ExtLo[AprYear$StationID=="USC00301752"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(AprYear$year[AprYear$StationID=="USC00304102"], AprYear$ExtHi[AprYear$StationID=="USC00304102"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Indian Lake, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00304102"], AprYear$ExtLo[AprYear$StationID=="USC00304102"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(AprYear$year[AprYear$StationID=="USC00304912"], AprYear$ExtHi[AprYear$StationID=="USC00304912"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Lowville, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00304912"], AprYear$ExtLo[AprYear$StationID=="USC00304912"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(AprYear$year[AprYear$StationID=="USC00306085"], AprYear$ExtHi[AprYear$StationID=="USC00306085"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Norwich, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00306085"], AprYear$ExtLo[AprYear$StationID=="USC00306085"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(AprYear$year[AprYear$StationID=="USC00306314"], AprYear$ExtHi[AprYear$StationID=="USC00306314"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Oswego, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00306314"], AprYear$ExtLo[AprYear$StationID=="USC00306314"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(AprYear$year[AprYear$StationID=="USC00309000"], AprYear$ExtHi[AprYear$StationID=="USC00309000"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Watertown, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USC00309000"], AprYear$ExtLo[AprYear$StationID=="USC00309000"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(AprYear$year[AprYear$StationID=="USW00014735"], AprYear$ExtHi[AprYear$StationID=="USW00014735"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Albany, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USW00014735"], AprYear$ExtLo[AprYear$StationID=="USW00014735"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(AprYear$year[AprYear$StationID=="USW00014750"], AprYear$ExtHi[AprYear$StationID=="USW00014750"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Glens Falls, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USW00014750"], AprYear$ExtLo[AprYear$StationID=="USW00014750"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(AprYear$year[AprYear$StationID=="USW00014771"], AprYear$ExtHi[AprYear$StationID=="USW00014771"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Syracuse, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USW00014771"], AprYear$ExtLo[AprYear$StationID=="USW00014771"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(AprYear$year[AprYear$StationID=="USW00094725"], AprYear$ExtHi[AprYear$StationID=="USW00094725"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Massena, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USW00094725"], AprYear$ExtLo[AprYear$StationID=="USW00094725"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(AprYear$year[AprYear$StationID=="USW00094790"], AprYear$ExtHi[AprYear$StationID=="USW00094790"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Watertown Airport, NY",
     ylim = c(-20, 30))
lines(AprYear$year[AprYear$StationID=="USW00094790"], AprYear$ExtLo[AprYear$StationID=="USW00094790"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# MAY
# station 1 - Boonville 
plot(MayYear$year[MayYear$StationID=="USC00300785"], MayYear$ExtHi[MayYear$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Boonville, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00300785"], MayYear$ExtLo[MayYear$StationID=="USC00300785"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(MayYear$year[MayYear$StationID=="USC00301752"], MayYear$ExtHi[MayYear$StationID=="USC00301752"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Cooperstown, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00301752"], MayYear$ExtLo[MayYear$StationID=="USC00301752"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(MayYear$year[MayYear$StationID=="USC00304102"], MayYear$ExtHi[MayYear$StationID=="USC00304102"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Indian Lake, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00304102"], MayYear$ExtLo[MayYear$StationID=="USC00304102"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(MayYear$year[MayYear$StationID=="USC00304912"], MayYear$ExtHi[MayYear$StationID=="USC00304912"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Lowville, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00304912"], MayYear$ExtLo[MayYear$StationID=="USC00304912"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(MayYear$year[MayYear$StationID=="USC00306085"], MayYear$ExtHi[MayYear$StationID=="USC00306085"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Norwich, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00306085"], MayYear$ExtLo[MayYear$StationID=="USC00306085"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(MayYear$year[MayYear$StationID=="USC00306314"], MayYear$ExtHi[MayYear$StationID=="USC00306314"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Oswego, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00306314"], MayYear$ExtLo[MayYear$StationID=="USC00306314"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(MayYear$year[MayYear$StationID=="USC00309000"], MayYear$ExtHi[MayYear$StationID=="USC00309000"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Watertown, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USC00309000"], MayYear$ExtLo[MayYear$StationID=="USC00309000"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(MayYear$year[MayYear$StationID=="USW00014735"], MayYear$ExtHi[MayYear$StationID=="USW00014735"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Albany, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USW00014735"], MayYear$ExtLo[MayYear$StationID=="USW00014735"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(MayYear$year[MayYear$StationID=="USW00014750"], MayYear$ExtHi[MayYear$StationID=="USW00014750"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Glens Falls, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USW00014750"], MayYear$ExtLo[MayYear$StationID=="USW00014750"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(MayYear$year[MayYear$StationID=="USW00014771"], MayYear$ExtHi[MayYear$StationID=="USW00014771"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Syracuse, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USW00014771"], MayYear$ExtLo[MayYear$StationID=="USW00014771"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(MayYear$year[MayYear$StationID=="USW00094725"], MayYear$ExtHi[MayYear$StationID=="USW00094725"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Massena, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USW00094725"], MayYear$ExtLo[MayYear$StationID=="USW00094725"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(MayYear$year[MayYear$StationID=="USW00094790"], MayYear$ExtHi[MayYear$StationID=="USW00094790"],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Watertown Airport, NY",
     ylim = c(-10, 35))
lines(MayYear$year[MayYear$StationID=="USW00094790"], MayYear$ExtLo[MayYear$StationID=="USW00094790"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# extreme temperatures by decade ---- 
# MARCH 
# station 1 - Boonville 
# could points be better than lines?
plot(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtHi[MarDecade$StationID=="USC00300785"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Boonville, NY",
     ylim = c(-20,20))
points(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtLo[MarDecade$StationID=="USC00300785"],
      col = "skyblue", pch = 19)
lines(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtLo[MarDecade$StationID=="USC00300785"],
       col = "skyblue") 
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(MarDecade$Decade[MarDecade$StationID=="USC00301752"], MarDecade$ExtHi[MarDecade$StationID=="USC00301752"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Cooperstown, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USC00301752"], MarDecade$ExtLo[MarDecade$StationID=="USC00301752"],
      col = "skyblue", pch = 19)
lines(MarDecade$Decade[MarDecade$StationID=="USC00301752"], MarDecade$ExtLo[MarDecade$StationID=="USC00301752"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(MarDecade$Decade[MarDecade$StationID=="USC00304102"], MarDecade$ExtHi[MarDecade$StationID=="USC00304102"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Indian Lake, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00304102"], MarDecade$ExtLo[MarDecade$StationID=="USC00304102"],
      col = "skyblue")
points(MarDecade$Decade[MarDecade$StationID=="USC00304102"], MarDecade$ExtLo[MarDecade$StationID=="USC00304102"],
      col = "skyblue", pch = 19)  
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(MarDecade$Decade[MarDecade$StationID=="USC00304912"], MarDecade$ExtHi[MarDecade$StationID=="USC00304912"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Lowville, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USC00304912"], MarDecade$ExtLo[MarDecade$StationID=="USC00304912"],
      col = "skyblue", pch = 19) 
lines(MarDecade$Decade[MarDecade$StationID=="USC00304912"], MarDecade$ExtLo[MarDecade$StationID=="USC00304912"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(MarDecade$Decade[MarDecade$StationID=="USC00306085"], MarDecade$ExtHi[MarDecade$StationID=="USC00306085"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Norwich, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USC00306085"], MarDecade$ExtLo[MarDecade$StationID=="USC00306085"],
      col = "skyblue", pch = 19) 
lines(MarDecade$Decade[MarDecade$StationID=="USC00306085"], MarDecade$ExtLo[MarDecade$StationID=="USC00306085"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(MarDecade$Decade[MarDecade$StationID=="USC00306314"], MarDecade$ExtHi[MarDecade$StationID=="USC00306314"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Oswego, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USC00306314"], MarDecade$ExtLo[MarDecade$StationID=="USC00306314"],
      col = "skyblue", pch = 19) 
lines(MarDecade$Decade[MarDecade$StationID=="USC00306314"], MarDecade$ExtLo[MarDecade$StationID=="USC00306314"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(MarDecade$Decade[MarDecade$StationID=="USC00309000"], MarDecade$ExtHi[MarDecade$StationID=="USC00309000"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Watertown, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USC00309000"], MarDecade$ExtLo[MarDecade$StationID=="USC00309000"],
      col = "skyblue", pch = 19) 
lines(MarDecade$Decade[MarDecade$StationID=="USC00309000"], MarDecade$ExtLo[MarDecade$StationID=="USC00309000"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(MarDecade$Decade[MarDecade$StationID=="USW00014735"], MarDecade$ExtHi[MarDecade$StationID=="USW00014735"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Albany, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USW00014735"], MarDecade$ExtLo[MarDecade$StationID=="USW00014735"],
      col = "skyblue", pch = 19)  
lines(MarDecade$Decade[MarDecade$StationID=="USW00014735"], MarDecade$ExtLo[MarDecade$StationID=="USW00014735"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(MarDecade$Decade[MarDecade$StationID=="USW00014750"], MarDecade$ExtHi[MarDecade$StationID=="USW00014750"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Glens Falls, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USW00014750"], MarDecade$ExtLo[MarDecade$StationID=="USW00014750"],
      col = "skyblue", pch = 19)  
lines(MarDecade$Decade[MarDecade$StationID=="USW00014750"], MarDecade$ExtLo[MarDecade$StationID=="USW00014750"],
      col = "skyblue")     
legend("topright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(MarDecade$Decade[MarDecade$StationID=="USW00014771"], MarDecade$ExtHi[MarDecade$StationID=="USW00014771"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Syracuse, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USW00014771"], MarDecade$ExtLo[MarDecade$StationID=="USW00014771"],
      col = "skyblue", pch = 19) 
lines(MarDecade$Decade[MarDecade$StationID=="USW00014771"], MarDecade$ExtLo[MarDecade$StationID=="USW00014771"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(MarDecade$Decade[MarDecade$StationID=="USW00094725"], MarDecade$ExtHi[MarDecade$StationID=="USW00094725"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Massena, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USW00094725"], MarDecade$ExtLo[MarDecade$StationID=="USW00094725"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(MarDecade$Decade[MarDecade$StationID=="USW00094790"], MarDecade$ExtHi[MarDecade$StationID=="USW00094790"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Watertown Airport, NY",
     ylim = c(-30, 30))
points(MarDecade$Decade[MarDecade$StationID=="USW00094790"], MarDecade$ExtLo[MarDecade$StationID=="USW00094790"],
      col = "skyblue", pch = 19) 
lines(MarDecade$Decade[MarDecade$StationID=="USW00094790"], MarDecade$ExtLo[MarDecade$StationID=="USW00094790"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# APRIL
# station 1 - Boonville 
plot(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtHi[AprDecade$StationID=="USC00300785"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Boonville, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtLo[AprDecade$StationID=="USC00300785"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtLo[AprDecade$StationID=="USC00300785"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(AprDecade$Decade[AprDecade$StationID=="USC00301752"], AprDecade$ExtHi[AprDecade$StationID=="USC00301752"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Cooperstown, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00301752"], AprDecade$ExtLo[AprDecade$StationID=="USC00301752"],
      col = "skyblue", pch = 19)  
lines(AprDecade$Decade[AprDecade$StationID=="USC00301752"], AprDecade$ExtLo[AprDecade$StationID=="USC00301752"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(AprDecade$Decade[AprDecade$StationID=="USC00304102"], AprDecade$ExtHi[AprDecade$StationID=="USC00304102"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Indian Lake, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00304102"], AprDecade$ExtLo[AprDecade$StationID=="USC00304102"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USC00304102"], AprDecade$ExtLo[AprDecade$StationID=="USC00304102"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(AprDecade$Decade[AprDecade$StationID=="USC00304912"], AprDecade$ExtHi[AprDecade$StationID=="USC00304912"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Lowville, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00304912"], AprDecade$ExtLo[AprDecade$StationID=="USC00304912"],
      col = "skyblue", pch = 19)   
lines(AprDecade$Decade[AprDecade$StationID=="USC00304912"], AprDecade$ExtLo[AprDecade$StationID=="USC00304912"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(AprDecade$Decade[AprDecade$StationID=="USC00306085"], AprDecade$ExtHi[AprDecade$StationID=="USC00306085"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Norwich, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00306085"], AprDecade$ExtLo[AprDecade$StationID=="USC00306085"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USC00306085"], AprDecade$ExtLo[AprDecade$StationID=="USC00306085"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(AprDecade$Decade[AprDecade$StationID=="USC00306314"], AprDecade$ExtHi[AprDecade$StationID=="USC00306314"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Oswego, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00306314"], AprDecade$ExtLo[AprDecade$StationID=="USC00306314"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USC00306314"], AprDecade$ExtLo[AprDecade$StationID=="USC00306314"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(AprDecade$Decade[AprDecade$StationID=="USC00309000"], AprDecade$ExtHi[AprDecade$StationID=="USC00309000"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Watertown, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USC00309000"], AprDecade$ExtLo[AprDecade$StationID=="USC00309000"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USC00309000"], AprDecade$ExtLo[AprDecade$StationID=="USC00309000"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(AprDecade$Decade[AprDecade$StationID=="USW00014735"], AprDecade$ExtHi[AprDecade$StationID=="USW00014735"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Albany, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USW00014735"], AprDecade$ExtLo[AprDecade$StationID=="USW00014735"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USW00014735"], AprDecade$ExtLo[AprDecade$StationID=="USW00014735"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(AprDecade$Decade[AprDecade$StationID=="USW00014750"], AprDecade$ExtHi[AprDecade$StationID=="USW00014750"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Glens Falls, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USW00014750"], AprDecade$ExtLo[AprDecade$StationID=="USW00014750"],
      col = "skyblue", pch = 19)
lines(AprDecade$Decade[AprDecade$StationID=="USW00014750"], AprDecade$ExtLo[AprDecade$StationID=="USW00014750"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(AprDecade$Decade[AprDecade$StationID=="USW00014771"], AprDecade$ExtHi[AprDecade$StationID=="USW00014771"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Syracuse, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USW00014771"], AprDecade$ExtLo[AprDecade$StationID=="USW00014771"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USW00014771"], AprDecade$ExtLo[AprDecade$StationID=="USW00014771"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(AprDecade$Decade[AprDecade$StationID=="USW00094725"], AprDecade$ExtHi[AprDecade$StationID=="USW00094725"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Massena, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USW00094725"], AprDecade$ExtLo[AprDecade$StationID=="USW00094725"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USW00094725"], AprDecade$ExtLo[AprDecade$StationID=="USW00094725"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(AprDecade$Decade[AprDecade$StationID=="USW00094790"], AprDecade$ExtHi[AprDecade$StationID=="USW00094790"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Watertown Airport, NY",
     ylim = c(-20, 30))
points(AprDecade$Decade[AprDecade$StationID=="USW00094790"], AprDecade$ExtLo[AprDecade$StationID=="USW00094790"],
      col = "skyblue", pch = 19) 
lines(AprDecade$Decade[AprDecade$StationID=="USW00094790"], AprDecade$ExtLo[AprDecade$StationID=="USW00094790"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# MAY
# station 1 - Boonville 
plot(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtHi[MayDecade$StationID=="USC00300785"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Boonville, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtLo[MayDecade$StationID=="USC00300785"],
      col = "skyblue", pch = 19)
lines(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtLo[MayDecade$StationID=="USC00300785"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(MayDecade$Decade[MayDecade$StationID=="USC00301752"], MayDecade$ExtHi[MayDecade$StationID=="USC00301752"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Cooperstown, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00301752"], MayDecade$ExtLo[MayDecade$StationID=="USC00301752"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USC00301752"], MayDecade$ExtLo[MayDecade$StationID=="USC00301752"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(MayDecade$Decade[MayDecade$StationID=="USC00304102"], MayDecade$ExtHi[MayDecade$StationID=="USC00304102"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Indian Lake, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00304102"], MayDecade$ExtLo[MayDecade$StationID=="USC00304102"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USC00304102"], MayDecade$ExtLo[MayDecade$StationID=="USC00304102"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(MayDecade$Decade[MayDecade$StationID=="USC00304912"], MayDecade$ExtHi[MayDecade$StationID=="USC00304912"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Lowville, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00304912"], MayDecade$ExtLo[MayDecade$StationID=="USC00304912"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USC00304912"], MayDecade$ExtLo[MayDecade$StationID=="USC00304912"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(MayDecade$Decade[MayDecade$StationID=="USC00306085"], MayDecade$ExtHi[MayDecade$StationID=="USC00306085"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Norwich, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00306085"], MayDecade$ExtLo[MayDecade$StationID=="USC00306085"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USC00306085"], MayDecade$ExtLo[MayDecade$StationID=="USC00306085"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(MayDecade$Decade[MayDecade$StationID=="USC00306314"], MayDecade$ExtHi[MayDecade$StationID=="USC00306314"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Oswego, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00306314"], MayDecade$ExtLo[MayDecade$StationID=="USC00306314"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USC00306314"], MayDecade$ExtLo[MayDecade$StationID=="USC00306314"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(MayDecade$Decade[MayDecade$StationID=="USC00309000"], MayDecade$ExtHi[MayDecade$StationID=="USC00309000"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Watertown, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USC00309000"], MayDecade$ExtLo[MayDecade$StationID=="USC00309000"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USC00309000"], MayDecade$ExtLo[MayDecade$StationID=="USC00309000"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(MayDecade$Decade[MayDecade$StationID=="USW00014735"], MayDecade$ExtHi[MayDecade$StationID=="USW00014735"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Albany, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USW00014735"], MayDecade$ExtLo[MayDecade$StationID=="USW00014735"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USW00014735"], MayDecade$ExtLo[MayDecade$StationID=="USW00014735"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(MayDecade$Decade[MayDecade$StationID=="USW00014750"], MayDecade$ExtHi[MayDecade$StationID=="USW00014750"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Glens Falls, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USW00014750"], MayDecade$ExtLo[MayDecade$StationID=="USW00014750"],
      col = "skyblue", pch = 19)   
lines(MayDecade$Decade[MayDecade$StationID=="USW00014750"], MayDecade$ExtLo[MayDecade$StationID=="USW00014750"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(MayDecade$Decade[MayDecade$StationID=="USW00014771"], MayDecade$ExtHi[MayDecade$StationID=="USW00014771"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Syracuse, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USW00014771"], MayDecade$ExtLo[MayDecade$StationID=="USW00014771"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USW00014771"], MayDecade$ExtLo[MayDecade$StationID=="USW00014771"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(MayDecade$Decade[MayDecade$StationID=="USW00094725"], MayDecade$ExtHi[MayDecade$StationID=="USW00094725"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Massena, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USW00094725"], MayDecade$ExtLo[MayDecade$StationID=="USW00094725"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USW00094725"], MayDecade$ExtLo[MayDecade$StationID=="USW00094725"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(MayDecade$Decade[MayDecade$StationID=="USW00094790"], MayDecade$ExtHi[MayDecade$StationID=="USW00094790"],
     type = "o",
     pch = 19,
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Watertown Airport, NY",
     ylim = c(-10, 35))
points(MayDecade$Decade[MayDecade$StationID=="USW00094790"], MayDecade$ExtLo[MayDecade$StationID=="USW00094790"],
      col = "skyblue", pch = 19) 
lines(MayDecade$Decade[MayDecade$StationID=="USW00094790"], MayDecade$ExtLo[MayDecade$StationID=="USW00094790"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# all months on same graph -- BOONVILLE EXAMPLE
plot(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtHi[MarDecade$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Boonville, NY",
     ylim = c(-30,30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtLo[MarDecade$StationID=="USC00300785"],
      col = "skyblue") 
lines(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtHi[AprDecade$StationID=="USC00300785"],
      col = "tomato3", lty = "dashed")
lines(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtLo[AprDecade$StationID=="USC00300785"],
      col = "skyblue", lty = "dashed")
lines(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtHi[MayDecade$StationID=="USC00300785"],
      col = "tomato3", lty = "dotted")
lines(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtLo[MayDecade$StationID=="USC00300785"],
      col = "skyblue", lty = "dotted")
legend("bottomright", c("May Hi", "April Hi", "March Hi", "May Lo", "April Lo", "March Lo"), col = c("tomato3", "tomato3", "tomato3","skyblue","skyblue","skyblue"), lty = 3:1, bty="n", cex=.75)


# number of extreme days by year ----

# boonville 
# MARCH
MarYear$ExHiCount <- as.vector(MarYear$ExHiCount)
MarYear$ExLoCount <- as.vector(MarYear$ExLoCount)
# station 1
plot(MarYear$year[MarYear$StationID=="USC00300785"], MarYear$ExHiCount[MarYear$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00300785"], MarYear$ExLoCount[MarYear$StationID=="USC00300785"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# APRIL
AprYear$ExHiCount <- as.vector(AprYear$ExHiCount)
AprYear$ExLoCount <- as.vector(AprYear$ExLoCount)
# station 1
plot(AprYear$year[AprYear$StationID=="USC00300785"], AprYear$ExHiCount[AprYear$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00300785"], AprYear$ExLoCount[AprYear$StationID=="USC00300785"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# MAY
MayYear$ExHiCount <- as.vector(MayYear$ExHiCount)
MayYear$ExLoCount <- as.vector(MayYear$ExLoCount)
#station 1
plot(MayYear$year[MayYear$StationID=="USC00300785"], MayYear$ExHiCount[MayYear$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00300785"], MayYear$ExLoCount[MayYear$StationID=="USC00300785"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# cooperstown
# march
plot(MarYear$year[MarYear$StationID=="USC00301752"], MarYear$ExHiCount[MarYear$StationID=="USC00301752"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Cooperstown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00301752"], MarYear$ExLoCount[MarYear$StationID=="USC00301752"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Cooperstown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USC00301752"], AprYear$ExHiCount[AprYear$StationID=="USC00301752"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Cooperstown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00301752"], AprYear$ExLoCount[AprYear$StationID=="USC00301752"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Cooperstown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USC00301752"], MayYear$ExHiCount[MayYear$StationID=="USC00301752"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Cooperstown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00301752"], MayYear$ExLoCount[MayYear$StationID=="USC00301752"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Cooperstown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# indian lake
# march
plot(MarYear$year[MarYear$StationID=="USC00304102"], MarYear$ExHiCount[MarYear$StationID=="USC00304102"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Indian Lake, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00304102"], MarYear$ExLoCount[MarYear$StationID=="USC00304102"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Indian Lake, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USC00304102"], AprYear$ExHiCount[AprYear$StationID=="USC00304102"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Indian Lake, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00304102"], AprYear$ExLoCount[AprYear$StationID=="USC00304102"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Indian Lake, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USC00304102"], MayYear$ExHiCount[MayYear$StationID=="USC00304102"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Indian Lake, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00304102"], MayYear$ExLoCount[MayYear$StationID=="USC00304102"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Indian Lake, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# lowville 
# march
plot(MarYear$year[MarYear$StationID=="USC00304912"], MarYear$ExHiCount[MarYear$StationID=="USC00304912"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Lowville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00304912"], MarYear$ExLoCount[MarYear$StationID=="USC00304912"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Lowville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USC00304912"], AprYear$ExHiCount[AprYear$StationID=="USC00304912"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Lowville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00304912"], AprYear$ExLoCount[AprYear$StationID=="USC00304912"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Lowville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USC00304912"], MayYear$ExHiCount[MayYear$StationID=="USC00304912"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Lowville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00304912"], MayYear$ExLoCount[MayYear$StationID=="USC00304912"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Lowville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# norwich
# march
plot(MarYear$year[MarYear$StationID=="USC00306085"], MarYear$ExHiCount[MarYear$StationID=="USC00306085"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Norwich, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00306085"], MarYear$ExLoCount[MarYear$StationID=="USC00306085"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Norwich, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USC00306085"], AprYear$ExHiCount[AprYear$StationID=="USC00306085"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Norwich, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00306085"], AprYear$ExLoCount[AprYear$StationID=="USC00306085"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Norwich, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USC00306085"], MayYear$ExHiCount[MayYear$StationID=="USC00306085"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Norwich, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00306085"], MayYear$ExLoCount[MayYear$StationID=="USC00306085"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Norwich, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# oswego
# march
plot(MarYear$year[MarYear$StationID=="USC00306314"], MarYear$ExHiCount[MarYear$StationID=="USC00306314"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Oswego, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00306314"], MarYear$ExLoCount[MarYear$StationID=="USC00306314"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Oswego, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USC00306314"], AprYear$ExHiCount[AprYear$StationID=="USC00306314"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Oswego, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00306314"], AprYear$ExLoCount[AprYear$StationID=="USC00306314"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Oswego, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USC00306314"], MayYear$ExHiCount[MayYear$StationID=="USC00306314"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Oswego, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00306314"], MayYear$ExLoCount[MayYear$StationID=="USC00306314"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Oswego, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# Watertown
# march
plot(MarYear$year[MarYear$StationID=="USC00309000"], MarYear$ExHiCount[MarYear$StationID=="USC00309000"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Watertown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USC00309000"], MarYear$ExLoCount[MarYear$StationID=="USC00309000"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Watertown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USC00309000"], AprYear$ExHiCount[AprYear$StationID=="USC00309000"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Watertown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USC00309000"], AprYear$ExLoCount[AprYear$StationID=="USC00309000"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Watertown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USC00309000"], MayYear$ExHiCount[MayYear$StationID=="USC00309000"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Watertown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USC00309000"], MayYear$ExLoCount[MayYear$StationID=="USC00309000"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Watertown, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# albany
# march
plot(MarYear$year[MarYear$StationID=="USW00014735"], MarYear$ExHiCount[MarYear$StationID=="USW00014735"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Albany, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USW00014735"], MarYear$ExLoCount[MarYear$StationID=="USW00014735"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Albany, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USW00014735"], AprYear$ExHiCount[AprYear$StationID=="USW00014735"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Albany, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USW00014735"], AprYear$ExLoCount[AprYear$StationID=="USW00014735"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Albany, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USW00014735"], MayYear$ExHiCount[MayYear$StationID=="USW00014735"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Albany, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USW00014735"], MayYear$ExLoCount[MayYear$StationID=="USW00014735"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Albany, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# glens falls
# march
plot(MarYear$year[MarYear$StationID=="USW00014750"], MarYear$ExHiCount[MarYear$StationID=="USW00014750"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Glens Falls, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USW00014750"], MarYear$ExLoCount[MarYear$StationID=="USW00014750"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Glens Falls, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USW00014750"], AprYear$ExHiCount[AprYear$StationID=="USW00014750"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Glens Falls, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USW00014750"], AprYear$ExLoCount[AprYear$StationID=="USW00014750"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Glens Falls, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USW00014750"], MayYear$ExHiCount[MayYear$StationID=="USW00014750"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Glens Falls, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USW00014750"], MayYear$ExLoCount[MayYear$StationID=="USW00014750"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Glens Falls, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# syracuse
# march
plot(MarYear$year[MarYear$StationID=="USW00014771"], MarYear$ExHiCount[MarYear$StationID=="USW00014771"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Syracuse, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USW00014771"], MarYear$ExLoCount[MarYear$StationID=="USW00014771"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Syracuse, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USW00014771"], AprYear$ExHiCount[AprYear$StationID=="USW00014771"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Syracuse, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USW00014771"], AprYear$ExLoCount[AprYear$StationID=="USW00014771"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Syracuse, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USW00014771"], MayYear$ExHiCount[MayYear$StationID=="USW00014771"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Syracuse, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USW00014771"], MayYear$ExLoCount[MayYear$StationID=="USW00014771"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Syracuse, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))

# massena
# march
plot(MarYear$year[MarYear$StationID=="USW00094725"], MarYear$ExHiCount[MarYear$StationID=="USW00094725"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Massena, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USW00094725"], MarYear$ExLoCount[MarYear$StationID=="USW00094725"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Massena, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USW00094725"], AprYear$ExHiCount[AprYear$StationID=="USW00094725"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Massena, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USW00094725"], AprYear$ExLoCount[AprYear$StationID=="USW00094725"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Massena, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USW00094725"], MayYear$ExHiCount[MayYear$StationID=="USW00094725"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Massena, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USW00094725"], MayYear$ExLoCount[MayYear$StationID=="USW00094725"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Massena, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))


# Watertown airport
# march
plot(MarYear$year[MarYear$StationID=="USW00094790"], MarYear$ExHiCount[MarYear$StationID=="USW00094790"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Watertown Airport, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MarYear$year[MarYear$StationID=="USW00094790"], MarYear$ExLoCount[MarYear$StationID=="USW00094790"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Watertown Airport, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# april
plot(AprYear$year[AprYear$StationID=="USW00094790"], AprYear$ExHiCount[AprYear$StationID=="USW00094790"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Watertown Airport, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(AprYear$year[AprYear$StationID=="USW00094790"], AprYear$ExLoCount[AprYear$StationID=="USW00094790"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Watertown Airport, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
# may
plot(MayYear$year[MayYear$StationID=="USW00094790"], MayYear$ExHiCount[MayYear$StationID=="USW00094790"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Watertown Airport, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))
plot(MayYear$year[MayYear$StationID=="USW00094790"], MayYear$ExLoCount[MayYear$StationID=="USW00094790"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Watertown Airport, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0,16))


# number of extreme days by decade ----
# only boonville right now

# MARCH
MarDecade$ExHiCount <- as.vector(MarDecade$ExHiCount)
MarDecade$ExLoCount <- as.vector(MarDecade$ExLoCount)
# station 1
plot(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExHiCount[MarDecade$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
plot(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExLoCount[MarDecade$StationID=="USC00300785"], 
     type = "h",
     col = "skyblue",
     main = "March Extreme Low Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))

# APRIL
AprDecade$ExHiCount <- as.vector(AprDecade$ExHiCount)
AprDecade$ExLoCount <- as.vector(AprDecade$ExLoCount)
# station 1
plot(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExHiCount[AprDecade$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
plot(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExLoCount[AprDecade$StationID=="USC00300785"], 
     type = "h",
     col = "skyblue",
     main = "April Extreme Low Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,50))

# MAY
MayDecade$ExHiCount <- as.vector(MayDecade$ExHiCount)
MayDecade$ExLoCount <- as.vector(MayDecade$ExLoCount)
#station 1
plot(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExHiCount[MayDecade$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
plot(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExLoCount[MayDecade$StationID=="USC00300785"], 
     type = "h",
     col = "skyblue",
     main = "May Extreme Low Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,50))

# Decadal Trends in Extreme Temps by Month
# create scatter plot march high temperatures
MarchHiAv <- mean(AllData$HiTmax[AllData$Month == "Mar"], na.rm = TRUE)
ggplot(data = MarDecade, aes(x = Decade, y = ExtHi, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MarchHiAv)+
  theme_classic()+
  labs(title = "March Extreme High Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# scatter plot march low temperatures
MarchLoAv <- mean(AllData$LoTmin[AllData$Month == "Mar"], na.rm = TRUE)
ggplot(data = MarDecade, aes(x = Decade, y = ExtLo, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MarchLoAv)+
  theme_classic()+
  labs(title = "March Extreme Low Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# create scatter plot april high temperatures
AprilHiAv <- mean(AllData$HiTmax[AllData$Month == "Apr"], na.rm = TRUE)
ggplot(data = AprDecade, aes(x = Decade, y = ExtHi, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = AprilHiAv)+
  theme_classic()+
  labs(title = "April Extreme High Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# scatter plot april low temperatures
AprilLoAv <- mean(AllData$LoTmin[AllData$Month == "Apr"], na.rm = TRUE)
ggplot(data = AprDecade, aes(x = Decade, y = ExtLo, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = AprilLoAv)+
  theme_classic()+
  labs(title = "April Extreme Low Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# create scatter plot may high temperatures
MayHiAv <- mean(AllData$HiTmax[AllData$Month == "May"], na.rm = TRUE)
ggplot(data = MayDecade, aes(x = Decade, y = ExtHi, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MayHiAv)+
  theme_classic()+
  labs(title = "May Extreme High Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# scatter plot march low temperatures
MayLoAv <- mean(AllData$LoTmin[AllData$Month == "May"], na.rm = TRUE)
ggplot(data = MayDecade, aes(x = Decade, y = ExtLo, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MayLoAv)+
  theme_classic()+
  labs(title = "May Extreme Low Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

### freeze thaw ----
# Number of Freeze Thaw Days Graphs
# have 20 as differentiating mark but we could look up how many in one year is problematic and use that as a threshold

# station 1
ggplot(data = stn1, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn1$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Boonville, NY")

# station 2
ggplot(data = stn2, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn2$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Cooperstown, NY")

# station 3
ggplot(data = stn3, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn3$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Indian Lake, NY")

# station 4
ggplot(data = stn4, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn4$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Lowville, NY")

# station 5
ggplot(data = stn5, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn5$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Norwich, NY")

# station 6
ggplot(data = stn6, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn6$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Oswego, NY")

# station 7
ggplot(data = stn7, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn7$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Watertown, NY")

# station 8
ggplot(data = stn8, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn8$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Albany, NY")

# station 9
ggplot(data = stn9, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn9$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Glens Falls, NY")

# station 10
ggplot(data = stn10, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn10$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Syracuse, NY")

# station 11
ggplot(data = stn11, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn11$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Massena, NY")

# station 12
ggplot(data = stn12, aes(x = year, y = FTdays)) +
  geom_bar(position = "dodge", stat="identity", fill = ifelse(stn12$FTdays > 20, "tomato3", "deepskyblue3"))+
  theme_classic()+
  labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Watertown Airport, NY")


# Freeze Thaw Amplitude Graphs
### first three graphs show three options for visualizing this
### add standard deviation as error bar on scatter plot by decade

# station 1
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00300785",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Boonville, NY")

# station 2
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00301752",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Cooperstown Airport, NY")

# station 3
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00304102",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Indian Lake, NY")

# station 4
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00304912",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Lowville, NY")

# station 5
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00306085",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Norwich, NY")

# station 6
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00306314",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Oswego, NY")

# station 7 
ggplot(data = SpringDecade[SpringDecade$StationID == "USC00309000",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Watertown, NY")

# station 8
ggplot(data = SpringDecade[SpringDecade$StationID == "USW00014735",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Albany, NY")

# station 9
ggplot(data = SpringDecade[SpringDecade$StationID == "USW00014750",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Glens Falls, NY")

# station 10
ggplot(data = SpringDecade[SpringDecade$StationID == "USW00014771",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Syracuse, NY")

# station 11
ggplot(data = SpringDecade[SpringDecade$StationID == "USW00094725",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Massena, NY")

# station 12
ggplot(data = SpringDecade[SpringDecade$StationID == "USW00094790",], aes(x = Decade, y = FTrange, color = Month))+
  geom_point() +
  geom_line() +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Watertown Airport, NY")

### Heat Maps

# Station 8
# raw temps
stn8all <- subset(SpringData, SpringData$StationID == "USW00014735")
ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#4393c3",
                       mid = "#d1e5f0",
                       high = "#d6604d",
                       na.value = "white")

# standardized anomalies
ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white")

# raw anomalies
ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4393c3",
                       mid = "#d1e5f0",
                       high = "#d6604d",
                       na.value = "white")





