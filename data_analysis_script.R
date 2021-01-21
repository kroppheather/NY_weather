library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(sp)
library(ggplot2)
library(zoo)
library(jpeg)


### Set up directories   -----

# Creating user numbers for each person
Users = c(1, # Abby
          2, # Professor Kropp
          3) # Rachel

# Creating a directory with all of our file paths 
# to pull data
diru = c("/Users/abby/Documents/NYweather",
         "/Users/hkropp/Google Drive/research/students/NYweather/data",
         "/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/data")
# to save plots
plotDIR = c("/Users/abby/Documents/NYweather/plots", 
            "/Users/hkropp/Google Drive/research/students/NYweather/plots", 
            "/Users/rachelpike/Desktop/2020-2021/Research/plots")

plotDIR = c("/Users/abby/Documents/NYweather/plots", 
            "/Users/hkropp/Google Drive/research/students/NYweather/plots", 
            "/Users/rachelpike/Desktop/2020-2021/Research/plots")

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
# Remove years with too much missing data 
TmaxData <- inner_join(TmaxData, TmaxDataYear, by = c("id" = "station", "year"="year"))

# Getting rid of years with less than 171 observations in TmaxData
TmaxData <- inner_join(TmaxData, TmaxDataYear, by = c("id" = "station", "year" = "year"))

# Counting observations per year for the tmin data
# Make new data frame with just the id, tmin value, and year
TminDataYear <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="length")

# Changing column names
colnames(TminDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
TminDataYear <- subset(TminDataYear, TminDataYear$ncount >= 171)
# get rid of years with missing data from TminData
TminData <- inner_join(TminData, TminDataYear, by = c("id" = "station", "year"="year"))

# Getting rid of years with less than 171 observations in TminData
TminData <- inner_join(TminData, TminDataYear, by = c("id" = "station", "year" = "year"))

# Counting observations per year for the prcp data
# Make new data frame with just the id, prcp value, and year
PrcpDataYear <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="length")

# Changing column names
colnames(PrcpDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
PrcpDataYear <- subset(PrcpDataYear, PrcpDataYear$ncount >= 171)
# get rid of years with missing data from PrcpData
PrcpData <- inner_join(PrcpData, PrcpDataYear, by = c("id" = "station", "year"="year"))

# Getting rid of years with less than 171 observations in PrcpData
PrcpData <- inner_join(PrcpData, PrcpDataYear, by = c("id" = "station", "year" = "year"))

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

# look at tmax stations
# make a map of all weather sites
plot(ez, col=ez@data$col, border=NA)
legend("topleft", paste(MajorZones$MAJOR),fill=MajorZones$col, bty="n", cex=0.35)
plot(siteP, add=TRUE, pch=19, col=rgb(0.5,0.5,0.5,0.45), cex=0.5)
# look at Tmax
TmaxStn$station_id <- TmaxStn$station
sitesMax <- left_join(TmaxStn,StationInfo, by ="station_id")
maxPoints <- SpatialPoints(matrix(c(sitesMax $long,sitesMax $lat), ncol=2,byrow=FALSE),
                           CRS( "+init=epsg:4326") )
#reproject points into the ez coordinate system (utm)
maxP <- spTransform(maxPoints ,ez@proj4string)
plot(maxP, col="grey25",pch=19, add=TRUE)
title(main= "Map of TMax Stations")


# now look at Tmin stations
# remake a map of all weather sites
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


# Now look at PRCP
# remake a map of all weather sites
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


### identify sites with all data types and create AllStn data frame
### do we need to keep T1 and T2? 
AllStnT1 <- data.frame(station_id = sitesMax$station_id, 
                       lat = sitesMax$lat, 
                       long = sitesMax$long, 
                       StationName = sitesMax$name,
                       StartTmax = sitesMax$min) 
sitesMin2 <- data.frame(station_id = sitesMin$station_id, StartTmin = sitesMin$min)
AllStnT2 <- inner_join(AllStnT1, sitesMin2, by="station_id")
sitesPrcp2 <- data.frame(station_id = sitesPrcp$station_id, StartPrcp = sitesPrcp$min)
AllStn <- inner_join(AllStnT2, sitesPrcp2, by="station_id")
AllStn$stnID <- seq(1, nrow(AllStn))
AllStn$name <- c("Boonville", "Cooperstown", "Indian Lake", "Lowville", "Norwich", "Oswego",
                 "Watertown", "Albany AP", "Glens Falls AP", "Syracuse AP", "Massena AP", "Watertown AP")



# map the stations that have all data types
# remake a map of all weather sites
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


### WEEK 1 ----
### Creating one large data frame ----

# get rid of T1 and T2 ?
# join tmax, tmin, and prcp data 
AllDataT1 <- full_join(TmaxData, TminData, by = c("id"="id", "date" = "date", "year"="year", "DOY" = "DOY"))
AllDataT2 <- full_join(AllDataT1, PrcpData, by = c("id"="id", "date" = "date", "year"="year", "DOY" = "DOY"))

# add station names of the 12 good stations
AllData <- inner_join(AllDataT2, AllStn, by = c("id"="station_id"))

# add month column
AllData$Month <- month(AllData$date, label = TRUE)

# add decade column
AllData$Decade <- AllData$year - (AllData$year %% 10)


# Subset to just keep id, tmin, tmax, year, doy
AllData <- data.frame(StationID = AllData$id, 
                      StationName = AllData$StationName,
                      Name = AllData$name,
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
AllData$FreezeThaw <- ifelse(AllData$tmin<(-2.2) & AllData$tmax>0, 1 , NA)


# Adding Freeze Thaw Flags for types of days
# Day Types: 1 = min/max < 0, 2 = min < 0 and max > 0, 3 = min/max > 0, 
AllData$DayType <- ifelse(AllData$tmin<0 & AllData$tmax<0, 1,
                          ifelse(AllData$tmin<=0 & AllData$tmax>=0, 2,
                                 ifelse(AllData$tmin>0 & AllData$tmax>0, 3, 0)))

# Adding range of freeze thaw column
AllData$FTrange <- ifelse(AllData$FreezeThaw == 1, AllData$tmax - AllData$tmin, NA)


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


# checking continuity of tav data
# filtering out tav with na
TavData <- AllData %>% drop_na(tav)
# count number of tav observations per year
TavCount <- aggregate(TavData$tav, by = list(TavData$StationID, TavData$StationName, TavData$Year), FUN = "length")
colnames(TavCount) <- c("StationID", "StationName", "Year", "ncount")

# identify years with less than 10 missing observations
TavCount <- subset(TavCount, TavCount$ncount >= 171)
# keep only years with enough tav data
TavData <- inner_join(TavData, TavCount, by = c("StationID", "StationName", "Year"))

# add thawing degree day accumulation to TavData
TavData <- TavData %>%
  group_by(Year, StationID) %>%
  arrange(DOY) %>%
  mutate(TDD = cumsum(ifelse(is.na(tav), 0, ifelse(tav >= 0, tav, 0))))

# add growing degree day accumulation (for apples)
TavData <- TavData %>%
  group_by(Year, StationID) %>%
  arrange(DOY) %>%
  mutate(GDD41 = cumsum(ifelse(is.na(tav), 0, ifelse(tav >= 5, (tav-5), 0))))


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
SpringMonths <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID, SpringData$StationName, SpringData$Name, SpringData$Month), FUN="mean", na.rm = TRUE)
colnames(SpringMonths) <- c("year","StationID","StationName", "Name", "month","tmax")
SpringMonths$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Name,SpringData$Month), FUN="mean", na.rm = TRUE)$x
SpringMonths$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Name,SpringData$Month), FUN="mean", na.rm = TRUE)$x
# add columns of extreme hi and lo temperature values
SpringMonths$ExtHi <- aggregate(SpringData$HiTmax, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name,SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringMonths$ExtLo <- aggregate(SpringData$LoTmin, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name,SpringData$Month), FUN = "mean", na.rm = TRUE)$x
# add columns counting extreme temp flags
SpringMonths$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name,SpringData$Name,SpringData$Month) , FUN = "sum", na.rm = TRUE)$x
SpringMonths$ExLoCount <- aggregate(SpringData$ExtrLo, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name,SpringData$Month), FUN = "sum", na.rm = TRUE)$x
# add columns with freeze thaw flags and range
SpringMonths$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name,SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringMonths$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name,SpringData$Month), FUN="mean", na.rm = TRUE)$x


# spring data frame with yearly averages - spring months averaged together
SpringYear <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID, SpringData$StationName,SpringData$Name), FUN="mean", na.rm = TRUE)
colnames(SpringYear) <- c("year","StationID","StationName","Name","tmax")
SpringYear$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Name), FUN="mean", na.rm = TRUE)$x
SpringYear$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Name), FUN="mean", na.rm = TRUE)$x
# add columns of extreme hi and lo temperature values
SpringYear$ExtHi <- aggregate(SpringData$HiTmax, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name), FUN = "mean", na.rm = TRUE)$x
SpringYear$ExtLo <- aggregate(SpringData$LoTmin, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name), FUN = "mean", na.rm = TRUE)$x
# add columns counting extreme temp flags
SpringYear$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name) , FUN = "sum", na.rm = TRUE)$x
SpringYear$ExLoCount <- aggregate(SpringData$ExtrLo, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name), FUN = "sum", na.rm = TRUE)$x
# add columns with freeze thaw flags and range
SpringYear$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name), FUN="sum", na.rm = TRUE)$x
SpringYear$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Name), FUN="mean", na.rm = TRUE)$x


# decade averages by month
SpringDecade<- aggregate(SpringData$tmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Name, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)
colnames(SpringDecade) <- c("StationID", "StationName", "Name", "Decade", "Month", "tmax")
SpringDecade$tmin <- aggregate(SpringData$tmin, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$tav <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExtHi <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID,  SpringData$Name,SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringDecade$ExLoCount <- aggregate(SpringData$ExtrLo,by=list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringDecade$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecade$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x

# decade averages with spring months averaged together
SpringDecadeAv <- aggregate(SpringData$tmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Name, SpringData$Decade), FUN = "mean", na.rm = TRUE)
colnames(SpringDecadeAv) <- c("StationID", "StationName","Name", "Decade", "tmax")
SpringDecadeAv$tmin <- aggregate(SpringData$tmin, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$tav <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExtHi <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$StationID, SpringData$Decade), FUN="sum", na.rm = TRUE)$x
SpringDecadeAv$ExLoCount <- aggregate(SpringData$ExtrLo,by=list(SpringData$StationID, SpringData$Decade), FUN="sum", na.rm = TRUE)$x
SpringDecadeAv$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecadeAv$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x


## Linear regressions for tav ----
RegressionTav <- data.frame(StationID=character(0), 
                            StationName=character(0), 
                            Slope = numeric(0), 
                            SlopeP = numeric(0), 
                            Int = numeric(0), 
                            IntP = numeric(0),
                            Rsq = numeric(0),
                            Sig = numeric(0))

for (i in 1:nrow(AllStn)){
  RegressionTav[i,1] <- AllStn$station_id[i]
  RegressionTav[i,2] <- AllStn$StationName[i]
  current.mod <- lm(SpringYear$tav[SpringYear$StationID == AllStn$station_id[i]] ~ SpringYear$year[SpringYear$StationID == AllStn$station_id[i]])
  current.res <- rstandard(current.mod)
  RegressionTav[i,3] <- summary(current.mod)$coefficients[2,1]
  RegressionTav[i,4] <- summary(current.mod)$coefficients[2,4]
  RegressionTav[i,5] <- summary(current.mod)$coefficients[1,1]
  RegressionTav[i,6] <- summary(current.mod)$coefficients[1,4]
  RegressionTav[i,7] <- summary(current.mod)$r.squared
  RegressionTav[i,8] <- ifelse(RegressionTav[i,4] <= 0.05 & RegressionTav[i,6] <= 0.05, 1, 0)
  
}

## Linear regressions for tmin ----
RegressionTmin <- data.frame(StationID=character(0), 
                             StationName=character(0), 
                             Slope = numeric(0), 
                             SlopeP = numeric(0), 
                             Int = numeric(0), 
                             IntP = numeric(0),
                             Rsq = numeric(0),
                             Sig = numeric(0))

for (i in 1:nrow(AllStn)){
  RegressionTmin[i,1] <- AllStn$station_id[i]
  RegressionTmin[i,2] <- AllStn$StationName[i]
  current.mod <- lm(SpringYear$tmin[SpringYear$StationID == AllStn$station_id[i]] ~ SpringYear$year[SpringYear$StationID == AllStn$station_id[i]])
  current.res <- rstandard(current.mod)
  RegressionTmin[i,3] <- summary(current.mod)$coefficients[2,1]
  RegressionTmin[i,4] <- summary(current.mod)$coefficients[2,4]
  RegressionTmin[i,5] <- summary(current.mod)$coefficients[1,1]
  RegressionTmin[i,6] <- summary(current.mod)$coefficients[1,4]
  RegressionTmin[i,7] <- summary(current.mod)$r.squared
  RegressionTmin[i,8] <- ifelse(RegressionTmin[i,4] <= 0.05 & RegressionTmin[i,6] <= 0.05, 1, 0)
  
  
}

## Linear regressions for tmax ----
RegressionTmax <- data.frame(StationID=character(0), 
                             StationName=character(0), 
                             Slope = numeric(0), 
                             SlopeP = numeric(0), 
                             Int = numeric(0), 
                             IntP = numeric(0),
                             Rsq = numeric(0),
                             Sig = numeric(0))

for (i in 1:nrow(AllStn)){
  RegressionTmax[i,1] <- AllStn$station_id[i]
  RegressionTmax[i,2] <- AllStn$StationName[i]
  current.mod <- lm(SpringYear$tmax[SpringYear$StationID == AllStn$station_id[i]] ~ SpringYear$year[SpringYear$StationID == AllStn$station_id[i]])
  current.res <- rstandard(current.mod)
  RegressionTmax[i,3] <- summary(current.mod)$coefficients[2,1]
  RegressionTmax[i,4] <- summary(current.mod)$coefficients[2,4]
  RegressionTmax[i,5] <- summary(current.mod)$coefficients[1,1]
  RegressionTmax[i,6] <- summary(current.mod)$coefficients[1,4]
  RegressionTmax[i,7] <- summary(current.mod)$r.squared
  RegressionTmax[i,8] <- ifelse(RegressionTmax[i,4] <= 0.05 & RegressionTmax[i,6] <= 0.05, 1, 0)
  
  
}


### General temperature trends ----
# plot general temperature trends
# creating for loop for plots
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringYear, SpringYear$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))
  
  # plot general temperature trends
  ggplot(data = current_data, aes(x = year))+
    geom_line(aes(y = tav, color = "Average"))+
    geom_abline(data = RegressionTav, aes(slope = Slope[i], intercept = Int[i]), color = alpha("slateblue3", 0.6), 
                linetype = ifelse(RegressionTav$Sig[i] == 1, "solid", "longdash"))+
    geom_line(aes(y = tmax, color = "Maximum"))+
    geom_abline(data = RegressionTmax, aes(slope = Slope[i], intercept = Int[i]), color = alpha("tomato4", 0.6),
                linetype = ifelse(RegressionTmax$Sig[i] == 1, "solid", "longdash"))+
    geom_line(aes(y = tmin, color = "Minimum"))+
    geom_abline(data = RegressionTmin, aes(slope = Slope[i], intercept = Int[i]), color = alpha("deepskyblue3", 0.6),
                linetype = ifelse(RegressionTmin$Sig[i] == 1, "solid", "longdash"))+
    scale_color_manual(values = c("slateblue1","tomato3","skyblue"), name = "Temperature Measurement")+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
    labs(x = "Year", y = "Temperature (celsius)", title = paste0("Spring Temperatures in ", AllStn$name[i], ", NY"))
  ggsave(paste0("temp_trends_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}


# average temperatures by decade for all stations ----
ggplot(data = SpringDecadeAv, aes(x = Decade, y = tav, color = Name))+
  geom_line()+
  scale_color_brewer(palette = "Paired", name = "Station Name")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Decade", y = "Temperature (celsius)", title = "Average Spring Temperatures")
ggsave("average_all.png", plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))


# rolling average temperatures for all stations ----
# add rolling av column - move this step higher?
current_dataT1 <- SpringYear %>%
  group_by(StationID) %>%
  mutate(RollAv5 = rollmean(tav, k = 5, fill = NA),
         RollAv10 = rollmean(tav, k = 10, fill = NA))

current_range <- data.frame(year=seq(1893, 2019))
current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))

# 5 year 
ggplot(data = current_data, aes(x = year, y = RollAv5, color = Name))+
  geom_line()+
  scale_color_brewer(palette = "Paired", name = "Station Name")+
  xlab("Year")+
  ylab("Temperature (C)")+
  ggtitle("Average Mar-Apr-May Temperatures (5 year rolling)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
ggsave("RollAv5_All.png", plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))

# 10 year 
ggplot(data = current_data, aes(x = year, y = RollAv10, color = Name))+
  geom_line(alpha = .8)+
  scale_color_manual(values = c("#822944",
                                "#cfc83c",
                                "#89abf0",
                                "#c58644",
                                "#82edcd",
                                "#7fd06a",
                                "#fb99ad",
                                "#ef622e",
                                "#5b6b26",
                                "#003fbb",
                                "#d83d79",
                                "#9f4ca5"), 
                     name = "Station Name")+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  labs(x = "Year", y = "Temperature (C)")+
  ggtitle("Average Mar-Apr-May Temperatures")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
ggsave("RollAv10_All.png", plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))



# violin plots to show distribution of temperatures ----
# jan - june
for (i in 1:nrow(AllStn)){
  
  current_data = subset(TavData, TavData$StationID == AllStn$station_id[i])
  current_data$Decade <- as.factor(current_data$Decade)
  
  ggplot(data = current_data, aes(x = Decade, y = tav, group = Decade))+
    geom_violin(fill = "lightskyblue")+
    geom_boxplot(width = .4, fill = "lightskyblue")+
    scale_x_discrete(breaks = current_data$Decade, labels = current_data$Decade)+
    ylab("Average Temperature (C)")+
    ggtitle(paste0("Distribution of Daily Average Temperatures in ", AllStn$name[i], ", NY"))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
  ggsave(paste0("violin_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}

# mar - may
for (i in 1:nrow(AllStn)){
  
  current_data = subset(SpringData, SpringData$StationID == AllStn$station_id[i])
  current_data$Decade <- as.factor(current_data$Decade)
  
  ggplot(data = current_data, aes(x = Decade, y = tav, group = Decade))+
    geom_violin(fill = "lightskyblue")+
    geom_boxplot(width = .4, fill = "lightskyblue")+
    scale_x_discrete(breaks = current_data$Decade, labels = current_data$Decade)+
    ylab("Average Temperature (C)")+
    ggtitle(paste0("Distribution of Daily Average Spring Temperatures in ", AllStn$name[i], ", NY"))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
  ggsave(paste0("violin_spring_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}


# mar-apr-may separate months
# month IDs to use in loop
monthID <- c("Mar","Apr","May")
monthName <- c("March","April","May")
colorID <- c("skyblue", "lightgreen", "springgreen4")

# loop through stations
for (i in 1:nrow(AllStn)){
  
  current_data = subset(SpringData, SpringData$StationID == AllStn$station_id[i])
  current_data$Decade <- as.factor(current_data$Decade)
  
  # loop through  months 
  for (j in 1:3){
    
    ggplot(data = current_data[current_data$Month == monthID[j],], aes(x = Decade, y = tav, group = Decade))+
      geom_violin(position = position_dodge(1), fill = colorID[j])+
      geom_boxplot(position = position_dodge(1), width = .2, fill = colorID[j])+
      ylab("Average Temperature (C)")+
      ylim(c(-20,30))+
      ggtitle(paste0("Distribution of Daily Average ", monthName[j], " Temperatures in ", AllStn$name[i], ", NY"))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    ggsave(paste0("violin_", monthID[j], "_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
    
  }
}  



# mar-apr-may on same plot
current_data = subset(SpringData, SpringData$StationID == AllStn$station_id[2])
current_data$Decade <- as.factor(current_data$Decade)

ggplot(data = current_data, aes(x = Decade, y = tav, fill = Month))+
  geom_violin(position = position_dodge(1))+
  geom_boxplot(position = position_dodge(1), width = .2)+
  scale_fill_manual(values = c("skyblue", "lightgreen","springgreen4"))+
  scale_x_discrete(breaks = current_data$Decade, labels = current_data$Decade)+
  ylab("Average Temperature (C)")+
  ggtitle(paste0("Distribution of Daily Average Temperatures in ", AllStn$name[2], ", NY"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
ggsave(paste0("violin_maraprmay_", AllStn$name[2],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))



### EXTREME TEMPERATURES ----

# Extreme temperatures by year ----
# separated by month
# loop through different stations
for (i in 1:nrow(AllStn)){
  
  current_dataT1 <- subset(SpringMonths, SpringMonths$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=rep(seq(AllStn[i,5],2019), times = 3), month = rep(month(c(3,4,5), label = TRUE), each = (2020 - AllStn$StartTmax[AllStn$stnID == i])))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year", "month" = "month"))
  current_data <- current_data[order(current_data$year),]
  
  # loop through different months 
  for (j in 1:3){
    
    png(paste0(plotDIR[usernumber], "/ex_temps", AllStn$name[i], "_", monthID[j], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
    
    # plot
    plot(current_data$year[current_data$month == monthID[j]], current_data$ExtHi[current_data$month == monthID[j]],
         type = "l",
         lwd = 2, 
         col = "tomato3",
         xlab = "Year",
         ylab = "Temperature (Celsius)",
         main = paste("Extreme", monthName[j] , "Temperatures in", AllStn$name[i], ", NY"),
         ylim = c(min(current_data$ExtLo[current_data$month == monthID[j]], na.rm=TRUE)-5, max(current_data$ExtHi[current_data$month == monthID[j]], na.rm=TRUE)+5),
         xlim = c(min(current_data$year[current_data$month == monthID[j]]), 2019))
    lines(current_data$year[current_data$month == monthID[j]], current_data$ExtLo[current_data$month == monthID[j]],
          lwd = 2, 
          col = "skyblue")     
    legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n")
    
    dev.off()
  }
}  

# all months on same plot
for (i in 1:nrow(AllStn)){
  
  current_dataT1 <- subset(SpringMonths, SpringMonths$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=rep(seq(AllStn[i,5],2019), times = 3), month = rep(month(c(3,4,5), label = TRUE), each = (2020 - AllStn$StartTmax[AllStn$stnID == i])))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year", "month" = "month"))
  current_data <- current_data[order(current_data$year),]
  
  png(paste0(plotDIR[usernumber], "/ex_temps_", AllStn$name[i], "_", "allmonths", ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  plot(current_data$year[current_data$month == "Mar"], current_data$ExtHi[current_data$month == "Mar"],
       type = "l",
       lwd = 2,
       col = "skyblue",
       xlab = "Decade",
       ylab = "Temperature (Celsius)",
       main = paste0("Extreme Spring Temperatures in ", AllStn$name[i], ", NY"),
       ylim = c(min(current_data$ExtLo[current_data$month == "Mar"], na.rm=TRUE)-7, max(current_data$ExtHi[current_data$month == "May"], na.rm=TRUE)+5),
       xlim = c(min(current_data$year), 2019))
  lines(current_data$year[current_data$month == "Mar"], current_data$ExtLo[current_data$month == "Mar"],
        lwd = 2,  col = "skyblue", lty = "dotted") 
  lines(current_data$year[current_data$month == "Apr"], current_data$ExtHi[current_data$month == "Apr"],
        lwd = 2, col = "lightgreen")
  lines(current_data$year[current_data$month == "Apr"], current_data$ExtLo[current_data$month == "Apr"],
        lwd = 2, col = "lightgreen", lty = "dotted")
  lines(current_data$year[current_data$month == "May"], current_data$ExtHi[current_data$month == "May"],
        lwd = 2, col = "springgreen4")
  lines(current_data$year[current_data$month == "May"], current_data$ExtLo[current_data$month == "May"],
        lwd = 2, col = "springgreen4", lty = "dotted")
  legend("bottomleft", c("May Hi", "April Hi", "March Hi", "May Lo", "April Lo", "March Lo"), col = c("springgreen4", "lightgreen", "skyblue","springgreen4","lightgreen","skyblue"), lty = (c(1,1,1,3,3,3)), lwd = 2, bty="n", cex=.75)
  
  dev.off()
}  



# Extreme temperatures by decade ---- 
#  note: decade averages from SpringDecade are still impacted by missing data 

# loop through different stations
for (i in 1:nrow(AllStn)){
  
  current_data <- subset(SpringDecade, SpringDecade$StationID == AllStn$station_id[i])
  
  # loop through different months 
  for (j in 1:3){
    
    png(paste0(plotDIR[usernumber], "/ex_temps_decade_", AllStn$name[i], "_", monthID[j], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
    
    # plot
    plot(current_data$Decade[current_data$Month == monthID[j]], current_data$ExtHi[current_data$Month == monthID[j]],
         type = "o",
         pch = 19,
         col = "tomato3",
         xlab = "Decade",
         ylab = "Temperature (Celsius)",
         main = paste("Extreme", monthName[j] , "Temperatures in", AllStn$name[i], ", NY"),
         ylim = c(min(current_data$ExtLo[current_data$Month == monthID[j]], na.rm=TRUE)-5, max(current_data$ExtHi[current_data$Month == monthID[j]], na.rm=TRUE)+5),
         xlim = c(min(current_data$Decade), 2010))
    points(current_data$Decade[current_data$Month == monthID[j]], current_data$ExtLo[current_data$Month == monthID[j]],
           col = "skyblue", pch = 19)
    lines(current_data$Decade[current_data$Month == monthID[j]], current_data$ExtLo[current_data$Month == monthID[j]],
          col = "skyblue") 
    legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)
    
    dev.off()
  }
}  

# extreme temps by decade - all spring months on same graph
# by decade so we still have the problem of missing data

for (i in 1:nrow(AllStn)){
  
  current_data <- subset(SpringDecade, SpringDecade$StationID == AllStn$station_id[i])
  
  png(paste0(plotDIR[usernumber], "/ex_temps_decade_", AllStn$name[i], "_", "allmonths", ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  plot(current_data$Decade[current_data$Month == "Mar"], current_data$ExtHi[current_data$Month == "Mar"],
       type = "l",
       lwd = 2,
       col = "tomato3",
       xlab = "Decade",
       ylab = "Temperature (Celsius)",
       main = paste0("Extreme Spring Temperatures in ", AllStn$name[i], ", NY"),
       ylim = c(-30,30),
       xlim = c(min(current_data$Decade), 2010))
  lines(current_data$Decade[current_data$Month == "Mar"], current_data$ExtLo[current_data$Month == "Mar"],
        lwd = 2,  col = "skyblue") 
  lines(current_data$Decade[current_data$Month == "Apr"], current_data$ExtHi[current_data$Month == "Apr"],
        lwd = 2, col = "tomato3", lty = "dashed")
  lines(current_data$Decade[current_data$Month == "Apr"], current_data$ExtLo[current_data$Month == "Apr"],
        lwd = 2, col = "skyblue", lty = "dashed")
  lines(current_data$Decade[current_data$Month == "May"], current_data$ExtHi[current_data$Month == "May"],
        lwd = 2, col = "tomato3", lty = "dotted")
  lines(current_data$Decade[current_data$Month == "May"], current_data$ExtLo[current_data$Month == "May"],
        lwd = 2, col = "skyblue", lty = "dotted")
  legend("bottomright", c("May Hi", "April Hi", "March Hi", "May Lo", "April Lo", "March Lo"), col = c("tomato3", "tomato3", "tomato3","skyblue","skyblue","skyblue"), lty = 3:1, bty="n", cex=.75)
  
  dev.off()
}  

# Number of extreme days by year ----

# loop through different stations
for (i in 1:nrow(AllStn)){
  
  current_data <- subset(SpringMonths, SpringMonths$StationID == AllStn$station_id[i])
  for (j in 1:3){
    
    png(paste0(plotDIR[usernumber], "/ex_tempN_", AllStn$name[i], "_", monthID[j], ".png"), 
        width = 10, height = 10, units = "in", res = 144, pointsize = 15)
    
    plot(current_data$year[current_data$month == monthID[j]]+.25, current_data$ExHiCount[current_data$month == monthID[j]],
         type = "h",
         lwd = 3,
         col = "tomato3",
         main = paste0(monthName[j]," Extreme Temperature Days in ", AllStn$name[i], ", NY"),
         xlab = "Year",
         ylab = "Number of Days",
         ylim = c(0, max(c(current_data$ExHiCount[current_data$month == monthID[j]],current_data$ExLoCount[current_data$month == monthID[j]]))+1),
         xlim = c(min(current_data$year[current_data$month == monthID[j]]), 2019))
    lines(current_data$year[current_data$month == monthID[j]]-.25, current_data$ExLoCount[current_data$month == monthID[j]],
          type = "h",
          lwd = 3,
          col = "skyblue")
    legend("topright", c("Low","High"), col = c("skyblue","tomato3"), lwd = 2, bty = "n")
    
    dev.off()
  }
}



# Number of extreme days by decade ----

# loop through stations
for (i in 1:nrow(AllStn)){
  
  current_data <- subset(SpringDecade, SpringDecade$StationID == AllStn$station_id[i])
  
  # loop through months
  for (j in 1:3){
    
    png(paste0(plotDIR[usernumber], "/ex_temp_decN_", AllStn$name[i], "_", monthID[j], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
    
    plot(current_data$Decade[current_data$Month == monthID[j]]+.5, current_data$ExHiCount[current_data$Month == monthID[j]],
         type = "h",
         lwd = 5,
         col = "tomato3",
         main = paste0(monthName[j]," Extreme Temperature Days in ", AllStn$name[i], ", NY"),
         xlab = "Decade",
         ylab = "Number of Days",
         ylim = c(0, max(c(current_data$ExHiCount[current_data$Month == monthID[j]],current_data$ExLoCount[current_data$Month == monthID[j]])+2)),
         xlim = c(min(current_data$Decade[current_data$Month == monthID[j]]), 2010))
    points(current_data$Decade[current_data$Month == monthID[j]]-.5, current_data$ExLoCount[current_data$Month == monthID[j]],
           type = "h",
           lwd = 5,
           col = "skyblue")
    legend("topright", c("High","Low"), col = c("tomato3","skyblue"), lwd = 2, bty = "n")
    
    dev.off()
    
  }
  
}



### FREEZE THAW ----
# Number of Freeze Thaw Days Graphs ----
# loop through stations
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringYear, SpringYear$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))
  current_av <- mean(current_data$FTdays, na.rm = TRUE)
  
  ggplot(data = current_data, aes(x = year, y = FTdays)) +
    geom_line(color = "deepskyblue3")+
    geom_point(pch = 20, color = "deepskyblue3")+
    geom_hline(yintercept = current_av, color = "red3")+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(x = "Year", y = "Number of Freeze Thaw Days", title = paste0("Spring Freeze Thaw Days in ", AllStn$name[i],", NY"))
  ggsave(paste0("num_FTdays_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}

# Freeze Thaw Amplitude Graphs ----
# Creating spring year data frame with rolling avs
SpringYear1 <- SpringYear %>%
  group_by(StationID) %>%
  mutate(FTRollAv5 = rollmean(FTrange, k = 5, fill = NA),
         FTRollAv10 = rollmean(FTrange, k = 10, fill = NA),
         FTRollAv15 = rollmean(FTrange, k = 15, fill = NA),
         FTRollAv20 = rollmean(FTrange, k = 20, fill = NA))

### some graphs missing may data 
# fix title --> goes off the page
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringYear1, SpringYear1$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))
  
  # plot general temperature trends
  ggplot(data = current_data, aes(x = year, y = FTRollAv5))+
    geom_line(color = "deepskyblue3") +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(x = "Year", y = "Temperature Range (celcius)", 
         title = paste0("Amplitude of Spring (Mar-May) Freeze Thaw Days in ", AllStn$name[i],", NY"),
         subtitle = "Calculated with a 5 Year Rolling Averge")
  
  ggsave(paste0("FT_amp_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}



### Heat Maps ----
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringData, SpringData$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  ggplot(data = current_data, mapping = aes(x = Year, y = DayID, fill = tav)) +
    geom_tile() +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(title = paste0("Daily Spring Temperatures in ", AllStn$name[i],", NY"))+
    geom_hline(yintercept = c(1, 32, 62))+
    scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
    scale_fill_gradient2(name = "Temperature (c)",
                         low = "#2166ac",
                         mid = "#d8daeb",
                         high = "#b2182b",
                         na.value = "white")
  ggsave(paste0("raw_temp_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
  
  # standardized anomalies
  ggplot(data = current_data, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
    geom_tile() +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(title = paste0("Standardized Daily Spring Temperature Anomalies in ", AllStn$name[i],", NY"))+
    geom_hline(yintercept = c(1, 32, 62))+
    scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
    scale_fill_gradient2(name = "Temperature Anomaly",
                         low = "#4575b4",
                         mid = "#ffffbf",
                         high = "#d73027",
                         na.value = "white") 
  ggsave(paste0("std_anom_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
  
  # raw anomalies
  ggplot(data = current_data, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
    geom_tile() +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(title = paste0("Raw Spring Temperatures Anomalies in ", AllStn$name[i],", NY"))+
    geom_hline(yintercept = c(1, 32, 62))+
    scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
    scale_fill_gradient2(name = "Temperature Anomaly",
                         low = "#4575b4",
                         mid = "#ffffbf",
                         high = "#d73027",
                         na.value = "white") 
  ggsave(paste0("raw_anom_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}


# count number of days with temperature anomaly >10
springEx <- SpringData[(SpringData$AnRaw >= 10) | (SpringData$AnRaw <= -10),]
springEx <- springEx %>% drop_na(AnRaw)
anomN <- aggregate(springEx$AnRaw, by = list(springEx$StationID, springEx$StationName, springEx$Decade), FUN = "length")
colnames(anomN) <- c("StationID", "StationName", "Decade","AnomN")

# look at extreme anomalies by decade for individual station 
subset(anomN, anomN$StationID == AllStn$station_id[6])



### WEEK 2 ----
### Freeze Thaw Day Type Heat Maps----

for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(AllData, AllData$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  # had to make it a factor to avoid this error: "Error: Continuous value supplied to discrete scale"
  current_data$DayType <- as.factor(current_data$DayType) 
  
  ggplot(data= current_data, mapping = aes(x= Year, y = DOY, fill = DayType))+
    geom_tile() +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
    scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
    labs(title = paste0("Types of Days in ", AllStn$name[i], ", NY"))
  ggsave(paste0("day_type_hm_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}

### Thawing Degree Days ----
# Accumulation Jan 1 - June 30 ----
# do we want to have multiple stations on the same graph?

for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(TavData, TavData$StationID == AllStn$station_id[i])
  current_dataT2 <- subset(current_dataT1, current_dataT1$DOY == max(current_dataT1$DOY))
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT2, current_range, by = c("Year" = "year"))
  current_data <- current_data[order(current_data$Year),]
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/tdd_bar", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # get the base plot with just the first year on there
  plot(current_data$Year, current_data$TDD,
       type = "o",
       pch = 20,
       xlab = "Year",
       ylab = "Degrees (C)",
       lwd = 1.5,
       col = "deepskyblue3",
       main = paste0("Thawing Degree Day Accumulation (Jan - June) in ", AllStn$name[i], ", NY"))
  
  dev.off()
}


# TDD accumulation curves ----

# creating for loop for plots
for (i in 1:nrow(AllStn)){
  current_data <- subset(TavData, TavData$StationID == AllStn$station_id[i])
  stnyrs <- unique(data.frame(Year = current_data$Year, Decade = current_data$Decade)) 
  stnyrs$color <- ifelse(stnyrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/tdd_", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # get the base plot with just the first year on there
  plot(current_data$DOY[current_data$Year == stnyrs$Year[i]], current_data$TDD[current_data$Year == stnyrs$Year[i]],
       type = "l",
       col = stnyrs$color[1],
       ylim = c(0, round(max(current_data$TDD), digits = -2) + 100),
       xlab = "DOY",
       ylab = "Degrees (C)",
       main = paste("Thawing Degree Days Accumulation", AllStn$name[i]))
  # loop through the rest of the years starting at the second index and add the line onto the plot
  # current year just keeps track of what year we're on to make it easier but we don't have to use it
  for (j in 2:nrow(stnyrs)){
    current_year = (stnyrs$Year[j])
    lines(current_data$DOY[current_data$Year == current_year], current_data$TDD[current_data$Year == current_year],
          col = stnyrs$color[j])
  }
  legend("topleft", c("Before 2010","2010-2019"), col = c("#00008b","#FF9900"), lwd = 2, bty = "n", cex = 1)
  dev.off()
}

### Day of Last Freeze ----
# creating last freeze data frame 
LastFreeze <- subset(AllData, AllData$tmin <= 0) 
# finding last day of freeze in each year
LastFreeze <- aggregate(LastFreeze$DOY, by = list(LastFreeze$StationID, LastFreeze$StationName, LastFreeze$Year), FUN = "max")
colnames(LastFreeze) <- c("StationID", "StationName", "Year", "DOY")
# joining with all data to get the TDD on the day of last freeze
LastFreeze <- inner_join(LastFreeze, TavData, by = c("StationID", "StationName", "Year","DOY"))
LastFreeze <- LastFreeze[c("StationID", "StationName", "Year","DOY", "tmin", "TDD")]

# Looking at hard freezes (<-5)
# creating last freeze data frame 
LastHardFreeze <- subset(AllData, AllData$tmin <= -5) 
# finding last day of freeze in each year
LastHardFreeze <- aggregate(LastHardFreeze$DOY, by = list(LastHardFreeze$StationID, LastHardFreeze$StationName, LastHardFreeze$Year), FUN = "max")
colnames(LastHardFreeze) <- c("StationID", "StationName", "Year", "DOY")
# joining with all data to get the TDD on the day of last freeze
LastHardFreeze <- inner_join(LastHardFreeze, TavData, by = c("StationID", "StationName", "Year","DOY"))
LastHardFreeze <- LastHardFreeze[c("StationID", "StationName", "Year","DOY", "tmin", "TDD")]


# Plots of day of last freeze with thawing degree days
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  current_data$TminFlag <- ifelse(current_data$tmin < -2.2, current_data$DOY, NA)
  current_data$TDDFlag <- ifelse(current_data$tmin < -2.2, current_data$TDD, NA)
  current_data <- current_data[order(current_data$Year),]
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/lf_tdd_", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # get the base plot with just the first year on there
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(current_data$Year, current_data$DOY,
       type = "l",
       col = alpha("black", 0.7),
       ylim = c(105, 250),
       xlab = "Year",
       ylab = "DOY of Last Freeze",
       main = paste0("Day of Year of Last Freeze in ", AllStn$name[i],", NY"))
  points(current_data$Year, current_data$TminFlag,
         type = "p",
         col = "deepskyblue2",
         pch = 8)
  par(new = TRUE)
  plot(current_data$Year, current_data$TDD,
       type = "l",
       lwd = 1.5,
       col = alpha("red3",0.7),
       ylim = c(-200,900),
       axes = FALSE, 
       xlab = "", 
       ylab = "")
  points(current_data$Year, current_data$TDDFlag,
         type = "p",
         col = "deepskyblue2",
         ylim = c(-200,900),
         pch = 8)
  axis(side = 4, at = seq(0,900, by = 200))
  mtext("Accumulated TDD (C)", side = 4, line = 3) 
  legend("topright", c("DOY","TDD", "Tmin < -2.2˚C"), col = c("black","red3", "deepskyblue2"), 
         lwd = 2, bty = "n", lty = c(1, 1, NA), pch = c(NA, NA, 8), cex = 1)
  dev.off()
}


# plots of temperature on day of last freeze
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  current_data <- current_data[order(current_data$Year),]
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/lf_temp_", AllStn$name[i], ".png"),width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(current_data$Year, current_data$DOY,
       type = "l",
       lwd = 1.5,
       pch = 20,
       col = alpha("black", 0.7),
       xlab = "Year",
       ylab = "DOY of Last Freeze",
       main = paste0("TMIN on Day of Last Freeze in ", AllStn$name[i], ", NY"),
       ylim = c(105,190))
  par(new = TRUE)
  plot(current_data$Year, current_data$tmin,
       type = "l",
       col = alpha("deepskyblue3",0.7),
       lwd = 1.5,
       xlab = "",
       ylab = "",
       axes = FALSE,
       ylim = c(-10, 0))
  axis(side = 4, at = seq(-10, 0, by = 2))
  mtext("Minimum Temperature (C)", side = 4, line = 3)
  legend("bottom", c("DOY","Tmin"), col = c("black","deepskyblue3"), lwd = 2, bty = "n", cex = 1)
  
  dev.off()
}


### Cold Snaps in Late Spring ----
# subsetting to just freezing days
FreezeDays <- AllData[AllData$tmin <= 0,1:6]
FreezeDays <- aggregate(FreezeDays$DOY, by = list(FreezeDays$StationID, FreezeDays$StationName, FreezeDays$Year, FreezeDays$Month), FUN = "length")
colnames(FreezeDays) <- c("StationID", "StationName", "Year", "Month", "FreezeDays")

# subsetting to hard freeze days
HardFreezeDays <- AllData[AllData$tmin <= -5,1:6]
HardFreezeDays <- aggregate(HardFreezeDays$DOY, by = list(HardFreezeDays$StationID, HardFreezeDays$StationName, HardFreezeDays$Year, HardFreezeDays$Month), FUN = "length")
colnames(HardFreezeDays) <- c("StationID", "StationName", "Year", "Month", "FreezeDays")


# plotting number of days below 0 and -5 for march
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(FreezeDays, StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[1, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  current_data2T1 <- subset(HardFreezeDays, StationID == AllStn$station_id[i])
  current_range2 <- data.frame(year=seq(AllStn[1, 5], 2019))
  current_data2 <- full_join(current_data2T1, current_range, by = c("Year" = "year"))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/mar_freeze_days_", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # march plots
  plot(current_data$Year[current_data$Month == "Mar"], current_data$FreezeDays[current_data$Month == "Mar"],
       type = "h",
       pch = 20,
       lwd = 2,
       col = "lightskyblue",
       xlab = "Year",
       ylab = "Number of Days",
       main = paste0("Number of Freezes and Hard Freezes in March in ", AllStn$name[i] ,", NY"))
  points(current_data2$Year[current_data2$Month == "Mar"], current_data2$FreezeDays[current_data2$Month == "Mar"],
         type = "h",
         col = "green4",
         lwd = 2)
  legend("topright", c("Days Below 0˚C","Days Below -5˚C"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = 1)
  
  dev.off()
}

# plotting number of days below 0 and -5 for april
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(FreezeDays, StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[1, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  current_data2T1 <- subset(HardFreezeDays, StationID == AllStn$station_id[i])
  current_range2 <- data.frame(year=seq(AllStn[1, 5], 2019))
  current_data2 <- full_join(current_data2T1, current_range, by = c("Year" = "year"))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/apr_freeze_days_", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # april plots
  plot(current_data$Year[current_data$Month == "Apr"], current_data$FreezeDays[current_data$Month == "Apr"],
       type = "h",
       pch = 20,
       lwd = 2,
       col = "lightskyblue",
       xlab = "Year",
       ylab = "Number of Days",
       main = paste0("Number of Freezes and Hard Freezes in April in ", AllStn$name[i] ,", NY"))
  points(current_data2$Year[current_data2$Month == "Apr"], current_data2$FreezeDays[current_data2$Month == "Apr"],
         type = "h",
         col = "green4",
         lwd = 2)
  legend("topright", c("Days Below 0˚C","Days Below -5˚C"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = 1)
  
  dev.off()
}


### Apple Growing Degree Days ----
# Apples plots for 2012
# make list of stations that have 2012 for tavdata
tav2012 <- subset(TavData, Year == 2012)
stn2012 <- data.frame(StationID = unique(tav2012$StationID), name = unique(tav2012$Name))

for (i in 1:nrow(stn2012)){
  current_data <- subset(tav2012, StationID == stn2012$StationID[i])
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/apple_gdd_", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # create plot of DOY last freeze and temperature
  plot(current_data$DOY, current_data$GDD41,
       type = "l",
       lwd = 1.5,
       col = "deepskyblue3",
       xlab = "DOY",
       ylab = "Degrees (C)",
       main = paste0("2012 Apple Growing Degree Days in ", stn2012$name[i], ", NY"))
  abline(h = 100, col = alpha("red3", 0.6), lwd = 1.5)
  abline(h = 400, col = alpha("blueviolet", 0.6), lwd = 1.5)
  abline(v = LastFreeze$DOY[LastFreeze$StationID == stn2012$StationID[i] & LastFreeze$Year == 2012], col = alpha("goldenrod3", 0.6), lwd = 1.5)
  abline(v = LastHardFreeze$DOY[LastHardFreeze$StationID == stn2012$StationID[i] & LastHardFreeze$Year == 2012], col = alpha("darkgreen", 0.6), lwd = 1.5)
  legend("topleft", c("GDD Needed for Budding", "GDD Needed for Leaf Out","Last Day Below -0˚C","Last Day Below -5˚C", "Accumulated GDD"), 
         col = c("red3", "blueviolet", "goldenrod3","darkgreen","deepskyblue3"), lwd = 2, bty = "n", cex = 1)
  
  dev.off()
}

# focus on one station for storymap
# find temperatures at days noted as damaging freeze
frz2012 <- subset(tav2012, (tav2012$DOY == 87) | (tav2012$DOY == 119) | (tav2012$DOY == 120))
frz2012 <- data.frame(StationID = frz2012$StationID,
                      StationName = frz2012$StationName,
                      DOY = frz2012$DOY,
                      tmin = frz2012$tmin)
frz2012[frz2012$StationID == AllStn$station_id[10],]

# read in pictures of apple stations
silvertip <- readJPEG(paste0(diru[usernumber], "/silvertip.jpg"))
firstbloom <- readJPEG(paste0(diru[usernumber], "/firstbloom.jpeg"))

# plot of 2012 apple freeze in syracuse
png(paste0(plotDIR[usernumber], "/apple_gdd_final_", AllStn$name[10], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)

plot(tav2012$DOY[tav2012$StationID == AllStn$station_id[10]], tav2012$GDD41[tav2012$StationID == AllStn$station_id[10]],
     type = "l",
     lwd = 1.5,
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "2012 Apple Growing Degree Days in Syracuse, NY")
rect(xleft = -10, xright = 200, ybottom = 400, ytop =  1500, col = alpha("blueviolet", 0.2), border = NA)
rect(xleft = -10, xright = 200, ybottom = 100, ytop =  400, col = alpha("lightgreen", 0.2), border = NA)
abline(h = 100, col = alpha("springgreen4", 0.6), lwd = 1.5)
abline(h = 400, col = alpha("blueviolet", 0.6), lwd = 1.5)

arrows(x0 = 87, y0 = 300, x1 = 87, y1 = 180, length = 0.1, lwd = 2)
text(87, 350, labels = "-6.7 ˚C")
arrows(x0 = 119, y0 = 410, x1 = 119, y1 = 280, length = 0.1, lwd = 2)
text(119, 450, labels = "-3.3 ˚C")

rasterImage(silvertip, 0, 110, 20, 220)
text(20, 120, pos = 4, labels = "First Bud at 100 GDD", cex = 0.75, col = "springgreen4")
rasterImage(firstbloom, 0, 410, 20, 520)
text(20, 420, pos = 4, labels = "First Bloom at 400 GDD", cex = 0.75, col = "blueviolet")

legend("topleft", "Accumulated GDD", col = "deepskyblue3", lwd = 2, bty = "n", cex = 1)

dev.off()

# look at Syracuse 2012 data
syr2012 <- tav2012[tav2012$StationID == AllStn$station_id[10],]