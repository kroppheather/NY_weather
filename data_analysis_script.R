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

# Getting rid of years with less than 171 observations in TmaxData
TmaxData <- inner_join(TmaxData, TmaxDataYear, by = c("id" = "station", "year" = "year"))

# Counting observations per year for the tmin data
# Make new data frame with just the id, tmin value, and year
TminDataYear <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="length")

# Changing column names
colnames(TminDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
TminDataYear <- subset(TminDataYear, TminDataYear$ncount >= 171)

# Getting rid of years with less than 171 observations in TminData
TminData <- inner_join(TminData, TminDataYear, by = c("id" = "station", "year" = "year"))

# Counting observations per year for the prcp data
# Make new data frame with just the id, prcp value, and year
PrcpDataYear <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="length")

# Changing column names
colnames(PrcpDataYear) <- c("station", "year", "ncount")

PrcpDataYear <- subset(PrcpDataYear, PrcpDataYear$ncount >= 171)

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
# map colors dont show up for tmin or all stn (get an error with rep function)
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

### identify sites with all data types and create AllStn data frame
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



# map the stations that have all data
#look at weather stations
plot(siteP, pch=19)
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


### WEEK 1 ----
### Creating one large data frame ----

# join tmax, tmin, and prcp data
AllDataT1 <- full_join(TmaxData, TminData, by = c("id"="id", "date" = "date", "year"="year", "DOY" = "DOY"))
AllDataT2 <- full_join(AllDataT1, PrcpData, by = c("id"="id",  "date" = "date", "year"="year", "DOY" = "DOY"))

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


# filtering out tav with na
TavData <- AllData %>% drop_na(tav)
TavCount <- aggregate(TavData$tav, by = list(TavData$StationID, TavData$StationName, TavData$Year), FUN = "length")
colnames(TavCount) <- c("StationID", "StationName", "Year", "ncount")
TavCount <- subset(TavCount, TavCount$ncount >= 171)

TavData <- inner_join(TavData, TavCount, by = c("StationID", "StationName", "Year"))

# add thawing degree day accumulation
TavData <- TavData %>%
  group_by(Year, StationID) %>%
  arrange(DOY) %>%
  mutate(TDD = cumsum(ifelse(is.na(tav), 0, ifelse(tav >= 0, tav, 0))))

# add growing degree day accumulation (for apples)
TavData <- TavData %>%
  group_by(Year, StationID) %>%
  arrange(DOY) %>%
  mutate(GDD41 = cumsum(ifelse(is.na(tav), 0, ifelse(tav >= 5, tav - 5, 0))))

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

# data frame with yearly averages - spring months averaged together
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
SpringDecade<- aggregate(SpringData$tmax, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)
colnames(SpringDecade) <- c("StationID", "StationName", "Name", "Decade", "Month", "tmax")
SpringDecade$tmin <- aggregate(SpringData$tmin, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$tav <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExtHi <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringDecade$ExLoCount <- aggregate(SpringData$ExtrLo,by=list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringDecade$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecade$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x

# decade averages with spring months averaged together
SpringDecadeAv <- aggregate(SpringData$tmax, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade), FUN = "mean", na.rm = TRUE)
colnames(SpringDecadeAv) <- c("StationID", "StationName", "Name", "Decade", "tmax")
SpringDecadeAv$tmin <- aggregate(SpringData$tmin, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$tav <- aggregate(SpringData$tav, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExtHi <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName,SpringData$Name, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x
SpringDecadeAv$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$StationID, SpringData$Decade), FUN="sum", na.rm = TRUE)$x
SpringDecadeAv$ExLoCount <- aggregate(SpringData$ExtrLo,by=list(SpringData$StationID, SpringData$Decade), FUN="sum", na.rm = TRUE)$x
SpringDecadeAv$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecadeAv$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade), FUN = "mean", na.rm = TRUE)$x

# subset to specific months
# all march data
# maybe get rid of depending on for loop
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

## Linear regressions for tav ----
RegressionTav <- data.frame(StationID=character(0), 
                            StationName=character(0), 
                            Slope = numeric(0), 
                            SlopeP = numeric(0), 
                            Int = numeric(0), 
                            IntP = numeric(0),
                            Rsq = numeric(0))

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
  
}

## Linear regressions for tmin ----
RegressionTmin <- data.frame(StationID=character(0), 
                             StationName=character(0), 
                             Slope = numeric(0), 
                             SlopeP = numeric(0), 
                             Int = numeric(0), 
                             IntP = numeric(0),
                             Rsq = numeric(0))

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
  
}

## Linear regressions for tmax ----
RegressionTmax <- data.frame(StationID=character(0), 
                             StationName=character(0), 
                             Slope = numeric(0), 
                             SlopeP = numeric(0), 
                             Int = numeric(0), 
                             IntP = numeric(0),
                             Rsq = numeric(0))

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
    geom_abline(data = RegressionTav, aes(slope = Slope[i], intercept = Int[i]), color = alpha("slateblue3", 0.6))+
    geom_line(aes(y = tmax, color = "Maximum"))+
    geom_abline(data = RegressionTmax, aes(slope = Slope[i], intercept = Int[i]), color = alpha("tomato4", 0.6))+
    geom_line(aes(y = tmin, color = "Minimum"))+
    geom_abline(data = RegressionTmin, aes(slope = Slope[i], intercept = Int[i]), color = alpha("deepskyblue3", 0.6))+
    scale_color_manual(values = c("slateblue1","tomato3","skyblue"), name = "Temperature Measurement")+
    theme_classic()+
    labs(x = "Year", y = "Temperature (celsius)", title = paste0("Spring Temperatures in ", AllStn$name[i], ", NY"))
  ggsave(paste0("temp_trends_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}

# average temperatures by decade for all stations
ggplot(data = SpringDecadeAv, aes(x = Decade, y = tav, color = Name))+
  geom_line()+
  scale_color_brewer(palette = "Paired", name = "Station Name")+
  theme_classic()+
  xlim(1950,2010)+
  labs(x = "Decade", y = "Temperature (celsius)", title = "Average Spring Temperatures")
ggsave("average_all.png", plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))


### EXTREME TEMPERATURES ----

# Extreme temperatures by year ----
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

# Extreme temperatures by decade ---- 
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


# Number of extreme days by year ----

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


# Number of extreme days by decade ----

# MARCH
MarDecade$ExHiCount <- as.vector(MarDecade$ExHiCount)
MarDecade$ExLoCount <- as.vector(MarDecade$ExLoCount)
# station 1
plot(MarDecade$Decade[MarDecade$StationID=="USC00300785"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00300785"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00300785"], 
       type = "h",
       col = "skyblue")

# station 2
plot(MarDecade$Decade[MarDecade$StationID=="USC00301752"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00301752"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Cooperstown, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00301752"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00301752"], 
       type = "h",
       col = "skyblue")

# station 3
plot(MarDecade$Decade[MarDecade$StationID=="USC00304102"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00304102"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Indian Lake, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00304102"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00304102"], 
       type = "h",
       col = "skyblue")

# station 4
plot(MarDecade$Decade[MarDecade$StationID=="USC00304912"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00304912"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Lowville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00304912"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00304912"], 
       type = "h",
       col = "skyblue")

# station 5
plot(MarDecade$Decade[MarDecade$StationID=="USC00306085"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00306085"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Norwich, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00306085"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00306085"], 
       type = "h",
       col = "skyblue")

# station 6
plot(MarDecade$Decade[MarDecade$StationID=="USC00306314"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00306314"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Oswego, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00306314"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00306314"], 
       type = "h",
       col = "skyblue")

# station 7
plot(MarDecade$Decade[MarDecade$StationID=="USC00309000"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USC00309000"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Watertown, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USC00309000"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USC00309000"], 
       type = "h",
       col = "skyblue")

# station 8
plot(MarDecade$Decade[MarDecade$StationID=="USW00014735"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USW00014735"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Albany, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USW00014735"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USW00014735"], 
       type = "h",
       col = "skyblue")

# station 9
plot(MarDecade$Decade[MarDecade$StationID=="USW00014750"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USW00014750"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Glens Falls, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,35))
points(MarDecade$Decade[MarDecade$StationID=="USW00014750"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USW00014750"], 
       type = "h",
       col = "skyblue")

# station 10
plot(MarDecade$Decade[MarDecade$StationID=="USW00014771"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USW00014771"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Syracuse, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USW00014771"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USW00014771"], 
       type = "h",
       col = "skyblue")

# station 11
plot(MarDecade$Decade[MarDecade$StationID=="USW00094725"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USW00094725"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Massena, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USW00094725"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USW00094725"], 
       type = "h",
       col = "skyblue")

# station 12
plot(MarDecade$Decade[MarDecade$StationID=="USW00094790"]+.5, MarDecade$ExHiCount[MarDecade$StationID=="USW00094790"], 
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Watertown Airport, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,30))
points(MarDecade$Decade[MarDecade$StationID=="USW00094790"]-.5, MarDecade$ExLoCount[MarDecade$StationID=="USW00094790"], 
       type = "h",
       col = "skyblue")

# APRIL
AprDecade$ExHiCount <- as.vector(AprDecade$ExHiCount)
AprDecade$ExLoCount <- as.vector(AprDecade$ExLoCount)
# station 1
plot(AprDecade$Decade[AprDecade$StationID=="USC00300785"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00300785"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00300785"], 
       type = "h",
       col = "skyblue")

# station 2
plot(AprDecade$Decade[AprDecade$StationID=="USC00301752"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00301752"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Cooperstown, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00301752"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00301752"], 
       type = "h",
       col = "skyblue")

# station 3
plot(AprDecade$Decade[AprDecade$StationID=="USC00304102"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00304102"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Indian Lake, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00304102"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00304102"], 
       type = "h",
       col = "skyblue")

# station 4
plot(AprDecade$Decade[AprDecade$StationID=="USC00304912"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00304912"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Lowville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00304912"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00304912"], 
       type = "h",
       col = "skyblue")

# station 5
plot(AprDecade$Decade[AprDecade$StationID=="USC00306085"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00306085"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Norwich, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00306085"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00306085"], 
       type = "h",
       col = "skyblue")

# station 6
plot(AprDecade$Decade[AprDecade$StationID=="USC00306314"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00306314"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Oswego, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00306314"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00306314"], 
       type = "h",
       col = "skyblue")

# station 7
plot(AprDecade$Decade[AprDecade$StationID=="USC00309000"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USC00309000"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Watertown, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USC00309000"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USC00309000"], 
       type = "h",
       col = "skyblue")

# station 8
plot(AprDecade$Decade[AprDecade$StationID=="USW00014735"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USW00014735"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Albany, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USW00014735"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USW00014735"], 
       type = "h",
       col = "skyblue")

# station 9
plot(AprDecade$Decade[AprDecade$StationID=="USW00014750"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USW00014750"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Glens Falls, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USW00014750"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USW00014750"], 
       type = "h",
       col = "skyblue")

# station 10
plot(AprDecade$Decade[AprDecade$StationID=="USW00014771"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USW00014771"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Syracuse, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USW00014771"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USW00014771"], 
       type = "h",
       col = "skyblue")

# station 11
plot(AprDecade$Decade[AprDecade$StationID=="USW00094725"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USW00094725"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Massena, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USW00094725"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USW00094725"], 
       type = "h",
       col = "skyblue")

# station 12
plot(AprDecade$Decade[AprDecade$StationID=="USW00094790"]+.5, AprDecade$ExHiCount[AprDecade$StationID=="USW00094790"], 
     type = "h",
     col = "tomato3",
     main = "April Extreme High Temperature Days in Watertown Airport, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,40))
points(AprDecade$Decade[AprDecade$StationID=="USW00094790"]-.5, AprDecade$ExLoCount[AprDecade$StationID=="USW00094790"], 
       type = "h",
       col = "skyblue")


# MAY
MayDecade$ExHiCount <- as.vector(MayDecade$ExHiCount)
MayDecade$ExLoCount <- as.vector(MayDecade$ExLoCount)
#station 1
plot(MayDecade$Decade[MayDecade$StationID=="USC00300785"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00300785"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Boonville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00300785"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00300785"], 
       type = "h",
       col = "skyblue")

# station 2
plot(MayDecade$Decade[MayDecade$StationID=="USC00301752"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00301752"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Cooperstown, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00301752"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00301752"], 
       type = "h",
       col = "skyblue")

# station 3
plot(MayDecade$Decade[MayDecade$StationID=="USC00304102"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00304102"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Indian Lake, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00304102"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00304102"], 
       type = "h",
       col = "skyblue")

# station 4
plot(MayDecade$Decade[MayDecade$StationID=="USC00304912"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00304912"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Lowville, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00304912"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00304912"], 
       type = "h",
       col = "skyblue")

# station 5
plot(MayDecade$Decade[MayDecade$StationID=="USC00306085"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00306085"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Norwich, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00306085"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00306085"], 
       type = "h",
       col = "skyblue")

# station 6
plot(MayDecade$Decade[MayDecade$StationID=="USC00306314"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00306314"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Oswego, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00306314"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00306314"], 
       type = "h",
       col = "skyblue")

# station 7
plot(MayDecade$Decade[MayDecade$StationID=="USC00309000"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USC00309000"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Watertown, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USC00309000"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USC00309000"], 
       type = "h",
       col = "skyblue")

# station 8
plot(MayDecade$Decade[MayDecade$StationID=="USW00014735"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USW00014735"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Albany, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USW00014735"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USW00014735"], 
       type = "h",
       col = "skyblue")

# station 9
plot(MayDecade$Decade[MayDecade$StationID=="USW00014750"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USW00014750"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Glens Falls, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USW00014750"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USW00014750"], 
       type = "h",
       col = "skyblue")

# station 10
plot(MayDecade$Decade[MayDecade$StationID=="USW00014771"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USW00014771"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Syracuse, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USW00014771"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USW00014771"], 
       type = "h",
       col = "skyblue")

# station 11
plot(MayDecade$Decade[MayDecade$StationID=="USW00094725"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USW00094725"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Massena, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USW00094725"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USW00094725"], 
       type = "h",
       col = "skyblue")

# station 12
plot(MayDecade$Decade[MayDecade$StationID=="USW00094790"]+.5, MayDecade$ExHiCount[MayDecade$StationID=="USW00094790"], 
     type = "h",
     col = "tomato3",
     main = "May Extreme High Temperature Days in Watertown Airport, NY",
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0,45))
points(MayDecade$Decade[MayDecade$StationID=="USW00094790"]-.5, MayDecade$ExLoCount[MayDecade$StationID=="USW00094790"], 
       type = "h",
       col = "skyblue")

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

### FREEZE THAW ----
# Number of Freeze Thaw Days Graphs ----
# have 20 as differentiating mark but we could look up how many in one year is problematic and use that as a threshold
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringYear, SpringYear$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))
  
  # plot general temperature trends
  ggplot(data = current_data, aes(x = year, y = FTdays)) +
    geom_bar(position = "dodge", stat="identity", fill = ifelse(current_data$FTdays > 20, "tomato3", "deepskyblue3"))+
    theme_classic()+
    labs(x = "Year", y = "Number of Freeze Thaw Days", title = paste0("Spring Freeze Thaw Days in ", AllStn$name[i],", NY"))
  
  ggsave(paste0("num_FTdays_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}

# Freeze Thaw Amplitude Graphs ----
### some graphs missing may data 
# lots of missing data in these graphs -- should we change or remove them?
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringDecade, SpringDecade$StationID == AllStn$station_id[i])
  #current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  #current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))
  
  # plot general temperature trends
  ggplot(data = current_dataT1, aes(x = Decade, y = FTrange, color = Month))+
    geom_point() +
    geom_line() +
    theme_classic()+
    labs(x = "Year", y = "Temperature Range (celcius)", title = paste0("Temperature Amplitude of Spring Freeze Thaw Days in ", AllStn$name[i],", NY"))
  
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
    scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
    scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
    labs(title = paste0("Types of Days in ", AllStn$name[i], ", NY"))
  ggsave(paste0("day_type_hm_", AllStn$name[i],".png"), plot = last_plot(), device = png(), path = paste0(plotDIR[usernumber], "/"))
}

### Thawing Degree Days ----
# Accumulation Jan 1 - June 30 ----
# what were these graphs supposed to look like??
# did we want the total accumulated per year?
# use station 1 as test


for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(TavData, TavData$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/tdd_bar", AllStn$name[i], ".png"))

  # get the base plot with just the first year on there
  plot(current_data$Year, current_data$TDD,
       type = "h",
       xlab = "Year",
       ylab = "Degrees (C)",
       lwd = 3,
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
  png(paste0(plotDIR[usernumber], "/tdd_", AllStn$name[i], ".png"))

  # get the base plot with just the first year on there
  plot(current_data$DOY[current_data$Year == stnyrs$Year[1]], current_data$TDD[current_data$Year == stnyrs$Year[1]],
     type = "l",
     col = stnyrs$color[1],
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
  current_data <- current_data[order(current_data$Year),]
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/lf_tdd_", AllStn$name[i], ".png"), 500, 500)

  # get the base plot with just the first year on there
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(current_data$Year, current_data$DOY,
       type = "l",
       col = alpha("black", 0.7),
       ylim = c(105, 250),
       xlab = "Year",
       ylab = "DOY of Last Freeze",
       main = paste0("Day of Year of Last Freeze in ", AllStn$name[i],", NY"))
  par(new = TRUE)
  plot(current_data$Year, current_data$TDD,
       type = "l",
       lwd = 1.5,
       col = alpha("red3",0.7),
       ylim = c(-200,900),
       axes = FALSE, 
       xlab = "", 
       ylab = "")
  axis(side = 4, at = seq(0,900, by = 200))
  mtext("Accumulated TDD (C)", side = 4, line = 3) 
  legend("bottom", c("DOY","TDD"), col = c("black","red3"), lwd = 2, bty = "n", cex = .5)
  
  dev.off()
}


# plots of temperature on day of last freeze
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  current_data <- current_data[order(current_data$Year),]
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/lf_temp_", AllStn$name[i], ".png"),500, 500)

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
  legend("bottom", c("DOY","Tmin"), col = c("black","deepskyblue3"), lwd = 2, bty = "n", cex = .5)
  
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


# plotting number of days below freezing for march and april
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(FreezeDays, StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/freeze_days_", AllStn$name[i], ".png"))

  # create bar plot of DOY last freeze and temperature
  plot(current_data$Year[current_data$Month == "Mar"], current_data$FreezeDays[current_data$Month == "Mar"],
       type = "h",
       pch = 20,
       lwd = 2,
       col = "lightskyblue",
       xlab = "Year",
       ylab = "Number of Days",
       main = paste0("Days below Freezing in", AllStn$name[i] ,", NY"))
  points(current_data$Year[current_data$Month == "Apr"], current_data$FreezeDays[current_data$Month == "Apr"],
         type = "h",
         col = "green4",
         lwd = 2)
  legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
  
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
  png(paste0(plotDIR[usernumber], "/apple_gdd_", AllStn$name[i], ".png"), 500, 500)
  
  # create bar plot of DOY last freeze and temperature
  plot(current_data$DOY, current_data$GDD41,
       type = "l",
       lwd = 1.5,
       col = "deepskyblue3",
       xlab = "DOY",
       ylab = "Degrees (C)",
       main = paste0("2012 Apple Growing Degree Days in ", stn2012$name[i], ", NY"))
  abline(h = 100, col = "red3", lwd = 1.5)
  abline(h = 400, col = "blueviolet", lwd = 1.5)
  abline(v = LastFreeze$DOY[LastFreeze$StationID == stn2012$StationID[i] & LastFreeze$Year == 2012], col = "goldenrod3", lwd = 1.5)
  abline(v = LastHardFreeze$DOY[LastHardFreeze$StationID == stn2012$StationID[i] & LastHardFreeze$Year == 2012], col = "darkgreen", lwd = 1.5)
  legend("topleft", c("GDD Needed for Budding", "GDD Needed for Leaf Out","Last Day Below -0˚C","Last Day Below -5˚C", "Accumulated GDD"), 
         col = c("red3", "blueviolet", "goldenrod3","darkgreen","deepskyblue3"), lwd = 2, bty = "n", cex = .5)
  
  dev.off()
}

# some example plots
# Boonville
plot(TavData$DOY[TavData$Year == 2012 & TavData$StationID == "USC00300785"], TavData$GDD41[TavData$Year == 2012 & TavData$StationID == "USC00300785"],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "Apple Growing Degree Days Accumulation Boonville, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red3")
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USC00300785" & LastFreeze$Year == 2012], col = "goldenrod3", lwd = 1.5)
abline(v = LastHardFreeze$DOY[LastHardFreeze$StationID == "USC00300785" & LastHardFreeze$Year == 2012], col = "darkgreen", lwd = 1.5)


# Lowville
plot(TavData$DOY[TavData$Year == 2012 & TavData$StationID == "USC00304912"], TavData$GDD41[TavData$Year == 2012 & TavData$StationID == "USC00304912"],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "Apple Growing Degree Days Accumulation Lowville, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red3")
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USC00304912" & LastFreeze$Year == 2012])

# Albany
plot(TavData$DOY[TavData$Year == 2012 & TavData$StationID == "USW00014735"], TavData$GDD41[TavData$Year == 2012 & TavData$StationID == "USW00014735"],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "Apple Growing Degree Days Accumulation Albany, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red3")
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USW00014735" & LastFreeze$Year == 2012])


# Boonville
plot(TavData$DOY[TavData$Year == 2012 & TavData$StationID == "USC00300785"], TavData$GDD41[TavData$Year == 2012 & TavData$StationID == "USC00300785"],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "Apple Growing Degree Days Accumulation Boonville, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red3")
abline(v = LastHardFreeze$DOY[LastHardFreeze$StationID == "USC00300785" & LastHardFreeze$Year == 2012])

# Lowville
plot(TavData$DOY[TavData$Year == 2012 & TavData$StationID == "USC00304912"], TavData$GDD41[TavData$Year == 2012 & TavData$StationID == "USC00304912"],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "Apple Growing Degree Days Accumulation Lowville, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red3")
abline(v = LastHardFreeze$DOY[LastHardFreeze$StationID == "USC00304912" & LastHardFreeze$Year == 2012])

# Albany
plot(TavData$DOY[TavData$Year == 2012 & TavData$StationID == "USW00014735"], TavData$GDD41[TavData$Year == 2012 & TavData$StationID == "USW00014735"],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Degrees (C)",
     main = "Apple Growing Degree Days Accumulation Albany, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red3")
abline(v = LastHardFreeze$DOY[LastHardFreeze$StationID == "USW00014735" & LastHardFreeze$Year == 2012])


