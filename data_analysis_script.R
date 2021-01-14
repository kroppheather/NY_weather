#updated 1/4

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
# to pull data
diru = c("/Users/abby/Documents/NYweather",
         "/Users/hkropp/Google Drive/research/students/NYweather/data",
         "/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/data")
# to save plots
plotDIR = c("/Users/abby/Documents/NYweather/plots", 
            "/Users/hkropp/Google Drive/research/students/NYweather/plots", 
            "/Users/rachelpike/Desktop/2020-2021/Research/plots")

# Choosing the user number - CHANGE THIS VALUE 
usernumber = 1

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

# Counting observations per year for the tmin data
# Make new data frame with just the id, tmin value, and year
TminDataYear <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="length")

# Changing column names
colnames(TminDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
TminDataYear <- subset(TminDataYear, TminDataYear$ncount >= 171)
# get rid of years with missing data from TminData
TminData <- inner_join(TminData, TminDataYear, by = c("id" = "station", "year"="year"))

# Counting observations per year for the prcp data
# Make new data frame with just the id, prcp value, and year
PrcpDataYear <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="length")

# Changing column names
colnames(PrcpDataYear) <- c("station", "year", "ncount")

# Getting rid of rows with less than 171 observations
PrcpDataYear <- subset(PrcpDataYear, PrcpDataYear$ncount >= 171)
# get rid of years with missing data from PrcpData
PrcpData <- inner_join(PrcpData, PrcpDataYear, by = c("id" = "station", "year"="year"))

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

### identify sites with all data types
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



### Creating one large data frame ----

# GET RID OF THIS
# make data frame to join with AllData to fill in missing spots
# create a data frame with all years and stations 
# StnList <- as.vector(AllStn$station_id)
# AllYear <- data.frame(year=rep(seq(1890,2020), each = 12))
# AllYear$id = rep(StnList, times = 131)
# 
# # create a data frame with all years, stations, and months 
# AllMonth <- data.frame(year=rep(seq(1890,2020), each = 12, times = 6))
# AllMonth$id <- rep(StnList, times = 131*6)
# AllMonth$Month <- rep(month(c(1, 2, 3, 4, 5, 6), label = TRUE), each = 12, times = 131)


# GET RID OF T1 AND T2
# join tmax, tmin, and prcp data into AllData
AllDataT1 <- full_join(TmaxData, TminData, by = c("id"="id", "date" = "date", "year"="year", "DOY" = "DOY"))
AllDataT2 <- full_join(AllDataT1, PrcpData, by = c("id"="id", "date" = "date", "year"="year", "DOY" = "DOY"))

# add station names of the 12 good stations
AllData <- inner_join(AllDataT2, AllStn, by = c("id"="station_id"))

# add month column
AllData$Month <- month(AllData$date, label = TRUE)

# add decade column
AllData$Decade <- AllData$year - (AllData$year %% 10)

# GET RID OF THIS
# add back in years/months with missing data (fill with NA)
# AllData <- full_join(AllData, AllMonth, by = c("year","id","Month"))

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
# Maybe change these numbers?
AllData$FreezeThaw <- ifelse(AllData$tmin<(-2.2) & AllData$tmax>0, 1 , NA)

# adding column of all freeze-thaw flags
# 1 - freeze
# 2 - freeze thaw day
# 3 - thaw 


AllData$DayType <- ifelse(AllData$tmin<0 & AllData$tmax<0, 1,
                          ifelse(AllData$tmin<=0 & AllData$tmax>=0, 2,
                                 ifelse(AllData$tmin>0 & AllData$tmax>0, 3, 0)))

# format as factor - REMOVE IF NOT USED
# AllData$DayType <- as.factor(AllData$DayType)

# Adding range of freeze thaw column
AllData$FTrange <- ifelse(AllData$FreezeThaw == 1, AllData$tmax - AllData$tmin, NA)

# Making station column a factor - REMOVE IF NOT USED
# AllData$StationID <- as.factor(AllData$StationID)



# Extreme values (occur <5% of the time) 
# what would happen if we changed to <1% ???

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
# CHECK IF ALL COLUMNS ARE NECESSARY IN TAVDATA

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


# # subset to each station 
# alldata1 <- subset(AllData, AllData$StationID=="USC00300785")
# alldata2 <- subset(AllData, AllData$StationID=="USC00301752")
# alldata3 <- subset(AllData, AllData$StationID=="USC00304102")
# alldata4 <- subset(AllData, AllData$StationID=="USC00304912")
# alldata5 <- subset(AllData, AllData$StationID=="USC00306085")
# alldata6 <- subset(AllData, AllData$StationID=="USC00306314")
# alldata7 <- subset(AllData, AllData$StationID=="USC00309000")
# alldata8 <- subset(AllData, AllData$StationID=="USW00014735")
# alldata9 <- subset(AllData, AllData$StationID=="USW00014750")
# alldata10 <- subset(AllData, AllData$StationID=="USW00014771")
# alldata11 <- subset(AllData, AllData$StationID=="USW00094725")
# alldata12 <- subset(AllData, AllData$StationID=="USW00094790")


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

# REMOVE
# join with AllMonth to fill in missing months
# SpringMonths <- full_join(SpringMonths, AllMonth[AllMonth$Month %in% c("Mar","Apr","May"),], by = (c("year" = "year", "StationID" = "id", "month" = "Month")))


# data frame with yearly averages - spring months averaged together
SpringYear <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID, SpringData$StationName,SpringData$Name), FUN="mean", na.rm = TRUE)
colnames(SpringYear) <- c("year","StationID","StationName","Name", "tmax")
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

# REMOVE
# join with AllYear to fill in missing years
# SpringYear <- full_join(SpringYear, AllYear, by = (c("year" = "year", "StationID" = "id")))


# MAKE SURE EVERY AGGREGATE DOESNT NEED SPRINGDATA$NAME
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

# # TRY TO MAKE FOR LOOP SO WE DONT NEED THESE SUBSETS
# # subset to specific months
# # all march data
# MarData <- subset(AllData, AllData$Month == "Mar")
# # march yearly averages
# MarYear <- subset(SpringMonths, SpringMonths$month == "Mar")
# # march decade averages
# MarDecade <- subset(SpringDecade, SpringDecade$Month == "Mar")
# 
# # all april data
# AprData <- subset(AllData, AllData$Month == "Apr")
# # april yearly averages
# AprYear <- subset(SpringMonths, SpringMonths$month == "Apr")
# # april decade averages
# AprDecade <- subset(SpringDecade, SpringDecade$Month == "Apr")
# 
# # all may data
# MayData <- subset(AllData, AllData$Month == "May")
# # may yearly averages
# MayYear <- subset(SpringMonths, SpringMonths$month == "May")
# # may decade averages
# MayDecade <- subset(SpringDecade, SpringDecade$Month == "May")
# 
# # # subset to specific stations
# # # yearly averages
# stn1 <- subset(SpringYear, SpringYear$StationID == "USC00300785")
# stn2 <- subset(SpringYear, SpringYear$StationID == "USC00301752")
# stn3 <- subset(SpringYear, SpringYear$StationID == "USC00304102")
# stn4 <- subset(SpringYear, SpringYear$StationID == "USC00304912")
# stn5 <- subset(SpringYear, SpringYear$StationID == "USC00306085")
# stn6 <- subset(SpringYear, SpringYear$StationID == "USC00306314")
# stn7 <- subset(SpringYear, SpringYear$StationID == "USC00309000")
# stn8 <- subset(SpringYear, SpringYear$StationID == "USW00014735")
# stn9 <- subset(SpringYear, SpringYear$StationID == "USW00014750")
# stn10 <- subset(SpringYear, SpringYear$StationID == "USW00014771")
# stn11 <- subset(SpringYear, SpringYear$StationID == "USW00094725")
# stn12 <- subset(SpringYear, SpringYear$StationID == "USW00094790")


### General temperature trends ----

# line graphs of tmax, tmin, tav over time
# for loop for plots
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(SpringYear, SpringYear$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("year" = "year"))
  
  
  # plot general temperature trends
  ggplot(data = current_data, aes(x = year))+
    geom_line(aes(y = tav, color = "Average"))+ 
    geom_line(aes(y = tmax, color = "Maximum"))+
    geom_line(aes(y = tmin, color = "Minimum"))+
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

## DO WE WANT LINEAR REGRESSIONS IN FINAL CODE?
# # by decade
# # station 1 model
# stn1dc.mod <- lm(stn1dc$tav ~ stn1dc$Decade)
# # assumptions
# stn1dc.res <- rstandard(stn1dc.mod)
# qqnorm(stn1dc.res)
# qqline(stn1dc.res)
# plot(stn1dc$Decade, stn1dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn1dc.mod)
# plot(stn1dc$Decade, stn1dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn1dc.mod)
# 
# # station 2 model
# stn2dc.mod <- lm(stn2dc$tav ~ stn2dc$Decade)
# # assumptions
# stn2dc.res <- rstandard(stn2dc.mod)
# qqnorm(stn2dc.res)
# qqline(stn2dc.res)
# plot(stn2dc$Decade, stn2dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn2dc.mod)
# plot(stn2dc$Decade, stn2dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn2dc.mod)
# 
# # station 3 model
# stn3dc.mod <- lm(stn3dc$tav ~ stn3dc$Decade)
# # assumptions
# stn3dc.res <- rstandard(stn3dc.mod)
# qqnorm(stn3dc.res)
# qqline(stn3dc.res)
# plot(stn3dc$Decade, stn3dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn3dc.mod)
# plot(stn3dc$Decade, stn3dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn3dc.mod)
# 
# # station 4 model
# stn4dc.mod <- lm(stn4dc$tav ~ stn4dc$Decade)
# # assumptions
# stn4dc.res <- rstandard(stn4dc.mod)
# qqnorm(stn4dc.res)
# qqline(stn4dc.res)
# plot(stn4dc$Decade, stn4dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn4dc.mod)
# plot(stn4dc$Decade, stn4dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn4dc.mod)
# 
# # station 5 model
# stn5dc.mod <- lm(stn5dc$tav ~ stn5dc$Decade)
# # assumptions
# stn5dc.res <- rstandard(stn5dc.mod)
# qqnorm(stn5dc.res)
# qqline(stn5dc.res)
# plot(stn5dc$Decade, stn5dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn5dc.mod)
# plot(stn5dc$Decade, stn5dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn5dc.mod)
# 
# # station 6 model
# stn6dc.mod <- lm(stn6dc$tav ~ stn6dc$Decade)
# # assumptions
# stn6dc.res <- rstandard(stn6dc.mod)
# qqnorm(stn6dc.res)
# qqline(stn6dc.res)
# plot(stn6dc$Decade, stn6dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn6dc.mod)
# plot(stn6dc$Decade, stn6dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn6dc.mod)
# 
# # station 7 model
# stn7dc.mod <- lm(stn7dc$tav ~ stn7dc$Decade)
# # assumptions
# stn7dc.res <- rstandard(stn7dc.mod)
# qqnorm(stn7dc.res)
# qqline(stn7dc.res)
# plot(stn7dc$Decade, stn7dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn7dc.mod)
# plot(stn7dc$Decade, stn7dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn7dc.mod)
# 
# # station 8 model -- SIGNIFICANT!
# stn8dc.mod <- lm(stn8dc$tav ~ stn8dc$Decade)
# # assumptions
# stn8dc.res <- rstandard(stn8dc.mod)
# qqnorm(stn8dc.res)
# qqline(stn8dc.res)
# plot(stn8dc$Decade, stn8dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn8dc.mod)
# plot(stn8dc$Decade, stn8dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn8dc.mod)
# 
# # station 9 model
# stn9dc.mod <- lm(stn9dc$tav ~ stn9dc$Decade)
# # assumptions
# stn9dc.res <- rstandard(stn9dc.mod)
# qqnorm(stn9dc.res)
# qqline(stn9dc.res)
# plot(stn9dc$Decade, stn9dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn9dc.mod)
# plot(stn9dc$Decade, stn9dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn9dc.mod)
# 
# # station 10 model
# stn10dc.mod <- lm(stn10dc$tav ~ stn10dc$Decade)
# # assumptions
# stn10dc.res <- rstandard(stn10dc.mod)
# qqnorm(stn10dc.res)
# qqline(stn10dc.res)
# plot(stn10dc$Decade, stn10dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn10dc.mod)
# plot(stn10dc$Decade, stn10dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn10dc.mod)
# 
# # station 11 model
# stn11dc.mod <- lm(stn11dc$tav ~ stn11dc$Decade)
# # assumptions
# stn11dc.res <- rstandard(stn11dc.mod)
# qqnorm(stn11dc.res)
# qqline(stn11dc.res)
# plot(stn11dc$Decade, stn11dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn11dc.mod)
# plot(stn11dc$Decade, stn11dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn11dc.mod)
# 
# # station 12 model
# stn12dc.mod <- lm(stn12dc$tav ~ stn12dc$Decade)
# # assumptions
# stn12dc.res <- rstandard(stn12dc.mod)
# qqnorm(stn12dc.res)
# qqline(stn12dc.res)
# plot(stn12dc$Decade, stn12dc.res,
#      xlab = "average temp",
#      ylab = "standardized residual")
# # add a horizontal line at zero
# abline(h=0)
# # regression
# summary(stn12dc.mod)
# plot(stn12dc$Decade, stn12dc$tav,
#      ylab = "average temp",
#      xlab = "decade")
# abline(stn12dc.mod)


### EXTREME TEMPERATURES ----

# use for reference
# to subset by month
monthID <- c("Mar","Apr","May")
# to put entire month name in title
monthName <- c("March","April","May")


# Extreme temperatures by year ----

##  work on getting ylim to change with current_data

#test
current_dataT1 <- subset(SpringMonths, SpringMonths$StationID == AllStn$station_id[1])
current_range <- data.frame(year=rep(seq(AllStn[1,5],2019), times = 3), month = rep(month(c(3,4,5), label = TRUE), each = (2020 - AllStn$StartTmax[AllStn$stnID == 1])))
current_data <- full_join(current_dataT1, current_range, by = c("year" = "year", "month" = "month"))

# not working for monthID[2]
plot(current_data$year[current_data$month == monthID[1]], current_data$ExtHi[current_data$month == monthID[1]],
     type = "l",
     col = "tomato3",
     xlab = "Year",
     ylab = "Temperature (Celsius)",
     main = paste("Extreme", monthName[1] , "Temperatures in", AllStn$name[1], ", NY"),
     ylim = c(min(current_data$ExtLo[current_data$month == monthID[1]])-5, max(current_data$ExtHi[current_data$month == monthID[1]])+5))
lines(current_data$year[current_data$month == monthID[1]], current_data$ExtLo[current_data$month == monthID[1]],
      col = "skyblue")     
# maybe add trendline
lines(predict(lm(current_data$ExtHi[current_data$month == monthID[3]] ~ current_data$year[current_data$month == monthID[3]])))
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n")



# for loop for extreme temperature plots
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
  
 

# Extreme temperatures by decade ---- 
# maybe get combine extreme temp graphs for year and decade -- make year lines more transparent
# use SpringDecade - note: decade averages are still impacted by missing data 

# for loop for plots
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


# plot of extreme temperatures all spring months on same graph
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
## stopped plotting, need to test number of extreme days by year and decade

# test
current_data <- subset(SpringMonths, SpringMonths$StationID == AllStn$station_id[1])

plot(current_data$year[current_data$month == "May"]+.25, current_data$ExHiCount[current_data$month == "May"],
     type = "h",
     col = "tomato3",
     main = "March Extreme High Temperature Days in Boonville, NY",
     xlab = "Year",
     ylab = "Number of Days",
     ylim = c(0, max(c(current_data$ExHiCount[current_data$month == "May"],current_data$ExLoCount[current_data$month == "May"]))),
     xlim = c(min(current_data$year[current_data$month == "May"]), 2019))
lines(current_data$year[current_data$month == "May"], current_data$ExLoCount[current_data$month == "May"],
      type = "h",
      col = "skyblue")
legend("topright", c("High","Low"), col = c("tomato3","skyblue"), lwd = 2, bty = "n", cex = .5)


# for loop for plots -  number of extreme temp days per year

for (i in 1:nrow(AllStn)){
  
  current_data <- subset(SpringMonths, SpringMonths$StationID == AllStn$station_id[i])
  for (j in 1:3){
    
    png(paste0(plotDIR[usernumber], "/ex_tempN_", AllStn$name[i], "_", monthID[j], ".png"), 
        width = 10, height = 10, units = "in", res = 144, pointsize = 15)
    
    plot(current_data$year[current_data$month == monthID[j]]+.25, current_data$ExHiCount[current_data$month == monthID[j]],
         type = "h",
         lwd = 2,
         col = "tomato3",
         main = paste0(monthName[j]," Extreme Temperature Days in ", AllStn$name[i], ", NY"),
         xlab = "Year",
         ylab = "Number of Days",
         ylim = c(0, max(c(current_data$ExHiCount[current_data$month == monthID[j]],current_data$ExLoCount[current_data$month == monthID[j]]))+1),
         xlim = c(min(current_data$year[current_data$month == monthID[j]]), 2019))
    lines(current_data$year[current_data$month == monthID[j]], current_data$ExLoCount[current_data$month == monthID[j]],
          type = "h",
          lwd = 2,
          col = "skyblue")
    legend("topright", c("Low","High"), col = c("skyblue","tomato3"), lwd = 2, bty = "n")
    
    dev.off()
  }
}







# Number of extreme days by decade ----
#  test
current_data <- subset(SpringDecade, SpringDecade$StationID == AllStn$station_id[1])
plot(current_data$Decade[current_data$Month == "Mar"]+.5, current_data$ExHiCount[current_data$Month == "Mar"],
     type = "h",
     lwd = 3,
     col = "tomato3",
     main = paste0(j," Extreme Temperature Days in ", AllStn$name[1], ", NY"),
     xlab = "Decade",
     ylab = "Number of Days",
     ylim = c(0, max(current_data$ExHiCount[current_data$Month == "Mar"])),
     xlim = c(min(current_data$Decade[current_data$Month == "Mar"]), 2010))
points(current_data$Decade[current_data$Month == "Mar"]-.5, current_data$ExLoCount[current_data$Month == "Mar"],
       type = "h",
       lwd = 3,
       col = "skyblue")
legend("topright", c("High","Low"), col = c("tomato3","skyblue"), lwd = 2, bty = "n")


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
  
 
## still need to 
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


# Freeze Thaw Amplitude Graphs ----
### some graphs missing may data 
### add standard deviation as error bar on scatter plot by decade (maybe)

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


### Heat Maps ----
# Station 1
# raw temps
stn1all <- subset(SpringData, SpringData$StationID == "USC00300785")
ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Spring Temperatures: Boonville, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white")

# standardized anomalies
ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Standardized Daily Temperature Anomalies: Boonville, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 

# raw anomalies
ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Temperature Anomalies: Boonville, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# Station 2
# raw temps
stn2all <- subset(SpringData, SpringData$StationID == "USC00301752")
ggplot(data = stn2all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Spring Temperatures: Cooperstown, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white")
# standardized anomalies
ggplot(data = stn2all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Standardized Daily Temperature Anomalies: Cooperstown, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# raw anomalies
ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Temperature Anomalies: Cooperstown, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# Station 3
# raw temps
stn3all <- subset(SpringData, SpringData$StationID == "USC00304102")
ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Spring Temperatures: Indian Lake, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white")
# standardized anomalies
ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Standardized Daily Temperature Anomalies: Indian Lake, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# raw anomalies
ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Temperature Anomalies: Indian Lake, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# Station 4
# raw temps
stn4all <- subset(SpringData, SpringData$StationID == "USC00304912")
ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Spring Temperatures: Lowville, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",)
# standardized anomalies
ggplot(data = stn4all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Standardized Daily Temperature Anomalies: Lowville, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# raw anomalies
ggplot(data = stn4all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Temperature Anomalies: Lowville, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 

# Station 5
# raw temps
stn5all <- subset(SpringData, SpringData$StationID == "USC00306085")
ggplot(data = stn5all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Spring Temperatures: Norwich, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white")
# standardized anomalies
ggplot(data = stn5all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Standardized Daily Temperature Anomalies: Norwich, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 
# raw anomalies
ggplot(data = stn5all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  labs(title = "Daily Temperature Anomalies: Norwich, NY")+
  geom_hline(yintercept = c(1, 32, 62))+
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") 

# Station 6
# raw temps
stn6all <- subset(SpringData, SpringData$StationID == "USC00306314")
ggplot(data = stn6all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Oswego, NY")

# standardized anomalies
ggplot(data = stn6all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Oswego, NY")

# raw anomalies
ggplot(data = stn6all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Oswego, NY")

# Station 7
# raw temps
stn7all <- subset(SpringData, SpringData$StationID == "USC00309000")
ggplot(data = stn7all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Watertown, NY")

# standardized anomalies
ggplot(data = stn7all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Watertown, NY")

# raw anomalies
ggplot(data = stn7all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Watertown, NY")

# Station 8
# raw temps
stn8all <- subset(SpringData, SpringData$StationID == "USW00014735")
ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Albany, NY")

# standardized anomalies
ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Albany, NY")

# raw anomalies
ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Albany, NY")

# Station 9
# raw temps
stn9all <- subset(SpringData, SpringData$StationID == "USW00014750")
ggplot(data = stn9all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Glens Falls, NY")

# standardized anomalies
ggplot(data = stn9all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Glens Falls, NY")

# raw anomalies
ggplot(data = stn9all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Glens Falls, NY")

# Station 10
# raw temps
stn10all <- subset(SpringData, SpringData$StationID == "USW00014771")
ggplot(data = stn10all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Syracuse, NY")

# standardized anomalies
ggplot(data = stn10all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Syracuse, NY")

# raw anomalies
ggplot(data = stn10all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Syracuse, NY")

# Station 11
# raw temps
stn11all <- subset(SpringData, SpringData$StationID == "USW00094725")
ggplot(data = stn11all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Massena, NY")

# standardized anomalies
ggplot(data = stn11all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Massena, NY")

# raw anomalies
ggplot(data = stn11all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Massena, NY")

# Station 12
# raw temps
stn12all <- subset(SpringData, SpringData$StationID == "USW00094790")
ggplot(data = stn12all, mapping = aes(x = Year, y = DayID, fill = tav)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperatures in Watertown Airport, NY")

# standardized anomalies
ggplot(data = stn12all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
  geom_tile() +
  theme_classic() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Standardized Spring Temperature Anomalies in Watertown Airport, NY")

# raw anomalies
ggplot(data = stn12all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
  geom_tile() +
  scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
  scale_fill_gradient2(name = "Temperature Anomaly",
                       low = "#4575b4",
                       mid = "#ffffbf",
                       high = "#d73027",
                       na.value = "white") +
  geom_hline(yintercept = c(1, 32, 62))+
  labs(title = "Spring Temperature Anomalies in Watertown Airport, NY")


### WEEK 2 ----
### Freeze Thaw Day Type Heat Maps----
# station 1
ggplot(data= alldata1, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Boonville, NY")
  
# station 2 
ggplot(data= alldata2, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Cooperstown, NY")

# station 3 
ggplot(data= alldata3, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Indian Lake, NY")

# station 4
ggplot(data= alldata4, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Lowville, NY")


# station 5
ggplot(data= alldata5, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Norwich, NY")

# station 6
ggplot(data= alldata6, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Oswego, NY")

# station 7
ggplot(data= alldata7, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Watertown, NY")

# station 8
ggplot(data= alldata8, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Albany, NY")

# station 9
ggplot(data= alldata9, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white")+
  labs(title = "Glens Falls, NY")

# station 10
ggplot(data= alldata10, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Syracuse, NY")

# station 11
ggplot(data= alldata11, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Massena, NY")

# station 12
ggplot(data= alldata12, mapping = aes(x= Year, y = DOY, fill = DayType))+
  geom_tile() +
  theme_classic()+
  scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
  scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
  labs(title = "Types of Days in Watertown Airport, NY")

### Thawing Degree Days ----
# Accumulation Jan 1 - June 30

# station 1 
plot(alldata1$Year, alldata1$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Thawing Degree Day Accumulation (Jan - June) Boonville, NY")

# station 2
plot(alldata2$Year, alldata2$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Thawing Degree Day Accumulation (Jan - June) Cooperstown, NY")

# station 3
plot(alldata3$Year, alldata3$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Thawing Degree Day Accumulation (Jan - June) Indian Lake, NY")

# station 4
plot(alldata4$Year, alldata4$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Thawing Degree Day Accumulation (Jan - June) Lowville, NY")

# station 5
plot(alldata5$Year, alldata5$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Norwich, NY")

# station 6
plot(alldata6$Year, alldata6$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Oswego, NY")

# station 7
plot(alldata7$Year, alldata7$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Watertown, NY")

# station 8
plot(alldata8$Year, alldata8$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Albany, NY")

# station 9
plot(alldata9$Year, alldata9$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Glens Falls, NY")

# station 10
plot(alldata10$Year, alldata10$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Syracuse, NY")

# station 11
plot(alldata11$Year, alldata11$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Massena, NY")

# station 12
plot(alldata12$Year, alldata12$TDD,
     type = "h",
     xlab = "Year",
     ylab = "Degrees (C)",
     main = "Annual Thawing Degree Day Accumulation (Jan - June) Watetown Airport, NY")

# TDD accumulation curves ----

# creating for loop for plots
for (i in 1:nrow(AllStn)){
  tavdata1 <- subset(TavData, TavData$StationID == AllStn$station_id[i])
  stn1yrs <- unique(data.frame(Year = tavdata1$Year, Decade = tavdata1$Decade)) 
  stn1yrs$color <- ifelse(stn1yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/tdd_", AllStn$name[i], ".png"), width = 10, height = 10, units = "in", res = 144, pointsize = 15)
  
  # get the base plot with just the first year on there
  plot(tavdata1$DOY[tavdata1$Year == stn1yrs$Year[1]], tavdata1$TDD[tavdata1$Year == stn1yrs$Year[1]],
       type = "l",
       col = stn1yrs$color[1],
       xlab = "Day of Year",
       ylab = "Degrees (C)",
       main = paste("Thawing Degree Days Accumulation", AllStn$name[i]))
  # loop through the rest of the years starting at the second index and add the line onto the plot
  # current year just keeps track of what year we're on to make it easier but we don't have to use it
  for (j in 2:nrow(stn1yrs)){
    current_year = (stn1yrs$Year[j])
    lines(tavdata1$DOY[tavdata1$Year == current_year], tavdata1$TDD[tavdata1$Year == current_year],
          col = stn1yrs$color[j])
  }
  dev.off()
}

# # station 1
# stn1yrs <- unique(data.frame(Year = alldata1$Year, Decade = alldata1$Decade)) 
# stn1yrs$color <- ifelse(stn1yrs$Decade >= 2010, "#FF9900", alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata1$DOY[alldata1$Year == stn1yrs$Year[1]], alldata1$TDD[alldata1$Year == stn1yrs$Year[1]],
#      type = "l",
#      col = stn1yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Boonville, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn1yrs)){
#   current_year = (stn1yrs$Year[i])
#   lines(alldata1$DOY[alldata1$Year == current_year], alldata1$TDD[alldata1$Year == current_year],
#         col = stn1yrs$color[i])
# }
# 
# # station 2
# stn2yrs <- unique(data.frame(Year = alldata2$Year, Decade = alldata2$Decade)) 
# stn2yrs$color <- ifelse(stn2yrs$Decade >= 2010, "#FF9900", alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata2$DOY[alldata2$Year == stn2yrs$Year[1]], alldata2$TDD[alldata2$Year == stn2yrs$Year[1]],
#      type = "l",
#      col = stn2yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Cooperstown, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# for (i in 2:nrow(stn2yrs)){
#   current_year = (stn2yrs$Year[i])
#   if (sum(alldata2$TDD[alldata2$Year == current_year]) == 0){
#     next
#   }
#   lines(alldata2$DOY[alldata2$Year == current_year], alldata2$TDD[alldata2$Year == current_year],
#         col = stn2yrs$color[i])
# }
# 
# 
# 
# # station 3
# stn3yrs <- unique(data.frame(Year = alldata3$Year, Decade = alldata3$Decade)) 
# stn3yrs$color <- ifelse(stn3yrs$Decade >= 2010, "#FF9900", alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata3$DOY[alldata3$Year == stn3yrs$Year[1]], alldata3$TDD[alldata3$Year == stn3yrs$Year[1]],
#      type = "l",
#      col = stn3yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Indian Lake, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # loop through the rest of the years starting at the second index and add the line onto the plot
# for (i in 2:nrow(stn3yrs)){
#   current_year = (stn3yrs$Year[i])
#   if (sum(alldata3$TDD[alldata3$Year == current_year]) == 0){
#     next
#   }
#   lines(alldata3$DOY[alldata3$Year == current_year], alldata3$TDD[alldata3$Year == current_year],
#         col = stn3yrs$color[i])
# }
# 
# # station 4
# stn4yrs <- unique(data.frame(Year = alldata4$Year, Decade = alldata4$Decade)) 
# stn4yrs$color <- ifelse(stn4yrs$Decade >= 2010, "#FF9900", alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata4$DOY[alldata4$Year == stn4yrs$Year[1]], alldata4$TDD[alldata4$Year == stn4yrs$Year[1]],
#      type = "l",
#      col = stn4yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Lowville, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # loop through the rest of the years starting at the second index and add the line onto the plot
# for (i in 2:nrow(stn4yrs)){
#   current_year = (stn4yrs$Year[i])
#   if (sum(alldata4$TDD[alldata4$Year == current_year]) == 0){
#     next
#   }
#   lines(alldata4$DOY[alldata4$Year == current_year], alldata4$TDD[alldata4$Year == current_year],
#         col = stn4yrs$color[i])
# }
# 
# # station 5
# stn5yrs <- unique(data.frame(Year = alldata5$Year, Decade = alldata5$Decade)) 
# stn5yrs$color <- ifelse(stn5yrs$Decade >= 2010, "#FF9900", alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata5$DOY[alldata5$Year == stn5yrs$Year[1]], alldata5$TDD[alldata5$Year == stn5yrs$Year[1]],
#      type = "l",
#      col = stn5yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Norwich, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# for (i in 2:nrow(stn5yrs)){
#   current_year = (stn5yrs$Year[i])
#   lines(alldata5$DOY[alldata5$Year == current_year], alldata5$TDD[alldata5$Year == current_year],
#         col = stn5yrs$color[i])
# }
# 
# # station 6
# stn6yrs <- unique(data.frame(Year = alldata6$Year, Decade = alldata6$Decade)) 
# stn6yrs$color <- ifelse(stn6yrs$Decade >= 2010, "#FF9900", alpha("#00008b", 0.3))
# # highlight just 2012 
# # stn6yrs$color <- ifelse(stn6yrs$Year == 2012, "#FF9900", alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata6$DOY[alldata6$Year == stn6yrs$Year[1]], alldata6$TDD[alldata6$Year == stn6yrs$Year[1]],
#      type = "l",
#      col = stn6yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Oswego, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# for (i in 2:nrow(stn6yrs)){
#   current_year = (stn6yrs$Year[i])
#   if (sum(alldata6$TDD[alldata6$Year == current_year]) == 0){
#     next
#   }
#   lines(alldata6$DOY[alldata6$Year == current_year], alldata6$TDD[alldata6$Year == current_year],
#         col = stn6yrs$color[i])
# }
# 
# # station 7
# stn7yrs <- unique(data.frame(Year = alldata7$Year, Decade = alldata7$Decade)) 
# stn7yrs$color <- ifelse(stn7yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata2$DOY[alldata7$Year == stn7yrs$Year[1]], alldata7$TDD[alldata7$Year == stn7yrs$Year[1]],
#      type = "l",
#      col = stn7yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Watertown, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn7yrs)){
#   current_year = (stn7yrs$Year[i])
#   lines(alldata7$DOY[alldata7$Year == current_year], alldata7$TDD[alldata7$Year == current_year],
#         col = stn7yrs$color[i])
# }
# 
# # station 8
# stn8yrs <- unique(data.frame(Year = alldata8$Year, Decade = alldata8$Decade)) 
# stn8yrs$color <- ifelse(stn8yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata8$DOY[alldata8$Year == stn8yrs$Year[1]], alldata8$TDD[alldata8$Year == stn8yrs$Year[1]],
#      type = "l",
#      col = stn8yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Albany, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn8yrs)){
#   current_year = (stn8yrs$Year[i])
#   lines(alldata8$DOY[alldata8$Year == current_year], alldata8$TDD[alldata8$Year == current_year],
#         col = stn8yrs$color[i])
# }
# 
# # station 9
# stn9yrs <- unique(data.frame(Year = alldata9$Year, Decade = alldata9$Decade)) 
# stn9yrs$color <- ifelse(stn9yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata9$DOY[alldata9$Year == stn9yrs$Year[1]], alldata9$TDD[alldata9$Year == stn9yrs$Year[1]],
#      type = "l",
#      col = stn9yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Glens Falls, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn9yrs)){
#   current_year = (stn9yrs$Year[i])
#   lines(alldata9$DOY[alldata9$Year == current_year], alldata9$TDD[alldata9$Year == current_year],
#         col = stn9yrs$color[i])
# }
# 
# # station 10
# stn10yrs <- unique(data.frame(Year = alldata10$Year, Decade = alldata10$Decade)) 
# stn10yrs$color <- ifelse(stn10yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata10$DOY[alldata10$Year == stn10yrs$Year[1]], alldata10$TDD[alldata10$Year == stn10yrs$Year[1]],
#      type = "l",
#      col = stn10yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Syracuse, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn10yrs)){
#   current_year = (stn10yrs$Year[i])
#   lines(alldata10$DOY[alldata10$Year == current_year], alldata10$TDD[alldata10$Year == current_year],
#         col = stn10yrs$color[i])
# }
# 
# # station 11
# stn11yrs <- unique(data.frame(Year = alldata11$Year, Decade = alldata11$Decade)) 
# stn11yrs$color <- ifelse(stn11yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata11$DOY[alldata11$Year == stn11yrs$Year[1]], alldata11$TDD[alldata11$Year == stn11yrs$Year[1]],
#      type = "l",
#      col = stn11yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Massena, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn11yrs)){
#   current_year = (stn11yrs$Year[i])
#   lines(alldata11$DOY[alldata11$Year == current_year], alldata11$TDD[alldata11$Year == current_year],
#         col = stn11yrs$color[i])
# }
# 
# # station 12
# stn12yrs <- unique(data.frame(Year = alldata12$Year, Decade = alldata12$Decade)) 
# stn12yrs$color <- ifelse(stn12yrs$Decade >= 2010, alpha("#FF9900", 1), alpha("#00008b", 0.3))
# # get the base plot with just the first year on there
# plot(alldata12$DOY[alldata12$Year == stn12yrs$Year[1]], alldata12$TDD[alldata12$Year == stn12yrs$Year[1]],
#      type = "l",
#      col = stn12yrs$color[1],
#      xlab = "DOY",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Days Accumulation Watertown Airport, NY")
# # loop through the rest of the years starting at the second index and add the line onto the plot
# # current year just keeps track of what year we're on to make it easier but we don't have to use it
# for (i in 2:nrow(stn12yrs)){
#   current_year = (stn12yrs$Year[i])
#   lines(alldata12$DOY[alldata12$Year == current_year], alldata12$TDD[alldata12$Year == current_year],
#         col = stn12yrs$color[i])
# }
# 


### Day of Last Freeze ----

# change to just tmin < -2.2 and pick one even colder -- look for dangerous threshold

# create data frame with all days below freezing
LastFreeze <- subset(AllData, AllData$tmin <= 0) 
# find day of last freeze in each year
LastFreeze <- aggregate(LastFreeze$DOY, by = list(LastFreeze$StationID, LastFreeze$StationName, LastFreeze$Year), FUN = "max")
colnames(LastFreeze) <- c("StationID", "StationName", "Year", "DOY")
# join with all data to get the TDD on the day of last freeze
LastFreeze <- inner_join(LastFreeze, TavData, by = c("StationID", "StationName", "Year","DOY"))
LastFreeze <- LastFreeze[c("StationID", "StationName", "Year","DOY", "tmin","TDD")]

# Plots of day of last freeze and accumulated TDD
# set up loop
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  # save as png
  png(paste0(plotDIR[usernumber], "/last_freeze_", AllStn$name[i], ".png"))
  
  # plot
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(current_data$Year, current_data$DOY,
       type = "l",
       col = alpha("black", 0.7),
       xlab = "Year",
       ylab = "DOY of Last Freeze",
       main = "Day of Year of Last Freeze in Boonville, NY")
  par(new = TRUE)
  plot(current_data$Year, current_data$TDD,
       type = "l",
       col = "red",              
       axes = FALSE, 
       xlab = "", 
       ylab = "")
  axis(side = 4, at = pretty(range(current_dataT1$TDD)))      
  mtext("Accumulated TDD (C)", side = 4, line = 3)
  
  dev.off()
}

# # station 1
# 
# 
# # station 2
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00301752"], LastFreeze$DOY[LastFreeze$StationID == "USC00301752"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Cooperstown, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00301752"], LastFreeze$TDD[LastFreeze$StationID == "USC00301752"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00301752"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 3
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00304102"], LastFreeze$DOY[LastFreeze$StationID == "USC00304102"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Indian Lake, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00304102"], LastFreeze$TDD[LastFreeze$StationID == "USC00304102"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00304102"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 4
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00304912"], LastFreeze$DOY[LastFreeze$StationID == "USC00304912"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Lowville, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00304912"], LastFreeze$TDD[LastFreeze$StationID == "USC00304912"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00304912"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 5
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306085"], LastFreeze$DOY[LastFreeze$StationID == "USC00306085"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Norwich, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306085"], LastFreeze$TDD[LastFreeze$StationID == "USC00306085"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00306085"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 6
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306314"], LastFreeze$DOY[LastFreeze$StationID == "USC00306314"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Oswego, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306314"], LastFreeze$TDD[LastFreeze$StationID == "USC00306314"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00306314"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 7
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00309000"], LastFreeze$DOY[LastFreeze$StationID == "USC00309000"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Watertown, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00309000"], LastFreeze$TDD[LastFreeze$StationID == "USC00309000"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00309000"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 8
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014735"], LastFreeze$DOY[LastFreeze$StationID == "USW00014735"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Albany, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014735"], LastFreeze$TDD[LastFreeze$StationID == "USW00014735"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USW00014735"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 9
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014750"], LastFreeze$DOY[LastFreeze$StationID == "USW00014750"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Glens Falls, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014750"], LastFreeze$TDD[LastFreeze$StationID == "USW00014750"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USW00014750"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 10
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014771"], LastFreeze$DOY[LastFreeze$StationID == "USW00014771"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Syracuse, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014771"], LastFreeze$TDD[LastFreeze$StationID == "USW00014771"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USW00014771"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 11
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00094725"], LastFreeze$DOY[LastFreeze$StationID == "USW00094725"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Massena, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00094725"], LastFreeze$TDD[LastFreeze$StationID == "USW00094725"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USW00094725"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)
# 
# # station 12
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00094790"], LastFreeze$DOY[LastFreeze$StationID == "USW00094790"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Watertown Airport, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00094790"], LastFreeze$TDD[LastFreeze$StationID == "USW00094790"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USW00094790"])))      
# mtext("Accumulated TDD (C)", side = 4, line = 3)

# plots of minimum temperature on day of last freeze 
# station 1
plot(LastFreeze$Year[LastFreeze$StationID == "USC00300785"], LastFreeze$tmin[LastFreeze$StationID == "USC00300785"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Boonville, NY")

# station 2
plot(LastFreeze$Year[LastFreeze$StationID == "USC00301752"], LastFreeze$tmin[LastFreeze$StationID == "USC00301752"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Cooperstown, NY")

# station 3
plot(LastFreeze$Year[LastFreeze$StationID == "USC00304102"], LastFreeze$tmin[LastFreeze$StationID == "USC00304102"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Indian Lake, NY")

# station 4
plot(LastFreeze$Year[LastFreeze$StationID == "USC00304912"], LastFreeze$tmin[LastFreeze$StationID == "USC00304912"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Lowville, NY")

# station 5
plot(LastFreeze$Year[LastFreeze$StationID == "USC00306085"], LastFreeze$tmin[LastFreeze$StationID == "USC00306085"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Norwich, NY")

# station 6
plot(LastFreeze$Year[LastFreeze$StationID == "USC00306314"], LastFreeze$tmin[LastFreeze$StationID == "USC00306314"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Oswego, NY")

# station 7
plot(LastFreeze$Year[LastFreeze$StationID == "USC00309000"], LastFreeze$tmin[LastFreeze$StationID == "USC00309000"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Watertown, NY")

# station 8
plot(LastFreeze$Year[LastFreeze$StationID == "USW00014735"], LastFreeze$tmin[LastFreeze$StationID == "USW00014735"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Albany, NY")

# station 9
plot(LastFreeze$Year[LastFreeze$StationID == "USW00014750"], LastFreeze$tmin[LastFreeze$StationID == "USW00014750"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Glens Falls, NY")

# station 10
plot(LastFreeze$Year[LastFreeze$StationID == "USW00014771"], LastFreeze$tmin[LastFreeze$StationID == "USW00014771"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Syracuse, NY")

# station 11
plot(LastFreeze$Year[LastFreeze$StationID == "USW00094725"], LastFreeze$tmin[LastFreeze$StationID == "USW00094725"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Massena, NY")

# station 12
plot(LastFreeze$Year[LastFreeze$StationID == "USW00094790"], LastFreeze$tmin[LastFreeze$StationID == "USW00094790"],
     type = "l",
     col = "deepskyblue3",
     xlab = "Year",
     ylab = "Temperature (C)",
     main = "TMIN on Day of Last Freeze in Watertown Airport, NY")

### Number of spring days below freezing ----
FreezeDays <- AllData[AllData$DayType == 1,1:6]
FreezeDays <- aggregate(FreezeDays$DOY, by = list(FreezeDays$StationID, FreezeDays$StationName, FreezeDays$Year, FreezeDays$Month), FUN = "length")
colnames(FreezeDays) <- c("StationID", "StationName", "Year", "Month","FreezeDays")

# station 1
plot(FreezeDays$Year[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Mar"],
     type = "h",
     col = "lightskyblue",
     lwd = 2, 
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Boonville, NY")
lines(FreezeDays$Year[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Apr"],
       type = "h",
       lwd = 2,
       col = "green4")
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 2
plot(FreezeDays$Year[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Mar"],
     type = "h",
     lwd = 2, 
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Cooperstown, NY")
lines(FreezeDays$Year[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Apr"],
      type = "h",
      lwd = 2, 
      col = "green4")
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 3
plot(FreezeDays$Year[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Mar"],
     type = "h",
     lwd = 2, 
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Indian Lake, NY")
lines(FreezeDays$Year[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Apr"],
      type = "h",
      lwd = 2, 
      col = "green4")
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 4
plot(FreezeDays$Year[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Mar"],
     type = "h",
     lwd = 2, 
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Lowville, NY")
lines(FreezeDays$Year[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Apr"],
      type = "h",
      lwd = 2, 
      col = "green4")
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 5
plot(FreezeDays$Year[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Mar"],
     type = "h",
     lwd = 2, 
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Norwich, NY")
lines(FreezeDays$Year[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Apr"],
      type = "h",
      lwd = 2, 
      col = "green4")
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 6
plot(FreezeDays$Year[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Mar"],
     type = "h",
     lwd = 2, 
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Oswego, NY")
lines(FreezeDays$Year[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Apr"],
      type = "h",
      lwd = 2, 
      col = "green4")
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 7
plot(FreezeDays$Year[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Mar"],
     type = "h",
     pch = 20,
     lwd = 2,
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Watertown, NY")
points(FreezeDays$Year[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Apr"],
       type = "h",
       col = "green4",
       lwd = 2)
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 8
plot(FreezeDays$Year[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Mar"],
     type = "h",
     pch = 20,
     lwd = 2,
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Albany, NY")
points(FreezeDays$Year[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Apr"],
       type = "h",
       col = "green4",
       lwd = 2)
legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 9
plot(FreezeDays$Year[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Mar"],
     type = "h",
     pch = 20,
     lwd = 2,
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Glens Falls, NY")
points(FreezeDays$Year[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Apr"],
       type = "h",
       col = "green4",
       lwd = 2)
legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 10
plot(FreezeDays$Year[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Mar"],
     type = "h",
     pch = 20,
     lwd = 2,
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Syracuse, NY")
points(FreezeDays$Year[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Apr"],
       type = "h",
       col = "green4",
       lwd = 2)
legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 11
plot(FreezeDays$Year[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Mar"],
     type = "h",
     pch = 20,
     lwd = 2,
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Massena, NY")
points(FreezeDays$Year[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Apr"],
       type = "h",
       col = "green4",
       lwd = 2)
legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

# station 12
plot(FreezeDays$Year[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Mar"],
     type = "h",
     pch = 20,
     lwd = 2,
     col = "lightskyblue",
     xlab = "Year",
     ylab = "Number of Days",
     main = "Days below Freezing in Watertown Airport, NY")
points(FreezeDays$Year[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Apr"],
       type = "h",
       col = "green4",
       lwd = 2)
legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)


# growing degree days ----
# apples base = 41 degrees F

# look at tmin below freezing ()

# Norwich 
plot(alldata5$DOY[alldata5$Year == 2012], alldata5$GDD41[alldata5$Year == 2012],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Growing Degree Days",
     main = "Apple Growing Degree Days Accumulation Norwich, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red4")
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USC00306085" & LastFreeze$Year == 2012])

# boonville 
plot(alldata1$DOY[alldata1$Year == 2012], alldata1$GDD41[alldata1$Year == 2012],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Growing Degree Days",
     main = "Apple Growing Degree Days Accumulation Boonville, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red4")
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USC00300785" & LastFreeze$Year == 2012])

# syracuse
plot(alldata10$DOY[alldata10$Year == 2012], alldata10$GDD41[alldata10$Year == 2012],
     type = "l",
     col = "deepskyblue3",
     xlab = "DOY",
     ylab = "Growing Degree Days",
     main = "Apple Growing Degree Days Accumulation Syracuse, NY")
abline(h = 100, col = "red3")
abline(h = 400, col = "red4")
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USW00014771" & LastFreeze$Year == 2012])



