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
# do we need an na.rm here?
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
# #set up colors based on major zone
# MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
# #colors
# MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
#                     "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
#                     "#576356","#31474f" )
# #add colors to plot back in
# # ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
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
# #set up colors based on major zone
# MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
# #colors
# MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
#                     "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
#                     "#576356","#31474f" )
# #add colors to plot back in
# ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
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
# #set up colors based on major zone
# MajorZones <- data.frame(MAJOR = unique(ez@data$MAJOR))
# #colors
# MajorZones$col <- c("#e28946",	"#ebb355","#db5236","#36638f","#74a1c3",
#                     "#df9880",	"#8687c1","#4069bf","#0d4247",	"#ff5b3e",
#                     "#576356","#31474f" )
# #add colors to plot back in
# ez@data <- left_join(ez@data,MajorZones, by="MAJOR")
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

# create a data frame with all years and stations to join in with AllData to fill in missing years
# StnList <- as.vector(AllStn$station_id)
# AllYear <- data.frame(year=seq(1890,2020))
# 
# AllYear$id = rep(StnList1, times = 131)
# 
# # create a data frame with all years, stations, and months back to join in with AllData to fill in missing years
# AllMonth <- data.frame(year=rep(seq(1890,2020), each = 12, times = 6))
# AllMonth$id <- rep(StnList, times = 131*6)
# AllMonth$Month <- rep(month(c(1, 2, 3, 4, 5, 6), label = TRUE), each = 12, times = 131)

# join tmax, tmin, and prcp data
AllDataT1 <- full_join(TmaxData, TminData, by = c("id"="id", "date" = "date", "year"="year", "DOY" = "DOY"))
AllDataT2 <- full_join(AllDataT1, PrcpData, by = c("id"="id",  "date" = "date", "year"="year", "DOY" = "DOY"))

# add station names of the 12 good stations
AllData <- inner_join(AllDataT2, AllStn, by = c("id"="station_id"))

# add month column
AllData$Month <- month(AllData$date, label = TRUE)

# add decade column
AllData$Decade <- AllData$year - (AllData$year %% 10)

# adding back in missing years
# AllData <- full_join(AllData, AllMonth, by = c("year" = "year", "id" ="id", "Month" = "Month"))

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

# Making Day Type a Factor
#AllData$DayType <- as.factor(AllData$DayType)

# Making station column a factor
#AllData$StationID <- as.factor(AllData$StationID)

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

# 
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

# # join with alldata to fill in missing months
# SpringMonths <- full_join(SpringMonths, AllMonth[AllMonth$Month %in% c("Mar","Apr","May"),], by = (c("year" = "year", "StationID" = "id", "month" = "Month")))

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
  
  plot(SpringYear$year[SpringYear$StationID == AllStn$station_id[6]], 
       SpringYear$tmax[SpringYear$StationID == AllStn$station_id[6]],
       ylab = "max temp",
       xlab = "year",
       pch = 20)
  abline(current.mod)
}

current.mod <- lm(SpringYear$tmax[SpringYear$StationID == AllStn$station_id[6]] ~ SpringYear$year[SpringYear$StationID == AllStn$station_id[6]])

plot(SpringYear$year[SpringYear$StationID == AllStn$station_id[6]], 
     SpringYear$tmax[SpringYear$StationID == AllStn$station_id[6]],
     ylab = "max temp",
     xlab = "year",
     pch = 20)
abline(current.mod)


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

# # station 1
# ggplot(data = stn1, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn1$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Boonville, NY")
# 
# # station 2
# ggplot(data = stn2, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn2$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Cooperstown, NY")
# 
# # station 3
# ggplot(data = stn3, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn3$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Indian Lake, NY")
# 
# # station 4
# ggplot(data = stn4, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn4$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Lowville, NY")
# 
# # station 5
# ggplot(data = stn5, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn5$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Norwich, NY")
# 
# # station 6
# ggplot(data = stn6, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn6$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Oswego, NY")
# 
# # station 7
# ggplot(data = stn7, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn7$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Watertown, NY")
# 
# # station 8
# ggplot(data = stn8, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn8$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Albany, NY")
# 
# # station 9
# ggplot(data = stn9, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn9$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Glens Falls, NY")
# 
# # station 10
# ggplot(data = stn10, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn10$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Syracuse, NY")
# 
# # station 11
# ggplot(data = stn11, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn11$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Massena, NY")
# 
# # station 12
# ggplot(data = stn12, aes(x = year, y = FTdays)) +
#   geom_bar(position = "dodge", stat="identity", fill = ifelse(stn12$FTdays > 20, "tomato3", "deepskyblue3"))+
#   theme_classic()+
#   labs(x = "Year", y = "Number of Freeze Thaw Days", title = "Spring Freeze Thaw Days in Watertown Airport, NY")
# 

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
# 
# # station 1
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00300785",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Boonville, NY")
# 
# # station 2
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00301752",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Cooperstown Airport, NY")
# 
# # station 3
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00304102",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Indian Lake, NY")
# 
# # station 4
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00304912",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Lowville, NY")
# 
# # station 5
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00306085",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Norwich, NY")
# 
# # station 6
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00306314",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Oswego, NY")
# 
# # station 7 
# ggplot(data = SpringDecade[SpringDecade$StationID == "USC00309000",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Watertown, NY")
# 
# # station 8
# ggplot(data = SpringDecade[SpringDecade$StationID == "USW00014735",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Albany, NY")
# 
# # station 9
# ggplot(data = SpringDecade[SpringDecade$StationID == "USW00014750",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Glens Falls, NY")
# 
# # station 10
# ggplot(data = SpringDecade[SpringDecade$StationID == "USW00014771",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Syracuse, NY")
# 
# # station 11
# ggplot(data = SpringDecade[SpringDecade$StationID == "USW00094725",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Massena, NY")
# 
# # station 12
# ggplot(data = SpringDecade[SpringDecade$StationID == "USW00094790",], aes(x = Decade, y = FTrange, color = Month))+
#   geom_point() +
#   geom_line() +
#   theme_classic()+
#   labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Watertown Airport, NY")


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
# # Station 1
# # raw temps
# stn1all <- subset(SpringData, SpringData$StationID == "USC00300785")
# ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Spring Temperatures: Boonville, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white")
# 
# # standardized anomalies
# ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Standardized Daily Temperature Anomalies: Boonville, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# 
# # raw anomalies
# ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Temperature Anomalies: Boonville, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # Station 2
# # raw temps
# stn2all <- subset(SpringData, SpringData$StationID == "USC00301752")
# ggplot(data = stn2all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Spring Temperatures: Cooperstown, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white")
# # standardized anomalies
# ggplot(data = stn2all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Standardized Daily Temperature Anomalies: Cooperstown, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # raw anomalies
# ggplot(data = stn1all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Temperature Anomalies: Cooperstown, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # Station 3
# # raw temps
# stn3all <- subset(SpringData, SpringData$StationID == "USC00304102")
# ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Spring Temperatures: Indian Lake, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white")
# # standardized anomalies
# ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Standardized Daily Temperature Anomalies: Indian Lake, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # raw anomalies
# ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Temperature Anomalies: Indian Lake, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # Station 4
# # raw temps
# stn4all <- subset(SpringData, SpringData$StationID == "USC00304912")
# ggplot(data = stn3all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Spring Temperatures: Lowville, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",)
# # standardized anomalies
# ggplot(data = stn4all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Standardized Daily Temperature Anomalies: Lowville, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # raw anomalies
# ggplot(data = stn4all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Temperature Anomalies: Lowville, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# 
# # Station 5
# # raw temps
# stn5all <- subset(SpringData, SpringData$StationID == "USC00306085")
# ggplot(data = stn5all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Spring Temperatures: Norwich, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white")
# # standardized anomalies
# ggplot(data = stn5all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Standardized Daily Temperature Anomalies: Norwich, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# # raw anomalies
# ggplot(data = stn5all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   labs(title = "Daily Temperature Anomalies: Norwich, NY")+
#   geom_hline(yintercept = c(1, 32, 62))+
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") 
# 
# # Station 6
# # raw temps
# stn6all <- subset(SpringData, SpringData$StationID == "USC00306314")
# ggplot(data = stn6all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Oswego, NY")
# 
# # standardized anomalies
# ggplot(data = stn6all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Oswego, NY")
# 
# # raw anomalies
# ggplot(data = stn6all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Oswego, NY")
# 
# # Station 7
# # raw temps
# stn7all <- subset(SpringData, SpringData$StationID == "USC00309000")
# ggplot(data = stn7all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Watertown, NY")
# 
# # standardized anomalies
# ggplot(data = stn7all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Watertown, NY")
# 
# # raw anomalies
# ggplot(data = stn7all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Watertown, NY")
# 
# # Station 8
# # raw temps
# stn8all <- subset(SpringData, SpringData$StationID == "USW00014735")
# ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Albany, NY")
# 
# # standardized anomalies
# ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Albany, NY")
# 
# # raw anomalies
# ggplot(data = stn8all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Albany, NY")
# 
# # Station 9
# # raw temps
# stn9all <- subset(SpringData, SpringData$StationID == "USW00014750")
# ggplot(data = stn9all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Glens Falls, NY")
# 
# # standardized anomalies
# ggplot(data = stn9all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Glens Falls, NY")
# 
# # raw anomalies
# ggplot(data = stn9all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Glens Falls, NY")
# 
# # Station 10
# # raw temps
# stn10all <- subset(SpringData, SpringData$StationID == "USW00014771")
# ggplot(data = stn10all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Syracuse, NY")
# 
# # standardized anomalies
# ggplot(data = stn10all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Syracuse, NY")
# 
# # raw anomalies
# ggplot(data = stn10all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Syracuse, NY")
# 
# # Station 11
# # raw temps
# stn11all <- subset(SpringData, SpringData$StationID == "USW00094725")
# ggplot(data = stn11all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Massena, NY")
# 
# # standardized anomalies
# ggplot(data = stn11all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Massena, NY")
# 
# # raw anomalies
# ggplot(data = stn11all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Massena, NY")
# 
# # Station 12
# # raw temps
# stn12all <- subset(SpringData, SpringData$StationID == "USW00094790")
# ggplot(data = stn12all, mapping = aes(x = Year, y = DayID, fill = tav)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature (c)",
#                        low = "#2166ac",
#                        mid = "#d8daeb",
#                        high = "#b2182b",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperatures in Watertown Airport, NY")
# 
# # standardized anomalies
# ggplot(data = stn12all, mapping = aes(x = Year, y = DayID, fill = AnStd)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Standardized Spring Temperature Anomalies in Watertown Airport, NY")
# 
# # raw anomalies
# ggplot(data = stn12all, mapping = aes(x = Year, y = DayID, fill = AnRaw)) +
#   geom_tile() +
#   theme_classic() +
#   scale_y_continuous("Month", breaks = c(1, 32, 62), labels = c("March", "April", "May")) +
#   scale_fill_gradient2(name = "Temperature Anomaly",
#                        low = "#4575b4",
#                        mid = "#ffffbf",
#                        high = "#d73027",
#                        na.value = "white") +
#   geom_hline(yintercept = c(1, 32, 62))+
#   labs(title = "Spring Temperature Anomalies in Watertown Airport, NY")

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
# 
# # station 1
# alldata1$DayType <- as.factor(alldata1$DayType)
# ggplot(data= alldata1, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Boonville, NY")
# 
# # station 2 
# ggplot(data= alldata2, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Cooperstown, NY")
# 
# # station 3 
# ggplot(data= alldata3, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Indian Lake, NY")
# 
# # station 4
# ggplot(data= alldata4, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Lowville, NY")
# 
# # station 5
# ggplot(data= alldata5, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Norwich, NY")
# 
# # station 6
# ggplot(data= alldata6, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Oswego, NY")
# 
# # station 7
# ggplot(data= alldata7, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Watertown, NY")
# 
# # station 8
# ggplot(data= alldata8, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Albany, NY")
# 
# # station 9
# ggplot(data= alldata9, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Glens Falls, NY")
# 
# # station 10
# ggplot(data= alldata10, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Syracuse, NY")
# 
# # station 11
# ggplot(data= alldata11, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Massena, NY")
# 
# # station 12
# ggplot(data= alldata12, mapping = aes(x= Year, y = DOY, fill = DayType))+
#   geom_tile() +
#   theme_classic()+
#   scale_y_continuous("Month", breaks = c(1, 32, 61, 93, 124, 156), labels = c("January", "February", "March", "April", "May","June")) +
#   scale_fill_manual(name = "Day Types", values = c("#3399FF", "#FFFF99", "#EE6F6F"), na.value = "white", labels = c("Freezing", "Freeze-Thaw", "Warm", "Missing Data"))+
#   labs(title = "Types of Days in Watertown Airport, NY")

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

# # station 1 
# plot(alldata1$Year, alldata1$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Day Accumulation (Jan - June) Boonville, NY")
# 
# # station 2
# plot(alldata2$Year, alldata2$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Day Accumulation (Jan - June) Cooperstown, NY")
# 
# # station 3
# plot(alldata3$Year, alldata3$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Day Accumulation (Jan - June) Indian Lake, NY")
# 
# # station 4
# plot(alldata4$Year, alldata4$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Thawing Degree Day Accumulation (Jan - June) Lowville, NY")
# 
# # station 5
# plot(alldata5$Year, alldata5$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Norwich, NY")
# 
# # station 6
# plot(alldata6$Year, alldata6$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Oswego, NY")
# 
# # station 7
# plot(alldata7$Year, alldata7$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Watertown, NY")
# 
# # station 8
# plot(alldata8$Year, alldata8$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Albany, NY")
# 
# # station 9
# plot(alldata9$Year, alldata9$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Glens Falls, NY")
# 
# # station 10
# plot(alldata10$Year, alldata10$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Syracuse, NY")
# 
# # station 11
# plot(alldata11$Year, alldata11$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Massena, NY")
# 
# # station 12
# plot(alldata12$Year, alldata12$TDD,
#      type = "h",
#      xlab = "Year",
#      ylab = "Degrees (C)",
#      main = "Annual Thawing Degree Day Accumulation (Jan - June) Watetown Airport, NY")

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
# for (i in 2:nrow(stn3yrs)){
#   current_year = (stn3yrs$Year[i])
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
# for (i in 2:nrow(stn4yrs)){
#   current_year = (stn4yrs$Year[i])
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

### Day of Last Freeze ----
# creating last freeze data frame 
LastFreeze <- subset(AllData, AllData$tmin <= 0) 
# finding last day of freeze in each year
LastFreeze <- aggregate(LastFreeze$DOY, by = list(LastFreeze$StationID, LastFreeze$StationName, LastFreeze$Year), FUN = "max")
colnames(LastFreeze) <- c("StationID", "StationName", "Year", "DOY")
# joining with all data to get the TDD on the day of last freeze
LastFreeze <- inner_join(LastFreeze, TavData, by = c("StationID", "StationName", "Year","DOY"))
LastFreeze <- LastFreeze[c("StationID", "StationName", "Year","DOY", "tmin", "TDD")]


# Plots of day of last freeze with thawing degree days
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/lf_tdd_", AllStn$name[i], ".png"))

  # get the base plot with just the first year on there
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(current_data$Year, current_data$DOY,
       type = "l",
       col = alpha("black", 0.7),
       xlab = "Year",
       ylab = "DOY of Last Freeze",
       main = paste0("Day of Year of Last Freeze in ", AllStn$name[i],", NY"))
  par(new = TRUE)
  plot(current_data$Year, current_data$TDD,
       type = "l",
       lwd = 1.5,
       col = "red",              
       axes = FALSE, 
       xlab = "", 
       ylab = "")
  axis(side = 4, at = pretty(range(current_dataT1$TDD)))
  mtext("Accumulated TDD (C)", side = 4, line = 3)  
  
  dev.off()
}
# 
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00300785"], LastFreeze$DOY[LastFreeze$StationID == "USC00300785"],
#      type = "l",
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "Day of Year of Last Freeze in Boonville, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00300785"], LastFreeze$TDD[LastFreeze$StationID == "USC00300785"],
#      type = "l",
#      col = "red",              
#      axes = FALSE, 
#      xlab = "", 
#      ylab = "")
# axis(side = 4, at = pretty(range(LastFreeze$TDD[LastFreeze$StationID == "USC00300785"])))      # Add second axis
# mtext("Accumulated TDD (C)", side = 4, line = 3) 
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


# testing how to fill in missing values
# would apply to these graphs and graphs above
# ggplot removes values but base r doesnt but ggplot wont let us have 
# two y axis that aren't transofrmations of each other
current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[2])
current_range <- data.frame(year=seq(AllStn[2, 5], 2019))
current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
current_data$tmin <- ifelse(is.na(current_data$tmin), NaN, current_data$tmin) 
current_data$DOY <- ifelse(is.na(current_data$tmin), NaN, current_data$DOY) 

current_data2 <- current_data[order(current_data$Year),]

# # saving plot as a png
# png(paste0(plotDIR[usernumber], "/lf_temp_", AllStn$name[i], ".png"))

par(mar = c(5, 4, 4, 4) + 0.3)
plot(current_data2$Year, current_data2$DOY,
     type = "l",
     lwd = 1.5,
     pch = 20,
     col = alpha("black", 0.7),
     xlab = "Year",
     ylab = "DOY of Last Freeze",
     main = paste0("TMIN on Day of Last Freeze in ", AllStn$name[2], ", NY"),
     ylim = c(100,190))
par(new = TRUE)
plot(current_data2$Year, current_data2$tmin,
     type = "l",
     col = alpha("deepskyblue3",0.7),
     lwd = 1.5,
     xlab = "",
     ylab = "",
     axes = FALSE,
     ylim = c(-10, 0))
axis(side = 4, at = seq(-10, 0, by = 2))
mtext("Minimum Temperature (C)", side = 4, line = 3) 


# plots of temperature on day of last freeze
for (i in 1:nrow(AllStn)){
  current_dataT1 <- subset(LastFreeze, LastFreeze$StationID == AllStn$station_id[i])
  current_range <- data.frame(year=seq(AllStn[i, 5], 2019))
  current_data <- full_join(current_dataT1, current_range, by = c("Year" = "year"))
  
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/lf_temp_", AllStn$name[i], ".png"))
  
  # create bar plot of DOY last freeze and temperature
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(current_data$Year, current_data$DOY,
       type = "l",
       col = alpha("black", 0.7),
       xlab = "Year",
       ylab = "DOY of Last Freeze",
       main = paste0("TMIN on Day of Last Freeze in ", AllStn$name[i], ", NY"))
  par(new = TRUE)
  plot(current_data$Year, current_data$tmin,
       type = "l",
       col = "deepskyblue3",
       lwd = 1.5,
       xlab = "",
       ylab = "",
       axes = FALSE)
  axis(side = 4, at = pretty(range(current_dataT1$tmin)))
  mtext("Minimum Temperature (C)", side = 4, line = 3) 
  
  dev.off()
}
# 
# # station 6
# par(mar = c(5, 4, 4, 4) + 0.3)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306314"], LastFreeze$DOY[LastFreeze$StationID == "USC00306314"],
#      type = "l",
#      col = alpha("black", 0.7),
#      xlab = "Year",
#      ylab = "DOY of Last Freeze",
#      main = "TMIN on Day of Last Freeze in Oswego, NY")
# par(new = TRUE)
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306314"], LastFreeze$tmin[LastFreeze$StationID == "USC00306314"],
#      type = "l",
#      col = "deepskyblue3",
#      lwd = 1.5,
#      xlab = "",
#      ylab = "",
#      axes = FALSE)
# axis(side = 4, at = pretty(range(LastFreeze$tmin[LastFreeze$StationID == "USC00306314"])))
# mtext("Minimum Temperature (C)", side = 4, line = 3) 
# 
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00306314"], LastFreeze$tmin[LastFreeze$StationID == "USC00306314"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Oswego, NY")
# 
# # station 7
# plot(LastFreeze$Year[LastFreeze$StationID == "USC00309000"], LastFreeze$tmin[LastFreeze$StationID == "USC00309000"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Watertown, NY")
# 
# # station 8
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014735"], LastFreeze$tmin[LastFreeze$StationID == "USW00014735"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Albany, NY")
# 
# # station 9
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014750"], LastFreeze$tmin[LastFreeze$StationID == "USW00014750"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Glens Falls, NY")
# 
# # station 10
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00014771"], LastFreeze$tmin[LastFreeze$StationID == "USW00014771"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Syracuse, NY")
# 
# # station 11
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00094725"], LastFreeze$tmin[LastFreeze$StationID == "USW00094725"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Massena, NY")
# 
# # station 12
# plot(LastFreeze$Year[LastFreeze$StationID == "USW00094790"], LastFreeze$tmin[LastFreeze$StationID == "USW00094790"],
#      type = "l",
#      col = "deepskyblue3",
#      xlab = "Year",
#      ylab = "Temperature (C)",
#      main = "TMIN on Day of Last Freeze in Watertown Airport, NY")
# 

### Cold Snaps in Late Spring ----
# subsetting to just freezing days
# make a graph of -5 threshold for 
FreezeDays <- AllData[AllData$tmin <= 0,1:6]
FreezeDays <- aggregate(FreezeDays$DOY, by = list(FreezeDays$StationID, FreezeDays$StationName, FreezeDays$Year, FreezeDays$Month), FUN = "length")
colnames(FreezeDays) <- c("StationID", "StationName", "Year", "Month", "FreezeDays")

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
# 
# # station 1
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Boonville, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00300785" & FreezeDays$Month == "Apr"],
#       type = "h",
#       col = "green4",
#       lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 2
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      ylim = c(0,20),
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Cooperstown, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00301752" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 3
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Indian Lake, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304102" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 4
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Lowville, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00304912" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 5
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Norwich, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306085" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 6
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Oswego, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00306314" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 7
# plot(FreezeDays$Year[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Watertown, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USC00309000" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 8
# plot(FreezeDays$Year[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Albany, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014735" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topright", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 9
# plot(FreezeDays$Year[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Glens Falls, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014750" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 10
# plot(FreezeDays$Year[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Syracuse, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00014771" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 11
# plot(FreezeDays$Year[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Massena, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094725" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)
# 
# # station 12
# plot(FreezeDays$Year[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Mar"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Mar"],
#      type = "h",
#      pch = 20,
#      lwd = 2,
#      col = "lightskyblue",
#      xlab = "Year",
#      ylab = "Number of Days",
#      main = "Days below Freezing in Massena, NY")
# points(FreezeDays$Year[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Apr"], FreezeDays$FreezeDays[FreezeDays$StationID == "USW00094790" & FreezeDays$Month == "Apr"],
#        type = "h",
#        col = "green4",
#        lwd = 2)
# legend("topleft", c("March","April"), col = c("lightskyblue","green4"), lwd = 2, bty = "n", cex = .5)

### Apple Growing Degree Days ----
# Apples plots for 2012
# make list of stations that have 2012 for tavdata
for (i in 1:nrow(AllStn)){
  current_data <- subset(TavData, StationID == AllStn$station_id[i])
  # do some kind of check to see if 2012 exists at the station
  ifelse(current_data)
  # saving plot as a png
  png(paste0(plotDIR[usernumber], "/apple_gdd_", AllStn$name[i], ".png"))
  
  # create bar plot of DOY last freeze and temperature
  plot(current_data$DOY[current_data$Year == 2012], current_data$GDD41[current_data$Year == 2012],
       type = "l",
       col = "deepskyblue3",
       xlab = "DOY",
       ylab = "Degrees (C)",
       main = paste0("2012 Apple Growing Degree Days in", AllStn$name[i], ", NY"))
  abline(h = 100, col = "red3")
  abline(h = 400, col = "red3")
  abline(v = LastFreeze$DOY[LastFreeze$StationID == AllStn$station_id[i] & LastFreeze$Year == 2012])
  
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
abline(v = LastFreeze$DOY[LastFreeze$StationID == "USC00300785" & LastFreeze$Year == 2012])
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

# Looking at hard freezes (<-5)
# creating last freeze data frame 
LastHardFreeze <- subset(AllData, AllData$tmin <= -5) 
# finding last day of freeze in each year
LastHardFreeze <- aggregate(LastHardFreeze$DOY, by = list(LastHardFreeze$StationID, LastHardFreeze$StationName, LastHardFreeze$Year), FUN = "max")
colnames(LastHardFreeze) <- c("StationID", "StationName", "Year", "DOY")
# joining with all data to get the TDD on the day of last freeze
LastHardFreeze <- inner_join(LastHardFreeze, TavData, by = c("StationID", "StationName", "Year","DOY"))
LastHardFreeze <- LastHardFreeze[c("StationID", "StationName", "Year","DOY", "tmin", "TDD")]

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


