#updated 12/15

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
AllData$Month <- month(AllData$date)

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
SpringData <- subset(AllData, AllData$Month %in% c(3,4,5))



# make a data frame of yearly averages
SpringYear <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID, SpringData$StationName, SpringData$Month), FUN="mean", na.rm = TRUE)
colnames(SpringYear) <- c("year","StationID","StationName", "month","tmax")
SpringYear$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Month), FUN="mean", na.rm = TRUE)$x
SpringYear$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID,SpringData$StationName,SpringData$Month), FUN="mean", na.rm = TRUE)$x

# add columns of extreme hi and lo temperature values
SpringYear$ExtHi <- aggregate(SpringData$HiTmax, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringYear$ExtLo <- aggregate(SpringData$LoTmin, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN = "mean", na.rm = TRUE)$x
# add columns counting extreme temp flags
SpringYear$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month) , FUN = "sum", na.rm = TRUE)$x
SpringYear$ExLoCount <- aggregate(SpringData$ExtrLo, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN = "sum", na.rm = TRUE)$x

# add columns with freeze thaw flags and range
SpringYear$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN="sum", na.rm = TRUE)$x
SpringYear$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID, SpringData$StationName,SpringData$Month), FUN="mean", na.rm = TRUE)$x

# create new data frame by decade
SpringDecade <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)
colnames(SpringDecade) <- c("StationID", "StationName", "Decade", "Month", "ExtHi")
SpringDecade$ExtLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecade$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x


# subset to specific months
# all march data
MarData <- subset(AllData, AllData$Month == 3)
# march yearly averages
MarYear <- subset(SpringYear, SpringYear$month == 3)
# march decade averages
MarDecade <- subset(SpringDecade, SpringDecade$Month == 3)
 
# all april data
AprData <- subset(AllData, AllData$Month == 4)
# april yearly averages
AprYear <- subset(SpringYear, SpringYear$month == 4)
# april decade averages
AprDecade <- subset(SpringDecade, SpringDecade$Month == 4)

# all may data
MayData <- subset(AllData, AllData$Month == 5)
# may yearly averages
MayYear <- subset(SpringYear, SpringYear$month == 5)
# may decade averages
MayDecade <- subset(SpringDecade, SpringDecade$Month == 5)

### general temperature trends ----

# line graphs of tmax, tmin, tav over time
# station 1 - Boonville 
stn1 <- subset(SpringYear, SpringYear$station == "USC00300785")
ggplot(data = stn1, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Boonville, NY")

# station 2 - Cooperstown
stn2 <- subset(SpringYear, SpringYear$station == "USC00301752")
ggplot(data = stn2, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Cooperstown, NY")

# station 3 - Indian Lake
stn3 <- subset(SpringYear, SpringYear$station == "USC00304102")
ggplot(data = stn3, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Indian Lake, NY")

# station 4 - Lowville
stn4 <- subset(SpringYear, SpringYear$station == "USC00304912")
ggplot(data = stn4, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Lowville, NY")

# station 5 - Norwich
stn5 <- subset(SpringYear, SpringYear$station == "USC00306085")
ggplot(data = stn5, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Norwich, NY")

# station 6  - Oswego
stn6 <- subset(SpringYear, SpringYear$station == "USC00306314")
ggplot(data = stn6, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Oswego, NY")

# station 7 - Watertown
stn7 <- subset(SpringYear, SpringYear$station == "USC00309000")
ggplot(data = stn7, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Watertown, NY")

# station 8 - Albany
stn8 <- subset(SpringYear, SpringYear$station == "USW00014735")
ggplot(data = stn8, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Albany, NY")

# station 9 - Glens Falls
stn9 <- subset(SpringYear, SpringYear$station == "USW00014750")
ggplot(data = stn9, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Glens Falls, NY")

# station 10 - Syracuse
stn10 <- subset(SpringYear, SpringYear$station == "USW00014771")
ggplot(data = stn10, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Syracuse, NY")

# station 11 - Massena
stn11 <- subset(SpringYear, SpringYear$station == "USW00094725")
ggplot(data = stn11, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Massena, NY")

# station 12 - Watertown Airport
stn12 <- subset(SpringYear, SpringYear$station == "USW00094790")
ggplot(data = stn12, aes(x = year))+
  geom_line(aes(y = tav, color = "average temp"))+ 
  geom_line(aes(y = tmax, color = "maximum temp"))+
  geom_line(aes(y = tmin, color = "minimum temp"))+
  scale_color_manual(values = c("slateblue1","tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Spring Temperatures in Watertown Airport, NY")

## linear regressions
# station 1 model
stn1.mod <- lm(stn1$tav ~ stn1$year)
# check assumptions
stn1.res <- rstandard(stn1mod)
qqnorm(stn1.res)
qqline(stn1.res)
plot(stn1$year, stn1.res,
     xlab = "average temp",
     ylab = "standardized residual")
summary(stn1.mod)

plot(stn1$year, stn1$tav,
     ylab = "average temp",
     xlab = "year")

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
plot(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtHi[MarDecade$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Boonville, NY",
     ylim = c(-20,20))
lines(MarDecade$Decade[MarDecade$StationID=="USC00300785"], MarDecade$ExtLo[MarDecade$StationID=="USC00300785"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(MarDecade$Decade[MarDecade$StationID=="USC00301752"], MarDecade$ExtHi[MarDecade$StationID=="USC00301752"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Cooperstown, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00301752"], MarDecade$ExtLo[MarDecade$StationID=="USC00301752"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(MarDecade$Decade[MarDecade$StationID=="USC00304102"], MarDecade$ExtHi[MarDecade$StationID=="USC00304102"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Indian Lake, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00304102"], MarDecade$ExtLo[MarDecade$StationID=="USC00304102"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(MarDecade$Decade[MarDecade$StationID=="USC00304912"], MarDecade$ExtHi[MarDecade$StationID=="USC00304912"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Lowville, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00304912"], MarDecade$ExtLo[MarDecade$StationID=="USC00304912"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(MarDecade$Decade[MarDecade$StationID=="USC00306085"], MarDecade$ExtHi[MarDecade$StationID=="USC00306085"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Norwich, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00306085"], MarDecade$ExtLo[MarDecade$StationID=="USC00306085"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(MarDecade$Decade[MarDecade$StationID=="USC00306314"], MarDecade$ExtHi[MarDecade$StationID=="USC00306314"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Oswego, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00306314"], MarDecade$ExtLo[MarDecade$StationID=="USC00306314"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(MarDecade$Decade[MarDecade$StationID=="USC00309000"], MarDecade$ExtHi[MarDecade$StationID=="USC00309000"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Watertown, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USC00309000"], MarDecade$ExtLo[MarDecade$StationID=="USC00309000"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(MarDecade$Decade[MarDecade$StationID=="USW00014735"], MarDecade$ExtHi[MarDecade$StationID=="USW00014735"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Albany, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USW00014735"], MarDecade$ExtLo[MarDecade$StationID=="USW00014735"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(MarDecade$Decade[MarDecade$StationID=="USW00014750"], MarDecade$ExtHi[MarDecade$StationID=="USW00014750"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Glens Falls, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USW00014750"], MarDecade$ExtLo[MarDecade$StationID=="USW00014750"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(MarDecade$Decade[MarDecade$StationID=="USW00014771"], MarDecade$ExtHi[MarDecade$StationID=="USW00014771"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Syracuse, NY",
     ylim = c(-30, 30))
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
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme March Temperatures in Watertown Airport, NY",
     ylim = c(-30, 30))
lines(MarDecade$Decade[MarDecade$StationID=="USW00094790"], MarDecade$ExtLo[MarDecade$StationID=="USW00094790"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# APRIL
# station 1 - Boonville 
plot(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtHi[AprDecade$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Boonville, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00300785"], AprDecade$ExtLo[AprDecade$StationID=="USC00300785"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(AprDecade$Decade[AprDecade$StationID=="USC00301752"], AprDecade$ExtHi[AprDecade$StationID=="USC00301752"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Cooperstown, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00301752"], AprDecade$ExtLo[AprDecade$StationID=="USC00301752"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(AprDecade$Decade[AprDecade$StationID=="USC00304102"], AprDecade$ExtHi[AprDecade$StationID=="USC00304102"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Indian Lake, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00304102"], AprDecade$ExtLo[AprDecade$StationID=="USC00304102"],
      col = "skyblue")     
legend("right", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(AprDecade$Decade[AprDecade$StationID=="USC00304912"], AprDecade$ExtHi[AprDecade$StationID=="USC00304912"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Lowville, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00304912"], AprDecade$ExtLo[AprDecade$StationID=="USC00304912"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(AprDecade$Decade[AprDecade$StationID=="USC00306085"], AprDecade$ExtHi[AprDecade$StationID=="USC00306085"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Norwich, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00306085"], AprDecade$ExtLo[AprDecade$StationID=="USC00306085"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(AprDecade$Decade[AprDecade$StationID=="USC00306314"], AprDecade$ExtHi[AprDecade$StationID=="USC00306314"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Oswego, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00306314"], AprDecade$ExtLo[AprDecade$StationID=="USC00306314"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(AprDecade$Decade[AprDecade$StationID=="USC00309000"], AprDecade$ExtHi[AprDecade$StationID=="USC00309000"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Watertown, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USC00309000"], AprDecade$ExtLo[AprDecade$StationID=="USC00309000"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(AprDecade$Decade[AprDecade$StationID=="USW00014735"], AprDecade$ExtHi[AprDecade$StationID=="USW00014735"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Albany, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USW00014735"], AprDecade$ExtLo[AprDecade$StationID=="USW00014735"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(AprDecade$Decade[AprDecade$StationID=="USW00014750"], AprDecade$ExtHi[AprDecade$StationID=="USW00014750"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Glens Falls, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USW00014750"], AprDecade$ExtLo[AprDecade$StationID=="USW00014750"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(AprDecade$Decade[AprDecade$StationID=="USW00014771"], AprDecade$ExtHi[AprDecade$StationID=="USW00014771"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Syracuse, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USW00014771"], AprDecade$ExtLo[AprDecade$StationID=="USW00014771"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(AprDecade$Decade[AprDecade$StationID=="USW00094725"], AprDecade$ExtHi[AprDecade$StationID=="USW00094725"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Massena, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USW00094725"], AprDecade$ExtLo[AprDecade$StationID=="USW00094725"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(AprDecade$Decade[AprDecade$StationID=="USW00094790"], AprDecade$ExtHi[AprDecade$StationID=="USW00094790"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme April Temperatures in Watertown Airport, NY",
     ylim = c(-20, 30))
lines(AprDecade$Decade[AprDecade$StationID=="USW00094790"], AprDecade$ExtLo[AprDecade$StationID=="USW00094790"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# MAY
# station 1 - Boonville 
plot(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtHi[MayDecade$StationID=="USC00300785"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Boonville, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00300785"], MayDecade$ExtLo[MayDecade$StationID=="USC00300785"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 2 - Cooperstown
plot(MayDecade$Decade[MayDecade$StationID=="USC00301752"], MayDecade$ExtHi[MayDecade$StationID=="USC00301752"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Cooperstown, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00301752"], MayDecade$ExtLo[MayDecade$StationID=="USC00301752"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n", cex=.75)

# station 3 - Indian Lake
plot(MayDecade$Decade[MayDecade$StationID=="USC00304102"], MayDecade$ExtHi[MayDecade$StationID=="USC00304102"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Indian Lake, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00304102"], MayDecade$ExtLo[MayDecade$StationID=="USC00304102"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 4 - Lowville
plot(MayDecade$Decade[MayDecade$StationID=="USC00304912"], MayDecade$ExtHi[MayDecade$StationID=="USC00304912"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Lowville, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00304912"], MayDecade$ExtLo[MayDecade$StationID=="USC00304912"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 5 - Norwich
plot(MayDecade$Decade[MayDecade$StationID=="USC00306085"], MayDecade$ExtHi[MayDecade$StationID=="USC00306085"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Norwich, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00306085"], MayDecade$ExtLo[MayDecade$StationID=="USC00306085"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 6  - Oswego
plot(MayDecade$Decade[MayDecade$StationID=="USC00306314"], MayDecade$ExtHi[MayDecade$StationID=="USC00306314"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Oswego, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00306314"], MayDecade$ExtLo[MayDecade$StationID=="USC00306314"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 7 - Watertown
plot(MayDecade$Decade[MayDecade$StationID=="USC00309000"], MayDecade$ExtHi[MayDecade$StationID=="USC00309000"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Watertown, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USC00309000"], MayDecade$ExtLo[MayDecade$StationID=="USC00309000"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 8 - Albany
plot(MayDecade$Decade[MayDecade$StationID=="USW00014735"], MayDecade$ExtHi[MayDecade$StationID=="USW00014735"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Albany, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USW00014735"], MayDecade$ExtLo[MayDecade$StationID=="USW00014735"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 9 - Glens Falls
plot(MayDecade$Decade[MayDecade$StationID=="USW00014750"], MayDecade$ExtHi[MayDecade$StationID=="USW00014750"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Glens Falls, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USW00014750"], MayDecade$ExtLo[MayDecade$StationID=="USW00014750"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 10 - Syracuse
plot(MayDecade$Decade[MayDecade$StationID=="USW00014771"], MayDecade$ExtHi[MayDecade$StationID=="USW00014771"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Syracuse, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USW00014771"], MayDecade$ExtLo[MayDecade$StationID=="USW00014771"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 11 - Massena
plot(MayDecade$Decade[MayDecade$StationID=="USW00094725"], MayDecade$ExtHi[MayDecade$StationID=="USW00094725"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Massena, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USW00094725"], MayDecade$ExtLo[MayDecade$StationID=="USW00094725"],
      col = "skyblue")     
legend("bottomright", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)

# station 12 - Watertown Airport
plot(MayDecade$Decade[MayDecade$StationID=="USW00094790"], MayDecade$ExtHi[MayDecade$StationID=="USW00094790"],
     type = "l",
     col = "tomato3",
     xlab = "Decade",
     ylab = "Temperature (Celsius)",
     main = "Extreme May Temperatures in Watertown Airport, NY",
     ylim = c(-10, 35))
lines(MayDecade$Decade[MayDecade$StationID=="USW00094790"], MayDecade$ExtLo[MayDecade$StationID=="USW00094790"],
      col = "skyblue")     
legend("topleft", c("Extreme High", "Extreme Low"), col = c("tomato3","skyblue"), lwd = 2, bty="n",cex=.75)




# graph number of extreme temperature days in each month
# ONLY BOONVILLE RN, COPY FOR THE REST OF THE STATIONS

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




# # graph the extreme high and low temperatures 
# # for spring months averaged together
# ggplot(data = stn1, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Boonville, NY")
# 
# ggplot(data = stn2, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Cooperstown, NY")
# 
# ggplot(data = stn3, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Indian Lake, NY")
# 
# ggplot(data = stn4, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Lowville, NY")
# 
# ggplot(data = stn5, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Norwich, NY")
# 
# ggplot(data = stn6, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Oswego, NY")
# 
# ggplot(data = stn7, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Watertown, NY")
# 
# ggplot(data = stn8, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Albany, NY")
# 
# ggplot(data = stn9, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Glens Falls, NY")
# 
# ggplot(data = stn10, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Syracuse, NY")
# 
# ggplot(data = stn11, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Massena, NY")
# 
# ggplot(data = stn12, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Watertown Airport, NY")

# # all spring months averaged together 
# # graph number of extreme temperature days per year
# # station 1 - Boonville 
# Stn1 <- pivot_longer(stn1, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn1, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Boonville, NY")
# 
# # station 2 - Cooperstown
# Stn2 <- pivot_longer(stn2, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn2, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Cooperstown, NY")
# 
# # station 3 - Indian Lake
# Stn3 <- pivot_longer(stn3, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn1, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Boonville, NY")
# 
# # station 4 - Lowville
# Stn4 <- pivot_longer(stn4, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn4, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Lowville, NY")
# 
# # station 5 - Norwich
# Stn5 <- pivot_longer(stn5, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn5, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Norwich, NY")
# 
# # station 6  - Oswego
# Stn6 <- pivot_longer(stn6, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn6, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Oswego, NY")
# 
# # station 7 - Watertown
# Stn7 <- pivot_longer(stn7, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn7, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Watertown, NY")
# 
# # station 8 - Albany
# Stn8 <- pivot_longer(stn8, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn8, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Albany, NY")
# 
# # station 9 - Glens Falls
# Stn9 <- pivot_longer(stn9, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn9, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Glens Falls, NY")
# 
# # station 10 - Syracuse
# Stn10 <- pivot_longer(stn10, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn10, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Syracuse, NY")
# 
# # station 11 - Massena
# Stn11 <- pivot_longer(stn11, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn11, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Massena, NY")
# 
# # station 12 - Watertown Airport
# Stn12 <- pivot_longer(stn12, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
# ggplot(data = Stn12, aes(x = year, y = value, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Watertown Airport, NY")
# 
# # graph the extreme high and low temperatures 
# ### need to fix something with the hi/low temp column
# ### right now they are almost all the same for every year 
# ggplot(data = stn1, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Boonville, NY")
# 
# ggplot(data = stn2, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Cooperstown, NY")
# 
# ggplot(data = stn3, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Indian Lake, NY")
# 
# ggplot(data = stn4, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Lowville, NY")
# 
# ggplot(data = stn5, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Norwich, NY")
# 
# ggplot(data = stn6, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Oswego, NY")
# 
# ggplot(data = stn7, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Watertown, NY")
# 
# ggplot(data = stn8, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Albany, NY")
# 
# ggplot(data = stn9, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Glens Falls, NY")
# 
# ggplot(data = stn10, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Syracuse, NY")
# 
# ggplot(data = stn11, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Massena, NY")
# 
# ggplot(data = stn12, aes(x = year))+
#   geom_line(aes(y = ExHiTmax, color = "High"))+ 
#   geom_line(aes(y = ExLoTmin, color = "Low"))+
#   scale_color_manual(values = c("tomato3","skyblue"))+
#   theme_classic()+
#   labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Watertown Airport, NY")


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
# maybe we can find a better style of graph to represent these numebrs
# does mean amplitude really mean that much on a yearly basis?

# station 1
ggplot(data = stn1, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Boonville, NY")

# station 2
ggplot(data = stn2, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Cooperstown Airport, NY")

# station 3
ggplot(data = stn1, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Indian Lake, NY")

# station 4
ggplot(data = stn4, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Lowville, NY")

# station 5
ggplot(data = stn5, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Norwich, NY")

# station 6
ggplot(data = stn6, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Oswego, NY")

# station 7
ggplot(data = stn7, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Watertown, NY")

# station 8
ggplot(data = stn1, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Albany, NY")

# station 9
ggplot(data = stn9, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Glens Falls, NY")

# station 10
ggplot(data = stn10, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Syracuse, NY")

# station 11
ggplot(data = stn11, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Massena, NY")

# station 12
ggplot(data = stn12, aes(x = year, y = FTrange))+
  geom_line(color = "deepskyblue3") +
  theme_classic()+
  labs(x = "Year", y = "Temperature Range (celcius)", title = "Temperature Amplitude of Spring Freeze Thaw Days in Watertown Airport, NY")


