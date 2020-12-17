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
#plot(siteP, add=TRUE, pch=19, col=rgb(0.5,0.5,0.5,0.45), cex=0.5)
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

### general temperature trends ----
# started 12/15


# make a data frame of yearly averages of all 3 temperature variables
SpringYear <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID), FUN="mean", na.rm = TRUE)
colnames(SpringYear) <- c("year","station","tmax")
SpringYear$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID), FUN="mean", na.rm = TRUE)$x
SpringYear$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID), FUN="mean", na.rm = TRUE)$x


# add columns of extreme hi and lo temperature values
SpringYear$ExtHi <- aggregate(SpringData$HiTmax, by=list(SpringData$Year, SpringData$StationID), FUN = "mean", na.rm = TRUE)$x
SpringYear$ExtLo <- aggregate(SpringData$LoTmin, by=list(SpringData$Year, SpringData$StationID), FUN = "mean", na.rm = TRUE)$x
# add columns counting extreme temp flags
SpringYear$ExHiCount <- aggregate(SpringData$ExtrHi, by=list(SpringData$Year, SpringData$StationID) , FUN = "sum", na.rm = TRUE)$x
SpringYear$ExLoCount <- aggregate(SpringData$ExtrLo, by=list(SpringData$Year, SpringData$StationID), FUN = "sum", na.rm = TRUE)$x

# add columns with freeze thaw flags and range
SpringYear$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID), FUN="sum", na.rm = TRUE)$x
SpringYear$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID), FUN="mean", na.rm = TRUE)$x

# create new data frame by decade
SpringDecade <- aggregate(SpringData$HiTmax, by = list(SpringData$StationID, SpringData$StationName, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)
colnames(SpringDecade) <- c("StationID", "StationName", "Decade", "Month", "AvExHi")
SpringDecade$AvExLo <- aggregate(SpringData$LoTmin, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x
SpringDecade$FTdays <- aggregate(SpringData$FreezeThaw, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "sum", na.rm = TRUE)$x / 30
SpringDecade$FTrange <- aggregate(SpringData$FTrange, by = list(SpringData$StationID, SpringData$Decade, SpringData$Month), FUN = "mean", na.rm = TRUE)$x

## use AllStn for subsetting ##

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


### extreme temperatures ----

# graph number of extreme temperature days per year
# station 1 - Boonville 
Stn1 <- pivot_longer(stn1, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn1, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Boonville, NY")

# station 2 - Cooperstown
Stn2 <- pivot_longer(stn2, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn2, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Cooperstown, NY")

# station 3 - Indian Lake
Stn3 <- pivot_longer(stn3, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn1, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Boonville, NY")

# station 4 - Lowville
Stn4 <- pivot_longer(stn4, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn4, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Lowville, NY")

# station 5 - Norwich
Stn5 <- pivot_longer(stn5, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn5, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Norwich, NY")

# station 6  - Oswego
Stn6 <- pivot_longer(stn6, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn6, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Oswego, NY")

# station 7 - Watertown
Stn7 <- pivot_longer(stn7, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn7, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Watertown, NY")

# station 8 - Albany
Stn8 <- pivot_longer(stn8, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn8, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Albany, NY")

# station 9 - Glens Falls
Stn9 <- pivot_longer(stn9, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn9, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Glens Falls, NY")

# station 10 - Syracuse
Stn10 <- pivot_longer(stn10, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn10, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Syracuse, NY")

# station 11 - Massena
Stn11 <- pivot_longer(stn11, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn11, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Massena, NY")

# station 12 - Watertown Airport
Stn12 <- pivot_longer(stn12, cols=c("ExHiCount", "ExLoCount"), names_to = "variable", values_to = "value")
ggplot(data = Stn12, aes(x = year, y = value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Days with Extreme Temperatures", title = "Extreme Temperatures in Watertown Airport, NY")

# graph the extreme high and low temperatures 
### need to fix something with the hi/low temp column
### right now they are almost all the same for every year 
ggplot(data = stn1, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Boonville, NY")

ggplot(data = stn2, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Cooperstown, NY")

ggplot(data = stn3, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Indian Lake, NY")

ggplot(data = stn4, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Lowville, NY")

ggplot(data = stn5, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Norwich, NY")

ggplot(data = stn6, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Oswego, NY")

ggplot(data = stn7, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Watertown, NY")

ggplot(data = stn8, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Albany, NY")

ggplot(data = stn9, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Glens Falls, NY")

ggplot(data = stn10, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Syracuse, NY")

ggplot(data = stn11, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Massena, NY")

ggplot(data = stn12, aes(x = year))+
  geom_line(aes(y = ExtHi, color = "High"))+ 
  geom_line(aes(y = ExtLo, color = "Low"))+
  scale_color_manual(values = c("tomato3","skyblue"))+
  theme_classic()+
  labs(x = "Year", y = "Temperature (celsius)", title = "Extreme High and Low Temperatures in Watertown Airport, NY")

### Extreme Temperatures by decade

# create scatter plot march high temperatures
MarchEx <- subset(SpringDecade, SpringDecade$Month == "Mar")
MarchHiAv <- mean(AllData$HiTmax[AllData$Month == "Mar"], na.rm = TRUE)
ggplot(data = MarchEx, aes(x = Decade, y = AvExHi, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MarchHiAv)+
  theme_classic()+
  labs(title = "March Extreme High Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# scatter plot march low temperatures
MarchLoAv <- mean(AllData$LoTmin[AllData$Month == "Mar"], na.rm = TRUE)
ggplot(data = MarchEx, aes(x = Decade, y = AvExLo, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MarchLoAv)+
  theme_classic()+
  labs(title = "March Extreme Low Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# create scatter plot april high temperatures
AprilEx <- subset(SpringDecade, SpringDecade$Month == "Apr")
AprilHiAv <- mean(AllData$HiTmax[AllData$Month == "Apr"], na.rm = TRUE)
ggplot(data = AprilEx, aes(x = Decade, y = AvExHi, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = AprilHiAv)+
  theme_classic()+
  labs(title = "April Extreme High Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# scatter plot april low temperatures
AprilLoAv <- mean(AllData$LoTmin[AllData$Month == "Apr"], na.rm = TRUE)
ggplot(data = AprilEx, aes(x = Decade, y = AvExLo, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = AprilLoAv)+
  theme_classic()+
  labs(title = "April Extreme Low Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# create scatter plot may high temperatures
MayEx <- subset(SpringDecade, SpringDecade$Month == "May")
MayHiAv <- mean(AllData$HiTmax[AllData$Month == "May"], na.rm = TRUE)
ggplot(data = MayEx, aes(x = Decade, y = AvExHi, color = StationName))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = MayHiAv)+
  theme_classic()+
  labs(title = "May Extreme High Temperatures by Decade", x = "Decade", y = "Temperature (celcius)")

# scatter plot march low temperatures
MayLoAv <- mean(AllData$LoTmin[AllData$Month == "May"], na.rm = TRUE)
ggplot(data = MayEx, aes(x = Decade, y = AvExLo, color = StationName))+
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
ggplot(data = stn2, aes(x = year, y = FTrange))+
  geom_point(color = "deepskyblue3") +
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

