#updated 12/15

library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(sp)
library(ggplot2)
library(cowplot)


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
AllData$Month <- month(AllData$date)

# Subset to just keep id, tmin, tmax, year, doy
AllData <- data.frame(StationID = AllData$id, 
                      StationName = AllData$name.x, 
                      DOY = AllData$DOY, 
                      Month = AllData$Month,
                      Year = AllData$year, 
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
ExtrVals <- aggregate(AllData$tmax, by = list(AllData$StationID, AllData$Month), FUN = "quantile", prob = 0.95, na.rm = TRUE)
# lowest 5% tmin
ExtrVals$tmin <- aggregate(AllData$tmin, by = list(AllData$StationID, AllData$Month), FUN = "quantile", prob = 0.05, na.rm = TRUE)$x
# max tmax
ExtrVals$max <- aggregate(AllData$tmax, by = list(AllData$StationID, AllData$Month), max, na.rm = TRUE)$x
# min tmin
ExtrVals$min <- aggregate(AllData$tmin, by = list(AllData$StationID, AllData$Month), min, na.rm = TRUE)$x

# clean up data table 
ExtrVals <- data.frame(StationID = ExtrVals$Group.1,
                       Month = ExtrVals$Group.2,
                       MinTmin = ExtrVals$min,
                       LowTmin = ExtrVals$tmin,
                       HighTmax = ExtrVals$x,
                       MaxTmax = ExtrVals$max)

# join to AllData
AllData <- left_join(AllData, ExtrVals, by = c("StationID", "Month"))

# add flag for extreme high tmax
AllData$ExtrHi <- ifelse(AllData$tmax>=AllData$HighTmax, 1, NA)

# add flag for extreme low tmin
AllData$ExtrLo <- ifelse(AllData$tmin<=AllData$LowTmin, 1,NA)

## subset to spring data frame
SpringData <- subset(AllData, AllData$Month %in% c(3,4,5))



### general temperature trends ----
# started 12/15

# make a data frame of yearly averages of all 3 temperature variables
SpringYear <- aggregate(SpringData$tmax, by=list(SpringData$Year,SpringData$StationID), FUN="mean", na.rm = TRUE)
colnames(SpringYear) <- c("year","station","tmax")
SpringYear$tmin <- aggregate(SpringData$tmin, by=list(SpringData$Year,SpringData$StationID), FUN="mean", na.rm = TRUE)$x
SpringYear$tav <- aggregate(SpringData$tav, by=list(SpringData$Year,SpringData$StationID), FUN="mean", na.rm = TRUE)$x
SpringYear$FTdays <- aggregate(SpringData$FreezeThaw, by=list(SpringData$Year, SpringData$StationID), FUN="sum", na.rm = TRUE)$x
SpringYear$FTrange <- aggregate(SpringData$FTrange, by=list(SpringData$Year, SpringData$StationID), FUN="mean", na.rm = TRUE)$x


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


