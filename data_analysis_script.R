#updated 12/14

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


### Map sites ----
#start by mapping all sites
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


### identify sites with all data ---
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

### start getting trends for tmin data ---
TminBoonville <- subset(TminData, TminData$id == "USC00300785")
BoonvilleYear <- aggregate(TminBoonville$tmin, by=list(TminBoonville$year), FUN="mean")
colnames(BoonvilleYear) <- c("year", "tminAVG")
ggplot(data = BoonvilleYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Boonville, NY",
       x= "Year",
       y= "Average Minimum Temperature (C)")

# Cooperstown
TminCoop <- subset(TminData, TminData$id == "USC00301752")
CoopYear <- aggregate(TminCoop$tmin, by=list(TminCoop$year), FUN="mean")
colnames(CoopYear) <- c("year", "tminAVG")
ggplot(data = CoopYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Cooperstown, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Indian Lake
TminIL <- subset(TminData, TminData$id == "USC00304102")
ILYear <- aggregate(TminIL$tmin, by=list(TminIL$year), FUN="mean")
colnames(ILYear) <- c("year", "tminAVG")
ggplot(data = ILYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Indian Lake, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Lowville
TminLowville <- subset(TminData, TminData$id == "USC00304912")
LowvilleYear <- aggregate(TminLowville$tmin, by=list(TminLowville$year), FUN="mean")
colnames(LowvilleYear) <- c("year", "tminAVG")
ggplot(data = LowvilleYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Lowville, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Norwich
TminNorwich <- subset(TminData, TminData$id == "USC00306085")
NorwichYear <- aggregate(TminNorwich$tmin, by=list(TminNorwich$year), FUN="mean")
colnames(NorwichYear) <- c("year", "tminAVG")
ggplot(data = NorwichYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Norwich, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Oswego
TminOswego <- subset(TminData, TminData$id == "USC00306314")
OswegoYear <- aggregate(TminOswego$tmin, by=list(TminOswego$year), FUN="mean")
colnames(OswegoYear) <- c("year", "tminAVG")
ggplot(data = OswegoYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Oswego, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Watertown
TminWater <- subset(TminData, TminData$id == "USC00309000")
WaterYear <- aggregate(TminWater$tmin, by=list(TminWater$year), FUN="mean")
colnames(WaterYear) <- c("year", "tminAVG")
ggplot(data = WaterYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Watertown, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Albany
TminAlb <- subset(TminData, TminData$id == "USW00014735")
AlbYear <- aggregate(TminAlb$tmin, by=list(TminAlb$year), FUN="mean")
colnames(AlbYear) <- c("year", "tminAVG")
ggplot(data = AlbYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Albany, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Glens Falls
TminGF <- subset(TminData, TminData$id == "USW00014750")
GFYear <- aggregate(TminGF$tmin, by=list(TminGF$year), FUN="mean")
colnames(GFYear) <- c("year", "tminAVG")
ggplot(data = GFYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Glens Falls, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Syracuse
TminSyr <- subset(TminData, TminData$id == "USW00014771")
SyrYear <- aggregate(TminSyr$tmin, by=list(TminSyr$year), FUN="mean")
colnames(SyrYear) <- c("year", "tminAVG")
ggplot(data = SyrYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Syracuse, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Massena
TminMass <- subset(TminData, TminData$id == "USW00094725")
MassYear <- aggregate(TminMass$tmin, by=list(TminMass$year), FUN="mean")
colnames(MassYear) <- c("year", "tminAVG")
ggplot(data = MassYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Massena, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

# Watertown Airport
TminWaterAP <- subset(TminData, TminData$id == "USW00094790")
WaterAPYear <- aggregate(TminWaterAP$tmin, by=list(TminWaterAP$year), FUN="mean")
colnames(WaterAPYear) <- c("year", "tminAVG")
ggplot(data = WaterAPYear, aes(x=year, y=tminAVG))+
  geom_line()+
  theme_classic()+
  labs(title="Average Annual Minimum Temperature for Watertown Airport, NY",
       x= "Year",
       y= "Average Minimum Temperature (˚C)")

### trends for precip data ---
# Boonville
PrcpBoonville <- subset(PrcpData, PrcpData$id == "USC00300785")
BoonvilleYearPr <- aggregate(PrcpBoonville$prcp, by=list(PrcpBoonville$year), FUN="sum")
colnames(BoonvilleYearPr) <-c("year","totalPRCP")
ggplot(data = BoonvilleYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Boonville, NY",
       x= "Year",
       y= "Total Precipitation (mm)")
# Cooperstown
PrcpCoop <- subset(PrcpData, PrcpData$id == "USC00301752")
CoopYearPr <- aggregate(PrcpCoop$prcp, by=list(PrcpCoop$year), FUN="sum")
colnames(CoopYearPr) <- c("year", "totalPRCP")
ggplot(data = CoopYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Cooperstown, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Indian Lake
PrcpIL <- subset(PrcpData, PrcpData$id == "USC00304102")
ILYearPr <- aggregate(PrcpIL$prcp, by=list(PrcpIL$year), FUN="sum")
colnames(ILYearPr) <- c("year", "totalPRCP")
ggplot(data = ILYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Indian Lake, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Lowville
PrcpLowville <- subset(PrcpData, PrcpData$id == "USC00304912")
LowvilleYearPr <- aggregate(PrcpLowville$prcp, by=list(PrcpLowville$year), FUN="sum")
colnames(LowvilleYearPr) <- c("year", "totalPRCP")
ggplot(data = LowvilleYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Lowville, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Norwich
PrcpNorwich <- subset(PrcpData, PrcpData$id == "USC00306085")
NorwichYearPr <- aggregate(PrcpNorwich$prcp, by=list(PrcpNorwich$year), FUN="sum")
colnames(NorwichYearPr) <- c("year", "totalPRCP")
ggplot(data = NorwichYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Norwich, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Oswego
PrcpOswego <- subset(PrcpData, PrcpData$id == "USC00306314")
OswegoYearPr <- aggregate(PrcpOswego$prcp, by=list(PrcpOswego$year), FUN="sum")
colnames(OswegoYearPr) <-c("year", "totalPRCP")
ggplot(data = OswegoYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Oswego, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Watertown
PrcpWater <- subset(PrcpData, PrcpData$id =="USC00309000")
WaterYearPr <- aggregate(PrcpWater$prcp, by=list(PrcpWater$year), FUN="sum")
colnames(WaterYearPr) <-c("year", "totalPRCP")
ggplot(data = WaterYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Watertown, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Albany
PrcpAlb <-subset(PrcpData, PrcpData$id == "USW00014735")
AlbYearPr <- aggregate(PrcpAlb$prcp, by=list(PrcpAlb$year), FUN="sum")
colnames(AlbYearPr) <-c("year", "totalPRCP")
ggplot(data = AlbYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Albany, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Glens Falls
PrcpGF <- subset(PrcpData, PrcpData$id == "USW00014750")
GFYearPr <- aggregate(PrcpGF$prcp, by=list(PrcpGF$year), FUN="sum")
colnames(GFYearPr) <-c("year", "totalPRCP")
ggplot(data = GFYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Glens Falls, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Syracuse
PrcpSyr <- subset(PrcpData, PrcpData$id == "USW00014771")
SyrYearPr <- aggregate(PrcpSyr$prcp, by=list(PrcpSyr$year), FUN="sum")
colnames(SyrYearPr) <-c("year", "totalPRCP")
ggplot(data = SyrYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Syracuse, NY",
       x= "Year",
       y= "Total Precipitation (mm)")


# Massena
PrcpMass <-subset(PrcpData, PrcpData$id == "USW00094725")
MassYearPr <- aggregate(PrcpMass$prcp, by=list(PrcpMass$year), FUN="sum")
colnames(MassYearPr) <-c("year", "totalPRCP")
ggplot(data = MassYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Massena, NY",
       x= "Year",
       y= "Total Precipitation (mm)")

# Watertown Airport
PrcpWaterAP <- subset(PrcpData, PrcpData$id == "USW00094790")
WaterAPYearPr <- aggregate(PrcpWaterAP$prcp, by=list(PrcpWaterAP$year), FUN="sum")
colnames(WaterAPYearPr) <-c("year", "totalPRCP")
ggplot(data = WaterAPYearPr, aes(x=year, y=totalPRCP))+
  geom_line()+
  theme_classic()+
  labs(title="Total Precipitation (January-June) for Watertown Airport, NY",
       x= "Year",
       y= "Total Precipitation (mm)")
