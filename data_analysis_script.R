#updated 10/14

library(tidyr)
library(lubridate)

# Reading in prcp data 
PrcpData <- read.csv("/Users/abby/Documents/NYweather/prcp_all.csv", na.strings=c(""," ", "NA"))

# Reading in tmax data 
TmaxData <- read.csv("/Users/abby/Documents/NYweather/Tmax_all.csv", na.strings=c(""," ", "NA"))

# Reading in tmin data
TminData <- read.csv("/Users/abby/Documents/NYweather/Tmin_all.csv", na.strings=c(""," ", "NA"))

# Omitting na values from the data sets
PrcpData <- PrcpData %>% drop_na(prcp)
TmaxData <- TmaxData %>% drop_na(tmax)
TminData <- TminData %>% drop_na(tmin)

# Omit data with quality flags
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

# Adding a day of year column
PrcpData$DOY <- yday(PrcpData$date)
TmaxData$DOY <- yday(TmaxData$date)
TminData$DOY <- yday(TminData$date)

# Subsetting to first half of year
PrcpData <- subset(PrcpData, PrcpData$DOY < 182)
TmaxData <- subset(TmaxData, TmaxData$DOY < 182)
TminData <- subset(TminData, TminData$DOY < 182)

# Counting observations per year for the tmax data
# Make new data frame with station, year, ncount
TmaxDataYear <- aggregate(TmaxData$tmax, by=list(TmaxData$id,TmaxData$year), FUN="length")
# Changing column names
colnames(TmaxDataYear) <- c("station", "year", "ncount")
# Getting rid of stations with less than 180 observations
TmaxDataYear <- subset(TmaxDataYear, TmaxDataYear$ncount >= 180)

# Counting observations per year for the tmin data
# Make new data frame with station, year, ncount
TminDataYear <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="length")
# Changing column names
colnames(TminDataYear) <- c("station", "year", "ncount")
# Getting rid of stations with less than 180 observations
TminDataYear <- subset(TminDataYear, TminDataYear$ncount >= 180)


# Counting observations per year for the prcp data
# Make new data frame with station, prcp, ncount
PrcpDataYear <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="length")
# Changing column names
colnames(PrcpDataYear) <- c("station", "year", "ncount")
# Getting rid of stations with less than 180 observations
PrcpDataYear <- subset(PrcpDataYear, PrcpDataYear$ncount >= 180)


# Make a new data frame with number of years of tmax data, max year, min year, and range of years
  # will use range of years to compare with ycount to see if continuous
TmaxStn <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="length")
TmaxStn$min <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="min")$x
TmaxStn$max <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="max")$x
TmaxStn$range <- TmaxStn$max +1 - TmaxStn$min

# Renaming columns
colnames(TmaxStn) <- c("station", "ycount","min", "max", "range")

# getting rid of stations with less than 50 years of data
TmaxStn <-subset(TmaxStn, TmaxStn$ycount >=50)

# add a column of continuity
TmaxStn$pctcont <- TmaxStn$ycount/TmaxStn$range
# subsetting stations where pctcont >= .75
TmaxStn <- subset(TmaxStn, TmaxStn$pctcont >= .75)


# Make a new data frame with number of years of tmin data, max year, min year, and range of years
# will use range of years to compare with ycount to see if continuous
TminStn <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="length")
TminStn$min <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="min")$x
TminStn$max <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="max")$x
TminStn$range <- TminStn$max +1 - TminStn$min

# Renaming columns
colnames(TminStn) <- c("station", "ycount","min", "max", "range")

# getting rid of stations with less than 50 years of data
TminStn <-subset(TminStn, TminStn$ycount >=50)

# add a column of continuity
TminStn$pctcont <- TminStn$ycount/TminStn$range
# subsetting stations where pctcont >= .75
TminStn <- subset(TminStn, TminStn$pctcont >= .75)


# Make a new data frame with number of years of prcp data, max year, min year, and range of years
# will use range of years to compare with ycount to see if continuous
PrcpStn <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="length")
PrcpStn$min <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="min")$x
PrcpStn$max <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="max")$x
PrcpStn$range <- PrcpStn$max +1 - PrcpStn$min

# Renaming columns
colnames(PrcpStn) <- c("station", "ycount","min", "max", "range")

# getting rid of stations with less than 50 years of data
PrcpStn <-subset(PrcpStn, PrcpStn$ycount >=50)

# add a column of continuity
PrcpStn$pctcont <- PrcpStn$ycount/PrcpStn$range
# subsetting stations where pctcont >= .75
PrcpStn <- subset(PrcpStn, PrcpStn$pctcont >= .75)
