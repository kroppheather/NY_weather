# Rachel Started 10/6

# NEXT STEPS (10/8): remove stations with less than half a year of data
#                    remove stations with quality flags
#                    figure out min/max year to see how many years each station has
#                    remove stations with less than 50 years of data (for visualization)
#                    remove stations with less than 75% of those 50 years (continuous)

library(tidyr)
library(lubridate)

# Reading in prcp data from google drive
PrcpData <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/data/prcp_all.csv")

# Reading in tmax data from google drive
TmaxData <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/data/Tmax_all.csv")

# Reading in tmin data from google drive
TminData <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/data/Tmin_all.csv")

# COME BACK HERE to remove any stations/years with a quality flag

# Omitting na values from the data sets
PrcpData <- PrcpData %>% drop_na(prcp)
TmaxData <- TmaxData %>% drop_na(tmax)
TminData <- TminData %>% drop_na(tmin)

# Formatting date columns as dates
PrcpData$date <- as.Date(PrcpData$date, "%Y-%m-%d")
TmaxData$date <- as.Date(TmaxData$date, "%Y-%m-%d")
TminData$date <- as.Date(TminData$date, "%Y-%m-%d")

# Adding a year column to each data set
PrcpData$year <- year(PrcpData$date)
TmaxData$year <- year(TmaxData$date)
TminData$year <- year(TminData$date)

# Counting observations per year for the tmax data
# Make new data frame with just the id, tmax value, and year
TmaxDataYear <- aggregate(TmaxData$tmax, by=list(TmaxData$id,TmaxData$year), FUN="mean")

# Changing column names
colnames(TmaxDataYear) <- c("station", "year", "mtmax")

# Add an ncount column to see the total observations per year
TmaxDataYear$ncount <- aggregate(TmaxData$tmax, by=list(TmaxData$id,TmaxData$year), FUN="length")$x


# Counting observations per year for the tmin data
# Make new data frame with just the id, tmin value, and year
TminDataYear <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="mean")

# Changing column names
colnames(TminDataYear) <- c("station", "year", "mtmin")

# Add an ncount column to see the total observations per year
TminDataYear$ncount <- aggregate(TminData$tmin, by=list(TminData$id,TminData$year), FUN="length")$x


# Counting observations per year for the prcp data
# Make new data frame with just the id, prcp value, and year
PrcpDataYear <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="sum")

# Changing column names
colnames(PrcpDataYear) <- c("station", "year", "tprcp")

# Add an ncount column to see the total observations per year
PrcpDataYear$ncount <- aggregate(PrcpData$prcp, by=list(PrcpData$id,PrcpData$year), FUN="length")$x


# Counting number of years per station for tmax- mean year doesn't mean anything to us
  # it just helps us count
TmaxStn <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="min")

# Renaming columns
colnames(TmaxStn) <- c("station", "MinYear")

# Adding a count of the years
TmaxStn$YearCount <- aggregate(TmaxDataYear$year, by=list(TmaxDataYear$station), FUN="length")$x


# Counting number of years per station for tmin - mean year doesn't mean anything to us
# it just helps us count
TminStn <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="min")

# Renaming columns
colnames(TminStn) <- c("station", "MinYear")

# Adding a count of the years
TminStn$YearCount <- aggregate(TminDataYear$year, by=list(TminDataYear$station), FUN="length")$x


# Counting number of years per station for prcp by first getting min year
PrcpStn <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="min")

# Renaming columns
colnames(PrcpStn) <- c("station", "MinYear")

# Adding a count of the years
PrcpStn$YearCount <- aggregate(PrcpDataYear$year, by=list(PrcpDataYear$station), FUN="length")$x

# We can add max year to understand better how many years are actually in data
