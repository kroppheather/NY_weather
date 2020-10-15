
#source("/Users/hkropp/Documents/GitHub/NY_weather/data_organize_script.r")


#read in prcp data
PRCPall <- read.csv("/Users/abby/Documents/NYweather/prcp_all.csv")
#read in tmax data
TMAXall <- read.csv("/Users/abby/Documents/NYweather/Tmax_all.csv")
#read in tmin data
TMINall <- read.csv("/Users/abby/Documents/NYweather/Tmin_all.csv")

#install tidy r to omit NA values in specific column
#install.packages("tidyr")
library(tidyr)

#omit NA values in data
PRCP <- PRCPall %>% drop_na("prcp")
TMAX <- TMAXall %>% drop_na("tmax")
TMIN <- TMINall %>% drop_na("tmin")

#format as dates
PRCP$date <- as.Date(PRCP$date, "%Y-%m-%d")
TMAX$date <- as.Date(TMAX$date, "%Y-%m-%d")
TMIN$date <- as.Date(TMIN$date, "%Y-%m-%d")

#install lubridate
#install.packages(lubridate)
library(lubridate)

#pull out year from date column
PRCP$year <- year(PRCP$date)
TMAX$year <- year(TMAX$date)
TMIN$year <- year(TMIN$date)

## reorganize data by year to get ncount per year

#calculate annual total precip
PRCPyear <-aggregate(PRCP$prcp, by=list(PRCP$id,PRCP$year), FUN="sum")
#change column names
colnames(PRCPyear) <- c("STATION","year","totalPRCP")             
#count number of observations per year
PRCPyear$ncount <-aggregate(PRCP$prcp, by=list(PRCP$id,PRCP$year), FUN="length")$x

#repeat for TMAX 
TMAXyear <-aggregate(TMAX$tmax, by=list(TMAX$id,TMAX$year), FUN="mean")
colnames(TMAXyear) <- c("STATION","year","meanTMAX")
TMAXyear$ncount <-aggregate(TMAX$tmax, by=list(TMAX$id,TMAX$year), FUN="length")$x

#repeat for TMIN
TMINyear <-aggregate(TMIN$tmin, by=list(TMIN$id,TMIN$year), FUN="mean")
colnames(TMINyear) <- c("STATION","year","meanTMIN")
TMINyear$ncount <-aggregate(TMIN$tmin, by=list(TMIN$id,TMIN$year), FUN="length")$x


## reorganize to get number of years per station
TMAXstn <-aggregate(TMAXyear$year, by=list(TMAXyear$STATION), FUN="mean")
colnames(TMAXstn) <-c("STATION", "mean")
TMAXstn$yrcount <-aggregate(TMAXyear$year, by=list(TMAXyear$STATION), FUN="length")$x

TMINstn <-aggregate(TMINyear$year, by=list(TMINyear$STATION), FUN="mean")
colnames(TMINstn) <-c("STATION", "mean")
TMINstn$yrcount <-aggregate(TMINyear$year, by=list(TMINyear$STATION), FUN="length")$x

PRCPstn <-aggregate(PRCPyear$year, by=list(PRCPyear$STATION), FUN="mean")
colnames(PRCPstn) <-c("STATION", "mean")
PRCPstn$yrcount <-aggregate(PRCPyear$year, by=list(PRCPyear$STATION), FUN="length")$x




