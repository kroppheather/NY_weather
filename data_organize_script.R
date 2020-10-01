files <- list.files("/Users/hkropp/Google Drive/GIS/NYweather/stations")
filesInfo <- read.csv("/Users/hkropp/Google Drive/GIS/NYweather/station_info.csv")

# Organizing the file data
#install.packages("dplyr")
library(dplyr)


# Sorting out which files have tmax
filesTmx <- grepl("tmax", files)

# Sorting out which files have tmin
filesTmn <- grepl("tmin", files)

# Sorting out which files have tmin
filesTav <- grepl("tavg", files)

# Sorting out which files have prcp
filesPr <- grepl("prcp", files)

# Making the file names in characters
stationNames <- character()

# Slicing just the name of the station off of the file name
for(i in 1: length(files)){
  
  stationNames[i] <- strsplit(files[i], "\\_")[[1]][1]
  
}

# Creating data frame with the logical values for each type of data
filesDF <- data.frame(filename = files,
                      tmaxL = filesTmx,
                      tminL = filesTmn,
                      tavgL = filesTav,
                      prL = filesPr,
                      station_id = stationNames)

# Joining our data frames
filesAll <- full_join(filesDF, filesInfo, by= "station_id")

# Seeing how many stations are in our lat long requirements
filesLatLon <- filesAll[filesAll$lat >= 42.5 & filesAll$long >=-76.5,]

# For each type of data make a new data frame with only files that include that type
filesTmax <- filesLatLon[filesLatLon$tmaxL == TRUE,]

filesTmin <- filesLatLon[filesLatLon$tminL == TRUE,]

filesTavg <- filesLatLon[filesLatLon$tmavgL == TRUE,]

filesPrcp <- filesLatLon[filesLatLon$prL == TRUE,]


