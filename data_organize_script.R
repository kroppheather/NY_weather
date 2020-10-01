#define usr
#station directory ID
#1 = Heather
#2 = Rachel & Abby
usr <- 1
#station directory
stationDir <- c("/Users/hkropp/Google Drive/GIS/NYweather/stations",
              "/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/stations")
infoDir <- c("/Users/hkropp/Google Drive/GIS/NYweather/",
             "/Volumes/GoogleDrive/.shortcut-targets-by-id/10ARTNFd7_vF4j5cC_nYlyqrsTjmLzCsj/NYweather/")
outDir <- "/Users/hkropp/Google Drive/research/students/NYweather/data"
files <- list.files(stationDir[usr])
filesInfo <- read.csv(paste0(infoDir[usr],"station_info.csv"))

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
filesTmax <- na.omit(filesLatLon[filesLatLon$tmaxL == TRUE,])

filesTmin <- na.omit(filesLatLon[filesLatLon$tminL == TRUE,])

filesPrcp <- na.omit(filesLatLon[filesLatLon$prL == TRUE,])

#start to read in csv files



PrcpData <- read.csv(paste0(stationDir[usr],"/",filesPrcp$filename[1]))

#read in precip
for(i in 2:nrow(filesPrcp)){
  PrcpData <- rbind(PrcpData, read.csv(paste0(stationDir[usr],"/",filesPrcp$filename[i])))
  
}
write.table(PrcpData, paste0(outDir,"/prcp_all.csv"), row.names=FALSE,sep=",")

#tmax
maxData <- read.csv(paste0(stationDir[usr],"/",filesTmax$filename[1]))

#read in precip
for(i in 2:nrow(filesTmax)){
  maxData <- rbind(maxData, read.csv(paste0(stationDir[usr],"/",filesTmax$filename[i])))
  
}
write.table(maxData, paste0(outDir,"/Tmax_all.csv"), row.names=FALSE,sep=",")


#tmin
minData <- read.csv(paste0(stationDir[usr],"/",filesTmin$filename[1]))

#read in precip
for(i in 2:nrow(filesTmin)){
  minData <- rbind(minData, read.csv(paste0(stationDir[usr],"/",filesTmin$filename[i])))
  
}
write.table(minData, paste0(outDir,"/Tmin_all.csv"), row.names=FALSE,sep=",")

