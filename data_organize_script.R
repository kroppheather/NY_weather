files <- list.files("/Users/hkropp/Google Drive/GIS/NYweather/stations")
filesInfo <- read.csv("/Users/hkropp/Google Drive/GIS/NYweather/station_info.csv")
#install.packages("dplyr")
library(dplyr)


typeD <- grepl("tmax", files)

fileNames <- character()

for(i in 1: length(files)){

    fileNames[i] <- strsplit(files[i], "\\_")[[1]][1]

}

filesDF <- data.frame(filename = files,
                      tmaxL = typeD,
                      station_id = fileNames)

filesAll <- full_join(filesDF, filesInfo, by= "station_id")

filesTm <- filesDF[filesDF$tmaxL == TRUE,]
