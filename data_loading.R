library(measurements)

## LOADING FILES, NAMING COLUMNS, AND FORMATTING FOR ANALYSIS

loadPhosData <- function() {
    # Load data and name variables.
    phos_data <- read.csv("data/Lake_Partners1.csv", sep=",", stringsAsFactors=F, quote="\"", skip=6)
    phos_data[1,9] <- "TP1"
    phos_data[1,10] <- "TP2"
    phos_data[1,7] <- "Lon"
    phos_data[1,6] <- "Lat"
    names(phos_data) <- phos_data[1,]
    
    # Convert numeric data to numeric and remove bad data.
    phos_data$TP1 <- as.numeric(phos_data$TP1) # numeric data
    phos_data <- phos_data[!is.na(phos_data$TP1),] # clean up data by removing entries with invalid TP1
    
    # Convert coded data to machine-readable formats.
    phos_data$Date <- as.Date(phos_data$Date, "%d-%b-%y") #convert to standardized machine-readable date format
    
    phos_data$Lat <- formatDMS(as.character(phos_data$Lat))
    phos_data$Lon <- formatDMS(as.character(phos_data$Lon))
    phos_data$Lon <- phos_data$Lon * -1 #Given we are in Western hemisphere
    
    return(phos_data)
}

loadDepthData <- function() {
    
    depth_data <- read.csv("data/Lake_Partners2.csv", sep=",", stringsAsFactors=F, quote="\"", skip=1)
    names(depth_data) <- c("Lake", "Township", "STN", "Site.ID", "Site.Description", "Lat", "Lon", "Year", "Depth", "Measurements")
    depth_data$Lon <- as.numeric(lapply(depth_data$Lon, formatDMS))
    depth_data$Lon <- depth_data$Lon * -1 #Given we are in Western hemisphere
    depth_data$Lat <- as.numeric(lapply(depth_data$Lat, formatDMS))
    
    return(depth_data)
}

loadCalData <- function() {
    
    cal_data <- read.csv("data/Lake_Partners3.csv", sep=",", stringsAsFactors=F, quote="\"", skip=1)
    cal_data[1,6] <- "Lat"
    cal_data[1,7] <- "Lon"
    cal_data[1,9] <- "Ca"
    names(cal_data) <- cal_data[1,]
    
    cal_data$Lon <- as.numeric(lapply(cal_data$Lon, formatDMS))
    cal_data$Lon <- cal_data$Lon * -1 #Given we are in Western hemisphere
    cal_data$Lat <- as.numeric(lapply(cal_data$Lat, formatDMS))
    
    return(cal_data)
}

## Helper functions

formatDMS <- function(DMS) {
    char <- as.character(DMS)
    if (is.na(char)) {
        num <- NA
    } else if (nchar(char) == 6) {
        spaced <- paste(substr(char, 1,2), substr(char, 3,4), substr(char, 5,6))
        num <- as.numeric(measurements::conv_unit(spaced, "deg_min_sec", "dec_deg"))
    } else {
        num <- NA
    }
    return(num)
}
