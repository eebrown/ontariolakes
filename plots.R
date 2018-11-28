
source("data_loading.R")

phos_data <- loadPhosData()
cal_data <- loadCalData()
depth_data <- loadDepthData()


# Consider removing outliers for phos data
# e.g. phos_data <- phos_data[phos_data$TP1 < 150,]

# e.g. plotPhosLake(phos_data, "SALERNO LAKE")
plotPhosLake <- function(phos_data, lake_name) {
    L <- phos_data$Lake==lake_name
    date <- phos_data[L,"Date"]
    TP1 <- phos_data[L,"TP1"]
    TP2 <- phos_data[L,"TP2"]
    
    phosphorus <- rowMeans(cbind(TP1, TP2), na.rm=T)
    
    plot(date, phosphorus, xlab="Date", ylab="Mean total phosphorus (µg/L)", main=lake_name)
    
    if (length(phosphorus) > 7) {
        model <- lm(phosphorus ~ date)
        if (is.na(summary(model)$coefficients[2,4])) {
            print("Model cannot compute.")
        } else if (summary(model)$coefficients[2,4] < 0.10) {
            abline(model)
        }
        return(model)
    }
}


plotDepthLake <- function(depth_data, lake_name) { #avg_model) {
    L <- depth_data$Lake==lake_name
    date <- depth_data[L,"Year"]
    depth <- depth_data[L, "Depth"]
    
    plot(date, depth, main=lake_name, xlab="Year", ylab="Secchi disk depth (m)")
    
      if (length(depth) > 7) {
        model <- lm(depth ~ date)
        if (is.na(summary(model)$coefficients[2,4])) {
            print("Model cannot compute.")
        } else if (summary(model)$coefficients[2,4] < 0.10) {
            abline(model)
        }
    }
    #plots the linear model of the average of all lakes in red
    #abline(avg_model, col="red")
}

plotCalcLake <- function(cal_data, lake_name) {
    L <- cal_data$Lake==lake_name
    date <- cal_data[L,"Date"]
    Ca <- cal_data[L, "Ca"]
    
    plot(date, Ca, xlab="Date", ylab="Mean calcium (mg/L)", main=lake_name)
    
    if (length(Ca) > 7) {
        model <- lm(Ca ~ date)
        if (is.na(summary(model)$coefficients[2,4])) {
            print("Model cannot compute.")
        } else if (summary(model)$coefficients[2,4] < 0.10) {
            abline(model)
        }
    return(model)
    }
}



# Average visibility model

lakes <- unique(depth_data$Lake)
years <- unique(depth_data$Year)
years <- sort(years[!is.na(years)])
mean_depth <- rep(NA, length(years))
sd_depth <- mean_depth
year <- mean_depth
depth_table <- data.frame(year, mean_depth, sd_depth)
for (i in 1:length(years)) {
    Y <- depth_data$Year==years[i]
    depth_year <- depth_data[Y, "Depth"]
    depth_year <- depth_year[!is.na(depth_year)]
    depth_table[i, "sd_depth"] <- sd(depth_year)
    depth_table[i, "mean_depth"] <- sum(depth_year)/length(depth_year)
    depth_table[i, "year"] <- years[i]
}

avg_model <- lm(depth_table$mean_depth ~ depth_table$year)



# Batch generate plots for each lake.

for (lake in unique(cal_data$Lake)[-1]) {
    cat("Working on", lake, "\n")
    pdf(paste("plots/calcium/", gsub("/","",lake, fixed=TRUE), "_calcium.pdf", sep=""))
    plotCalcLake(cal_data, lake)
    dev.off()
}

for (lake in unique(phos_data$Lake)[-1]) {
    cat("Working on", lake, "\n")
    pdf(paste("plots/phosphorus/", gsub("/","",lake, fixed=TRUE), "_phosphorus.pdf", sep=""))
    plotPhosLake(phos_data, lake)
    dev.off()
}

for (lake in unique(depth_data$Lake)[-1]) {
    cat("Working on", lake, "\n")
    pdf(paste("plots/visibility/", gsub("/","",lake, fixed=TRUE), "_visibility.pdf", sep=""))
    plotDepthLake(depth_data, lake)
    dev.off()
}
