
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
    model <- lm(TP1 ~ date)
    plot(date, TP1)
    abline(model)
}


plotDepthLake <- function(lake_name, depth_data, avg_model) {
    L <- depth_data$Lake==lake_name
    date <- depth_data[L,"Year"]
    depth <- depth_data[L, "Depth"]
    model <- lm(depth ~ date)
    plot(date, depth, main=lake_name, xlab="Year", ylab="Secchi disk depth (m)")
    abline(model)
    #plots the linear model of the average of all lakes in red
    abline(avg_model, col="red")
}



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



