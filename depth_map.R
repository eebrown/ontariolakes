
library(plotly)
library(plotlyGeoAssets)
library(htmlwidgets)

source("data_loading.R")

phos_data <- loadPhosData()
cal_data <- loadCalData()
depth_data <- loadDepthData()

# geo styling
g <- list(
scope = 'can',
showland = TRUE,
landcolor = toRGB("grey83"),
subunitcolor = toRGB("white"),
countrycolor = toRGB("white"),
showlakes = TRUE,
lakecolor = toRGB("white"),
showsubunits = TRUE,
showcountries = TRUE,
resolution = 50,
projection = list(
type = 'conic conformal',
rotation = list(lon = -84)
),
lonaxis = list(
showgrid = TRUE,
gridwidth = 0.5,
range = c(-96, -74),
dtick = 5
),
lataxis = list(
showgrid = TRUE,
gridwidth = 0.5,
range = c(42, 51),
dtick = 5
)
)

map_depth_year <- function(year) {
    data <- depth_data[depth_data$Year==year,]
    p <- plot_geo(data, lat = ~Lat, lon = ~Lon, color = ~Depth, offline=T) %>%
    add_markers(text = ~Lake, hoverinfo = "text") %>%
    layout(title = paste('Ontario Lake Secchi Disk Visibility Depths (m) in', year), geo = g)
    
    return(p)
}

m1980 <- map_depth_year(1980)
m1989 <- map_depth_year(1989)
m1990 <- map_depth_year(1990)
m1991 <- map_depth_year(1991)
m1992 <- map_depth_year(1992)
m1993 <- map_depth_year(1993)
m1994 <- map_depth_year(1994)
m1995 <- map_depth_year(1995)
m1996 <- map_depth_year(1996)
m1997 <- map_depth_year(1997)
m1998 <- map_depth_year(1998)
m1999 <- map_depth_year(1999)
m2000 <- map_depth_year(2000)
m2001 <- map_depth_year(2001)
m2002 <- map_depth_year(2002)
m2003 <- map_depth_year(2003)
m2004 <- map_depth_year(2004)
m2005 <- map_depth_year(2005)
m2006 <- map_depth_year(2006)
m2007 <- map_depth_year(2007)
m2008 <- map_depth_year(2008)
m2009 <- map_depth_year(2009)
m2010 <- map_depth_year(2010)
m2011 <- map_depth_year(2011)
m2012 <- map_depth_year(2012)
m2013 <- map_depth_year(2013)
m2014 <- map_depth_year(2014)
m2015 <- map_depth_year(2015)
m2016 <- map_depth_year(2016)

htmlwidgets::saveWidget(as_widget(m2016), "2016_secchi_depths.html")


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
