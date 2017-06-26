library(ggmap)
library(rworldmap)
library(scrubr)
sumFac <- function(d){summary(as.factor(d))}


colMeans(is.na(ausDataTrimmed))

############################

latLonCenter <-
    suppressMessages(geocode("UK"))

newmap <- suppressMessages(get_map(
    location = c(lon = latLonCenter$lon, lat = latLonCenter$lat),
    zoom = 3, maptype = "terrain", scale = 2
))
plot <- ggmap(newmap)

ggmap(newmap)

plot <-
    plot + geom_point(data = issues, aes(decimalLongitude, decimalLatitude, alpha = 0.5,
                                                      colour = 'red', fill = "red"), shape = 20, color = "red" )

suppressMessages(print(plot))

##############

library(OpenStreetMap)
library(ggplot2)
map <- openmap(c(79,-179),
               c(-79,179),zoom=1)
map <- openproj(map)

decimalLongitude

autoplot(map) + geom_point(data = new2, aes(projectedLongitude, projectedLatitude),size = 5, color = "#009E73")
+
    geom_point(data = new[logic,], aes(decimalLongitude, decimalLatitude),size =5, color = "salmon")

autoplot(map) + geom_point(data = new2[!logic,], aes(projectedLongitude, projectedLatitude),size = 4, color = "salmon") +
    geom_point(data = latNATesting[!logic,], aes(decimalLongitude, decimalLatitude),size =2, color = "#009E73")

####################


newmap <- getMap(resolution = "high")
plot(
    newmap,
    xlim = c(-180, 180),
    ylim = c(-90, 90),
    asp = 1
)

points(issues[!logic,]$decimalLongitude,
       issues[!logic,]$decimalLatitude,
       col = "blue",
       cex = .9)

points(new[!logic,]$projectedLongitude,
       new[!logic,]$projectedLatitude,
       col = "red",
       cex = .9)


################

df <- sample_data_1

# Remove unlikely points
NROW(df)
df_unlikely <- dframe(df) %>% coord_unlikely()
NROW(df_unlikely)
attr(df_unlikely, "coord_unlikely")


# Remove impossible coordinates
NROW(df)
df[1, "latitude"] <- 170
df <- dframe(df) %>% coord_impossible()
NROW(df)
attr(df, "coord_impossible")
# Remove incomplete cases
NROW(df)
df_inc <- dframe(df) %>% coord_incomplete()
NROW(df_inc)
attr(df_inc, "coord_incomplete")

# Remove unlikely points
NROW(ausDataTrimmed)
df_unlikely <- dframe(ausDataTrimmed) %>% coord_unlikely()
NROW(df_unlikely)
attr(df_unlikely, "coord_unlikely")


# Remove impossible coordinates
NROW(ausDataTrimmed)
#df[1, "latitude"] <- 170
df <- dframe(ausDataTrimmed) %>% coord_impossible()
NROW(df)
attr(df, "coord_impossible")
# Remove incomplete cases
NROW(ausDataTrimmed)
df_inc <- dframe(ausDataTrimmed) %>% coord_incomplete()
NROW(df_inc)
attr(df_inc, "coord_incomplete")

## By specific country name
NROW(t)
df_within <- dframe(t) %>% coord_within(country = "Australia")
NROW(df_within)
attr(df_within, "coord_within")


i <- finddecimals(issues, "decimalLongitude", "decimalLatitude")
fmtcheck()

modified()
nearestcell()

mapLayout <- get_map(location = c(lon = mean(wrongLong$decimalLongitude),
                                  lat = mean(wrongLong$decimalLatitude)), zoom = 3,
                     maptype = "terrain", scale = 2)
map <-ggmap(mapLayout) +
    geom_point(data = wrongLong, aes( x = decimalLongitude,
                               y = decimalLatitude,
                               alpha = 0.8, size = 5,
                               colour = 'red', fill = "red"), shape = 20) +
    guides(fill = FALSE, alpha = FALSE, size = FALSE)

plot(map)

library(plotly)
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = ~carat, y = ~price, color = ~carat,
        size = ~carat, text = ~paste("Clarity: ", clarity))

dat <- map_data("world", "canada") %>% group_by(group)

p <- plot_mapbox(dat, x = ~long, y = ~lat) %>%
    add_paths(size = I(2)) %>%
    add_segments(x = -100, xend = -50, y = 50, 75) %>%
    layout(mapbox = list(zoom = 0,
                         center = list(lat = ~median(lat),
                                       lon = ~median(long))
    ))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="mapbox/basic")
chart_link


f <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')

# geo styling
g <- list(
    scope = 'australia',
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
)

p <- plot_geo(badCauseCoordWithin[1:1000,], lat = ~decimalLatitude, lon = ~decimalLongitude)
chart_link = plotly_POST(p, filename="traffic",sharing = "public")
chart_link



