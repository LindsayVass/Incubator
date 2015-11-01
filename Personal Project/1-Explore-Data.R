library(dplyr)
library(ggmap)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zipcode)
library(rgdal)
library(rgeos)

# Load parking violation data
# https://www.opendataphilly.org/dataset/parking-violations
parking <- read.csv('Parking_Violations.csv')

# Load zip code shape files
# http://www.census.gov/cgi-bin/geo/shapefiles2010/file-download
zipShape <- readOGR(dsn = 'tl_2010_42_zcta510/', layer = 'tl_2010_42_zcta510')

# Clean up data frame -----------------------------------------------------

# remove violations with no coordinates
parkingClean <- parking %>%
  filter(Coordinates != '')

# clean up lat/lon data
parkingClean <- separate(parkingClean, Coordinates, c('Lat', 'Lon'), sep = ',')
parkingClean$Lat <- sub('\\(', '', parkingClean$Lat)
parkingClean$Lon <- sub('\\)', '', parkingClean$Lon)
parkingClean$Lat <- as.numeric(parkingClean$Lat)
parkingClean$Lon <- as.numeric(parkingClean$Lon)

# some data points have reversed lat/long so fix that
parkingClean <- parkingClean %>%
  mutate(LatFix = ifelse(Lat < 20, Lon, Lat),
         LonFix = ifelse(Lat < 20, Lat, Lon)) %>%
  select(-c(Lat, Lon)) %>%
  rename(Lat = LatFix, Lon = LonFix)

# fix date/time
parkingClean$Issue.Date.and.Time <- mdy_hms(parkingClean$Issue.Date.and.Time)
parkingClean <- parkingClean %>%
  mutate(DayOfWeek = wday(Issue.Date.and.Time, label = TRUE),
         WeekNumber = week(Issue.Date.and.Time),
         Month = month(Issue.Date.and.Time, label = TRUE),
         Year = year(Issue.Date.and.Time))

# clean up location data
parkingClean <- parkingClean %>%
  select(-c(Location, Violation.Location))
parkingClean <- separate(parkingClean, Location.Standardized, c('Address', 'Zip'), sep = ',')
parkingClean$Zip <- stringr::str_trim(parkingClean$Zip)
parkingClean$Zip <- clean.zipcodes(parkingClean$Zip)

# separate out PPA data
PPA <- parkingClean %>%
  filter(Issuing.Agency == "PPA")

# filter zipShape data based on zip codes in dataset
parkingZip <- unique(PPA$Zip)
zipShape <- zipShape[zipShape$ZCTA5CE10 %in% parkingZip, ]
cleanZipShape <- fortify(zipShape, region = 'ZCTA5CE10')
cleanZipShape <- merge(cleanZipShape, zipShape@data, by.x = 'id', by.y = 'ZCTA5CE10')

# Plot data ---------------------------------------------------------------

# examine PPA violations by day of week
dayBar <- PPA %>%
  group_by(DayOfWeek) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = DayOfWeek, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.title.x = element_blank()) +
  ylab("Total Number of Violations")
dayBar

# examine weekly PPA violations over the course of the year
weekLine <- PPA %>%
  group_by(WeekNumber) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = WeekNumber, y = Count)) +
  geom_line() +
  xlab('Week Number') +
  ylab('Total Number of Violations')
weekLine

# examine monthly PPA violations
monthBar <- PPA %>%
  group_by(Month) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Month, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.title.x = element_blank()) +
  ylab("Total Number of Violations")
monthBar

# violations by Zip
PPA$Zip <- as.factor(PPA$Zip)
zipBar <- PPA %>%
  group_by(Zip) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Zip, y = Count)) +
  geom_bar(stat = "identity") +
  xlab('Zip Code') +
  ylab("Total Number of Violations")
zipBar

# create bounding box for map using zipShape data
b <- bbox(zipShape)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])

# produce map of violations by zip code
map <- get_map(location = b, source = 'stamen', maptype = 'toner', crop = TRUE)
m <- ggmap(map)
zipCount <- PPA %>%
  group_by(Zip) %>%
  summarise(Count = n())
zipData  <- merge(zipCount, cleanZipShape, by.x = 'Zip', by.y = 'id')
zipCountMap <- m + 
  geom_polygon(data = zipData, aes(x = long, y = lat, group = Zip, fill = Count), color = 'black')  

# produce map of total violation revenue by zip code
zipRevenue <- PPA %>%
  group_by(Zip) %>%
  summarise(Revenue = sum(Fine))
zipRevenue <- merge(zipRevenue, cleanZipShape, by.x = 'Zip', by.y = 'id')
zipRevenueMap <- m +
  geom_polygon(data = zipRevenue, aes(x = long, y = lat, group = Zip, fill = Revenue), color = 'black')