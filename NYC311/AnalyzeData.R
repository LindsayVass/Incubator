library(data.table)
library(dplyr)
library(lubridate)
library(aspace)

nyc <- fread('nyc311calls.csv', sep = ',')

# rank agencies
agencyRank <- nyc %>%
  group_by(Agency) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# proportion of 2nd most popular agency
agency2Prop <- agencyRank$Count[2] / sum(agencyRank$Count)

# latitude percentiles
latPct10 <- quantile(nyc$Latitude, .1, na.rm = TRUE)
latPct90 <- quantile(nyc$Latitude, .9, na.rm = TRUE)
latDiff  <- latPct90 - latPct10

# clean time
nyc$`Created Date` <- mdy_hms(nyc$`Created Date`)
nyc$Hour <- hour(nyc$`Created Date`)
hist(nyc$Hour) # high number of 0s suggests inaccurate time
nycHour <- as.data.frame(nyc$Hour)
names(nycHour) = 'Hour'
nycHour <- nycHour %>%
  filter(Hour != 0) %>%
  group_by(Hour) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
hist(nycHour$Hour) # much better
hourCallDiff <- nycHour$Count[1] - nycHour$Count[nrow(nycHour)]

# get time between consecutive calls
callTimes <- nyc %>%
  select(c(`Created Date`, Hour)) %>%
  filter(Hour != 0) %>%
  arrange(`Created Date`)
firstCallTime  <- callTimes[1:nrow(callTimes) - 1,] %>%
  select(-Hour) %>%
  rename(FirstTime = `Created Date`)
secondCallTime <- callTimes[2:nrow(callTimes),] %>%
  select(-Hour) %>%
  rename(SecondTime = `Created Date`)
diffSecs <- cbind(firstCallTime, secondCallTime) %>%
  mutate(DiffSecs = seconds(SecondTime - FirstTime))
sdDiffSecs <- sd(diffSecs$DiffSecs)

# select coordinates of all calls
callCoords <- nyc %>%
  select(Latitude, Longitude) %>%
  filter(is.na(Latitude) == FALSE,
         is.na(Longitude) == FALSE)
sde <- calc_sde(id = 1, filename = 'SDE_Output.txt', points = callCoords, verbose = TRUE)

# degree to km conversions
# source: http://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-km-distance
latKm <- 110.574
lonKm <- abs(111.32*cos(sdeatt$CENTRE.x))
deg2Km2 <- mean(c(latKm, lonKm))^2

sde_km2 <- sdeatt$Area.sde * deg2Km2

# complaint types by borough
complaints <- nyc %>%
  select(c(`Complaint Type`, Borough))
names(complaints) <- c('Complaint', 'Borough')
complaintDist <- complaints %>%
  group_by(Complaint, Borough) %>%
  summarise(Count = n())
totalComplaints <- complaintDist %>%
  ungroup() %>%
  group_by(Complaint) %>%
  summarise(Count = sum(Count)) 
totCount <- sum(totalComplaints$Count)
totalComplaints <- totalComplaints %>%
  mutate(Prob = Count / totCount) %>%
  select(-Count)
boroughComplaints <- complaintDist %>%
  ungroup() %>%
  group_by(Borough) %>%
  summarise(Count = sum(Count))
boroughComplaints <- boroughComplaints %>%
  mutate(BorProb = Count / totCount) %>%
  select(-Count)
condProb <- complaintDist %>%
  mutate(ComBorProb = Count / totCount)
condProb <- inner_join(condProb, boroughComplaints)
condProb <- condProb %>%
  mutate(CondProb = ComBorProb / BorProb)
condProb <- inner_join(condProb, totalComplaints) %>%
  mutate(Surprise = CondProb / Prob) %>%
  arrange(desc(Surprise))
maxSurprise <- condProb$Surprise[1]