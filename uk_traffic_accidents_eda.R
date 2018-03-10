setwd("~/R Work")
#Load England traffic data
accidents_05_to_07 <- read.csv("~/R Work/accidents_2005_to_2007.csv", header = TRUE, stringsAsFactors = FALSE)
accidents_09_to_11 <- read.csv("~/R Work/accidents_2009_to_2011.csv", header = TRUE, stringsAsFactors = FALSE)
accidents_12_to_14 <- read.csv("~/R Work/accidents_2012_to_2014.csv", header = TRUE, stringsAsFactors = FALSE)

acc_05_11 <- rbind(accidents_05_to_07, accidents_09_to_11)
acc <- rbind(acc_05_11, accidents_12_to_14)
acc$Location_Easting_OSGR <- NULL
acc$Location_Northing_OSGR <- NULL
std_traffic <- read.csv("~/R Work/ukTrafficAADF.csv", header = TRUE, stringsAsFactors = FALSE)
std_traffic$Easting <- NULL
std_traffic$Northing <- NULL

#Let's run some EDAs

#What is the yearwise trend in road accidents?
acc_by_yr <- aggregate(acc$Accident_Index ~ acc$Year, acc, function(x)length(unique(x)))
#accidents in the UK have pretty consistently gone down. Interesting.

#Which is the most common day for road accidents?
acc_by_day <- aggregate(acc$Accident_Index ~ acc$Day_of_Week, acc, function(x)length(unique(x)))
acc_by_day <- acc_by_day[order(-acc_by_day$`acc$Accident_Index`),]
#Saturday followed by Friday is the day with most accidents
library(ggplot2)
day_plot <- ggplot(data= acc_by_day, aes(x=acc_by_day$`acc$Day_of_Week`, y=acc_by_day$`acc$Accident_Index`)) + geom_bar(stat="identity")
day_plot

#What is the general trend according to month?
acc$month <- strftime(x = acc$Date, format = "%m")
acc_by_month <- aggregate(acc$Accident_Index ~ acc$month, acc, function(x)length(unique(x)))
month_plot <- ggplot(data= acc_by_month, aes(x=acc_by_month$`acc$month`, y=acc_by_month$`acc$Accident_Index`)) + geom_bar(stat="identity")
month_plot
#Generally grows as the year goes on, but not by much. Is maximum in October and November though,
#despite there being no public holidays in Britain then, which is interesting.

#What is the time of day when most accidents occur?
grab.hrs <- function(vals) as.numeric(sub(pattern = ":.*", replacement = "",
                                          x = vals))
acc$hour <- grab.hrs(acc$Time)
acc_by_hour <- aggregate(acc$Accident_Index ~ acc$hour, acc, function(x)length(unique(x)))
#Most accidents occur between 3-5 pm: Rush hour.
total_accidents <- length(unique(acc$Accident_Index))
acc_by_hour$percent <- (acc_by_hour$`acc$Accident_Index` / total_accidents) * 100
hour_plot <- ggplot(data= acc_by_hour, aes(x=acc_by_hour$`acc$hour`, y=acc_by_hour$percent)) + geom_bar(stat="identity")
hour_plot

#What is the lighting conditions in most of the accidents?
acc_by_light <- aggregate(acc$Accident_Index ~ acc$Light_Conditions, acc, function(x)length(unique(x)))
acc_by_light <- acc_by_light[order(-acc_by_light$`acc$Accident_Index`),]
#Let's see where the dark without street lights accidents are located
no_light <- acc[acc$Light_Conditions == "Darkeness: No street lighting",]
nolight_map <- ggmap(acc_map) + geom_point(aes(x = Longitude, y = Latitude), data = no_light, alpha = .5, colour = 'black')
nolight_map
#Most accidents occur even with streetlights present. Most of the streets in the UK must have street lights.

#How are accidents distributed by district?
acc_by_dist <- aggregate(acc$Accident_Index ~ acc$Local_Authority_.District., acc, function(x)length(unique(x)))
acc_by_dist <- acc_by_dist[order(-acc_by_dist$`acc$Accident_Index`),]
dist_plot <- ggplot(data= acc_by_dist, aes(x=acc_by_dist$`acc$Local_Authority_.District.`, y=acc_by_dist$`acc$Accident_Index`)) + geom_bar(stat="identity")
dist_plot

#which are the busiest roads and which are the deadliest?
busiest <- cbind(std_traffic$Road, std_traffic$AllMotorVehicles)
deadliest <- aggregate(acc$Accident_Index ~ acc$Local_Authority_.Highway., acc, function(x)length(unique(x)))

library(ggmap)
#Let's see the distribution geographically, on a map
acc_map <- get_map(location = 'UK', zoom = 6)
mapPoints <- ggmap(acc_map) + geom_point(aes(x = Longitude, y = Latitude), data = acc, alpha = .5, colour = 'red')
mapPoints
#What are the weather conditions in an accident?
acc_by_weather <- aggregate(acc$Accident_Index ~ acc$Weather_Conditions, acc, function(x)length(unique(x)))
#'fine without high winds' has most accidents, unsurprising because those are the most common weather conditions you can find.

#Let's try to look at the distribution of accidents by severity level
level1 <- acc[acc$Accident_Severity == 1,]
level2 <- acc[acc$Accident_Severity == 2,]
level3 <- acc[acc$Accident_Severity == 3,]

#Plot accidents by level
mapl1 <- ggmap(acc_map) + geom_point(aes(x = Longitude, y = Latitude), data = level1, alpha = .5, colour = 'yellow')
mapl2 <- ggmap(acc_map) + geom_point(aes(x = Longitude, y = Latitude), data = level2, alpha = .5, colour = 'yellow')
mapl3 <- ggmap(acc_map) + geom_point(aes(x = Longitude, y = Latitude), data = level3, alpha = .5, colour = 'yellow')
#Notice how accidents, no matter their severity, are indeed concentrated around the same areas

#How many casualties are there generally?
acc_by_cas <- aggregate(acc$Accident_Index ~ acc$Number_of_Casualties, acc, function(x)length(unique(x)))

#Look at casualties based on district
cas_by_dist <- aggregate(acc$Number_of_Casualties ~ acc$Local_Authority_.District., acc, mean)

