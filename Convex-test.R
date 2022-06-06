#Importing necessary libraries
library(MASS)
library(tidyverse)
library(data.table)

#Country data import and first look
countries <- read.csv("Work-related/Convex/countries of the world.csv",
             dec = ",", header = TRUE, stringsAsFactors=FALSE)
View(countries)
str(countries)
summary(countries)

#Exploring the data with some plots
par(mfrow=c(2,2))
plot(countries$Area..sq..mi.., countries$Population, 
     log = "xy", col = "green", xlab = "Area (log of sq. m.)", 
     ylab = "Log of Population", main = "Area Vs Population")
boxplot(countries$GDP....per.capita. ~ countries$Climate,
     log = "y", varwidth = TRUE, col="light blue", xlab = "Region", 
     ylab = "Log of GDP per capita", main = "Regions by GDP per capita")
hist(countries$Birthrate, main = "Birthrates")
truehist(countries$Birthrate, col="red", main = "Probability density of Birthrates")

#Tallest buildings import 
buildings <- read_delim("Work-related/Convex/tallest_completed_buildings.csv")
View(buildings)
str(buildings)

#Largest cities import 
cities <- read.csv("Work-related/Convex/The Most Largest Cities in the World 1950 - 2035.csv",
          sep = ",", header = TRUE)
View(cities)
str(cities)
summary(cities)

#Transposing the cities data frame
tcities <- transpose(cities)

#Defining row and column names
rownames(tcities) <- colnames(cities)
colnames(tcities) <- paste(cities[,1], sep = ", ", cities[,2])

#Select correct rows and convert all variables to numeric
tcities <- slice(tcities, -(1:4))
tcities <- as.data.frame(apply(tcities, 2, as.numeric))

#Add year column
years <- c(1950:2035)
tcities$Year <- years 
View(tcities)
str(tcities)

#Creating time series plot
plot1 <- tcities %>%
  ggplot(aes(x=Year, y=`India, Mumbai`)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Population")
plot1

#Joining cities and buildings datasets (cb)
buildings <-rename(buildings, "City" = "CITY")
cb = left_join(x=buildings,y=cities,by="City")
View(cb)