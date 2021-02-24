#PROJECT 1: Restaurant

#Load the .csv data
Resturant="https://data.cityofnewyork.us/resource/qcdj-rwhu.csv"
Resturant<-read.csv(Resturant, header=TRUE,stringsAsFactors=F)
Resturant
head(Resturant)
str(Resturant)

# Business licenses are divided into active and inactive, so I think we should classify both and study only the active business restaurants.
activeR=subset(Resturant,Resturant$lic_status=="Active") #Table of active license Restaurant
inactiveR=subset(Resturant,Resturant$lic_status=="Inactive") #Table of inactive license Restaurant

# City Distribution
city=aggregate(Resturant[,c(1)], by=list(Resturant$city), length) #Showing the frequency of restaurant in each city
names(city)[names(city) == "x"] <- "number"
pie(city$x, labels = city$number, main="City distribution") #Showing the Pie chart of city distribution

#Map display
locations=aggregate(list(count=Resturant$swc_sq_ft), list(lat=Resturant$latitude, lon=Resturant$longitude, place=Resturant$zip),FUN=sum) #aggregate the geo location
library(ggmap)
library(gdtools)
library(fs)
library(sf)
library(mapview)
register_google(key = "AIzaSyAKl-9k7zIBHgqXRExg6XCeEuxf5BmdazY") #using google API
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf) #showing map
