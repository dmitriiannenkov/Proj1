### Data Analytics
### Author: Caesal Cheng
### jc10642@nyu.edu

#PROJECT 1: Restaurant

#Load the .csv data
Restaurant="https://data.cityofnewyork.us/resource/qcdj-rwhu.csv"
Restaurant<-read.csv(Restaurant, header=TRUE,stringsAsFactors=F)
Restaurant
head(Restaurant)
str(Restaurant)

# Business licenses are divided into active and inactive, so I think we should classify both and study only the active business restaurants.
activeR=subset(Restaurant,Restaurant$lic_status=="Active") #Table of active license Restaurant
inactiveR=subset(Restaurant,Restaurant$lic_status=="Inactive") #Table of inactive license Restaurant

# City Distribution
city=aggregate(Restaurant[,c(1)], by=list(Restaurant$city), length) #Showing the frequency of restaurant in each city
names(city)[names(city) == "x"] <- "number"
pie(city$number, labels = city$number, main="City distribution") #Showing the Pie chart of city distribution

# classify the type of restaurant and calculate the application duration
TotalEnclosed=aggregate(list(Count=Restaurant$license_nbr), by=list(type=Restaurant$app_swc_type, Location=Restaurant$city, zip=Restaurant$zip), FUN=length) 
Restaurant$app_status_date=as.Date(Restaurant$app_status_date)

ApplicationDuration <- function(submitTime, statusTime){
  TimeDiff <- difftime(as.Date(statusTime),as.Date(submitTime),units = "days")
  return(TimeDiff)
}

#TimeDifference=aggregate(Restaurant[,c(1)], by=list(Restaurant$app_status), function(Restaurant$submit_date,Restaurant$app) ApplicationDuration(Restaurant$submit_date,Restaurant$app){})

Restaurant<-cbind(Restaurant,ApplicationTime=difftime(as.Date(Restaurant$app_status_date),as.Date(Restaurant$submit_date),units = "days")) # Checks for a difference in time between submission date and app status

#Application Type
type=aggregate(list(Number=Restaurant[,c(1)]),by=list(app_type=Restaurant$app_status,dpqa_type=Restaurant$dpqa),length)
Status <- paste(type$app_type,type$dpqa_type)
stepview <- data.frame(Status,Number=type$Number)
library(plotrix)
pie(type$Number, labels = Status,col=rainbow(8), main="Application Step-view")

#Map display
locations=aggregate(list(count=Restaurant$swc_sq_ft), list(lat=Restaurant$latitude, lon=Restaurant$longitude, place=Restaurant$zip),FUN=sum) #aggregate the geo location
library(ggmap)
library(gdtools)
library(fs)
library(sf)
library(mapview)
register_google(key = "AIzaSyAKl-9k7zIBHgqXRExg6XCeEuxf5BmdazY") #using Google API
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf) #showing overall map

#TOP 3 Cities Map
NYClocations=aggregate(list(count=subset(Restaurant,Restaurant$city=="NEW YORK")$swc_sq_ft), list(lat=subset(Restaurant,Restaurant$city=="NEW YORK")$latitude, lon=subset(Restaurant,Restaurant$city=="NEW YORK")$longitude, place=subset(Restaurant,Restaurant$city=="NEW YORK")$zip),FUN=sum)
NYClocations_sf <- st_as_sf(NYClocations, coords = c("lon", "lat"), crs = 4326)
mapview(NYClocations_sf)#showing NYC map
BROOKLYNlocations=aggregate(list(count=subset(Restaurant,Restaurant$city=="BROOKLYN")$swc_sq_ft), list(lat=subset(Restaurant,Restaurant$city=="BROOKLYN")$latitude, lon=subset(Restaurant,Restaurant$city=="BROOKLYN")$longitude, place=subset(Restaurant,Restaurant$city=="BROOKLYN")$zip),FUN=sum)
BROOKLYNlocations_sf <- st_as_sf(BROOKLYNlocations, coords = c("lon", "lat"), crs = 4326)
mapview(BROOKLYNlocations_sf)#showing BROOKLYN map
ASTORIAlocations=aggregate(list(count=subset(Restaurant,Restaurant$city=="ASTORIA")$swc_sq_ft), list(lat=subset(Restaurant,Restaurant$city=="ASTORIA")$latitude, lon=subset(Restaurant,Restaurant$city=="ASTORIA")$longitude, place=subset(Restaurant,Restaurant$city=="ASTORIA")$zip),FUN=sum)
ASTORIAlocations_sf <- st_as_sf(ASTORIAlocations, coords = c("lon", "lat"), crs = 4326)
mapview(ASTORIAlocations_sf)#showing ASTORIA map