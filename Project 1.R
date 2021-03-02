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
city=city[order(city[,2], decreasing = TRUE),]
city1 <- (city[1:3,])
city1=city1 %>% add_row(Group.1 = "Other", number = sum(city[4:17,c(2)]))
label=paste(city1[,1],city1[,2])
city1=cbind(city1,label)
pie(city1$number, labels = city1$label, main="City distribution") #Showing the Pie chart of city distribution

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
library(waffle) 
SS <- c(`Application Review Completed Approved  714`=714, `Application Denied Denied - Refund Request  1`=1,`Pending Review Issued Temp Op Letter  248`=248, `Application Review Completed Process Completed  8`=8,`Pending Review Review Completed  1`=1, `Application Review Completed Under Review  1`=1,`Pending Review Under Review  24`=24, `Application Withdrawn Withdrawal - Refund Request  3`=3)
waffle(SS, rows=30, size=0.5, 
       colors=c("thistle2", "darkolivegreen1", "#a0d0de", "cornflowerblue","#969696", "lightcyan2", "antiquewhite1", "aquamarine"), 
       title="Status of application distribution")

#pre-COVID & post-COVID expired
install.packages("lubridate")
library(lubridate)
Resturant$expiration_date=as.Date(Resturant$expiration_date)
exp=subset(Resturant,Resturant$expiration_date<'2020-12-31') #expired application licenses
act=subset(exp,exp$lic_status=="Active") # Active license which is expired
inact=subset(exp,exp$lic_status=="Inactive") # inactive license which is expired
plot(act$expiration_date)


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


#Load the .csv data
zipda="https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv"
zipda<-read.csv(zipda, header=TRUE,stringsAsFactors=F)
zipda
head(zipda)
str(zipda)

# City Distribution
cityzz=aggregate(zipda[,c(4)], by=list(zipda$BOROUGH_GROUP), sum) #Showing the frequency of restaurant in each city
Statuss <- paste(cityzz$Group.1,cityzz$x)
stepviews <- data.frame(Statuss,cityzz)
pie(stepviews$x, labels = stepviews$Statuss, main="City distribution")

#Load the .csv data
zipa="https://data.cityofnewyork.us/resource/pitm-atqc.csv"
zipa<-read.csv(zipa, header=TRUE,stringsAsFactors=F)
zipa
head(zipa)
str(zipa)

# City Distribution
cityz=aggregate(zipa[,c(1)], by=list(zipa$borough), length) #Showing the frequency of restaurant in each city
Statusss <- paste(cityz$Group.1,cityz$x)
stepviewss <- data.frame(Statusss,cityz)
pie(stepviewss$x, labels = stepviewss$Statusss, main="City distribution")
