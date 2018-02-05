freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}   

fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
  return(temp_K)
}

getDistance <- function(starbucksLon,starbucksLat,long,lati) {
  temp_K <- sqrt((starbucksLon-long)^2+(starbucksLat-lati)^2)
  return(temp_K)
}




library(devtools)
install_github("gregce/ipify")

library(ipify)
my_ip<-get_ip()

myresults<-freegeoip(my_ip)

myLongitude<-myresults$longitude
myLatitude<-myresults$latitude

locations <- read.csv("starbucks_us_locations.csv")

colnames(locations) <- c("long","lat","name","address")




locations$distanceFromHere<-getDistance(locations$long,locations$lat,myLongitude,myLatitude)

locationsSorted<-locations[with(locations, order(distanceFromHere)), ]

locationsDesired=48

myDesiredCords<-locationsSorted[1:locationsDesired,c(1:2,5)]

shu.map <- GetMap(center = c(as.integer(myLongitude), as.integer(myLatitude)), zoom=11)
PlotOnStaticMap(shu.map)



latitudes<-myDesiredCords[,2]
longitudes<-myDesiredCords[,1]                    
MyMap <- MapBackground(lat=latitudes, lon=longitudes)
PlotOnStaticMap(MyMap,latitudes,longitudes,cex = 2,pch=20,col="red")
PlotOnStaticMap(MyMap,myLatitude,myLongitude,cex = 3,pch=20,col="blue",add=TRUE)



