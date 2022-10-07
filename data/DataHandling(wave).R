########################################
##Packages
########################################
require(xts)
require(forecast)
#for maps
require(maps)    # Provides functions that let us plot the maps
require(mapdata)    # Contains the hi-resolution points that mark out the countries.
require(sp)
require(rgdal)
require(maptools)
require(rgeos)
require(GeoXp)
require(ggplot2) #to use fortify function
library(shapefiles) #to simplyfy shape files
##For kriging
require(geoR) #for kriging
require(gstat)
require(rrcov)
require(automap)
#for spatial GEVs
require(SpatialExtremes)
require(fExtremes) #for GEV estimation
require(extRemes) #for GEV estimation and model checking
require(ismev)
require(gstat)
########################################
##Read data
########################################
setwd("~/Dropbox/Data/Waveheight")
temp = list.files()
temp <- temp[c(1:110)]
numbers <- c()
names <- c()
for(l in 1:length(temp)){
  numbers <- c(numbers, as.numeric(strsplit(temp[l], "\\_")[[1]][length(strsplit(temp[l], "\\_")[[1]])-1]))
  names <- c(names, strsplit(temp[l], "\\_")[[1]][length(strsplit(temp[l], "\\_")[[1]])])
}

time_index_hourly <- seq(from = as.POSIXct("1996-01-01 00:00"), to = as.POSIXct("2021-12-31 23:00"), by = "hour")
header_tag <- c("number", "date", "temp", "maxwave", "sigwave", "avgwave", "wavecycle")

#for loop
for(i in 1:length(temp)){
  setwd(temp[i])
  temp_inside = list.files()
  assign(names[i], c())
  for(j in 1:length(temp_inside)){
    assign(names[i], rbind(get(names[i]), read.table(temp_inside[j], sep=",", header=F, skip=7)))
  }
  setwd("~/Dropbox/Data/Waveheight")
}
finaldata <-c()

for(j in 1:length(names)){
  data <- get(names[j])
  if(ncol(data)>7){
    data <- data[,c(1,2,9,10,11,12,13)]
  }
  colnames(data) <- header_tag
  data <- within(data, datetime <- as.POSIXlt(paste(date),format = "%Y-%m-%d %H", tz="Asia/Seoul"))
  eventdata <- xts(rep(-1,length(time_index_hourly)), order.by = time_index_hourly)
  if(time_index_hourly[1]!=data$datetime[1]){
    NAtime <- seq(from = as.POSIXct("1996-01-01 00:00"), to = as.POSIXct(data$datetime[1])-3600, by = "hour")
    eventdata[NAtime] <- -999
  }
  if(time_index_hourly[length(time_index_hourly)]!=data$datetime[length(data$datetime)]){
    NAtime2 <- seq(from = as.POSIXct(data$datetime[length(data$datetime)])+3600, to = as.POSIXct("2021-12-31 23:00"), by = "hour")
    eventdata[NAtime2] <- -999
  }
  
  dupindex <- which(duplicated(data$datetime) | duplicated(data$datetime[nrow(data):1])[nrow(data):1])
  eliindex <- c()
  if(length(dupindex)!=0){
    for(i in 1:length(dupindex)){
      if(is.na(data$"temp"[dupindex[i]])==TRUE & is.na(data$maxwave[dupindex[i]])==TRUE & is.na(data$sigwave[dupindex[i]])==TRUE & is.na(data$avgwave[dupindex[i]])==TRUE && is.na(data$wavecycle[dupindex[i]])==TRUE){
        eliindex <- c(eliindex, dupindex[i])
      }
    }
  }
  
  if(is.null(eliindex)==F){
    data <- data[-eliindex,]
  }
  #data$precip[is.na(data$precip)] <- 0
  #final imputation
  for(i in 1:nrow(data)){
    eventdata[which(time(eventdata) == data$datetime[i])] <- data$maxwave[i]
  }
  eventdata[eventdata==-1 | eventdata==-999,] <- NA
  colnames(eventdata) <- names[j]
  
  finaldata <- cbind(finaldata, eventdata)
}

setwd("~/Dropbox/")
saveRDS(finaldata, file="Waveheight.RDS")

data <- readRDS("~/Dropbox/Data/Waveheight/Waveheight.RDS")
colnames(data) <- substring(colnames(data),2)

place <- read.csv("~/Dropbox/Data/Waveheight/place_waveheight.txt", header=T, sep=",")
place$type <- ifelse(as.numeric(place$지점)<=22400, "해양기상부이", "파고부이")
data <- data[,colnames(data) %in% as.character(place$지점)]


shape <- readShapePoly("~/Dropbox/Maps/KOR_COASTLINE/eez.shp")
area <- lapply(shape@polygons, function(x) 
  sapply(x@Polygons, function(y) y@area))
mainPolys <- lapply(area, function(x) which(x > 0.001))
shape@plotOrder <- 1:length(shape@polygons)
for(i in 1:length(mainPolys)){
  if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
    shape@polygons[[i]]@Polygons <- shape@polygons[[i]]@Polygons[mainPolys[[i]]]
    shape@polygons[[i]]@plotOrder <- 1:length(shape@polygons[[i]]@Polygons)
  }
}
for(i in 1:length(shape@polygons)){
  for(j in 1:length(shape@polygons[[i]]@Polygons)){
    temp <- as.data.frame(shape@polygons[[i]]@Polygons[[j]]@coords)
    names(temp) <- c("x", "y")
    temp2 <- dp(temp, 0.01)
    shape@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
  }
}
shape2 <- readShapePoly("~/Dropbox/Maps/KOR_COASTLINE/eez.shp")

KoreaWaveheight <- list()
KoreaWaveheight$data <- data
KoreaWaveheight$place <- place
KoreaWaveheight$shape <- shape
KoreaWaveheight$origshape <- shape2
saveRDS(KoreaWaveheight, "~/Dropbox/Data/Waveheight/KoreaWaveheight.RDS")
