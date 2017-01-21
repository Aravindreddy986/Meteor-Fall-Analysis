#loading the required packages
library(ggplot2)
library(ggmap)
library(xlsx)
library(tcltk)
library(sqldf)
library(pastecs)
 

## loading the data file
meteriod <-  read.csv(file.choose(), sep = ",", header = TRUE)
head(meteriod)

### cleaning the dataset by selecting the years between 860 and 2016
attach(meteriod)

meteriodt1 <- subset(meteriod, (year >=860 & year <= 2016) )
meteriodt1 <- subset(meteriodt1,(reclong<=180 & reclong>=-180 & (reclat!=0 | reclong!=0)))

meteriodt1 <- na.omit(meteriodt1)

# creating a sample data.frame with your lat/lon points
longlat <- as.data.frame(cbind(meteriodt1$reclong,meteriodt1$reclat))

## for fetching the location based on the longitude and latitude - but not working
for (i in 20001:nrow(meteriodt1))
{
  ##meteriodt1$location[i]<-
    revgeocode(c(meteriodt1$reclong[i], meteriodt1$reclat[i]))
}

#### longitude and latitude and region world 
world <- map_data("world")
head(world)

## plotting the meteroid locations
ggplot() +
  theme_bw() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill=NA, color="blue") +
  labs(y="latitude", x="longitude", alpha="Density of meteorites", title=paste("Density of all meteorite impacts in the world")) +
  stat_density2d(na.rm=TRUE,data=meteriodt1, aes(x=reclong, y=reclat, alpha=..level..),geom="polygon",fill="#FF2222") 

# Antartica longitude, latitude and region of the  world  
worldantartic <- subset(world, (world$region == "Antarctica"))
worldantartic <- worldantartic[order(worldantartic$lat),]

# Antarctic meteorite finds are so common that The Meteoritical Society (the source of this dataset) 
# has an option to only search non-Antarctic meteorites, just so the Antarctic finds don't completely overwhelm your search results.

ggplot(meteriodt1, aes(x=reclat)) + 
  geom_histogram(binwidth=5) +
  scale_x_continuous(breaks=seq(-90,90,by=10)) +
  labs(y="count", x="lattitude",  title=paste("Plot of count of all meteorite impacts based on the lattitude")) 

## Non Antartica longitude, latitude and region of the  world  
worldt <- subset(world, (world$lat <= -70))
antarticlatstarts <- max(worldantartic$lat)

## with out ntartica regions metroid data 
meteroidnonantartic <- subset(meteriodt1, (meteriodt1$reclat >= antarticlatstarts))

ggplot() +
  theme_bw() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill=NA, color="blue") +
  labs(y="latitude", x="longitude", alpha="Density of meteorites", title=paste("Density of all meteorite impacts in the world apart from antartica")) +
  stat_density2d(na.rm=TRUE,data=meteroidnonantartic, aes(x=reclong, y=reclat, alpha=..level..),geom="polygon",fill="#FF2222")


## fetching fell details, means removing the found details 
meteriodt2 <- meteriodt1
meteriodt2 <- subset(meteriodt2, (meteriodt2$fall == "Fell" ))

## order by year
meteriodt2 <- meteriodt2[order(meteriodt2$year),]

## year, cnt, sum(mass), avg(mass)
metetroidyearstat <- sqldf("Select year, count(*), SUM(mass) as sumofmass, AVG(MASS) as avgmass from meteriodt2 group by year")

metetroidyearstat$avgchangeinmass[1] <- 0 

for( i in 2: dim(metetroidyearstat)[1])
{
  metetroidyearstat$avgchangeinmass[i] <- (metetroidyearstat$avgmass[i] - metetroidyearstat$avgmass[i-1]) / (metetroidyearstat$avgmass[i-1]) 
}

## performing bootstrap for the stats dataset taking the year above 1900 and for 10 years and performing bootstrap for 1000 simulations

metetroidyearstat <- subset(metetroidyearstat, (metetroidyearstat$year >= 1900))
##metetroidyearstat <- metetroidyearstat[-114,]

## testing for 3 years taking initial change of 2014 as 2013's
for (i in 1:3)
{
  year <- metetroidyearstat$year[dim(metetroidyearstat)[1]] + i
  cnt <- 0
  sumofmass <- 0
  avgmass <- metetroidyearstat$avgmass[dim(metetroidyearstat)[1]]  #taking 2013 avgmass 
  randomchnge <- runif(1, min(metetroidyearstat$avgchangeinmass), max(metetroidyearstat$avgchangeinmass))
  print(randomchnge)
  avgmass <- avgmass* (1+randomchnge)
  
  metetroidyearstattest <- data.frame(year,cnt,sumofmass,avgmass,randomchnge)
  colnames(metetroidyearstattest) <- colnames(metetroidyearstat)
  
  metetroidyearstat <- rbind(metetroidyearstat,metetroidyearstattest)
  
}

## taking median of the avgmass change
for (i in 1:3)
{
  year <- metetroidyearstat$year[dim(metetroidyearstat)[1]] + 1
  cnt <- 0
  sumofmass <- 0
  avgmass <-  median(metetroidyearstat$avgmass) #taking median of the avgmass change
  randomchnge <- runif(1, min(metetroidyearstat$avgchangeinmass), max(metetroidyearstat$avgchangeinmass))
  print(randomchnge)
  avgmass <- avgmass* (1+randomchnge)
  
  metetroidyearstattest <- data.frame(year,cnt,sumofmass,avgmass,randomchnge)
  colnames(metetroidyearstattest) <- colnames(metetroidyearstat)
  
  metetroidyearstat <- rbind(metetroidyearstat,metetroidyearstattest)
  
}

## performing bootstrap for 1000 simulations

bootstrap <- data.frame()

for(j in 1:1000)
{
  metetroidyearstat <- metetroidyearstat[-114:-123,]
  ## taking median of the avgmass chang
  for (i in 1:10)
  {
    year <- metetroidyearstat$year[dim(metetroidyearstat)[1]] + 1
    cnt <- 0
    sumofmass <- 0
    avgmass <-  median(metetroidyearstat$avgmass) #taking median of the avgmass change
    randomchnge <- runif(1, min(metetroidyearstat$avgchangeinmass), max(metetroidyearstat$avgchangeinmass))
    print(randomchnge)
    avgmass <- avgmass* (1+randomchnge)
    
    metetroidyearstattest <- data.frame(year,cnt,sumofmass,avgmass,randomchnge)
    colnames(metetroidyearstattest) <- colnames(metetroidyearstat)
    
    metetroidyearstat <- rbind(metetroidyearstat,metetroidyearstattest)
    
  }
  final2024mass <- metetroidyearstat$avgmass[123]

  bootstraptest <- data.frame(j,final2024mass)
  bootstrap <- rbind(bootstrap,bootstraptest)  
  
  
}

head(bootstrap)
stat.desc(bootstrap$final2024mass)
summary(bootstrap$final2024mass)

##  fetching the details for the region India

meteroidus <- meteriodt1
meteroidus <- subset(meteriodt1,reclat>=25 & reclat<=47 & reclong>=70 & reclong<=122)

## order us details based on year, lat, long
meteroidus <- meteroidus[order(meteroidus$year,meteroidus$reclat,meteroidus$reclong),]
meteroidus$dist[1] <- 0
meteroidus <- meteroidus[,c(-1,-2,-3,-4,-5,-6,-10)]  ## removing unnecessary columns

## remove duplicates 
meteroidus <- meteroidus[!duplicated(meteroidus),]

## finding distance
for(i in 2:dim(meteroidus)[1])
{
  x <- c(meteroidus$reclat[i-1],meteroidus$reclong[i-1])
  y <- c(meteroidus$reclat[i],meteroidus$reclong[i])  
  
  meteroidus$dist[i] <- dist(rbind(y,x))
}

head(meteroidus)

