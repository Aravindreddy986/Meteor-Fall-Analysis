##  fetching the details for the region United states
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

