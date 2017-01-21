## performing bootstrap for 1000 simulations order us details based on year, lat, long
meteroidust1 <- meteroidus

meteroidust1$distchange[1] <- 0
meteroidust1$distchange[2] <- 0

## finding dist change
for( i in 3: dim(meteroidust1)[1])
{
  meteroidust1$distchange[i]<- (meteroidust1$dist[i] - meteroidust1$dist[i-1]) / (meteroidust1$dist[i-1]) 
}

plot(meteroidust1$year, meteroidust1$distchange)
bootstrap2 <- data.frame()

for(j in 1:1000)
{
  meteroidust1 <- meteroidust1[-162:-171,]
  ## taking median of the avgmass chang
  for (i in 1:10)
  {
    year <- meteroidust1$year[dim(meteroidust1)[1]] + 1
    reclat <- 0
    reclong <- 0
    dist <- median(meteroidust1$dist)   
    randomchnge <- runif(1, min(meteroidust1$distchange), max(meteroidust1$distchange))
    print(paste(dist, randomchnge,randomchnge/10000, sep = "  "))
    dist <- dist* (1+ (randomchnge/100) )
    
    meteroidust1test <- data.frame(year,reclat,reclong,dist,randomchnge)
    colnames(meteroidust1test) <- colnames(meteroidust1)
    
    meteroidust1 <- rbind(meteroidust1,meteroidust1test)
    
  }
  
  final2021dist <- meteroidust1$dist[171]
  
  initialchange <- meteroidust1$distchange[dim(meteroidust1)[1] -10]
  finalchange   <- meteroidust1$distchange[dim(meteroidust1)[1]]
  changeperct <- ((finalchange - initialchange)/initialchange) 
  
  bootstraptest <- data.frame(j,changeperct,final2021dist)
  bootstrap2 <- rbind(bootstrap2,bootstraptest)  
  
}

stat.desc(bootstrap2$changeperct)
summary(bootstrap2$final2021dist)

head(meteroidust1)


write.xlsx(meteroidust1, "A:/meteroidust1.xlsx")

