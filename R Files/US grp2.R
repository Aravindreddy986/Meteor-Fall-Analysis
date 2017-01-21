## performing bootstrap for 1000 simulations here ordered us details based on lat, long
meteroidust2 <- meteroidus

## removing year
meteroidust2 <- meteroidust2[,-1]
## order by 
meteroidust2 <- meteroidust2[order(meteroidust2$reclat,meteroidust2$reclong),]

meteroidust2$distchange[1] <- 0
meteroidust2$distchange[2] <- 0

## finding dist change
for( i in 3: dim(meteroidust2)[1])
{
  meteroidust2$distchange[i]<- (meteroidust2$dist[i] - meteroidust2$dist[i-1]) / (meteroidust2$dist[i-1]) 
}

head(meteroidust2)

bootstrap3 <- data.frame()
meteroidust2 <- subset(meteroidust2, (meteroidust2$reclat != 31.20000))

for(j in 1:1000)
{
  meteroidust2 <- meteroidust2[-161:-170,]
  ## taking median of the avgmass chang
  for (i in 1:10)
  {
    reclat  <- 0
    reclong <- 0
    dist    <-  median(meteroidust2$dist) #taking median of the dist 
    randomchnge <- runif(1, min(meteroidust2$distchange), max(meteroidust2$distchange))
    print(paste(dist, randomchnge, randomchnge/10000, sep = "  "))
    dist <- dist* (1+(randomchnge/1000) )
    
    meteroidust2test <- data.frame(reclat,reclong,dist,randomchnge)
    colnames(meteroidust2test) <- colnames(meteroidust2)
    
    meteroidust2 <- rbind(meteroidust2,meteroidust2test)
    
  }
  
  final2021dist <- meteroidust2$dist[170]
  
  initialchange <- meteroidust2$distchange[dim(meteroidust2)[1] -10]
  finalchange   <- meteroidust2$distchange[dim(meteroidust2)[1]]
  changeperct <- ((finalchange - initialchange)/initialchange) 
  
  bootstraptest <- data.frame(j,changeperct,final2021dist)
  bootstrap3 <- rbind(bootstrap3,bootstraptest)  
  
}

stat.desc(bootstrap3$changeperct)
summary(bootstrap3$final2021dist)

head(meteroidust2)

write.xlsx(meteroidust2, "A:/meteroidust2.xlsx")