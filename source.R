dataPrin <- read.csv(
  'activity.csv', colClasses = c('integer','Date','integer')
)
#dataPrin$nameD <- weekdays(as.Date(dataPrin$date))

dataPrinGroupInterval <- aggregate(
  list(steps = dataPrin$steps), 
  by = list(interval = dataPrin$interval) ,FUN=mean,
  na.rm = T
)
maxSteps <- subset(dataPrinGroupInterval,
                   steps==max(dataPrinGroupInterval$steps)
)

plot(
  x = dataPrinGroupInterval$interval,
  dataPrinGroupInterval$steps, type="l",
  main = "Mean of Steps per Interval",
  xlab = "Inteval", ylab = "Steps"
)
abline(v = maxSteps$interval, col = "blue", lty=2)+
  abline(h = maxSteps$steps, col = "blue", lty=2)+
  text(maxSteps$interval,0,maxSteps$interval)+
  text(0,maxSteps$steps,round(maxSteps$steps,1))

newData <- dataPrin
newData$nameD <- weekdays(as.Date(newData$date))
newData$id = seq(nrow(newData))
newData$nDay <- weekdays(newData$date)
newData$nMonth <- months(newData$date)

dataNA <- newData[is.na(newData$steps),]
nrow(dataNA)

for(reg in 1:nrow(dataNA)){
  dataTemp <- subset(newData, 
    nMonth == dataNA[reg,]$nMonth & nDay == dataNA[reg,]$nDay & 
    interval == dataNA[reg,]$interval 
  )
  newData[newData$id %in% dataTemp$id,'steps']<-mean(dataTemp$steps, na.rm = T)
}
dataPrinGroup2 <- aggregate(
  list(steps = newData$steps), 
  by = list(date = newData$date) ,FUN=sum
)
hist(dataPrinGroup2$steps, 
     main="Total number of steps taken each day",
     xlab = 'Number steps per day'
)

print(mean(newData$steps, na.rm = T))
print(median(newData$steps, na.rm = T))

