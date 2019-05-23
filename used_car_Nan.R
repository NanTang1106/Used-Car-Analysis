library(ggplot2)

setwd('/Users/nantang/Google Drive/STAT 403/Project')
car_dt <- read.csv('true_car_listings.csv', header=T)

sample_index <- sample(nrow(car_dt), 1000)
car_sample <- car_dt[sample_index, ]

nrow(car_dt[which(car_dt$Make=='Ferrari'),])

ferrari_dt <- car_dt[which(car_dt$Make=='Ferrari'),]

p1 <- ggplot(data = ferrari_dt, aes(x=Mileage, y=Price)) +
  geom_point(aes(col=Year)) + 
  geom_smooth(method=loess, se=F)

p2 <- ggplot(data = ferrari_dt, aes(x=State, y=Price)) +
  geom_point(aes(col=Model)) +
  geom_smooth(method=lm, se=F)

