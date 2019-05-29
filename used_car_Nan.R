library(ggplot2)

#setwd('/Users/nantang/Google Drive/STAT 403/Project')
setwd('/Users/nantang/Desktop/Used-Car-Analysis')
car_dt <- read.csv('tc.csv', header=T)

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

# catogorize car make
make_rank_dt <- read.csv('ordered_grouped_make.csv')
library(pastecs)

# use kde make average price, split by local minimum
# to reduce car make category's correlation to price
make_avg_den <- density(make_rank_dt$average, bw=8000)
ts_y<-ts(make_avg_den$y)
tp<-turnpoints(ts_y)
x_tp <- make_avg_den$x[tp$tppos]
y_tp <- make_avg_den$y[tp$tppos]
split_pt2 <- x_tp[which.min(y_tp)]
split_pt1 <- x_tp[which.max(y_tp)]
# now we have two split points

plot(density(make_rank_dt$average, bw=10000), lwd=2, 
     main='Kernel Density of Make Average Price')
abline(v=c(split_pt1, split_pt2), lty=3, lwd=2, col='red')

make_category <- rep(NA, nrow(make_rank_dt))
for (ii in 1:nrow(make_rank_dt)) {
  avg <- make_rank_dt$average[ii]
  if (avg <= split_pt1) {
    make_category[ii] <- 'low-end'
  } else if (avg > split_pt1 & avg <= split_pt2) {
    make_category[ii] <- 'mid-range'
  } else {
    make_category[ii] <- 'high-end'
  }
}

car_make_category <- data.frame(Make=make_rank_dt$Make, Category=make_category)
write.csv(car_make_category, 'make_category.csv')

# updata car dataset
temp <- rep(NA, nrow(car_dt))
for (ii in 1:nrow(car_dt)) {
  temp_make <- as.character(car_dt$Make[ii])
  temp[ii] <- as.character(car_make_category[car_make_category$Make==temp_make,2])
}

new_tc <- data.frame(car_dt, Category=as.factor(temp))
write.csv(new_tc, 'new_tc.csv')
