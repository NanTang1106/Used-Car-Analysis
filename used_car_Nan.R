library(ggplot2)
library(dplyr )
library(ggthemes)
library(gridExtra)
library(readr)
library(plotrix)

#setwd('/Users/nantang/Google Drive/STAT 403/Project')
setwd('/Users/nantang/Desktop/Used-Car-Analysis')
car_dt <- read_csv('tc.csv')

# Ferrari information
sample_index <- sample(nrow(car_dt), 1000)
car_sample <- car_dt[sample_index, ]

nrow(car_dt[which(car_dt$Make=='Ferrari'),])

ferrari_dt <- new_tc[which(new_tc$Make=='Ferrari'),]

p1 <- ggplot(data = ferrari_dt, aes(x=Mileage, y=Price)) +
  geom_point(aes(col=Year)) + 
  geom_smooth(method=loess, se=F)

p2 <- ggplot(data = ferrari_dt, aes(x=State, y=Price)) +
  geom_point(aes(col=Model)) +
  geom_smooth(method=lm, se=F)


## catogorize car make
make_rank_dt <- read_csv('ordered_grouped_make.csv')
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


## find out most popular car make in each category
new_tc <- read_csv('new_tc.csv')

low_end_count <- new_tc %>% 
  filter(Category=='low-end') %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

mid_range_count <- new_tc %>% 
  filter(Category=='mid-range') %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

high_end_count <- new_tc %>% 
  filter(Category=='high-end') %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

low_end_mp <- as.character(low_end_count[[1]][1])
mid_range_mp <- as.character(mid_range_count[[1]][1])
high_end_mp <- as.character(high_end_count[[1]][1])

# get data of most popular car in each catetory
low_end_mp_dt <- new_tc %>%
  filter(Make == low_end_mp)
mid_range_mp_dt <- new_tc %>%
  filter(Make == mid_range_mp)
high_end_mp_dt <- new_tc %>% 
  filter(Make == high_end_mp)

# remove outlier
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

low_end_dt <- new_tc %>%
  filter(Category == "low-end")
mid_range_dt <- new_tc %>%
  filter(Category == "mid-range")
high_end_dt <- new_tc %>% 
  filter(Category == "high-end")

#g <- ggplot(data=rbind(low_end_dt, mid_range_dt, high_end_dt))
#g + geom_density(aes(x=remove_outliers(Mileage), fill=Category), alpha=0.6)

# tufteboxplot - mileage 
# low-end
theme_set(theme_tufte()) 
g_le <- ggplot(data=low_end_dt)
g_le_ml_box <- g_le + geom_tufteboxplot(aes(x=Make, y=Mileage)) +
  ylim(0, 300000) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Low-end",
       y="Mileage")

# mid-range
g_md <- ggplot(data=mid_range_dt)
g_md_ml_box <- g_md + geom_tufteboxplot(aes(x=Make, y=Mileage)) +
  ylim(0, 300000) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Mid-range",
       caption="Source: TrueCar",
       y="Mileage")

# high-end
g_he <- ggplot(data=high_end_dt)
g_he_ml_box <- g_he + geom_tufteboxplot(aes(x=Make, y=Mileage)) +
  ylim(0, 300000) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="High-end",
       y="Mileage")

grid.arrange(g_le_ml_box, g_he_ml_box, g_md_ml_box,
             widths = c(3,3,2),
             layout_matrix = rbind(c(1, 1, 2),c(3, 3, 3)))

# histogram - year 
p <- multhist(list(low_end_dt$Year, mid_range_dt$Year, high_end_dt$Year), probability=T,
              main='Histograms of Year Grouped by Class', xlab='Vehicle\'s Year')
legend('topleft', c('low-end', 'mid-range', 'high-end'),
        col=c('gray30', 'gray55', 'gray80'),lwd=5, cex = 1, bty='n')


# mileage density curve
g_ml <- ggplot(data=new_tc, aes(Mileage))
g_ml_den <- g_ml + geom_density(aes(fill=Category), alpha=0.5) +
  xlim(0, 150000) +
  labs(title="Density Curve of Mileage")




