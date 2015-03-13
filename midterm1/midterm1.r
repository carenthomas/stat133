# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for houses
# in the city of Berkeley.


# calculate the mean and median bsqft of houses in Berkeley. Store these as the
# variables <mean.bsqft> and <med.bsqft> respectively.

mean.bsqft <- mean(housing$bsqft, na.rm=TRUE)
med.bsqft <- median(housing$bsqft, na.rm=TRUE)


# For each house in the dataset, calculate the squared difference between its
# bsqft and the median bsqft of houses in Berkeley. Store this as the variable
# <bsqft.diffs>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset

bsqft.diffs <- housing$bsqft^2 - med.bsqft^2



# Please create two new data frames with the following two subsets
# and store them with the indicated names:
# 1) houses whose bsqft is strictly greater than <mean.bsqft>:  <bsft.greater>
# 2) houses whose bsqft is less than or equal to  <mean.bsqft>: <bsqft.less>

bsqft.greater <- housing[housing$bsqft > mean.bsqft, colnames(housing)]
bsqft.less <- housing[housing$bsqft < mean.bsqft, colnames(housing)]


# For each of your subsets, create a vector giving the price of each house. Name
# these variables <rooms.greater.price> and <rooms.less.price>.

rooms.greater.price <- bsqft.greater$price
rooms.less.price <- bsqft.less$price


# Please implement the function priceByRooms. Your function should take the
# following arguments:
#
# <room.range>: a numeric vector of length 2 whose first and second observations
#   give the minimum and maximum number of rooms to consider
# <br>: a numeric vector giving the number of bedrooms for each observation
# <prices>: a numeric vector giving the price of each observation associated
#   with <br>
#
# Your function should return the average of <prices> for all observations with
# <br> in the range (inclusive) specified by <room.range>

priceByRooms <- function(room.range, br, prices) {
  prices[br > room.range[[1]] & br < room.range[[2]]]
  return(mean(prices))
}


# Please create a plot of house price (y-axis) against br (x-axis). Your plot
# should include the following features:
# 1) a title "Housing price vs Number of Rooms"
# 2) axis labels: "price" and "number of rooms"
# 3) plotting character set to 20

br.vals <- seq(1, 8, by=1)
price.avg <- 0
for (i in br.vals) {
  price.avg[i] <- AvgPriceByRooms(i, housing$br, housing$prices)
}

AvgPriceByRooms <- function(num, br, prices) {
  prices[br == num]
  return(mean(prices))
}

plot(x=br.vals, y=price.avg, xlab="Number of Rooms", ylab="Housing Price", main="Housing Price vs. Number of Rooms")


