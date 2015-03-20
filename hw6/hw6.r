# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

library(reshape)
library(ggplot2)

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted <- matrix(nrow=n.doctors, ncol=n.days)
  prev <- initial.doctors
  for (day in 1:n.days) {
    meeting <- sample(1:n.doctors, 2, replace=FALSE)
    replace <- sample(0:1, 1, prob=c(1-p,p))
    has_adopted[,day] <- prev
    if (prev[meeting[1]] != prev[meeting[2]] & replace == 1) {
      if (prev[meeting[1]] == 0) {
        has_adopted[meeting[1], day] <- 1
      }
      if (prev[meeting[1]] == 1) {
        has_adopted[meeting[2], day] <- 1
      }
    }
    prev <- has_adopted[, day]
  }
  return(has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

# Set parameters
n.doctors = 40
n.days = 20
prob = seq(.1, .9, by=.16)
initial.doctors <- c(sample(0:1, n.doctors, replace=TRUE, prob=c(.9, .1))) 
                     
# Generate data    
data <- matrix(nrow=6, ncol=n.days)
colnames(1:n.days)
rownames(prob)

for (p in 1:6) {
  out <- sim.doctors(initial.doctors, n.doctors, n.days, prob[p])
  for (day in 1:n.days) {
    data[p, day] <- sum(out[,day])
  }
}


plotdata <- melt(data) 
plotdata[,1] <- rep(prob, 20)
colnames(plotdata) <- c("probability","day", "num_adopted")
outputplot <- ggplot(plotdata, aes(x=day, y=num_adopted, group=probability, colour=probability)) + geom_line()
outputplot <- outputplot + xlab("Day") + ylab("Number of Doctors That Adopted") + ggtitle("Doctors and Drug Adoption")
outputplot
