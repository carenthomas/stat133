#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  m <- matrix(sample(0:2, r*c, replace=TRUE, prob=c(1 - p, p/2, p/2)), nrow=r, ncol=c)  
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){  
  
  grid.new <- FALSE
  
  m.check <- cbind(m[,2:ncol(m)], m[,1])
  m.final <- m
  
  for (red_r in 1:nrow(m)) {
    for (red_c in 1:ncol(m)) {
      if (m[red_r, red_c] == 1) {
        if (m.check[red_r, red_c] == 0) {
          m.final[red_r, red_c] <- 0
          if (red_c == ncol(m)) {
            m.final[red_r, 1] <- 1
          } else {
            m.final[red_r, red_c + 1] <- 1
          }
          grid.new <- TRUE
        }
      }
    }
  }
  
  m.check <-rbind(m.final[nrow(m),], m.final[1:(nrow(m)-1),])
  
  for (blue_r in 1:nrow(m)) {
    for (blue_c in 1:ncol(m)) {
      if (m[blue_r, blue_c] == 2) {
        if (m.check[blue_r, blue_c] == 0) {
          m.final[blue_r, blue_c] <- 0
          if (blue_r == 1) {
            m.final[nrow(m), blue_c] <- 2
          } else {
            m.final[blue_r - 1, blue_c] <- 2
          }
          grid.new <- TRUE
        }
      }
    }
  }
  
  return(list(m.final, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r, c, p)
  count <- 0
  for (i in 1:5000) {
    b <- bml.step(m)
    m <- b[[1]]
    if (b[[2]]) {
      count <- count + 1
    } else {
      return(list(m, count))
    }
  }
  return(list(m, count))
}
