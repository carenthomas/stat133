#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

source("bml_functions.R")

output <- matrix(nrow=19, ncol=10)
rownames(output) <- seq(0.05, 0.95, by=0.05)
colnames(output) <- seq(200, 2000, by=200)
output

for (r in 1:19) {
  for (c in 1:10) {
    temp <- c(1:10)
    for (i in 1:10) {
      temp[[i]] <- bml.sim(c*200, c*200, r*0.05)[[2]]
    } 
    output[r, c] <- mean(temp)
  }
}

outputdata <- out <- melt(outputdata) 
outputdata <- cbind(out, rep(seq(0.05, 0.95, by=0.05), 10))
colnames(outputdata) <- c("size","value", "density")
outputplot <- ggplot(out, aes(x=density, y=value, colour=size)) + geom_line()