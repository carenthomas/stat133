xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  
  y.random <- 0
  start <- 0
  end <- 0
  while (x[[end + 1]] == x[[start]]) {
    end = end + 1
  }
  for (i in c(1:length(y))) {
    if (x[[i]] != x[[start]]) {
      start = end + 1
      while (x[[end + 1]] == x[[start]]) {
        end = end + 1
      }
    }
    y.random[i] <- sample(y[start:end], 1, replace=rep)
  }
  return(y.random)
  
}

genBootR = function(fit, err, rep = FALSE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  y <- 0
  for (i in c(1:length(fit))) {
    y[[i]] <- fit[[i]] + sample(err, 1, replace=rep)
  }
  return(y)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree == 1) {
    coeff <- lm(y ~ x)
  } else {
    coeff <- lm(y ~ x + I(x^2))
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

  ### Use fitModel to fit a model to this bootstrap Y 
  if (fit == NULL) {
    ynew <- genBootY(data$x, data$y)
  } else {
    ynew <- genBootR(fit[,1], fit[,2], degree)
  }
  return(fitModel(data$x, ynew, degree))
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depenxding on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  coeff <- 0
  bootY <- 0
  bootR <- 0
  bootYq <- 0
  bootRq <- 0
  
  for (i in c(1:B)) {
    
    x <- oneBoot(data, degree=1)
    bootY[[i]] <- matrix(coefficients(x), nrow=2, ncol=B)
    f <- matrix(c(x$fitted, data$y - x$fitted), nrow=length(x$fitted), ncol=2)
    bootR[[i]] <- matrix(coefficients(oneBoot(data, fit=f, degree=1)), nrow=2, ncol=B)
    
    x <- oneBoot(data, degree=2)
    bootYq[[i]] <- matrix(coefficients(x), nrow=3, ncol=B)
    f <- matrix(c(x$fitted, data$y - x$fitted), nrow=length(x$fitted), ncol=2)
    bootRq[[i]] <- matrix(coefficients(oneBoot(data, fit=f, degree=2)), nrow=3, ncol=B)
  }
  
  coeff[[1]] = bootY
  coeff[[2]] = bootR
  coeff[[3]] = bootYq
  coeff[[4]] = bootRq
  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  

}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
