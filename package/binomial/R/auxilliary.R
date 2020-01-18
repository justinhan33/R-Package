#private function for computing mean
aux_mean <- function(trials, prob) {
  mean <- trials*prob
  return(mean)
}

#private function for computing variance
aux_variance <- function(trials, prob) {
  variance <- trials*prob*(1-prob)
  return(variance)
}

#private function for computing mode
aux_mode <- function(trials, prob) {
  if((((trials*prob)+prob)-floor(((trials*prob)+prob))) == 0) {
    mode <- c(((trials*prob)+prob), ((trials*prob)+prob)-1)
  }
  else {
    mode <- floor(((trials*prob)+prob))
  }
  return(mode)
}

#private function for computing skewness
aux_skewness <- function(trials, prob) {
  if((sqrt((trials*prob)*(1-prob))) == 0) {
    stop("Undefined")
  }
  else {
    skewness <- (1-(2*prob))/(sqrt((trials*prob)*(1-prob)))
  }
  return(skewness)
}

#private function for computing kurtosis
aux_kurtosis <- function(trials, prob) {
  if(((trials*prob)*(1-prob)) == 0) {
    stop("Undefined")
  }
  else {
    kurtosis <- (1-((6*prob)*(1-prob)))/((trials*prob)*(1-prob))
  }
  return(kurtosis)
}
