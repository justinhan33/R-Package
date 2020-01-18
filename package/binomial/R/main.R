#' @title Choose function
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials (numeric)
#' @param k number of successes (numeric)
#' @return Number of combinations
#' @export
#' @examples
#'
#' #calculates number of combinations 2 successes can occur in 5 trials
#'  bin_choose(n = 5, k = 2)
#'
#' #calculates number of combinations 0 successes can occur in 5 trials
#'  bin_choose(5, 0)
#'
#' #calculates number of combinations 1,2, or 3 successes can occur in 5 trials
#'  bin_choose(5, 1:3)
#'
bin_choose <- function(n, k) {
  if(sum(k > n) != 0) {
    stop("k cannot be greater than n")
  }
  else {
    choose <- factorial(n)/(factorial(k)*factorial(n-k))
  }
  return(choose)
}

#' @title Probability function
#' @description Calculates the probability of getting k successes in n trials
#' @param success number of successes (numeric)
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Probability of getting k successes in n trials
#' @export
#' @examples
#'
#' #probability of getting 2 successes in 5 trials
#' #(assuming prob of success = 0.5)
#'  bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' #probabilities of getting 2 or less successes in 5 trials
#' #(assuming prob of success = 0.5)
#'  bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'
#' #55 heads in 100 tosses of a loaded coin with 45% chance of heads
#'  bin_probability(success = 55, trials = 100, prob = 0.45)
#'
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  probability <- bin_choose(trials, success)*(prob^(success))*((1-prob)^(trials-success))
  return(probability)
}

#' @title Distribution function
#' @description Generates a data frame with the probability distribution
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Data frame with sucesses in the first column and probability in the second column
#' @export
#' @examples
#'
#' #binomial probability distribution
#'  bin_distribution(trials = 5, prob = 0.5)
#'
bin_distribution <- function(trials, prob) {
  success <- c(0:trials)
  probability <- bin_probability(success, trials, prob)
  distribution_df <- data.frame(success, probability)
  class(distribution_df) <- c("bindis", "data.frame")
  return(distribution_df)
}

#' @export
plot.bindis <- function(x) {
  barplot(x$probability, las = 1, names.arg = x$success, xlab = "successes", ylab = "probability")
}

#' @title Distribution and cumulative probabilties function
#' @description Generates a data frame with the probability distribution and the cumulative probabilities
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Data frame with sucesses in the first column, probability in the second column, and cumulative in the third column
#' @export
#' @examples
#'
#' #binomial cumulative distribution
#'  bin_cumulative(trials = 5, prob = 0.5)
#'
bin_cumulative <- function(trials, prob) {
  success <- c(0:trials)
  probability <- bin_probability(success, trials, prob)
  cumulative <- cumsum(probability)
  cumulative_df <- data.frame(success, probability, cumulative)
  class(cumulative_df) <- c("bincum", "data.frame")
  return(cumulative_df)
}

#' @export
plot.bincum <- function(x) {
  plot(x$success, x$cumulative, type = "o", las = 1, xlab="successes", ylab="probability")
}

#' @title Mean function
#' @description Calculates the mean
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Mean
#' @export
#' @examples
#'
#' #computes mean with corresponding arguments
#'  bin_mean(10, 0.3)
#'
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)
}

#' @title Variance function
#' @description Calculates the variance
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Variance
#' @export
#' @examples
#'
#' #computes variance with corresponding arguments
#'  bin_variance(10, 0.3)
#'
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)
}

#' @title Mode function
#' @description Calculates the mode
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Mode
#' @export
#' @examples
#'
#' #computes mode with corresponding arguments
#'  bin_mode(10, 0.3)
#'
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)
}

#' @title Skewness function
#' @description Calculates the skewness
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Skewness
#' @export
#' @examples
#'
#' #computes skewness with corresponding arguments
#'  bin_skewness(10, 0.3)
#'
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)
}

#' @title Kurtosis function
#' @description Calculates the kurtosis
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Kurtosis
#' @export
#' @examples
#'
#' #computes kurtosis with corresponding arguments
#'  bin_kurtosis(10, 0.3)
#'
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)
}

#' @title Variable function
#' @description Generates information of a variable's parameters
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return Printed content of the parameters of a "binvar" object: trials and prob
#' @export
#' @examples
#'
#' #List with named elements, trials and prob respectively
#'  bin_variable(trials = 10, p = 0.3)
#'
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  variable <- list(trials = trials, prob = prob)
  class(variable) <- c("binvar", "list")
  return(variable)
}

#' @export
print.binvar <- function(x, ...) {
  cat('"Binomial variable" \n\nParameters \n- number of trials:', x$trials,
      '\n- prob of success:', x$prob)
  invisible(x)
}

#' @export
summary.binvar <- function(x, ...) {
  cat('"Summary Binomial" \n\nParameters \n- number of trials:', x$trials,
      '\n- prob of success:', x$prob,
      '\n\nMeasures \n- mean    :', bin_mean(x$trials, x$prob),
      '\n- variance:', bin_variance(x$trials, x$prob),
      '\n- mode    :', bin_mode(x$trials, x$prob),
      '\n- skewness:', bin_skewness(x$trials, x$prob),
      '\n- kurtosis:', bin_kurtosis(x$trials, x$prob))
  invisible(x)
}




