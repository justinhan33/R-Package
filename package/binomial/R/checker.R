#private function to check if input is a valid probability value
check_prob <- function(prob) {
  if(prob >= 0 & prob <= 1) {
    TRUE
  }
  else {
    stop("Invalid input: 'prob' has to be a number greater than or equal to 0 AND less than or equal to 1")
  }
}

#private function to check if input is a valid value for number of trials
check_trials <- function(trials) {
  if((trials %% 1) == 0 ) {
    if(trials >= 0) {
      TRUE
    }
    else {
      stop("Invalid input: 'trials' has to be a non-negative integer")
    }
  }
  else {
    stop("Invalid input: 'trials' has to be an integer")
  }
}

#private function to check if input is a valid value for number of successes
check_success <- function(success, trials) {
  if(sum(success %% 1) == 0) {
    if(sum(success >= 0) == length(success)){
      if(sum(success <= trials) == length(success)) {
        TRUE
      }
      else {
        stop("Invalid input: elements of 'success' has to be less than or equal to 'trials'")
      }
    }
    else {
      stop("Invalid input: 'success' has to contain non-negative integer elements")
    }
  }
  else {
    stop("Invalid input: 'success' has to contain integer elements")
  }
}

