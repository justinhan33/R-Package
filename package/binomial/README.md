# Overview

This package includes basic functions that allow the user to generate visual illustrations of a binomial random variable's probability distribution and its cumulative probabilty distribution.

  - `bin_distribution()`  generates a table with the probability distribution
  - `bin_cumulative()`  generates a table with the cumulative probability distribution
  
The user can invoke the `plot()` method for a `bindis` or `bincum` object to see a visual display of the corresponding distributions.
 
Furthmore, this package contains functions for finding and displaying the basic properties (i.e. mean, variance, mode, skewness, and kurtosis) of a binomial random variable. 

  - `bin_mean()`  computes the mean 	
  - `bin_variance()`  computes the variance
  - `bin_mode()`  computes the mode
  - `bin_skewness()`  computes the skewness
  - `bin_kurtosis()`  computes the kurtosis
  
Some other useful functions allow the user to compute probabilities and the number of combinations that an event can occur. Lastly, there is a function that nicely summarizes all the properties of the binomial.

  - `bin_probability()` computes binomial probability of an event
  - `bin_choose()`	computes number of combinations
  - `bin_variable()`  creates a summary of the binomial properties
 
