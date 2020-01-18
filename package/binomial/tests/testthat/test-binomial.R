context("Tests for main functions")

#bin_choose
test_that("bin_choose produces output with valid length", {

  expect_length(bin_choose(5, 1:3), 3)
  expect_length(bin_choose(5, 2), 1)
})

test_that("bin_choose fails for invalid second parameter", {

  expect_error(bin_choose(5, 6))
  expect_error(bin_choose(5, c(4,5,6)))
})

test_that("bin_choose works with valid parameters", {

  expect_equal(bin_choose(5, 2), 10)
  expect_equal(bin_choose(10, 5), 252)
  expect_equal(bin_choose(12, c(10, 11)), c(66, 12))
})

#bin_probability
test_that("bin_probability fails for invalid parameters", {

  expect_error(bin_probability(2.5, 10, 0.5))
  expect_error(bin_probability(2, 10.5, 0.5))
  expect_error(bin_probability(2, 10, 2))
  expect_error(bin_probability(2, 10, -0.5))
  expect_error(bin_probability(-2, 10, 0.5))
  expect_error(bin_probability(2, -10, 0.5))
  expect_error(bin_probability(15, 10, 0.5))
})

test_that("bin_probability produces output with valid length", {

  expect_length(bin_probability(0:2, 5, 0.5), 3)
  expect_length(bin_probability(55, 100, 0.45), 1)
})

test_that("bin_probability produces output of class type equal to that of its parameters", {

  expect_is(bin_probability(55, 100, 0.45), class(55))
  expect_is(bin_probability(55, 100, 0.45), class(100))
  expect_is(bin_probability(55, 100, 0.45), class(0.45))
})

#bin_distribution
test_that("bin_distribution produces output of two classes", {

  expect_is(bin_distribution(5, 0.5), c("bindis", "data.frame"))
  expect_is(bin_distribution(8, 0.5), c("bindis", "data.frame"))
})

test_that("bin_distribution produces output with valid number of columns", {

  expect_length(bin_distribution(5, 0.5), 2)
  expect_length(bin_distribution(8, 0.5), 2)
})

test_that("bin_distribution fails with invalid parameters", {

  expect_error(bin_distribution(5, -0.5))
  expect_error(bin_distribution(8, 1.5))
  expect_error(bin_distribution(-10, 0.5))
  expect_error(bin_distribution(6.5, 0.5))
})

#bin_cumulative
test_that("bin_cumulative produces output of two classes", {

  expect_is(bin_cumulative(5, 0.5), c("bincum", "data.frame"))
  expect_is(bin_cumulative(8, 0.5), c("bincum", "data.frame"))
})

test_that("bin_cumulative produces output with valid number of columns", {

  expect_length(bin_cumulative(5, 0.5), 3)
  expect_length(bin_cumulative(8, 0.5), 3)
})

test_that("bin_cumulative fails with invalid parameters", {

  expect_error(bin_cumulative(5, -0.5))
  expect_error(bin_cumulative(8, 1.5))
  expect_error(bin_cumulative(-10, 0.5))
  expect_error(bin_cumulative(6.5, 0.5))
})


