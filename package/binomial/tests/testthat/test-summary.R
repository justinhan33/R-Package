context("Tests for auxiliary functions")

#aux_mean
test_that("aux_mean is less than or equal to the first parameter", {

  expect_lte(aux_mean(10, 0.5), 10)
  expect_lte(aux_mean(10, 1), 10)
  expect_lte(aux_mean(10, 0), 10)
  expect_lte(aux_mean(5, 0.5), 5)
  expect_lte(aux_mean(5, 1), 5)
  expect_lte(aux_mean(5, 0), 5)
})

test_that("aux_mean produces a single output", {

  expect_length(aux_mean(10, 0.5), 1)
  expect_length(aux_mean(13, 0.75), 1)
  expect_length(aux_mean(20, 1), 1)
  expect_length(aux_mean(25, 0), 1)
})

test_that("aux_mean produces output of class type equal to that of its parameters", {

  expect_is(aux_mean(10, 0.5), class(10))
  expect_is(aux_mean(10, 0.5), class(0.5))
  expect_is(aux_mean(17, 0.25), class(17))
  expect_is(aux_mean(17, 0.25), class(0.25))
})

#aux_variance
test_that("aux_variance produces a single output", {

  expect_length(aux_variance(10, 0.5), 1)
  expect_length(aux_variance(13, 0.75), 1)
  expect_length(aux_variance(20, 1), 1)
  expect_length(aux_variance(25, 0), 1)
})

test_that("aux_variance produces output of class type equal to that of its parameters", {

  expect_is(aux_variance(10, 0.5), class(10))
  expect_is(aux_variance(10, 0.5), class(0.5))
  expect_is(aux_variance(17, 0.25), class(17))
  expect_is(aux_variance(17, 0.25), class(0.25))
})

test_that("aux_variance produces non-negative output", {

  expect_gte(aux_variance(10, 0.5), 0)
  expect_gte(aux_variance(10, 0.75), 0)
  expect_gte(aux_variance(10, 0), 0)
  expect_gte(aux_variance(10, 1), 0)
})


#aux_mode
test_that("aux_mode produces output of class type equal to that of its parameters", {

  expect_is(aux_mode(10, 0.5), class(10))
  expect_is(aux_mode(10, 0.5), class(0.5))
  expect_is(aux_mode(17, 0.25), class(17))
  expect_is(aux_mode(17, 0.25), class(0.25))
})

test_that("aux_mode produces output with valid length", {

  expect_length(aux_mode(10, 0.5), 1)
  expect_length(aux_mode(9, 0.5), 2)
  expect_length(aux_mode(49, 0.3), 2)
})

test_that("aux_mode works with valid parameters", {

  expect_equal(aux_mode(10, 0.5), 5)
  expect_equal(aux_mode(92, 0.5), 46)
  expect_equal(aux_mode(49, 0.4), c(20, 19))
})

#aux_skewness
test_that("aux_skewness fails for certain parameters", {

  expect_error(aux_skewness(0, 0.5))
  expect_error(aux_skewness(10, 0))
  expect_error(aux_skewness(18, 1))
})

test_that("aux_skewness produces a single output", {

  expect_length(aux_skewness(10, 0.5), 1)
  expect_length(aux_skewness(49, 0.4), 1)
  expect_length(aux_skewness(92, 0.5), 1)
})

test_that("aux_skewness produces output of class type equal to that of its parameters", {

  expect_is(aux_skewness(10, 0.5), class(10))
  expect_is(aux_skewness(10, 0.5), class(0.5))
  expect_is(aux_skewness(17, 0.25), class(17))
  expect_is(aux_skewness(17, 0.25), class(0.25))
})

#aux_kurtosis
test_that("aux_kurtosis fails for certain parameters", {

  expect_error(aux_kurtosis(0, 0.5))
  expect_error(aux_kurtosis(10, 0))
  expect_error(aux_kurtosis(18, 1))
})

test_that("aux_kurtosis produces a single output", {

  expect_length(aux_kurtosis(10, 0.5), 1)
  expect_length(aux_kurtosis(49, 0.4), 1)
  expect_length(aux_kurtosis(92, 0.5), 1)
})

test_that("aux_kurtosis produces output of class type equal to that of its parameters", {

  expect_is(aux_kurtosis(10, 0.5), class(10))
  expect_is(aux_kurtosis(10, 0.5), class(0.5))
  expect_is(aux_kurtosis(17, 0.25), class(17))
  expect_is(aux_kurtosis(17, 0.25), class(0.25))
})
