context("Tests for checker functions")

#check_prob
test_that("check_prob works with parameter within range", {

  expect_true(check_prob(0))
  expect_true(check_prob(0.5))
  expect_true(check_prob(1))
})

test_that("check_prob produces a warning for parameter of invalid length", {

  expect_warning(check_prob(c(0.1,0.2)))
  expect_warning(check_prob(c(0, 0.5, 0.7, 0.88, 1)))
})

test_that("check_prob fails for parameter outside of range", {

  expect_error(check_prob(-0.5))
  expect_error(check_prob(1.5))
})

#check_trials
test_that("check_trials works with non-negative parameter", {

  expect_true(check_trials(5))
  expect_true(check_trials(10))
  expect_true(check_trials(15))
})

test_that("check_trials fails with negative parameter", {

  expect_error(check_trials(-5))
  expect_error(check_trials(-10))
  expect_error(check_trials(-15))
})

test_that("check_trials produces a warning for parameter of invalid length", {

  expect_warning(check_trials(c(10,11,12,13)))
  expect_warning(check_trials(c(23, 45, 67, 101, 2230)))
})

#check_success
test_that("check_success works valid paramters", {

  expect_true(check_success(1, 5))
  expect_true(check_success(c(1,2,3), 5))
})

test_that("check_success fails when first parameter is outside of range", {

  expect_error(check_success(-1, 5))
  expect_error(check_success(6, 5))
  expect_error(check_success(c(5,6,7,8), 7))
})

test_that("check_success produces a single output", {

  expect_length(check_success(1, 5), 1)
  expect_length(check_success(1, 5), 1)
  expect_length(check_success(c(1,2,3,4), 5), 1)
})
