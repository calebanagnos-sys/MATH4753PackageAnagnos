library(testthat)
library(MATH4753PackageAnagnos)

test_that("myncurve returns correct mu", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(result$mu, 0)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(mu = 2, sigma = 3, a = 1)
  expect_equal(result$sigma, 3)
})

test_that("myncurve computes correct probability", {
  result <- myncurve(mu = 0, sigma = 1, a = 1.96)
  expected_prob <- pnorm(1.96, mean = 0, sd = 1)
  expect_equal(result$prob, expected_prob, tolerance = 1e-6)
})
