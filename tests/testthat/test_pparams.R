library(diffpriv)
context("Privacy parameters")

test_that("Invalid privacy parameter objects", {
  expect_error(DPParamsEps(epsilon = 0))
  expect_error(DPParamsEps(epsilon = 1:2))
  expect_error(DPParamsDel(epsilon = 1, delta = 1))
  expect_error(DPParamsDel(epsilon = 0, delta = 0.5))
  expect_error(DPParamsDel(epsilon = 1, delta = 1:2))
  expect_error(DPParamsGam(epsilon = 1, delta = 0, gamma = 1))
  expect_error(DPParamsGam(epsilon = 0, delta = 0, gamma = 0.05))
  expect_error(DPParamsGam(epsilon = 1, delta = 1, gamma = 0.05))
  expect_error(DPParamsGam(epsilon = 1, delta = 0, gamma = c(0.05, 0.1)))
})

test_that("Valid privacy parameter objects", {
  expect_equal(DPParamsEps(epsilon = 1)@epsilon, 1)
})

test_that("Getters get from privacy parameter objects", {
  expect_equal(getEpsilon(DPParamsEps(epsilon=1)), 1)
  expect_equal(getEpsilon(DPParamsDel(epsilon=2, delta=0.5)), 2)
  expect_equal(getEpsilon(DPParamsGam(epsilon=3, delta=0.5, gamma=0.1)), 3)
  expect_equal(getDelta(DPParamsDel(epsilon=2, delta=0.5)), 0.5)
  expect_equal(getGamma(DPParamsGam(epsilon=3, delta=0.5, gamma=0.1)), 0.1)
})

test_that("Setters set privacy parameter objects", {
  x <- DPParamsEps(epsilon = 1)
  setEpsilon(x) <- 0.5
  expect_equal(getEpsilon(x), 0.5)
  x <- DPParamsDel(epsilon = 1, delta = 0.5)
  setEpsilon(x) <- 0.5
  expect_equal(getEpsilon(x), 0.5)
  rm(x)
})

test_that("Privacy parameter show() prints", {
  p <- DPParamsEps()
  expect_output(show(p), "differential privacy level", ignore.case = TRUE)
  p <- DPParamsDel()
  expect_output(show(p), "differential privacy level", ignore.case = TRUE)
  p <- DPParamsGam()
  expect_output(show(p), "differential privacy level", ignore.case = TRUE)
})

test_that("toGamma() converts to DPParamsGam", {
  eps <- 1
  del <- 0.1
  gam <- 0.2
  newgam <- 0.1
  ep <- DPParamsEps(epsilon = eps)
  dp <- DPParamsDel(epsilon = eps, delta = del)
  gp <- DPParamsGam(epsilon = eps, delta = del, gamma = gam)
  expect_equal(getEpsilon(toGamma(ep,newgam)), eps)
  expect_equal(getDelta(toGamma(ep,newgam)), 0)
  expect_equal(getGamma(toGamma(ep,newgam)), newgam)
  expect_equal(getEpsilon(toGamma(dp,newgam)), eps)
  expect_equal(getDelta(toGamma(dp,newgam)), del)
  expect_equal(getGamma(toGamma(dp,newgam)), newgam)
  expect_warning(gp <- toGamma(gp,newgam), "on DPParamsGam same as", ignore.case=TRUE)
  expect_equal(getGamma(gp), newgam)
})
