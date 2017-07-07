library(diffpriv)
context("DPMech sensitivity sampling")

test_that("sensitivitySampler methods check types", {
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dims = 1)
  P <- function(n) if (n > 1) as.list(rnorm(n)) else rnorm(1)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=1.1, m=100, k=99),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=0, m=100, k=99),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100.1, k=99),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=0, k=0),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100, k=99.9),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100, k=101),
    "k is not in", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100, k=0),
    "k is not in", ignore.case=TRUE)
  expect_error(.sensitivity_sampler_config(), "specify at least one",
    ignore.case = TRUE)
  expect_error(.sensitivity_sampler_config(m = 1.1), "m is not",
    ignore.case = TRUE)
  expect_error(.sensitivity_sampler_config(gamma = 1.1), "gamma is not",
    ignore.case = TRUE)
  expect_error(.sensitivity_sampler_config(gamma = 0.0001, m = 100),
    "must exceed", ignore.case = TRUE)
  qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
  m <- DPMechExponential(target = qualF, responseSet = as.list(letters))
  library(randomNames)
  P <- function(n) randomNames(n)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=1.1, m=100, k=99),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=0, m=100, k=99),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100.1, k=99),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=0, k=0),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100, k=99.9),
    "integer", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100, k=101),
    "k is not in", ignore.case=TRUE)
  expect_error(sensitivitySamplerManual(m, oracle=P, n=10, m=100, k=0),
    "k is not in", ignore.case=TRUE)
})

test_that("sensitivitySampler() returns expected responses", {
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dims = 1)
  expect_identical(m@sensitivity, Inf)
  expect_true(is.na(m@gammaSensitivity))
  P <- function(n) if (n > 1) as.list(rnorm(n)) else rnorm(1)
  gamma <- 0.33
  m <- sensitivitySampler(m, oracle = P, n = 100, gamma = gamma)
  expect_lt(m@sensitivity, Inf)
  expect_equal(m@gammaSensitivity, gamma)
})

test_that("Responses post sensitivitySampler() are RDP", {
  dim <- 1
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dims = dim)
  P <- function(n) if (n > 1) as.list(rnorm(n)) else rnorm(1)
  epsilon <- 0.1
  gamma <- 0.33
  n <- 100

  ## Case: Given gamma only
  m <- sensitivitySampler(m, oracle = P, n = n, gamma = gamma)
  R <- releaseResponse(m, DPParamsEps(epsilon = epsilon), rnorm(n))
  expect_true(is.element("response", names(R)))
  expect_true(is.numeric(R$response))
  expect_length(R$response, dim)
  expect_true(is.element("privacyParams", names(R)))
  expect_is(R$privacyParams, "DPParamsGam")
  expect_equal(getEpsilon(R$privacyParams), epsilon)
  expect_equal(getDelta(R$privacyParams), 0)
  expect_equal(getGamma(R$privacyParams), gamma)

  ## Case: Given m only
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dims = dim)
  m <- sensitivitySampler(m, oracle = P, n = n, m = 50)
  R <- releaseResponse(m, DPParamsEps(epsilon = epsilon), rnorm(n))
  expect_is(R$privacyParams, "DPParamsGam")

  ## Case: Given both m, gamma
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dims = dim)
  m <- sensitivitySampler(m, oracle = P, n = n, gamma = gamma, m = 50)
  R <- releaseResponse(m, DPParamsEps(epsilon = epsilon), rnorm(n))
  expect_is(R$privacyParams, "DPParamsGam")
})

test_that("sensitivitySampler() determines DPMechLaplace@dims", {
  m <- DPMechLaplace(target = function(xs) apply(xs, 2, mean))
  P <- function(n) matrix(rnorm(3*n), ncol=3)
  m <- sensitivitySampler(m, oracle = P, n = 10, gamma = 0.33)
  expect_equal(m@dims, 3)
})
