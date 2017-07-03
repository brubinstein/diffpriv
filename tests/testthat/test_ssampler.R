library(diffpriv)
context("DPMech sensitivity sampling")

test_that("sensitivitySampler methods check types", {
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dim = 1)
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
})

test_that("sensitivitySampler() returns expected responses", {
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dim = 1)
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
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dim = dim)
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
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dim = dim)
  m <- sensitivitySampler(m, oracle = P, n = n, m = 50)
  R <- releaseResponse(m, DPParamsEps(epsilon = epsilon), rnorm(n))
  expect_is(R$privacyParams, "DPParamsGam")

  ## Case: Given both m, gamma
  m <- DPMechLaplace(target = function(xs) mean(unlist(xs)), dim = dim)
  m <- sensitivitySampler(m, oracle = P, n = n, gamma = gamma, m = 50)
  R <- releaseResponse(m, DPParamsEps(epsilon = epsilon), rnorm(n))
  expect_is(R$privacyParams, "DPParamsGam")
})
