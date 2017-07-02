library(diffpriv)
context("Generic DP mechanisms")

test_that("DPMech show() runs without error", {
  qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
  rs <- as.list(letters)
  m <- DPMechExponential(sensitivity = 5, target = qualF, responseSet = rs)
  expect_output(show(m), "exponential mechanism", ignore.case = TRUE)
  m <- DPMechLaplace(target = function(xs) c(1, 2), sensitivity = 1, dim = 2)
  expect_output(show(m), "laplace mechanism", ignore.case = TRUE)
})

test_that("DPMechLaplace response dimension", {
  m_no <- DPMechLaplace(target = function(xs) 1, sensitivity = 1, dim = 2)
  m_ok <- DPMechLaplace(target = function(xs) c(1, 2), sensitivity = 1, dim = 2)
  p <- DPParamsEps()
  expect_warning(releaseResponse(m_no, privacyParams = p, X = 1:2),
    "Non-private target output has unexpected dimension.")
  expect_silent(releaseResponse(m_ok, privacyParams = p, X = 1:2))
  expect_length(releaseResponse(m_ok, privacyParams = p, X= 1:2)$response, 2)
})

test_that("DPMechLaplace checks are comprehensive", {
  p <- DPParamsEps(epsilon = 1)
  m <- DPMechLaplace(target = function(xs) "a", sensitivity = 1, dim = 1)
  expect_error(releaseResponse(m, privacyParams = p, X = 1:3), "numeric",
    ignore.case = TRUE)
  expect_error(sensitivityNorm(m, X1 = 1:2, X2 = 1:2), "numeric",
    ignore.case = TRUE)
  m <- DPMechLaplace(target = function(xs) 1, sensitivity = 1, dim = 2)
  expect_warning(sensitivityNorm(m, X1 = 1:2, X2 = 1:2), "dimension",
    ignore.case = TRUE)
})

test_that("DPMechExponential responses in responseSet", {
  qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
  rs <- as.list(letters)
  m <- DPMechExponential(sensitivity = 5, target = qualF, responseSet = rs)
  X <- strsplit("the quick brown fox jumps over the lazy dog"," ")[[1]]
  p <- DPParamsEps(epsilon = 1)
  expect_true(is.element(releaseResponse(m, p, X)$response, rs))
})

test_that("DPMechExponential upgrades to gamma", {
  qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
  rs <- as.list(letters)
  m <- DPMechExponential(sensitivity = 5, target = qualF, responseSet = rs)
  m@gammaSensitivity <- 0.1
  X <- strsplit("the quick brown fox jumps over the lazy dog"," ")[[1]]
  p <- DPParamsEps(epsilon = 1)
  r <- releaseResponse(m, p, X)
  expect_s4_class(r$privacyParams, "DPParamsGam")
  expect_equal(getEpsilon(r$privacyParams), 1)
  expect_equal(getDelta(r$privacyParams), 0)
  expect_equal(getGamma(r$privacyParams), 0.1)
})

test_that("DPMechExponential's Linfty sensitivity norm is accurate", {
  qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
  rs <- as.list(letters)
  m <- DPMechExponential(target = qualF, responseSet = rs)
  D1 <- c("abcde", "aaaaa")
  D2 <- c("abcde", "fgh")
  expect_equal(sensitivityNorm(m, D1, D2), 5)
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
})
