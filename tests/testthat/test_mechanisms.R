library(diffpriv)
context("Generic DP mechanisms")

test_that("DPMech show() runs without error", {
  qualF <- function(X) { function(r) sum(r == unlist(strsplit(X, ""))) }
  rs <- as.list(letters)
  m <- DPMechExponential(sensitivity = 5, target = qualF, responseSet = rs)
  expect_output(show(m), "exponential mechanism", ignore.case = TRUE)
  m@gammaSensitivity <- 0.2
  expect_output(show(m), "exponential mechanism", ignore.case = TRUE)
  m <- DPMechLaplace(target = function(xs) c(1, 2), sensitivity = 1, dims = 2)
  expect_output(show(m), "laplace mechanism", ignore.case = TRUE)
  m@gammaSensitivity <- 0.2
  expect_output(show(m), "laplace mechanism", ignore.case = TRUE)
  m <- DPMechGaussian(target = function(xs) c(1, 2), sensitivity = 1, dims = 2)
  expect_output(show(m), "gaussian mechanism", ignore.case = TRUE)
})

test_that("DPMechLaplace response dimension", {
  m_no <- DPMechLaplace(target = function(xs) 1, sensitivity = 1, dims = 2)
  m_ok <- DPMechLaplace(target = function(xs) c(1, 2), sensitivity = 1, dims=2)
  m_df <- DPMechLaplace(target = function(xs) 1)
  m_ey <- DPMechLaplace(target = function(xs) numeric())
  m_ey@dims <- 0
  p <- DPParamsEps()
  expect_warning(releaseResponse(m_no, privacyParams = p, X = 1:2),
    "Non-private target output has unexpected dimension.", ignore.case = TRUE)
  expect_warning(sensitivityNorm(m_no, 1:2, 1:2),
    "Non-private target output has unexpected dimension.", ignore.case = TRUE)
  expect_silent(releaseResponse(m_ok, privacyParams = p, X = 1:2))
  expect_length(releaseResponse(m_ok, privacyParams = p, X= 1:2)$response, 2)
  expect_warning(sensitivityNorm(m_df, 1:2, 1:2), "No expected dim",
    ignore.case = TRUE)
  expect_equal(sensitivityNorm(m_ey, 1:2, 1:2), 0)
})

test_that("DPMechGaussian response dimension", {
  m_ok <- DPMechGaussian(target = function(xs) c(1, 2), sensitivity = 1, dims=2)
  p <- DPParamsDel(epsilon = 1, delta = 0.1)
  expect_length(releaseResponse(m_ok, privacyParams = p, X= 1:2)$response, 2)
})

test_that("DPMechGaussian's .numericNorm() is accurate", {
  m <- DPMechGaussian(target = function(xs) mean(xs))
  xs <- c(1.5, -2)
  expect_equal(.numericNorm(m, xs, -xs), 5)
})

test_that("DPMechLaplace checks are comprehensive", {
  p <- DPParamsEps(epsilon = 1)
  m <- DPMechLaplace(target = function(xs) "a", sensitivity = 1, dims = 1)
  expect_error(releaseResponse(m, privacyParams = p, X = 1:3), "numeric",
    ignore.case = TRUE)
  expect_error(sensitivityNorm(m, X1 = 1:2, X2 = 1:2), "numeric",
    ignore.case = TRUE)
  m <- DPMechLaplace(target = function(xs) 1, sensitivity = 1, dims = 2)
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

test_that("DPMechExponential expects target returns function", {
  qualF <- function(X) mean(X)
  rs <- as.list(letters)
  m <- DPMechExponential(target = qualF, responseSet = rs)
  X <- 1:5
  expect_error(sensitivityNorm(m, X1 = X, X2 = X),
    "not a function", ignore.case = TRUE)
  expect_error(releaseResponse(m, privacyParams = DPParamsEps(), X = X),
    "not a function", ignore.case = TRUE)
})
