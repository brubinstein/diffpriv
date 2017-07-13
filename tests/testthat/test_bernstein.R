library(diffpriv)
context("Bernstein polynomials and mechanism")

test_that(".bernstein_lattice() creates the appropriate lattice", {
  latt22 <- .bernstein_lattice(d=2, k=2)
  latt11 <- .bernstein_lattice(d=1, k=1)
  expect_is(latt11, "matrix")
  expect_equal(nrow(latt22), 9)
  expect_equal(ncol(latt22), 2)
  expect_equal(nrow(latt11), 2)
  expect_equal(ncol(latt11), 1)
  expect_equal(min(latt22), 0)
  expect_equal(max(latt22), 2)
})

test_that(".bernstein_lattice() checks invalid input", {
  expect_error(.bernstein_lattice(d=1.1, k=2), "integer d", ignore.case=TRUE)
  expect_error(.bernstein_lattice(d=0, k=2), "integer d", ignore.case=TRUE)
  expect_error(.bernstein_lattice(d=1, k=2.5), "integer k", ignore.case=TRUE)
  expect_error(.bernstein_lattice(d=1, k=0), "integer k", ignore.case=TRUE)
})

test_that("bernstein() can construct S3 class", {
  tf <- function(xs) xs[1] * sin(xs[2]*10)
  d <- 2
  k <- 5
  bf <- bernstein(tf, dims = d, k = k)
  expect_is(bf, "bernstein")
  expect_true(any(names(bf) == "coeffs"))
  expect_length(bf$coeffs, (k+1)^d)
})

test_that("predict.bernstein() can compute values", {
  tf <- function(xs) xs[1] * sin(xs[2]*10)
  d <- 2
  k <- 5
  bf <- bernstein(tf, dims = d, k = k)
  r <- predict(bf, c(0.2, 0.3))
  expect_lte(r, 1)
  expect_gte(r, -1)
})

test_that("DPMechBernstein validity checks", {
  expect_error(DPMechBernstein(latticeK=1.2, dims=2),
    "latticeK should be positive integer", ignore.case = TRUE)
  expect_error(DPMechBernstein(latticeK=0, dims=2),
    "latticeK should be positive integer", ignore.case = TRUE)
  expect_error(DPMechBernstein(dims=1.2, latticeK=2),
    "dims should be positive integer", ignore.case = TRUE)
  expect_error(DPMechBernstein(dims=0, latticeK=2),
    "dims should be positive integer", ignore.case = TRUE)
})

test_that("DPMechBernstein show() operates", {
  m <- DPMechBernstein()
  expect_output(show(m), "bernstein mechanism", ignore.case = TRUE)
})

test_that("DPMechBernstein releaseResponse() checks", {
  f <- function(X) { X }
  d <- 2
  k <- 5
  m <- DPMechBernstein(target = f, latticeK = k, dims = d, sensitivity = 1)
  p <- DPParamsEps(epsilon = 1)
  expect_error(releaseResponse(m, privacyParams = p, X = rnorm(2)),
    "not a function", ignore.case = TRUE)
})

test_that("DPMechBernstein releaseResponse() operates", {
  f <- function(X) { function(xs) xs[1] * sin(xs[2]*10) }
  d <- 2
  k <- 5
  m <- DPMechBernstein(target = f, latticeK = k, dims = d, sensitivity = 1)
  p <- DPParamsEps(epsilon = 1)
  r <- releaseResponse(m, privacyParams = p, X = rnorm(2))
  expect_true(any(names(r) == "response"))
  expect_is(r$response, "function")
  newX <- c(0.5, 0.5)
  newY <- r$response(newX)
  expect_is(newY, "numeric")
  expect_length(newY, 1)
  newX <- matrix(1:4 / 4, nrow=2)
  newY <- r$response(newX)
  expect_is(newY, "numeric")
  expect_length(newY, 2)
})

test_that("DPMechBernstein sensitivityNorm() values in range", {
  f <- function(D) { function(x) D*(x * sin(x*10)) }
  m <- DPMechBernstein(target = f, latticeK = 10, dims = 1)
  X1 <- 1
  X2 <- 2
  r <- sensitivityNorm(m, X1, X2)
  expect_lte(r, abs(X2 - X1))
  expect_gte(r, 0)
})
