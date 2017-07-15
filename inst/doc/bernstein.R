## ----knitr_options, include=FALSE-------------------------
library(knitr)
opts_chunk$set(fig.width=12, fig.height=4, fig.path='', comment="#>",
               warning=FALSE, message=FALSE, tidy=FALSE, size="small")
options(width=60)
set.seed(3033362) # for reproducibility
# install package if necessary:
#if(!require("qtl")) install.packages("qtl", repos="http://cran.us.r-project.org")

## ----example1-1, include=TRUE, echo=TRUE, results='markup'----
library(diffpriv)
targetF <- function(x) x * sin(10 * x)
bernsteinF <- bernstein(targetF, dims = 1, k = 25)

## ----example1-2, include=TRUE, echo=TRUE, results='markup'----
bernsteinF$coeffs

## ----example1-3, include=TRUE, echo=TRUE, results='markup'----
predict(bernsteinF, D = 0.2)   # approximate f(0.5)
targetF(0.2)                   # actual f(0.5)

## ----example1-4, include=TRUE, echo=TRUE, results='markup', fig.show='hold', fig.width=7, fig.height=3.5, fig.cap = "Bernstein polynomial approximation (blue) vs target (red)."----
xs <- seq(from = 0, to = 1, length = 50)
plot(xs, targetF(xs), xlim = c(0,1), ylim = c(-1,1), lty = "dashed", lwd = 2,
     col = "red", type="l", xlab="x", ylab="y",
     main="Bernstein polynomial approximation")
lines(xs, predict(bernsteinF, xs), col = "blue", lwd = 2)

## ----example2-1, include=TRUE, echo=TRUE, results='markup'----
pck_regression <- function(D, bandwidth = 0.1) {
  K <- function(x) exp(-x^2/2)
  ids <- sort(D[,1], decreasing = FALSE, index.return = TRUE)$ix
  D <- D[ids, ]
  n <- nrow(D)
  ws <- (D[2:n,1] - D[1:(n-1),1]) * D[2:n,2]
  predictor <- function(x) {
    sum(ws * sapply((x - D[2:n,1]) / bandwidth, K)) / bandwidth
  }
  return(predictor)
}

## ----example2-2, include=TRUE, echo=TRUE, results='markup'----
N <- 250
D <- runif(N)
D <- cbind(D, sin(D*10)*D + rnorm(N, mean=0, sd=0.2))

## ----example2-3, include=TRUE, echo=TRUE, results='markup'----
## Non private fitting
model <- pck_regression(D)

## Bernstein non private fitting
K <- 25
bmodel <- bernstein(model, dims=1, k=K)

## Private Bernstein fitting
m <- DPMechBernstein(target=pck_regression, latticeK=K, dims=1)
P <- function(n) {  # a sampler of random, "plausible", datasets
  Dx <- runif(n)
  Dy <- rep(0, n)
  if (runif(1) < 0.95) Dy <- Dy + Dx
  if (runif(1) < 0.5) Dy <- Dy * sin(Dx)
  if (runif(1) < 0.5) Dy <- Dy * cos(Dx)
  cbind(Dx, Dy + rnorm(n, mean=0, sd=0.2))
}
m <- sensitivitySampler(m, oracle=P, n=N, gamma=0.20, m=500)
R <- releaseResponse(m, privacyParams=DPParamsEps(epsilon=5), X=D)
pmodel <- R$response

## ----example2-4, include=TRUE, echo=TRUE, results='markup'----
xs <- seq(from=0, to=1, length=50)
yhats   <- sapply(xs, model)
yhats.b <- predict(bmodel, xs)
yhats.p <- R$response(xs)

## ----example2-5, include=TRUE, echo=TRUE, results='markup', fig.show='hold', fig.width=7, fig.height=3.5, fig.cap = "Kernel regression on 1D training data (points): non-private model (red dashed); non-private Bernstein polynomial approximation (green dotted); private Bernstein mechanism (blue solid)."----
xlim <- c(0, 1)
ylim <- range(c(yhats.b, yhats.p, yhats, D[,2]))
plot(D, pch=20, cex=0.8, xlim=c(0,1), ylim=ylim, xlab="X", ylab="Y",
    main="Priestly-Chao Kernel Regression", col="grey")
lines(xs, yhats.p, col="blue",  type="l", lty="solid", lwd = 2)
lines(xs, yhats.b, col="green", type="l", lty="dotted", lwd = 2)
lines(xs, yhats,   col="red",   type="l", lty="dashed", lwd =2)

