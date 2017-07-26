n <- 250
D <- runif(n)
D <- cbind(D, sin(D*10)*D + rnorm(n, mean=0, sd=0.2))
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
P <- function(n) {  # a sampler of random, "plausible", datasets
  Dx <- runif(n)
  Dy <- rep(0, n)
  if (runif(1) < 0.95) Dy <- Dy + Dx
  if (runif(1) < 0.5) Dy <- Dy * sin(Dx)
  if (runif(1) < 0.5) Dy <- Dy * cos(Dx)
  cbind(Dx, Dy + rnorm(n, mean=0, sd=0.2))
}

plot(D, pch=20, cex=0.70, xlab="X", ylab="Y", ylim=c(-1,1.5), main="Priestly-Chao Kernel Regression")
model <- pck_regression(D)
xs <- seq(from=0, to=1, length=50)
ys <- sapply(xs, model)
lines(xs, ys, col="red", lwd=2, lty="dashed")
library(diffpriv)
m <- DPMechBernstein(target=pck_regression, latticeK=25, dims=1)
m <- sensitivitySampler(m, oracle=P, n=n, gamma=0.20, m=500)
R <- releaseResponse(m, DPParamsEps(epsilon=5), D)
privmodel <- R$response
ys <- R$response(xs)
lines(xs, ys, col="blue", lwd=2)

