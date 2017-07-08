## ----knitr_options, include=FALSE-------------------------
library(knitr)
opts_chunk$set(fig.width=12, fig.height=4, fig.path='', comment="#>",
               warning=FALSE, message=FALSE, tidy=FALSE, size="small")
options(width=60)
set.seed(53079239)
# install package if necessary:
#if(!require("qtl")) install.packages("qtl", repos="http://cran.us.r-project.org")

## ----genericLaplace, include=TRUE, echo=TRUE, results='markup'----
library(diffpriv)
f <- function(X) mean(X) ## target function
n <- 100 ## dataset size
mechanism <- DPMechLaplace(target = f, sensitivity = 1/n, dims = 1)
D <- runif(n, min = 0, max = 1) ## the sensitive database in [0,1]^n
pparams <- DPParamsEps(epsilon = 1) ## desired privacy budget
r <- releaseResponse(mechanism, privacyParams = pparams, X = D)
cat("Private response r$response:", r$response,
  "\nNon-private response f(D):  ", f(D))

## ----samplerExponential, include=TRUE, echo=TRUE, results='markup'----
library(randomNames) ## a package that generates representative random names
oracle <- function(n) randomNames(n)
D <- c("Michael Jordan", "Andrew Ng", "Andrew Zisserman","Christopher Manning",
       "Jitendra Malik", "Geoffrey Hinton", "Scott Shenker",
       "Bernhard Scholkopf", "Jon Kleinberg", "Judea Pearl")
n <- length(D)
f <- function(X) { function(r) sum(r == unlist(base::strsplit(X, ""))) }
rSet <- as.list(letters) ## the response set, letters a--z, must be a list
mechanism <- DPMechExponential(target = f, responseSet = rSet)
mechanism <- sensitivitySampler(mechanism, oracle = oracle, n = n, gamma = 0.1)
pparams <- DPParamsEps(epsilon = 1)
r <- releaseResponse(mechanism, privacyParams = pparams, X = D)
cat("Private response r$response: ", r$response,
  "\nNon-private f(D) maximizer:  ", letters[which.max(sapply(rSet, f(D)))])

