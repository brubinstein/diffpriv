
<!-- README.md is generated from README.Rmd. Please edit that file -->
diffpriv <img src="man/figures/logo.png" align="right" />
=========================================================

[![packageversion](https://img.shields.io/badge/Package%20version-0.4.2.9000-orange.svg?style=flat-square)](commits/master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/diffpriv)](https://cran.r-project.org/package=diffpriv) [![Travis Build Status](https://travis-ci.org/brubinstein/diffpriv.svg?branch=master)](https://travis-ci.org/brubinstein/diffpriv) [![Coverage Status](https://img.shields.io/codecov/c/github/brubinstein/diffpriv/master.svg)](https://codecov.io/github/brubinstein/diffpriv?branch=master) [![license](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)

Overview
--------

The `diffpriv` package makes privacy-aware data science in R easy. `diffpriv` implements the formal framework of differential privacy: differentially-private mechanisms can safely release to untrusted third parties: statistics computed, models fit, or arbitrary structures derived on privacy-sensitive data. Due to the worst-case nature of the framework, mechanism development typically requires involved theoretical analysis. `diffpriv` offers a turn-key approach to differential privacy by automating this process with sensitivity sampling in place of theoretical sensitivity analysis.

Installation
------------

Obtaining `diffpriv` is easy. From within R:

``` r
## Install the release version of diffpriv from CRAN:
install.packages("diffpriv")

## Install the latest development version of diffpriv from GitHub:
install.packages("devtools")
devtools::install_github("brubinstein/diffpriv")
```

Example
-------

A typical example in differential privacy is privately releasing a simple `target` function of privacy-sensitive input data `X`. Say the mean of `numeric` data:

``` r
## a target function we'd like to run on private data X, releasing the result
target <- function(X) mean(X)
```

First load the `diffpriv` package (installed as above) and construct a chosen differentially-private mechanism for privatizing `target`.

``` r
## target seeks to release a numeric, so we'll use the Laplace mechanism---a
## standard generic mechanism for privatizing numeric responses
library(diffpriv)
mech <- DPMechLaplace(target = target)
```

To run `mech` on a dataset `X` we must first determine the sensitivity of `target` to small changes to input dataset. One avenue is to analytically bound sensitivity (on paper; see the [vignette](http://brubinstein.github.io/diffpriv/articles/diffpriv.pdf)) and supply it via the `sensitivity` argument of mechanism construction: in this case, it is not hard if we assume bounded data, but in general sensitivity can be very non-trivial to calculate manually. The other approach, which we follow in this example, is sensitivity sampling: repeated probing of `target` to estimate sensitivity automatically. We need only specify a distribution for generating random probe datasets; `sensitivitySampler()` takes care of the rest. The price we pay for this convenience is the weaker form of random differential privacy.

``` r
## set a dataset sampling distribution, then estimate target sensitivity with
## sufficient samples for subsequent mechanism responses to achieve random
## differential privacy with confidence 1-gamma
distr <- function(n) rnorm(n)
mech <- sensitivitySampler(mech, oracle = distr, n = 5, gamma = 0.1)
#> Sampling sensitivity with m=285 gamma=0.1 k=285
mech@sensitivity    ## DPMech and subclasses are S4: slots accessed via @
#> [1] 0.8089517
```

With a sensitivity-calibrated mechanism in hand, we can release private responses on a dataset `X`, displayed alongside the non-private response for comparison:

``` r
X <- c(0.328,-1.444,-0.511,0.154,-2.062) # length is sensitivitySampler() n
r <- releaseResponse(mech, privacyParams = DPParamsEps(epsilon = 1), X = X)
cat("Private response r$response:   ", r$response,
  "\nNon-private response target(X):", target(X))
#> Private response r$response:    -1.119506 
#> Non-private response target(X): -0.707
```

Getting Started
---------------

The above example demonstrates the main components of `diffpriv`:

-   Virtual class `DPMech` for generic mechanisms that captures the non-private `target` and releases privatized responses from it. Current subclasses
    -   `DPMechLaplace`, `DPMechGaussian`: the Laplace and Gaussian mechanisms for releasing numeric responses with additive noise;
    -   `DPMechExponential`: the exponential mechanism for privately optimizing over finite sets (which need not be numeric); and
    -   `DPMechBernstein`: the Bernstein mechanism for privately releasing multivariate real-valued functions. See the [bernstein vignette](http://brubinstein.github.io/diffpriv/articles/bernstein.pdf) for more.
-   Class `DPParamsEps` and subclasses for encapsulating privacy parameters.
-   `sensitivitySampler()` method of `DPMech` subclasses estimates target sensitivity necessary to run `releaseResponse()` of `DPMech` generic mechanisms. This provides an easy alternative to exact sensitivity bounds requiring mathematical analysis. The sampler repeatedly probes `DPMech@target` to estimate sensitivity to data perturbation. Running mechanisms with obtained sensitivities yield random differential privacy.

Read the [package vignette](http://brubinstein.github.io/diffpriv/articles/diffpriv.pdf) for more, or [news](http://brubinstein.github.io/diffpriv/news/index.html) for the latest release notes.

Citing the Package
------------------

`diffpriv` is an open-source package offered with a permissive MIT License. Please acknowledge use of `diffpriv` by citing the paper on the sensitivity sampler:

> Benjamin I. P. Rubinstein and Francesco Aldà. "Pain-Free Random Differential Privacy with Sensitivity Sampling", to appear in the 34th International Conference on Machine Learning (ICML'2017), 2017.

Other relevant references to cite depending on usage:

-   **Differential privacy and the Laplace mechanism:** Cynthia Dwork, Frank McSherry, Kobbi Nissim, and Adam Smith. "Calibrating noise to sensitivity in private data analysis." In Theory of Cryptography Conference, pp. 265-284. Springer Berlin Heidelberg, 2006.
-   **The Gaussian mechanism:** Cynthia Dwork and Aaron Roth. "The algorithmic foundations of differential privacy." Foundations and Trends in Theoretical Computer Science 9(3–4), pp. 211-407, 2014.
-   **The exponential mechanism:** Frank McSherry and Kunal Talwar. "Mechanism design via differential privacy." In the 48th Annual IEEE Symposium on Foundations of Computer Science (FOCS'07), pp. 94-103. IEEE, 2007.
-   **The Bernstein mechanism:** Francesco Aldà and Benjamin I. P. Rubinstein. "The Bernstein Mechanism: Function Release under Differential Privacy." In Proceedings of the 31st AAAI Conference on Artificial Intelligence (AAAI'2017), pp. 1705-1711, 2017.
-   **Random differential privacy:** Rob Hall, Alessandro Rinaldo, and Larry Wasserman. "Random Differential Privacy." Journal of Privacy and Confidentiality, 4(2), pp. 43-59, 2012.
