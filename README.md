<!-- README.md is generated from README.Rmd. Please edit that file -->
diffpriv <img src="man/figures/logo.png" align="right" />
=========================================================

The `diffpriv` package makes privacy-aware data science under differential privacy easy in R. Differentially-private mechanisms can release as responses to untrusted third parties, models fit on privacy-sensitive data. But due to the worst-case nature of the framework, mechanism development typically requires involved theoretical analysis. `diffpriv` offers a turn-key approach to differential privacy by automating this process with sensitivity sampling.

Example
-------

A typical example in differential privacy is privately releasing a simple `target` function of privacy-sensitive input data `X`. Say the mean:

``` r
## privacy-sensitive dataset and target function we'd like to run and release
X <- list(0.328, -1.444, -0.511, 0.154, -2.062)
target <- function(X) mean(unlist(X))
```

Next load the `diffpriv` package (see how to get it below) and construct a chosen differentially-private mechanism for privatizing `target`.

``` r
## target seeks to release a scalar numeric, so we'll use the Laplace mechanism
library(diffpriv)
mech <- DPMechLaplace(target = target, dim = 1)
```

To run `mech` on `X` we must first deteremine the sensitivity of `target` to small changes to input dataset. We could either assume bounded data, or in this case we use sensitivity sampling: repeated probing of `target` to estimate sensitivity. We must specify a distribution `distr` for the probe datasets.

``` r
## set a dataset sampling distribution, then estimate target sensitivity with
## with sufficient samples for subsequent mechanism responses to achieve random
## differential privacy with confidence 1-gamma
distr <- function(n) if (n==1) rnorm(1) else if (n>1) as.list(rnorm(n))
mech <- sensitivitySampler(mech, oracle = distr, n = length(X), gamma = 0.1)
#> Sampling sensitivity with m=285 gamma=0.1 k=285
mech@sensitivity
#> [1] 0.9398667
```

And finally, private responses on `X`

``` r
releaseResponse(mech, privacyParams = DPParamsEps(epsilon=1), X = X)$response
#> [1] -2.430058
```

Installation
------------

To get the current development version from github:

``` r
install.packages("devtools")
devtools::install_github("brubinstein/diffpriv")
```

Getting Started
---------------

The example above demonstrates the main components of `diffpriv`:

-   Virtual class `DPMech` for generic mechanisms that captures the non-private `target` and releases privatized responses from it. Current subclasses
    -   `DPMechLaplace`: the Laplace mechanism for releasing numeric responses; and
    -   `DPMechExponential`: the exponential mechanism for privately optimizing over finite sets (not necessarily numeric).
-   Class `DPParamsEps` and subclasses for encapsulating privacy parameters.
-   `sensitivitySampler` method in `DPMech` subclasses can be used when exact bounds on the sensitivity of non-private `target` aren't available. It repeatedly probes `target` to estimate sensitivity. Running mechanisms with obtained sensitivites yield slightly weaked random differential privacy.

Read the package vignette for more.

Citing the Package
------------------

The `diffpriv` package is open-source with permissive MIT License. Please acknowledge use of `diffpriv` by citing the paper:

> Benjamin I. P. Rubinstein and Francesco Alda. "Pain-Free Random Differential Privacy with Sensitivity Sampling", accepted into the 34th International Conference on Machine Learning (ICML'2017), May 2017.

Other relevant references to cite depending on usage:

-   Differential privacy and the Laplace mechanism: Cynthia Dwork, Frank McSherry, Kobbi Nissim, and Adam Smith. "Calibrating noise to sensitivity in private data analysis." In Theory of Cryptography Conference, pp. 265-284. Springer Berlin Heidelberg, 2006.
-   Exponential mechanism: Frank McSherry and Kunal Talwar. "Mechanism design via differential privacy." In the 48th Annual IEEE Symposium on Foundations of Computer Science (FOCS'07), pp. 94-103. IEEE, 2007.
-   Random differential privacy: Rob Hall, Alessandro Rinaldo, and Larry Wasserman. "Random Differential Privacy." Journal of Privacy and Confidentiality, 4(2), pp. 43-59, 2012.
