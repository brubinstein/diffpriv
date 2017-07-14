# diffpriv 0.4.2

* Second vignette `bernstein` on: Bernstein approximations and use of `DPMechBernstein` for private function release.
* Minor edits to docs

# diffpriv 0.4.1

* Expanding test coverage of Bernstein mechanism and function approximation code.

# diffpriv 0.4.0

* Addition of `S3` constructor and `predict()` generic implementation for fitting (non-iterated) Bernstein polynomial function approximations.
* Addition of `DPMechBernstein` class implementing the Bernstein mechanism of Alda and Rubinstein (AAAI'2017), for privately releasing functions.
* Bug fix in the Laplace random sampler affecting `DPMechLaplace`
* Unit test coverage of new functionality; general documentation improvements.

# diffpriv 0.3.2

* Addition of `DPMechGaussian` class for the generic Gaussian mechanism to 
README, Vignette. Resolves #2
* Minor test additions.

# diffpriv 0.3.1

* Refactoring around `releaseResponse()` method in `DPMechNumeric`. Resolves #1
* Increased test coverage.

# diffpriv 0.3.0

* New `DPMechGaussian` class implementing the Gaussian mechanism, which 
achieves (epsilon,delta)-differential privacy by adding Gaussian noise to 
numeric responses calibrated by L2-norm sensitivity.
* Refactoring of `DPMechGaussian` and `DPMechLaplace` underneath a new 
`VIRTUAL` class `DPMechNumeric` which contains common methods, `dims` slot 
(formerly `dim` changed because `dim` is a special slot for S4).

# diffpriv 0.2.0

* `DPMechLaplace` objects can now be initialized without specifying 
non-private `target` response `dim`. In such cases, the sensitivity sampler 
will perform an additional `target` probe to determine `dim`.

# diffpriv 0.1.0.901

* Sensitivity sampler methods no longer require oracles that return lists.
Acceptable oracles may now return lists, matrices, data frames, numeric 
vectors, or char vectors. As a consequence some example code in docs, README and vignette, is simplified.

# diffpriv 0.1.0.900

* Initial release
