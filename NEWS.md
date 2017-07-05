# diffpriv 0.2.0

* `DPMechLaplace` objects can now be initialized without specifying non-private `target` response `dim`. In such cases, the sensitivity sampler will perform an additional `target` probe to determine `dim`.

# diffpriv 0.1.0.901

* Sensitivity sampler methods no longer require oracles that return lists. Acceptable oracles may now return lists, matrices, data frames, numeric vectors, or char vectors. As a consequence some example code in docs, README and vignette, is simplified.

# diffpriv 0.1.0.900

* Initial release
