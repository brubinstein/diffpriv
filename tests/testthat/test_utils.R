library(diffpriv)
context("General utilities")

test_that(".check_integer requires singleton numeric", {
  expect_false(.check_integer(1:3))
  expect_false(.check_integer(as.integer(c())))
  expect_false(.check_integer("foo bar"))
})

test_that(".check_integer is true on and only on integral", {
  expect_false(.check_integer(1.1))
  expect_false(.check_integer(-2.3))
  expect_true(.check_integer(1))
  expect_true(.check_integer(-2.0))
})

test_that(".rlap errors gracefully on ill-formed input", {
  expect_error(.rlap(1.2), "Given sample size is not scalar non-negative integer.")
  expect_error(.rlap(-1), "Given sample size is not scalar non-negative integer.")
  expect_error(.rlap(1, location="a"), "Given location is a not a scalar numeric.")
  expect_error(.rlap(2, location=1:3), "Given location is a not a scalar numeric.")
  expect_error(.rlap(1, scale="a"), "Given scale is not scalar non-negative.")
  expect_error(.rlap(2, scale=1:2), "Given scale is not scalar non-negative.")
  expect_error(.rlap(2, scale=-0.1), "Given scale is not scalar non-negative.")
})

test_that(".rlap produces expected numbers of observations", {
  expect_equal(length(.rlap(2.0)), 2)
  expect_equal(length(.rlap(0)), 0)
  expect_equal(length(.rlap(1)), 1)
  expect_equal(length(.rlap(1, location=-1)), 1)
  expect_equal(length(.rlap(1, location=+1)), 1)
  expect_equal(length(.rlap(2, location=1, scale=2)), 2)
})

test_that(".l1norm produces expected values", {
  expect_equal(.l1norm(numeric()), 0)
  expect_equal(.l1norm(c(-1, 0, 0.5)), 1.5)
})

test_that(".linfty_norm produces expected values", {
  expect_equal(.linfty_norm(numeric()), 0)
  expect_equal(.linfty_norm(c(-1, 0, 0.5)), 1)
})

test_that(".l2norm produces expected values", {
  expect_error(.l2norm("a"), "not numeric", ignore.case = TRUE)
  expect_equal(.l2norm(numeric()), 0)
  expect_equal(.l2norm(3:4), 5)
})

test_that(".constant_target is constant-valued", {
  expect_equal(.constant_target(1), 0)
  expect_equal(.constant_target(1:3), 0)
  expect_equal(.constant_target("a"), 0)
})

test_that(".generic_append() can append different types", {
  # lists
  expect_length(.generic_append(as.list(1:9), 10), 10)
  expect_length(.generic_append(list(), 1), 1)
  # matrices
  expect_equal(nrow(.generic_append(matrix(1:9, nrow=3), 1:3)), 4)
  expect_equal(nrow(.generic_append(matrix(nrow=0, ncol=3),
                                    matrix(1:3, nrow=1))), 1)
  # data frames
  D <- cbind(as.data.frame(matrix(1:9, nrow=3)), letters[1:3])
  Dnew <- cbind(as.data.frame(matrix(1:3, nrow=1)), "d")
  names(Dnew) <- names(D)
  expect_equal(nrow(.generic_append(D, Dnew)), 4)
  # numeric vectors
  expect_length(.generic_append(1:9, 10), 10)
  expect_length(.generic_append(numeric(), c(1)), 1)
  # character vectors
  expect_length(.generic_append(letters[1:9], letters[10]), 10)
  expect_length(.generic_append(character(0), "the brown fox"), 1)
  # rejects unexepcted
  expect_error(.generic_append(DPParamsEps(), 2), "unrecognized",
    ignore.case = TRUE)
})
