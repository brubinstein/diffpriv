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
