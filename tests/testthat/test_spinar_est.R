# spinar_est <- function(x, p)
test_that("input", {
  ######################## x ########################
  expect_error(spinar_est(x = c(1), p = 1), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  expect_error(spinar_est(x = c(1, 0), p = 2), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_est(x = c(2, 3), p = 2), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_est(x = c(-1, 2, 0, 4, 5), p = 1))
  expect_error(spinar_est(x = c(-1, 2, 0, 4, 5), p = 2))
  expect_error(spinar_est(x = c(1, 2.4, 0, 4, 5), p = 1), "Assertion on 'x' failed: Must be of type 'integerish'")
  expect_error(spinar_est(x = c(1, 2, 0, 4.1, 5), p = 2), "Assertion on 'x' failed: Must be of type 'integerish'")
  ######################## p ########################
  expect_error(spinar_est(x = c(2, 3, 1, 1, 0), p = "1"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_est(x = c(2, 3, 1, 1, 0)), 'argument "p" is missing, with no default')
  expect_error(spinar_est(x = c(2, 3, 1, 1, 0), p = 1.5), "Assertion on 'p' failed: Must be of type 'integerish'")
  expect_error(spinar_est(x = c(2, 3, 1, 1, 0), p = 0.5), "Assertion on 'p' failed: Must be of type 'integerish'")
  expect_error(spinar_est(x = c(2, 3, 1, 1, 0), p = -3), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spinar_est(x = c(2, 3, 1, 1, 0), p = c(2,1)), "Assertion on 'p' failed")
})

test_that("output", {
  ######################## size ########################
  expect_equal(length(spinar_est(x = c(2, 3, 1, 1), p = 2)), 6)
  expect_equal(length(spinar_est(x = c(2, 3, 1, 1, 0), p = 2)), 6)
  expect_equal(length(spinar_est(x = c(2, 3, 1, 1), p = 1)), 5)
  expect_equal(length(spinar_est(x = c(2, 3, 1, 1, 0), p = 1)), 5)
  expect_equal(length(spinar_est(x = c(1, 1, 1, 0), p = 1)), 3)
  expect_equal(spinar_est(x = c(2, 2, 2, 2, 2), p = 2), c(1, 0, 1, 0, 0))
  expect_equal(spinar_est(x = c(2, 2, 2, 2, 2), p = 1), c(1, 1, 0, 0))
  expect_equal(length(spinar_est(x=c(1, 3, 1, 1, 2, 2, 1, 2, 1, 4, 2, 2, 5, 5, 3, 2, 3, 3, 3, 3, 3, 2, 1, 2, 2, 4, 6, 5, 4, 8, 5, 8, 8, 10, 11), p=2)), 14)
  ######################## type ########################
  expect_type(spinar_est(x = c(2, 0, 1), p = 1), "double")
  expect_type(spinar_est(x = c(2, 0, 1), p = 1)[1], "double")
  ######################## values ########################
  expect_equal(sum(spinar_est(x=c(2, 0, 1), p=1)[2:4]), 1)
  expect_equal(sum(spinar_est(x=c(2, 9, 9, 9, 9, 1), p=1)[2:11]), 1)
  expect_equal(sum(spinar_est(x=c(2, 9, 9, 9, 9, 1), p=2)[3:12]), 1)
  expect_equal(sum(spinar_est(x=c(2, 9, 9, 9, 0, 1), p=2)[3:12]), 1)
  tmp = spinar_est(x=c(2, 8, 9, 9, 0, 1), p=2)
  expect_true(tmp[1] < 1)
  expect_true(tmp[1] > 0)
  expect_true(tmp[2] < 1)
  expect_true(tmp[2] > 0)
})
