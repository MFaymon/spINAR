test_that("spinar_est output size", {
  expect_equal(length(spinar_est(c(2, 3, 1, 1), 2)), 6)
  expect_equal(length(spinar_est(c(2, 3, 1, 1, 0), 2)), 6)
  expect_equal(length(spinar_est(c(2, 3, 1, 1), 1)), 5)
  expect_equal(length(spinar_est(c(2, 3, 1, 1, 0), 1)), 5)
  expect_equal(length(spinar_est(c(1, 1, 1, 0), 1)), 3)
  expect_equal(spinar_est(c(2, 2, 2, 2, 2), 2), c(1, 0, 1, 0, 0))
  expect_equal(spinar_est(c(2, 2, 2, 2, 2), 1), c(1, 1, 0, 0))
  expect_equal(length(spinar_est(c(1, 3, 1, 1, 2, 2, 1, 2, 1, 4, 2, 2, 5, 5, 3, 2, 3, 3, 3, 3, 3, 2, 1, 2, 2, 4, 6, 5, 4, 8, 5, 8, 8, 10, 11), 2)), 14)
})

test_that("check sum pmf equal 1", {
  expect_equal(sum(spinar_est(c(2, 0, 1), 1)[2:4]), 1)
  expect_equal(sum(spinar_est(c(2, 9, 9, 9, 9, 1), 1)[2:11]), 1)
  expect_equal(sum(spinar_est(c(2, 9, 9, 9, 9, 1), 2)[3:12]), 1)
  expect_equal(sum(spinar_est(c(2, 9, 9, 9, 0, 1), 2)[3:12]), 1)
})

test_that("alpha in range (0,1)", {
  expect_true(spinar_est(c(2, 8, 9, 9, 0, 1), 2)[1] < 1)
  expect_true(spinar_est(c(2, 8, 9, 9, 0, 1), 2)[1] > 0)
  expect_true(spinar_est(c(2, 8, 9, 9, 0, 1), 2)[2] < 1)
  expect_true(spinar_est(c(2, 8, 9, 9, 0, 1), 2)[1] > 0)
})

test_that("Non-interger p, non-vector dat or missing inputs should error", {
  expect_error(spinar_est(c(1), 1), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  expect_error(spinar_est(c(1, 0), 2), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_est(c(2, 3, 1, 1, 0), "1"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_est(c(2, 3, 1, 1, 0)), 'argument "p" is missing, with no default')
  expect_error(spinar_est(c(2, 3), 2), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
})


test_that("Check the type of output obtained", {
  expect_type(spinar_est(c(2, 0, 1), 1), "double")
  expect_type(spinar_est(c(2, 0, 1), 1)[1], "double")
})
