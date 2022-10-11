test_that("spinar output size", {
  expect_equal(length(spinar(c(2, 3, 1, 1), 2)), 6)
  expect_equal(length(spinar(c(2, 3, 1, 1, 0), 2)), 6)
  expect_equal(length(spinar(c(2, 3, 1, 1), 1)), 5)
  expect_equal(length(spinar(c(2, 3, 1, 1, 0), 1)), 5)
  expect_equal(length(spinar(c(1, 1, 1, 0), 1)), 3)
})

test_that("check sum pmf equal 1", {
  expect_equal(sum(spinar(c(2, 0, 1), 1)[2:4]), 1)
  expect_equal(sum(spinar(c(2, 9, 9, 9, 9, 1), 1)[2:11]), 1)
  expect_equal(sum(spinar(c(2, 9, 9, 9, 9, 1), 2)[3:12]), 1)
  expect_equal(sum(spinar(c(2, 9, 9, 9, 0, 1), 2)[3:12]), 1)
})

test_that("alpha in range (0,1)", {
  expect_true(spinar(c(2, 8, 9, 9, 0, 1), 2)[1] < 1)
  expect_true(spinar(c(2, 8, 9, 9, 0, 1), 2)[1] > 0)
  expect_true(spinar(c(2, 8, 9, 9, 0, 1), 2)[2] < 1)
  expect_true(spinar(c(2, 8, 9, 9, 0, 1), 2)[1] > 0)
})

test_that("Non-interger p, non-vector dat or missing inputs should error", {
  expect_error(spinar(c(1), 1)) # dat with p+1 entries
  expect_error(spinar(c(1, 0), 2))
  expect_error(spinar(c(2, 3, 1, 1, 0), "1")) # lag p must be integer
  expect_error(spinar(c(2, 3, 1, 1, 0))) # argument "p" is missing
  expect_error(spinar(c(2, 3), 2)) # no enough big input
  expect_error(spinar(c(2, 2, 2, 2, 2), 2))
  expect_error(spinar(c(2, 2, 2, 2, 2), 1))
})

test_that("Check the type of output obtained", {
  expect_type(spinar(c(2, 0, 1), 1), "double")
  expect_type(spinar(c(2, 0, 1), 1)[1], "double")
})
