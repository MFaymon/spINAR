test_that("spinar_est output size", {
  expect_equal(length(spinar_boot(c(1,2,3,0,0,0,2,2,5), 1, 50)), 50)
  expect_equal(length(spinar_boot(c(1,2,3,0,0,0,2,2,5), 1, 50)[[1]]), 7)
  # check for p = 2
  expect_equal(length(spinar_boot(c(1,2,3,0,0,0,2,2,5), 2, 50)[[1]]), 8)
  expect_equal(length(spinar_boot(c(1,2,3,0,0,0,2,2,5), 2, 50)), 50)
 })


test_that("check sum pmf equal 1", {
  expect_equal(sum(spinar_boot(c(1, 2, 3, 0, 0, 0, 2, 2, 5), 1, 10)[[1]][2:7]), 1)
})


test_that("alpha in range (0,1)", {
  expect_true(spinar_boot(c(1, 2, 3, 0, 0, 0, 1, 1, 5), 1, 10)[[1]][1] < 1)
})


test_that("Non-interger p, non-vector dat or missing inputs should error", {
  expect_error(spinar_boot(c(1), 1, 10), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
})


test_that("Input value p",{
  expect_error(spinar_boot(c(1,2,3,3,3,3), "1", 100), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
  expect_error(spinar_boot(c(1,2,3,3,3,3), 3, 100), "Assertion on 'p' failed: Element 1 is not <= 2.")
  expect_error(spinar_boot(c(1,2,3,3,3,3), 0, 100), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spinar_boot(c(1,2,3,3,9,9), "2", 100), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
})


test_that("Input value x",{
  expect_error(spinar_boot(c(1, 2, -3, 3, 3, 3), 1, 100))
})

test_that("Input value x",{
  expect_error(spinar_boot(c(1, 2, -3, 3, 3, 3), 1, 100))
})

test_that("Input value B",{
  expect_error(spinar_boot(c(1,2,3,3,3,3), 1, -10), "Assertion on 'B' failed: Element 1 is not >= 1.")
  expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 0), "Assertion on 'B' failed: Element 1 is not >= 1.")
})

test_that("Check the type of output obtained", {
  expect_type(spinar_boot(c(1, 2, 3, 3, 3, 3), 1, 20), "list")
})

