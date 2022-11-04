test_that("spinar sim output size", {
  expect_equal(length(spinar_penal(c(0,1,1,1,2,2),1,0,0)), 4)
  expect_equal(length(spinar_penal(c(2,2,4,5,6,-1),2,0,0)), 9)
  expect_equal(length(spinar_penal(c(2,2,4,5,6,3),2,1,1)), 9)
  # penal1 and penal2 can take any real value number
  expect_equal(length(spinar_penal(c(0,1,2,3,3,3), 1,3,1)), 5)
  expect_equal(length(spinar_penal(c(0,1,2,3,3,3), 1,-3,1)), 5)
  expect_equal(length(spinar_penal(c(0,1,2,3,3,3), 1,0.5,1)), 5)
  expect_equal(length(spinar_penal(c(0,1,2,3,3,3), 1,-0.5,1)),5)
})


test_that("vector with equal entries", {
  expect_equal(spinar_penal(c(0, 0, 0, 0, 0, 0, 0), 1, 0, 0), c(1, 1))
  expect_equal(spinar_penal(c(0, 0, 0, 0, 0, 0, 0), 1, 1, 0), c(1, 1))
  expect_equal(spinar_penal(c(0, 0, 0, 0, 0, 0, 0), 1, 1, 1), c(1, 1))
  expect_equal(spinar_penal(c(0, 0, 0, 0, 0, 0, 0), 2, 1, 1), c(1, 0, 1))
  expect_equal(spinar_penal(c(1, 1, 1, 1, 1, 1), 1, 0, 0), c(1, 1, 0))
  expect_equal(spinar_penal(c(1, 1, 1, 1, 1, 1), 2, 0, 0), c(1, 0, 1, 0))
})

test_that("input error on 'x'", {
  expect_error(spinar_penal(c(1), 2, 0, 0),  "Assertion on 'x' failed: Must have length >= 3, but has length 1.")
  expect_error(spinar_penal(c(0), 2, 0, 0),  "Assertion on 'x' failed: Must have length >= 3, but has length 1.")
  expect_error(spinar_penal(c(2, 1), 2, 0, 0),  "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_penal(c(0.5, 0.3), 2, 0, 0),  "Assertion on 'x' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spinar_penal(c(0.5, 0.3, 0.8, 0.2), 2, 0, 0),  "Assertion on 'x' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
})

test_that("input error on 'p'", {
  expect_error(spinar_penal(c(rpois(10, 0.5)), 3, 0, 1),  "Assertion on 'p' failed: Element 1 is not <= 2.")
  expect_error(spinar_penal(c(rpois(10, 0.5)), "1", 0, 1),  "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_penal(c(rpois(20, 0.2)), 0.5, 0, 1),  "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
})

test_that("input error on 'penal1' and 'penal2'", {
  expect_error(spinar_penal(c(rpois(15, 0.4)), 1, "1", 1), "Assertion on 'penal1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal(c(rpois(15, 0.4)), 1, c(1, 2), 1), "Assertion on 'penal1' failed: Must have length 1, but has length 2.")
  expect_error(spinar_penal(c(rpois(15, 0.4)), 1, c(1, 2)), "Assertion on 'penal1' failed: Must have length 1, but has length 2.")
})
