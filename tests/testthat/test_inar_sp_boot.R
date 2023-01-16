# spinar_boot <- function(x, p, B, setting, type, distr, M)
test_that("input", {
  ######################## x ########################
  expect_error(spINAR::spinar_boot(c(1), 1, 10, setting = "sp"), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  # P: checkmate lower = 0
  expect_error(spINAR::spinar_boot(c(1, 2, -3, 3, 3, 3), 1, 10, setting = "sp"))
  expect_error(spINAR::spinar_boot(c(1, 2, 1.5, 3, 3, 3), 1, 10, setting = "sp"), "Assertion on 'x' failed: Must be of type 'integerish', but element 3 is not close to an integer.")
  ######################## p ########################
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), "1", 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), 3, 100, setting = "sp"), "Assertion on 'p' failed: Element 1 is not <= 2.")
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), 0, 100, setting = "sp"), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), 1.5, 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spINAR::spinar_boot(c(1,2,3,3,9,9), "2", 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
  ######################## B ########################
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), 1, -10, setting = "sp"), "Assertion on 'B' failed: Element 1 is not >= 1.")
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), 1, 0, setting = "sp"), "Assertion on 'B' failed: Element 1 is not >= 1.")
  expect_error(spINAR::spinar_boot(c(1,2,3,3,3,3), 1, 10.5, setting = "sp"), "Assertion on 'B' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
})

test_that("output", {
  ######################## size ########################
  # size for p = 1
  #expect_equal(length(spINAR::spinar_boot(c(1,2,3,0,0,0,2,2,5), 1, 50, setting = "sp")), 50)
  #expect_equal(length(spINAR::spinar_boot(c(1,2,3,0,0,0,2,2,5), 1, 50, setting = "sp")[[1]]), 7)
  # size for p = 2
  #expect_equal(length(spINAR::spinar_boot(c(1,2,3,0,0,0,2,2,5), 2, 50, setting = "sp")[[1]]), 8)
  #expect_equal(length(spINAR::spinar_boot(c(1,2,3,0,0,0,2,2,5), 2, 50, setting = "sp")), 50)
  ######################## type ########################
  expect_type(spinar_boot(c(1, 2, 3, 3, 3, 3), 1, 20, setting = "sp"), "list")
  ######################## values ########################
  # sum pmf equal 1
  #expect_equal(sum(spINAR::spinar_boot(c(1, 2, 3, 0, 0, 0, 2, 2, 5), 1, 10, setting = "sp")[[1]][2:7]), 1)
  # alpha in range (0,1)
  expect_true(spINAR::spinar_boot(c(1, 2, 3, 0, 0, 0, 1, 1, 5), 1, 10, setting = "sp")[[1]][1] < 1)
})




