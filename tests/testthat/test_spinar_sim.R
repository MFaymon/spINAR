# spinar_sim <- function(n, p, alpha, pmf, prerun = 500)
test_that("warning",{
  ######################## sum pmf = 1 ########################
  expect_warning(spinar_sim(n = 100, p = 2, alpha = c(0.2, 0.3), pmf = c(0.1,0.5,0.2), prerun = 500), "Sum of pmf entries has been standardized to 1.")
  expect_warning(spinar_sim(n = 50, p = 1, alpha = 0.5, pmf = c(0.1,0.5,0.2)), "Sum of pmf entries has been standardized to 1.")
})

test_that("input", {
  ######################## n ########################
  expect_error(spinar_sim(n = -1, p = 1, alpha = 0.5, pmf = c(0.1,0.7,0.2)), "Assertion on 'n' failed: Element 1 is not >= 0.")
  expect_error(spinar_sim(n = 1.5, p = 1, alpha = 0.5, pmf = c(0.1,0.7,0.2)), "Assertion on 'n' failed: Must be of type 'integerish'")
  ######################## p ########################
  expect_error(spinar_sim(n = 50, p = "1", alpha = 0.5, pmf = c(0.1, 0.5, 0.4)),"Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_sim(n = 50, p = 0, alpha = 0.5, pmf = c(0.1,0.5,0.4)), "Assertion on 'p' failed")
  expect_error(spinar_sim(n = 50, p =  3, alpha =  0.5, pmf = c(0.1,0.5,0.4)), "Assertion on 'p' failed")
  expect_error(spinar_sim(n = 50, p =  1.2, alpha =  0.5, pmf = c(0.1,0.5,0.4)), "Assertion on 'p' failed: Must be of type 'integerish'")
  expect_error(spinar_sim(n = 50, p =  c(1,2), alpha =  0.5, pmf = c(0.1,0.5,0.4)))
  expect_error(spinar_sim(n = 50, p = 2, alpha = 0.5, pmf = c(0.1,0.7,0.2)), "Assertion on 'alpha' failed: Must have length 2, but has length 1.")
  expect_error(spinar_sim(n = 30, p = 1, alpha = c(0.5,0.2), pmf = c(0.1,0.7,0.2)), "Assertion on 'alpha' failed: Must have length 1, but has length 2.")
  expect_error(spinar_sim(n = 50, p = 1, alpha = c(0.1, 2), pmf = c(0.8,0.2)), "Assertion on 'alpha' failed: Must have length 1, but has length 2.")
  expect_error(spinar_sim(n = 60, p = 2, alpha = 0.2, pmf = c(0.5,0.5)), "Assertion on 'alpha' failed: Must have length 2, but has length 1.")
  ######################## alpha ########################
  expect_error(spinar_sim(n = 250, p = 2, alpha = c(4, 0.2), pmf = c(0.5,0.3,0.2)), "Assertion on 'alpha' failed: Element 1 is not <= 1.")
  expect_error(spinar_sim(n = 50, p = 2, alpha = c(-0.3, 0.2), pmf = c(0.5,0.3,0.2)), "Assertion on 'alpha' failed: Element 1 is not >= 0.")
  ######################## pmf ########################
  expect_error(spinar_sim(n = 40, p = 1, alpha = 0.5, pmf = 1), "Assertion on 'pmf' failed: Must have length >= 2, but has length 1.")
  ######################## prerun ########################
  expect_error(spinar_sim(30, 2, c(0.1, 0.2), c(0.25, 0.25, 0.25, 0.25), prerun = -10))
  ######################## n, p, prerun ##################
  expect_error(spinar_sim(n = 1, p = 2, alpha = c(0.2, 0.3), pmf = c(0.5, 0.3, 0.2), prerun = 1), "Sum of n and prerun must be greater than p.")
})

test_that("output", {
  ######################## size ########################
  expect_equal(length(spinar_sim(n = 10, p = 1, alpha = 0.5, pmf = c(0.1,0.5,0.4), prerun = 500)), 10)
  expect_equal(length(spinar_sim(n = 10, p = 1, alpha = 0.5, pmf = c(0.1,0.5,0.4))), 10)
  expect_equal(length(spinar_sim(n = 30, p = 2, alpha = c(0.1, 0.2), pmf = c(0.25, 0.25, 0.25, 0.25), prerun = 29)), 30)
  ######################## prerun ######################
  expect_equal(length(spinar_sim(n = 30, p = 2, alpha = c(0.1, 0.2), pmf = c(0.25, 0.25, 0.25, 0.25), prerun = 0)), 30)
  ######################## type ########################
  expect_type(spinar_sim(n = 30, p = 2, alpha = c(0.1, 0.2), pmf = c(0.25, 0.25, 0.25, 0.25), prerun = 29), 'double')
})





