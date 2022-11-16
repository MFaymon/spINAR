test_that("spinar sim output size", {
  expect_equal(length(spINAR::spinar_sim(n = 10, p = 1, alpha = 0.5, pmf = c(0.1,0.5,0.4), prerun = 500)), 10)
  expect_equal(length(spINAR::spinar_sim(n = 10, p = 1, alpha = 0.5, pmf = c(0.1,0.5,0.4))), 10)
  expect_equal(length(spINAR::spinar_sim(n = 30, p = 2, alpha = c(0.1, 0.2), pmf = c(0.25, 0.25, 0.25, 0.25), prerun = 29)), 30)
})

test_that("expect warning sum pmf = 1",{
  expect_warning(spINAR::spinar_sim(n = 100, p = 2, alpha = c(0.2, 0.3), pmf = c(0.1,0.5,0.2), prerun = 500), "Sum of pmf entries has been standardized to 1.")
  expect_warning(spINAR::spinar_sim(n = 50, p = 1, alpha = 0.5, pmf = c(0.1,0.5,0.2)), "Sum of pmf entries has been standardized to 1.")
})

test_that("incorrect size of pmf",{
  expect_error(spINAR::spinar_sim(n = 40, p = 1, alpha = 0.5, pmf = 1), "Assertion on 'pmf' failed: Must have length >= 2, but has length 1.")
})

test_that("errors in input p or in input alpha", {
  # p is not in {0,1}
  expect_error(spINAR::spinar_sim(50, "1", 0.5, c(0.1, 0.5, 0.4)),"Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spINAR::spinar_sim(50, 0, 0.5, c(0.1,0.5,0.4)), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spINAR::spinar_sim(50, 3, 0.5, c(0.1,0.5,0.4)), "Assertion on 'p' failed: Element 1 is not <= 2.")
  # the size of alpha is different that the size of p
  expect_error(spINAR::spinar_sim(n = 50, p = 2, alpha = 0.5, pmf = c(0.1,0.7,0.2)), "Assertion on 'alpha' failed: Must have length 2, but has length 1.")
  expect_error(spINAR::spinar_sim(n = 30, p = 1, alpha = c(0.5,0.2), pmf = c(0.1,0.7,0.2)), "Assertion on 'alpha' failed: Must have length 1, but has length 2.")
  expect_error(spINAR::spinar_sim(n = 50, p = 1, alpha = c(0.1, 2), pmf = c(0.8,0.2)), "Assertion on 'alpha' failed: Must have length 1, but has length 2.")
  expect_error(spINAR::spinar_sim(n = 60, p = 2, alpha = 0.2, pmf = c(0.5,0.5)), "Assertion on 'alpha' failed: Must have length 2, but has length 1.")
  # pmf in (0.1)
  expect_error(spINAR::spinar_sim(n = 250, p = 2, alpha = c(4, 0.2), pmf = c(0.5,0.3,0.2)), "Assertion on 'alpha' failed: Element 1 is not <= 1.")
  expect_error(spINAR::spinar_sim(n = 50, p = 2, alpha = c(-0.3, 0.2), pmf = c(0.5,0.3,0.2)), "Assertion on 'alpha' failed: Element 1 is not >= 0.")
  # what happens if pmf = c(0,0,0,0,1)
})

test_that("errors in prerun", {
  # prerun
  #expect_error(spINAR::spinar_sim(30, 2, c(0.1, 0.2), c(0.25, 0.25, 0.25, 0.25), prerun = 30)) #this should be an error?
  expect_error(spINAR::spinar_sim(30, 2, c(0.1, 0.2), c(0.25, 0.25, 0.25, 0.25), prerun = -10))
  #expect_error(spINAR::spinar_sim(800, 2, c(0.1, 0.2), c(0.25, 0.25, 0.25, 0.25), prerun = 900)) #this should be an error?
})

test_that("errors in input n", {
  expect_error(spinar_sim(n = -1, p = 1, alpha = 0.5, pmf = c(0.1,0.7,0.2)), "Assertion on 'n' failed: Element 1 is not >= 0.")
})




