#spinar_est_param <- function(x, p, type, distr)
test_that("input", {
  ######################## x ########################
  expect_error(spinar_est_param(x = c(1), p = 1, type = "mom", distr = "nb"), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  expect_error(spinar_est_param(x = c(1, 0), p = 2, type = "mom",  distr = "nb"), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_est_param(x = c(2, 3), p = 2, type = "mom",  distr = "nb"), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_est_param(x = c(-1, 2, 0, 4, 5), p = 1, type = "ml",  distr = "poi"), "Assertion on 'x' failed: Element 1 is not >= 0.")
  expect_error(spinar_est_param(x = c(-1, 2, 0, 4, 5), p = 2, type = "ml",  distr = "geo"), "Assertion on 'x' failed: Element 1 is not >= 0.")
  expect_error(spinar_est_param(x = c(1, 2.4, 0, 4, 5), p = 1, type = "ml",  distr = "poi"), "Assertion on 'x' failed: Must be of type 'integerish', but element 2 is not close to an integer.")
  expect_error(spinar_est_param(x = c(1, 2, 0, 4.1, 5), p = 2, type = "ml",  distr = "nb"), "Assertion on 'x' failed: Must be of type 'integerish', but element 4 is not close to an integer.")
  ######################## p ########################
  expect_error(spinar_est_param(x = c(2, 3, 1, 1, 0), p = "1", type = "mom",  distr = "nb"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_est_param(x = c(2, 3, 1, 1, 0), type = "mom",  distr = "nb"), 'argument "p" is missing, with no default')
  expect_error(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1.5, type = "mom",  distr = "nb"), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 0.5, type = "mom",  distr = "nb"), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spinar_est_param(x = c(2, 3, 1, 1, 0), p = -3, type = "mom",  distr = "nb"), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spinar_est_param(x = c(2, 3, 1, 1, 0), p = c(2,1), type = "mom",  distr = "nb"), "Assertion on 'p' failed: Must have length <= 1, but has length 2.")
  ######################## type ########################
  expect_error(spinar_est_param(x = rpois(10, 2), p = 1, type = "mon",  distr = "nb"))
  expect_error(spinar_est_param(x = rpois(10, 3), p = 1, type = 2,  distr = "nb"))
  expect_error(spinar_est_param(x = rpois(10, 4), p = 1, type = "MOM",  distr = "nb"))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 2, type = "mam",  distr = "nb"))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 2, type = 2,  distr = "nb"))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 2, type = mom,  distr = "nb"))
  ######################## distr ########################
  expect_error(spinar_est_param(x = rpois(10, 2), p = 1, type = "ml",  distr = poi), "object 'poi' not found")
  expect_error(spinar_est_param(x = rpois(10, 2), p = 1, type = "ml",  distr = "POI"))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 1, type = "ml",  distr = 2))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 2, type = "ml",  distr = poi))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 2, type = "ml",  distr = "POI"))
  expect_error(spinar_est_param(x = rpois(10, 2), p = 2, type = "ml",  distr = 2))
})

test_that("output size", {
  ######################## size ########################
  expect_equal(length(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="mom", distr = "geo")), 2)
  expect_equal(length(spinar_est_param(x = c(1, 1, 1, 0), p = 1, type="ml",distr = "geo")), 2)
  expect_equal(length(spinar_est_param(x=c(1, 3, 1, 1, 2, 2, 1, 2, 1, 4, 2, 2, 5, 5, 3, 2, 3, 3, 3, 3, 3, 2, 1, 2, 2, 4, 6, 5, 4, 8, 5, 8, 8, 10, 11), p=2, type="mom",distr = "poi")), 3)
  ######################## values ########################
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="mom", distr = "geo")[1]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="ml", distr = "geo")[1]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="mom", distr = "poi")[1]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="ml", distr = "poi")[1]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="mom", distr = "geo")[1]<1)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="ml", distr = "geo")[1]<1)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="mom", distr = "poi")[1]<1)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 1, type="ml", distr = "poi")[1]<1)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 2, type="mom", distr = "geo")[2]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 2, type="ml", distr = "geo")[2]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 2, type="mom", distr = "nb")[2]>0)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 2, type="ml", distr = "geo")[2]<1)
  expect_true(spinar_est_param(x = c(2, 3, 1, 1, 0), p = 2, type="mom", distr = "nb")[2]<1)
  ######################## type ########################
  expect_type(spinar_est_param(x = c(2, 0, 1,3,4,5), p = 1, type="ml", distr = "nb"), "double")
  expect_type(spinar_est_param(x = c(2, 0, 1,4,7,9, 1), p = 1, type="ml", distr = "nb")[1], "double")
})


