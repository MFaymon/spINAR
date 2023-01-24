# spinar_penal <- function(x, p, penal1=0, penal2=0)
test_that("input", {
  ######################## x ########################
  expect_error(spinar_penal(x=c(1), p=2, penal1=0, penal2=0),  "Assertion on 'x' failed")
  expect_error(spinar_penal(x=c(0), p=2, penal1=0, penal2= 0),  "Assertion on 'x' failed")
  expect_error(spinar_penal(x=c(2, 1), p=2, penal1=0, penal2=0),  "Assertion on 'x' failed")
  expect_error(spinar_penal(x=c(0.5, 0.3), p= 2, penal1=0, penal2=0),  "Assertion on 'x' failed: Must be of type 'integerish'")
  expect_error(spinar_penal(x=c(0.5, 0.3, 0.8, 0.2), p=2, penal1=0, penal2=0),  "Assertion on 'x' failed: Must be of type 'integerish'")
  expect_error(spinar_penal(x=c(-5, 3, 8, 2), p=2, penal1=0, penal2=0), "Assertion on 'x' failed")
  ######################## p ########################
  expect_error(spinar_penal(x=c(rpois(10, 0.5)), p=3, penal1=0, penal2=1),  "Assertion on 'p' failed")
  expect_error(spinar_penal(x=c(rpois(10, 0.5)), p="1", penal1=0, penal2=1),  "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_penal(x=c(rpois(20, 0.2)), p=0.5, penal1=0, penal2=1),  "Assertion on 'p' failed: Must be of type 'integerish'")
  ######################## penal1 ########################
  expect_error(spinar_penal(x=c(rpois(15, 0.4)), p=1, penal1="1", penal2=1), "Assertion on 'penal1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal(x=c(rpois(15, 0.3)), p=1, penal1=c(1, 2), penal2=1), "Assertion on 'penal1' failed: Must have length 1, but has length 2.")
  expect_error(spinar_penal(x=c(rpois(15, 0.2)), p=1, penal1=c(1, 2), penal2=1), "Assertion on 'penal1' failed: Must have length 1, but has length 2.")
  expect_error(spinar_penal(x=c(rpois(15, 0.1)), p=2, penal1="1", penal2=1), "Assertion on 'penal1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal(x=c(rpois(15, 0.9)), p=2, penal1=c(1, 2), penal2=1), "Assertion on 'penal1' failed")
  expect_error(spinar_penal(x=c(rpois(15, 0.8)), p=2, penal1=c(1, 2), penal2=1), "Assertion on 'penal1' failed")
  ######################## penal2 ########################
  expect_error(spinar_penal(x=c(rpois(15, 0.8)), p=1, penal1=1, penal2="1"), "Assertion on 'penal2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal(x=c(rpois(15, 0.9)), p=1, penal1=1, penal2=c(1, 2)), "Assertion on 'penal2' failed: Must have length 1, but has length 2.")
  expect_error(spinar_penal(x=c(rpois(15, 0.1)), p=1, penal1=1, penal2=c(1, 2)), "Assertion on 'penal2' failed: Must have length 1, but has length 2.")
  expect_error(spinar_penal(x=c(rpois(15, 0.3)), p=2, penal1=1, penal2="1"), "Assertion on 'penal2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal(x=c(rpois(15, 0.2)), p=2, penal1=1, penal2=c(1, 2)), "Assertion on 'penal2' failed: Must have length 1, but has length 2.")
  expect_error(spinar_penal(x=c(rpois(15, 0.4)), p=2, penal1=1, penal2=c(1, 2)), "Assertion on 'penal2' failed: Must have length 1, but has length 2.")
})

test_that("output", {
  ######################## size ########################
  expect_equal(length(spinar_penal(x = c(0, 1, 1, 1, 2, 2), p = 1, penal1 = 0, penal2 = 0)), 4)
  expect_equal(length(spinar_penal(x = c(2, 2, 4, 5, 6, 1), p = 2, penal1 = 0, penal2 = 0)), 9)
  expect_equal(length(spinar_penal(x = c(2, 2, 4, 5, 6, 3), p = 2, penal1 = 1, penal2 = 1)), 9)
  expect_equal(length(spinar_penal(x = c(0, 1, 2, 3, 3, 3), p = 1, penal1 = 3, penal2 = 1)), 5)
  expect_equal(length(spinar_penal(x = c(0, 1, 2, 3, 3, 3), p = 1, penal1 = -3, penal2 = 1)), 5)
  expect_equal(length(spinar_penal(x = c(0, 1, 2, 3, 3, 3), p = 1, penal1 = 0.5, penal2 = 1)), 5)
  expect_equal(length(spinar_penal(x = c(0, 1, 2, 3, 3, 3), p = 1, penal1 = -0.5, penal2 = 1)), 5)
  ######################## values ########################
  expect_equal(spinar_penal(x = c(0, 0, 0, 0, 0, 0, 0), p = 1, penal1 = 0, penal2 = 0), c(1, 1))
  expect_equal(spinar_penal(x = c(0, 0, 0, 0, 0, 0, 0), p = 1, penal1 = 1, penal2 = 0), c(1, 1))
  expect_equal(spinar_penal(x = c(0, 0, 0, 0, 0, 0, 0), p = 1, penal1 = 1, penal2 = 1), c(1, 1))
  expect_equal(spinar_penal(x = c(0, 0, 0, 0, 0, 0, 0), p = 2, penal1 = 1, penal2 = 1), c(1, 0, 1))
  expect_equal(spinar_penal(x = c(1, 1, 1, 1, 1, 1), p = 1, penal1 = 0, penal2 = 0), c(1, 1, 0))
  expect_equal(spinar_penal(x = c(1, 1, 1, 1, 1, 1), p = 2, penal1 = 0, penal2 = 0), c(1, 0, 1, 0))
  expect_true(spinar_penal(x=rpois(10, 5), p=2, penal1 = 10, penal2 = -5)[1] < 1)
  expect_true(spinar_penal(x=rpois(10, 4), p=2, penal1 = 10, penal2 = -5)[1] > 0)
  expect_true(spinar_penal(x=rpois(10, 8), p=2, penal1 = 10, penal2 = -5)[2] < 1)
  expect_true(spinar_penal(x=rpois(10, 6), p=2, penal1 = 10, penal2 = -5)[2] > 0)
  expect_equal(sum(spinar_penal(x=c(2, 0, 1), p=1, penal1 = 10, penal2 = -5)[2:4]), 1)
  expect_equal(sum(spinar_penal(x=c(2, 9, 9, 9, 9, 1), p=1, penal1 = 10, penal2 = -5)[2:11]), 1)
  expect_equal(sum(spinar_penal(x=c(2, 9, 9, 9, 9, 1), p=2, penal1 = 10, penal2 = -5)[3:12]), 1)
  expect_equal(sum(spinar_penal(x=c(2, 9, 9, 9, 0, 1), p=2, penal1 = 10, penal2 = -5)[3:12]), 1)
  ######################## type ########################
  expect_type(spinar_penal(x = c(0,1,2,3,3,3), 1,3,1), "double")
})


