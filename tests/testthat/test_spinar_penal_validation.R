# to do
### data generation
# dat <- spinar_sim(50, 1, 0.5, dpois(0:20,1))
## penalized semiparametric estimation with validation over both penalization parameters
# spinar_penal_val(dat, 1, validation=TRUE, over="both")
# spinar_penal_val <- function(x, p, validation, penal1, penal2, over, folds = 10, init1 = 1, init2 = 1)
### data generation

# dat <- spinar_sim(100, 1, 0.5, dpois(0:20,1))
## penalized semiparametric estimation with validation over L1 penalization parameter
# spinar_penal_val(dat, 1, validation=TRUE, penal2 = 0.1, over="L1")
# spinar_penal_val <- function(x, p, validation, penal1, penal2, over, folds = 10, init1 = 1, init2 = 1){

test_that("expect warning",{
  # validation = FALSE and no parameters for penal1 nor penal2
  expect_warning(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE, penal1 = 1), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE, penal2 = 10), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  # validation = TRUE and over = 'L1'
  #expect_warning(spinar_penal_val(x = rpois(10, 10), p = 1, validation = TRUE, over = "L2", folds = 5), 'if over = both, input values for penal1 and penal2 are ignored')
  # validation = TRUE and over = 'L2'
  #expect_warning(spinar_penal_val(x = rpois(10, 10), p = 1, validation = TRUE, over = "L2", folds = 5), 'if over = both, input values for penal1 and penal2 are ignored')
  # validation = TRUE and over = "both
  #expect_warning(spinar_penal_val(x = rpois(12, 10), p = 1, validation = TRUE, over = "both", folds = 5), 'if over = both, input values for penal1 and penal2 are ignored')
})

test_that("input", {
  ######################## x ########################

  ######################## p ########################

  ######################## validation ########################

  ######################## penal1 ########################
  ######################## penal2 ########################
  ######################## over ########################
  expect_error(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE), 'argument "over" is missing, with no default')

  ######################## folds ########################
  ######################## init1 ########################
  ######################## init2 ########################
})

test_that("output size", {
  ######################## size ########################
  expect_equal(length(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE, penal1=0, penal2=0, over = "both", folds = 20, init1 = 1, init2 = 1)), 6)
})



