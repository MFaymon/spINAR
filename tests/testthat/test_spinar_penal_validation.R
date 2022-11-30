test_that("input", {
  data_sample <- sample(1:40, 20)
  ######################## x ########################
  expect_error(spinar_penal_val(x = c(1), p = 1, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  expect_error(spinar_penal_val(x = c(1, 0), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_penal_val(x = c(2, 3), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  ######################## p ########################
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = "1", validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), validation = FALSE, penal1= 0, penal2= 0), 'argument "p" is missing, with no default')
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1.5, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 0.5, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = -3, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = c(2,1), validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must have length <= 1, but has length 2.")
  ######################## validation ########################
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = 'text'), "'validation' has to be logical")
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = 21), "'validation' has to be logical")
  expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = 'true'), "'validation' has to be logical")
  ######################## penal1 ########################
  # validation = TRUE and over = 'L1'
  expect_error(spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1", penal1= 0), "if over = L1, no value for penal1 allowed")
  #expect_error(spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1", penal2= 0), "if over = L1, no value for penal1 allowed")
  # validation = TRUE and over = 'L2'
  expect_warning(spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L2", folds = 2), 'value for penal1 is missing and is treated as zero')
  #expect_warning(spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L2", penal1 = 1, folds = 2), 'value for penal1 is missing and is treated as zero')
  #expect_warning(spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L2", penal2 = 1, folds = 2), 'value for penal1 is missing and is treated as zero')
  # validation = FALSE
  expect_warning(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE, penal1 = 1), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE, penal2 = 10), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  ######################## penal2 ########################
  #expect_warning(spinar_penal_val(x = rpois(20, 10), p = 1, validation = TRUE, over = "both", folds = 3), 'if over = both, input values for penal1 and penal2 are ignored')
  ######################## over ########################
  expect_error(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE), 'argument "over" is missing, with no default')
  expect_error(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE, over = "l1"))
  expect_error(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE, over = 4))
  ######################## folds ########################
  # expect_error(spinar_penal_val(x = c(0,1,2,3,3,4,4), p = 1, validation = TRUE, over = "both", folds = 3))
  # expect_error(spinar_penal_val(x = c(0,1,2,3,3,4,4), p = 1, validation = TRUE, over = "both", folds = 2))
  # expect_error(spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, over = "both", folds = 1))
  expect_error(spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, penal1 =0, penal2=0, folds = "1"))
  expect_error(spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, penal1 =0, penal2=0, folds = "pq"))
  expect_error(spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, penal1 =0, penal2=0, folds = TRUE))
  ######################## init1 ########################
  # expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = FALSE, penal1=0, penal2=0, init1 = "1"))
  # expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = FALSE, penal1=0, penal2=0, init1 = "p"))
  # expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = FALSE, penal1=0, penal2=0, init1 = FALSE))
  # expect_error(spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, penal1=0, penal2=0, over = "l1", init1 = c(5,6)))
  ######################## init2 ########################
})

test_that("output size", {
  ######################## size ########################
  expect_equal(length(spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = FALSE, penal1=0, penal2=0, over = "both", folds = 20, init1 = 1, init2 = 1)), 6)
  expect_equal(length(spinar_penal_val(x = data_sample, p = 1, validation = FALSE, penal1= 0, penal2= 0)), max(data_sample)+2)
  expect_equal(length(spinar_penal_val(x = data_sample, p = 2, validation = FALSE, penal1= 0, penal2= 0)), max(data_sample)+3)
  # expect_equal(length(spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1", penal2 =1)), max(data_sample)+2)
  ######################## type ########################
  expect_type(spinar_penal_val(x = data_sample, p = 1, validation = FALSE, penal1= 0, penal2= 0), "double")
  expect_type(spinar_penal_val(x = data_sample, p = 1, validation = FALSE, penal1= 0, penal2= 0)[1], "double")
  })


