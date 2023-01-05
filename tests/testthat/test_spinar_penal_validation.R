test_that("input", {
  data_sample <- spINAR::spinar_sim(20, 1, 0.5, dpois(0:20,1))
  data_small <- sample(1:40, 5)
  ######################## x ########################
  expect_error(spINAR::spinar_penal_val(x = c(1), p = 1, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  expect_error(spINAR::spinar_penal_val(x = c(1, 0), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spINAR::spinar_penal_val(x = c(1, 2, 3, 4, -1, 2, 3), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Element 5 is not >= 0.")
  ######################## p ########################
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = "1", validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), validation = FALSE, penal1= 0, penal2= 0), 'argument "p" is missing, with no default')
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1.5, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 0.5, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', but element 1 is not close to an integer.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = -3, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Element 1 is not >= 1.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = c(2,1), validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must have length 1, but has length 2.")
  ######################## validation ########################
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = 'text'), "Assertion on 'validation' failed: Must be of type 'logical', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = 21), "Assertion on 'validation' failed: Must be of type 'logical', not 'double'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 2, validation = 'true'), "Assertion on 'validation' failed: Must be of type 'logical', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1), 'argument "validation" is missing, with no default')
  ######################## penal1 ########################
  expect_error(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L2", penal1 = "text"), "Assertion on 'penal1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = data_sample, p = 2, validation = TRUE, over = "L2", penal1 = c(1,2)), "Assertion on 'penal1' failed: Must have length 1, but has length 2.")
  ######################## penal2 ########################
  expect_error(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1", penal2 = "text"), "Assertion on 'penal2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = data_sample, p = 2, validation = TRUE, over = "L1", penal2 = c(1,2,2)), "Assertion on 'penal2' failed: Must have length 1, but has length 3.")
  ######################## over ########################
  expect_error(spINAR::spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE, over = "l1"))
  expect_error(spINAR::spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE, over = 4))
  expect_error(spINAR::spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE, over = NULL))
  expect_error(spINAR::spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE, over = NA))
  expect_error(spINAR::spinar_penal_val(x = c(0,2,3,3,4,4), p = 1, validation = TRUE))
  ######################## folds ########################
  expect_error(spINAR::spinar_penal_val(x = c(1,1,2,3,3,4,4), p = 1, validation = TRUE, over = "both", folds = 10), "Assertion on 'folds' failed: Element 1 is not <= 4.")
  expect_error(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "both", folds = 30), "Assertion on 'folds' failed: Element 1 is not <= 10.")
  expect_error(spINAR::spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, over = "both", penal1 =0, penal2=0, folds = "1"), "Assertion on 'folds' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, over = "both", penal1 =0, penal2=0, folds = "pq"), "Assertion on 'folds' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = rpois(10,0.5), p = 1, validation = TRUE, over = "both", penal1 =0, penal2=0, folds = TRUE), "Assertion on 'folds' failed: Must be of type 'integerish', not 'logical'.")
  ######################## init1 ########################
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, over = "both",  init1 = "1", folds = 2), "Assertion on 'init1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, over = "L1", penal1=0, penal2=0, init1 = "p", folds = 2), "Assertion on 'init1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, over = "L1", penal1=0, penal2=0, init1 = FALSE, folds = 2), "Assertion on 'init1' failed: Must be of type 'numeric', not 'logical'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 2, validation = TRUE, over = "L2", penal1=0, penal2=0, init1 = c(5,6), folds = 2), "Assertion on 'init1' failed: Must have length 1, but has length 2.")
  ######################## init2 ########################
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, over = "both",  init2 = "1", folds = 2), "Assertion on 'init2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, over = "L1", penal1=0.5, penal2=0, init2 = "p", folds = 2), "Assertion on 'init2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 1, validation = TRUE, over = "L1", penal1=0.8, penal2=1, init2 = FALSE, folds = 2), "Assertion on 'init2' failed: Must be of type 'numeric', not 'logical'.")
  expect_error(spINAR::spinar_penal_val(x = c(2, 3, 1, 1, 0), p = 2, validation = TRUE, over = "L2", penal1=-2, penal2=0.4, init2 = c(5,6), folds = 2), "Assertion on 'init2' failed: Must have length 1, but has length 2.")
  ######################## warnings ########################
  # validation = TRUE and over = 'L1'
  expect_error(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1", penal1= 0.5), "if over = L1, no value for penal1 allowed")
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1"), "value for penal2 is missing and is treated as zero")
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1", init1 = 5,  init2 = 5, folds = 10), "value for penal2 is missing and is treated as zero")
  # validation = TRUE and over = 'L2'
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L2", folds = 2), 'value for penal1 is missing and is treated as zero')
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L2", folds = 5), 'value for penal1 is missing and is treated as zero')
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "L1"), 'value for penal2 is missing and is treated as zero')
  # validation = TRUE and over = 'both'
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "both", penal1 = 1), "if over = both, input values for penal1 and penal2 are ignored")
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "both", penal2 = 1), "if over = both, input values for penal1 and penal2 are ignored")
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = TRUE, over = "both", penal1 = 1, penal2 = 3), "if over = both, input values for penal1 and penal2 are ignored")
  # validation = FALSE
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = FALSE, over = "L1"), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = FALSE, over = "L1", penal1 = 1), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = FALSE, over = "L1", penal2 = 10), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
  expect_warning(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = FALSE, over = "L1", penal2 = 10), 'values for penal1 or penal2 are missing, they are therefore treated as zero')

})

test_that("output size", {
  ######################## size ########################
  deterministic_sample <- c(1,2,2,1,2,1,2,1,1,3,4,4,3,4,3,4,5,3,2,3,4,3,3,4,2,2,1,1,3,2,2,1,4,3,3,2)
  data_sample <- spINAR::spinar_sim(20, 1, 0.5, dpois(0:20,1))
  aux1 <- spINAR::spinar_penal_val(x = deterministic_sample, p = 1, validation = TRUE, over = "L1", penal2 = 0.5)
  expect_equal(length(aux1$penal1_opt), 1)
  aux2 <- spINAR::spinar_penal_val(x = deterministic_sample, p = 2, validation = TRUE, over = "L2", penal1 = -2)
  expect_equal(length(aux2$penal2_opt), 1)
  expect_equal(length(spinar_penal_val(x = data_sample, p = 1, validation = FALSE, over = "both", penal1= 0, penal2= 0)), max(data_sample)+2)
  expect_equal(length(spinar_penal_val(x = data_sample, p = 2, validation = FALSE, penal1= 0, penal2= 0)), max(data_sample)+3)
  ######################## type ########################
  expect_type(spINAR::spinar_penal_val(x = data_sample, p = 1, validation = FALSE, over = "both", penal1= 0, penal2= 0), "double")
  })


