# spinar_penal_val <- function(x, p, validation, penal1=NA, penal2=NA, over=NA, folds = 10, init1 = 1, init2 = 1)
test_that("input", {
  set.seed(123)
  tmp0 <- sample(1:10, 4, replace = TRUE)
  set.seed(123)
  tmp1 <- spinar_sim(20, 1, 0.5, dpois(0:20,1))
  ######################## x ########################
  expect_error(spinar_penal_val(x = c(1), p = 1, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
  expect_error(spinar_penal_val(x = c(2, 3), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Must have length >= 3, but has length 2.")
  expect_error(spinar_penal_val(x = c(1, 2, 3, 4, -1, 2, 3), p = 2, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'x' failed: Element 5 is not >= 0.")
  ######################## p ########################
  expect_error(spinar_penal_val(x = tmp0, p = "1", validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'.")
  expect_error(spinar_penal_val(x = tmp0, validation = FALSE, penal1= 0, penal2= 0), 'argument "p" is missing, with no default')
  expect_error(spinar_penal_val(x = tmp0, p = 1.5, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must be of type 'integerish'")
  expect_error(spinar_penal_val(x = tmp0, p = -3, validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed")
  expect_error(spinar_penal_val(x = tmp0, p = c(2,1), validation = FALSE, penal1= 0, penal2= 0), "Assertion on 'p' failed: Must have length 1, but has length 2.")
  ######################## validation ########################
  expect_error(spinar_penal_val(x = tmp0, p = 1, validation = 'text'), "Assertion on 'validation' failed: Must be of type 'logical', not 'character'.")
  expect_error(spinar_penal_val(x = tmp0, p = 1, validation = 21), "Assertion on 'validation' failed: Must be of type 'logical', not 'double'.")
  expect_error(spinar_penal_val(x = tmp0, p = 1), 'argument "validation" is missing, with no default')
  ######################## penal1 ########################
  expect_error(spinar_penal_val(x = tmp1, p = 1, validation = TRUE, over = "L2", penal1 = "text"), "Assertion on 'penal1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal_val(x = tmp1, p = 2, validation = TRUE, over = "L2", penal1 = c(1,2)), "Assertion on 'penal1' failed: Must have length 1, but has length 2.")
  ######################## penal2 ########################
  expect_error(spinar_penal_val(x = tmp1, p = 1, validation = TRUE, over = "L1", penal2 = "text"), "Assertion on 'penal2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal_val(x = tmp1, p = 2, validation = TRUE, over = "L1", penal2 = c(1,2,2)), "Assertion on 'penal2' failed: Must have length 1, but has length 3.")
  ######################## over ########################
  expect_error(spinar_penal_val(x = tmp1, p = 1, validation = TRUE, over = "l1"), "Assertion on 'over' failed")
  expect_error(spinar_penal_val(x = tmp1, p = 1, validation = TRUE, over = 4), "Assertion on 'over' failed")
  expect_error(spinar_penal_val(x = tmp1, p = 1, validation = TRUE, over = NULL), "Assertion on 'over' failed")
  ######################## folds ########################
  expect_error(spinar_penal_val(x = c(1,1,1), p = 2, validation = TRUE, over = "both", folds = 3), "Assertion on 'folds' failed")
  expect_error(spinar_penal_val(x = tmp1, p = 1, validation = TRUE, over = "both", penal1 =0, penal2=0, folds = "1"), "Assertion on 'folds' failed: Must be of type 'integerish', not 'character'.")
  ######################## init1 ########################
  expect_error(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "both",  init1 = "1", folds = 2), "Assertion on 'init1' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "L2", penal1=0, penal2=0, init1 = c(5,6), folds = 2), "Assertion on 'init1' failed: Must have length 1, but has length 2.")
  ######################## init2 ########################
  expect_error(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "both",  init2 = "1", folds = 2), "Assertion on 'init2' failed: Must be of type 'numeric', not 'character'.")
  expect_error(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "L2", penal1=-2, penal2=0.4, init2 = c(5,6), folds = 2), "Assertion on 'init2' failed: Must have length 1, but has length 2.")
  ######################## warnings ########################
  # validation = TRUE and over = 'L1'
   expect_error(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "L1", penal1= 0.5, folds = 2), "if over = L1, no value for penal1 allowed")
   expect_warning(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "L1", init1 = 5,  init2 = 5, folds = 2), "value for penal2 is missing and is treated as zero")
   # validation = TRUE and over = 'L2'
   expect_error(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "L2", penal2 =0, folds = 2, init2=0.5), "if over = L2, no value for penal2 allowed")
   expect_warning(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "L2", folds = 2, init2=0.5), 'value for penal1 is missing and is treated as zero')
   # validation = TRUE and over = 'both'
   expect_warning(spinar_penal_val(x = tmp0, p = 1, validation = TRUE, over = "both", penal1 = 1, folds=2, init1 =0 , init2 = 0.05), "if over = both, input values for penal1 and penal2 are ignored")
  # validation = FALSE
  expect_warning(spinar_penal_val(x = tmp0, p = 1, validation = FALSE, over = "L1", folds=2), 'values for penal1 or penal2 are missing, they are therefore treated as zero')
})
test_that("output", {
  ######################## size ########################
  set.seed(123)
  tmp2 <- sample(1:10, 6, replace = TRUE)
  aux1 <- spinar_penal_val(x = tmp2, p = 2, validation = FALSE, penal1= 0, penal2= 0, folds=2)
  expect_equal(length(aux1), max(tmp2)+3)
  aux2 <- spinar_penal_val(x = tmp2, p = 2, validation = TRUE, over = "L1", penal2= 0, init1=0, folds=2)
  expect_equal(length(aux2$parameters), max(tmp2)+3)
  expect_true(length(aux2$penal1_opt)==1)
  aux3 <- spinar_penal_val(x = tmp2, p = 2, validation = TRUE, over = "L2", penal1= 0, init2=0, folds=2)
  expect_true(length(aux3$penal2_opt)==1)
  expect_equal(length(aux3$parameters), max(tmp2)+3)
  aux4 <- spinar_penal_val(x = tmp2, p = 2, validation = TRUE, over = "both", folds=2, init1 = 0.95, init2 = 1.05)
  aux4_false <- spinar_penal_val(x = tmp2, p = 2, validation = TRUE, over = "both", folds=2, init1 = 0.95, init2 = 1.05, progress = FALSE)
  expect_identical(aux4, aux4_false)
  expect_equal(length(aux4$parameters), max(tmp2)+3)
  expect_true(length(aux4$penal1_opt)==1)
  expect_true(length(aux4$penal2_opt)==1)
  aux5 <- spinar_penal_val(x = c(1,2,1,2,3,2,1), p = 1, validation = TRUE, over = "L1", penal2 = 0.5, folds = 3)
  expect_true(length(aux5$parameters)==5)
  aux6 <- spinar_penal_val(x = c(1,2,1,2,3,2,1), p = 1, validation = TRUE, over = "L1", penal2 = 0.5, folds = 2)
  expect_true(length(aux6$parameters)==5)
  aux7 <- spinar_penal_val(x = c(1,1,1,1,1,1,1), p = 1, validation = TRUE, over = "L1", penal2 = 0.5, folds = 3)
  expect_true(length(aux7$parameters)==3)
  expect_true(length(aux7$penal1_opt)==1)
  ######################## type ########################
  expect_type(aux1, "double")
})
