# spinar_boot <- function(x, p, B, setting, type, distr, M)
 test_that("input", {
   ######################## x ########################
   expect_error(spinar_boot(c(1), 1, 10, setting = "sp"), "Assertion on 'x' failed: Must have length >= 2, but has length 1.")
   # P: checkmate lower = 0
   expect_error(spinar_boot(c(1, 2, -3, 3, 3, 3), 1, 10, setting = "sp"))
   expect_error(spinar_boot(c(1, 2, 1.5, 3, 3, 3), 1, 10, setting = "sp"), "Assertion on 'x' failed: Must be of type 'integerish'")
   ######################## p ########################
   expect_error(spinar_boot(c(1,2,3,3,3,3), "1", 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
   expect_error(spinar_boot(c(1,2,3,3,3,3), 3, 100, setting = "sp"), "Assertion on 'p' failed: Element 1 is not <= 2.")
   expect_error(spinar_boot(c(1,2,3,3,3,3), 0, 100, setting = "sp"), "Assertion on 'p' failed: Element 1 is not >= 1.")
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1.5, 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish'")
   expect_error(spinar_boot(c(1,2,3,3,9,9), "2", 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
   ######################## B ########################
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, -10, setting = "sp"), "Assertion on 'B' failed: Element 1 is not >= 1.")
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 0, setting = "sp"), "Assertion on 'B' failed: Element 1 is not >= 1.")
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 10.5, setting = "sp"), "Assertion on 'B' failed: Must be of type 'integerish'")
   ######################## setting ########################
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 10, setting = 0))
   ######################## type ########################
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 10, setting = "p", type = 2))
   ######################## distr ########################
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 10, setting = "p", type = "mom", distr = 21))
   ######################## M ########################
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 10, setting = "p", type = "mom", distr = 'poi', M="text"), "Assertion on 'M' failed: Must be of type 'integerish', not 'character'.")
   expect_error(spinar_boot(c(1,2,3,3,3,3), 1, 10, setting = "p", type = "mom", distr = 'poi', M=-1), "Assertion on 'M' failed: Element 1 is not >= 0.")
 })

 test_that("output", {
   ######################## size ########################
   # size for p = 1
   tmp = spinar_boot(c(1,2,3,0,0,0,2,2,5), 1, 50, setting = "sp")
   expect_equal(length(tmp), 50)
   expect_true(length(tmp[[1]])>2)
   # size for p = 2
   tmp = spinar_boot(c(1,2,3,0,0,0,2,2,5), 2, 50, setting = "sp")
   expect_true(length(tmp[[1]])>3)
   expect_equal(length(tmp), 50)
   ######################## type ########################
   expect_type(spinar_boot(c(1, 2, 3, 3, 3, 3), 1, 20, setting = "sp"), "list")
   ######################## values ########################
   # sum pmf equal 1
   aux1 <- spinar_boot(c(1, 2, 3, 0, 0, 0, 2, 2, 5), 1, 10, setting = "sp")[[1]]
   expect_equal(sum(aux1[2:length(aux1)]), 1)
   # alpha in range (0,1)
   expect_true(aux1[1] < 1)
   expect_true(aux1[1] > 0)
 })





