# spinar_boot <- function(x, p, B, setting, type = NA, distr = NA, M = 100, level = 0.05)
test_that("input", {
   tmp <- sample(1:10, 5, replace = TRUE)
   ######################## x ########################
   expect_error(spinar_boot(c(1), 1, 10, setting = "sp"), "Assertion on 'x' failed")
   expect_error(spinar_boot(c(1, 2, -3, 3, 3, 3), 1, 10, setting = "sp"))
   expect_error(spinar_boot(c(1, 2, 1.5, 3, 3, 3), 1, 10, setting = "sp"), "Assertion on 'x' failed: Must be of type 'integerish'")
   ######################## p ########################
   expect_error(spinar_boot(tmp, "1", 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
   expect_error(spinar_boot(tmp, 3, 100, setting = "sp"), "Assertion on 'p' failed")
   expect_error(spinar_boot(tmp, 0, 100, setting = "sp"), "Assertion on 'p' failed")
   expect_error(spinar_boot(tmp, 1.5, 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish'")
   expect_error(spinar_boot(tmp, "2", 100, setting = "sp"), "Assertion on 'p' failed: Must be of type 'integerish', not 'character'")
   ######################## B ########################
   expect_error(spinar_boot(tmp, 1, -10, setting = "sp"), "Assertion on 'B' failed")
   expect_error(spinar_boot(tmp, 1, 0, setting = "sp"), "Assertion on 'B' failed")
   expect_error(spinar_boot(tmp, 1, 10.5, setting = "sp"), "Assertion on 'B' failed: Must be of type 'integerish'")
   ######################## setting ########################
   expect_error(spinar_boot(tmp, 1, 10, setting = 0))
   ######################## type ########################
   expect_error(spinar_boot(tmp, 1, 10, setting = "p", type = 2))
   ######################## distr ########################
   expect_error(spinar_boot(tmp, 1, 10, setting = "p", type = "mom", distr = 21))
   ######################## M ########################
   expect_error(spinar_boot(tmp, 1, 10, setting = "p", type = "mom", distr = 'poi', M="text"), "Assertion on 'M' failed: Must be of type 'integerish', not 'character'.")
   expect_error(spinar_boot(tmp, 1, 10, setting = "p", type = "mom", distr = 'poi', M=-1), "Assertion on 'M' failed")
   ######################## level ########################
   expect_error(spinar_boot(tmp, 1, 10, setting = "p", type = "mom", distr = 'poi', M=2, level = 2), "Assertion on 'level' failed")
})


test_that("output", {
   ######################## size ########################
   set.seed(123)
   tmp = spinar_boot(x=sample(1:10, 10, replace=TRUE), p=2, B=2, setting = "sp")
   expect_true(expect_true(dim(tmp$x_star)[1]==2))
   expect_true(expect_true(dim(tmp$x_star)[2]==10))
   expect_true(sum(tmp$parameters_star[1,3:(length(tmp$parameters_star)/2)])==1)
   expect_true(length((tmp$parameters_star)) == 2*length(tmp$bs_ci))
   ######################## type ########################
   expect_type(tmp, "list")
   ######################## values ########################
   expect_true(tmp$parameters_star[1] < 1)
   expect_true(tmp$parameters_star[1] > 0)
})




