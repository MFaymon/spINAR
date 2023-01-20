# spinar_boot <- function(x, p, B, setting, type, distr, M)
test_that("input", {
  tmp <- sample(1:10, 5)
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
})

test_that("output", {
  ######################## size ########################
  tmp = spinar_boot(sample(1:10, 10), 2, 2, setting = "sp")
  expect_true(length(tmp[[1]])>3)
  expect_equal(length(tmp), 2)
  ######################## type ########################
  expect_type(tmp, "list")
  ######################## values ########################
  expect_true(tmp[[1]][1] < 1)
  expect_true(tmp[[1]][1] > 0)
})







